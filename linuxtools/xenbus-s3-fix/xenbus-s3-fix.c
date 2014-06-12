/*
 * Copyright (c) 2012 Citrix Systems, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/**
 * xenbus driver resume function (which is supposed to be done on resume from file)
 * gets incorrectly called on S3 resume, breaking domain S3. Fix is to stop calling resume function after s3.
 * Also workarounds of going to hibernate not working due to resume being called before it.
 * Also rides netfront state machine to do reconnect cycle after S4.
 */

#include <linux/module.h>
#include <linux/kobject.h>
#include <linux/sysfs.h>
#include <linux/klist.h>
#include <linux/device.h>
#include <linux/version.h>
#include <xen/xenbus.h>

/* externally defined in xenbus driver */
/* WARNING: in newer kernels they have changed suspend signature to not include pm_message_t */
extern int xenbus_dev_resume(struct device *dev);
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,39)
extern int xenbus_dev_suspend(struct device *dev);
#else
extern int xenbus_dev_suspend(struct device *dev, pm_message_t pm);
#endif

static void (*original_otherend_changed) (struct xenbus_device *dev, enum xenbus_state backend_state) = NULL;

static int reconnect_count = 0;

struct sysfs_dirent {
	atomic_t s_count;
	atomic_t s_active;
#ifdef CONFIG_DEBUG_LOCK_ALLOC
        struct lockdep_map      dep_map;
#endif
        struct sysfs_dirent     *s_parent;
        struct sysfs_dirent     *s_sibling;
        const char              *s_name;
	
        const void              *s_ns; /* namespace tag */
	struct kobject *hack;
};

struct subsys_private {
	struct kset subsys;
	struct kset *devices_kset;

	struct kset *drivers_kset;
	struct klist klist_devices;
	struct klist klist_drivers;
	struct blocking_notifier_head bus_notifier;
	unsigned int drivers_autoprobe:1;
	struct bus_type *bus;

	struct list_head class_interfaces;
	struct kset glue_dirs;
	struct mutex class_mutex;
	struct class *class;
};

#define to_subsys_private(obj) container_of(obj, struct subsys_private, subsys.kobj)

static struct bus_type *get_xen_bus(void)
{
	struct sysfs_dirent *d, *d2;
	struct kobject *k;
	struct subsys_private *p;
	struct bus_type *b;
	d = kernel_kobj->sd ? kernel_kobj->sd->s_parent : NULL;
	printk("D: %p\n", d);
        if (!d) {
            return NULL;
        }
#if ( LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,35) )
	d = sysfs_get_dirent(d, NULL, "bus");
#else
	d = sysfs_get_dirent(d, "bus");
#endif
	printk("D (root): %p\n", d);
        if (!d) {
            return NULL;
        }
#if ( LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,35) )
	d2 = sysfs_get_dirent(d, NULL, "xen");
#else
	d2 = sysfs_get_dirent(d, "xen");
#endif
	printk("D2 (xen): %p\n", d2);
	sysfs_put(d);
        if (!d2) {
            return NULL;
        }
	k = d2->hack;
	sysfs_put(d2);
	printk("K: %p\n", k);
        if (!k) {
            return NULL;
        }
	p = to_subsys_private(k);
	printk("P: %p\n", p);
        if (!p) {
            return NULL;
        }
	b = p->bus;
	printk("B: %p\n", b);
	return b;
}

static int handle_xenbus_dev_restore(struct device *dev)
{
    struct xenbus_device *xendev = to_xenbus_device(dev);
    int rv = xenbus_dev_resume(dev);
    if (rv) {
        return rv;
    }

    if (dev->driver && dev->driver->name) {
        printk("driver: %s\n", dev->driver->name);
        if (!strcmp(dev->driver->name, "vif")) {
            printk("netfront workaround: closing frontend after device restore\n");
            /* toggle flag to make sure we reconnect next time the backend closes */
            ++reconnect_count;
            /* notify backend */
            xenbus_switch_state(xendev, XenbusStateClosing);
        }
    }
    return 0;
}

static int handle_xenbus_dev_freeze(struct device *dev)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,39)
    return xenbus_dev_suspend(dev);
#else
    pm_message_t pm = { 0 };
    return xenbus_dev_suspend(dev, pm);
#endif
}

static int handle_xenbus_dev_cancel(struct device *dev)
{
    return 0;
}

static void handle_otherend_changed(struct xenbus_device *dev, enum xenbus_state backend_state)
{
    original_otherend_changed(dev, backend_state);
    if (dev->dev.driver && !strcmp(dev->dev.driver->name, "vif")) {
        /* automatically try to reopen driver after backend closure */
        if (backend_state == XenbusStateClosed) {
            if (reconnect_count) {
                --reconnect_count;
                xenbus_switch_state(dev, XenbusStateInitialising);
            }
        }
    }
}

static int ride_xenbus_state( struct device *dev, void *data )
{
    struct xenbus_driver *xendrv;
    if (dev->driver) {
        printk("ride xenbus state for %s\n", dev->driver->name);
        xendrv = to_xenbus_driver(dev->driver);
        if (!strcmp(dev->driver->name, "vif")) {
            printk("found vif driver\n");
            if (!original_otherend_changed) {
                printk("replacing vif driver xenbus state machine\n");
                original_otherend_changed = xendrv->otherend_changed;
                xendrv->otherend_changed = handle_otherend_changed;
            }
        }
    }
    return 0;
}

static const struct dev_pm_ops xenbus_pm_ops = {
    .suspend = NULL,
    .resume  = NULL,
    .freeze  = handle_xenbus_dev_freeze,
    .thaw    = handle_xenbus_dev_cancel,
    .restore = handle_xenbus_dev_restore,
};

static int __init
fix_s3_init (void)
{
    struct bus_type *xen;

    printk("initialising xen bus s3/s4 fixes\n");
    xen = get_xen_bus();
    printk("found xenbus @ %p\n", xen);
    if (xen) {
        /* frontend drivers set complete bollocks here, erase that */
        xen->suspend = NULL;
        xen->resume  = NULL;
        /* replace with pm operations which are less bollocks */
        xen->pm = &xenbus_pm_ops;
        /* ride the xenbus state machine like a pro */
        bus_for_each_dev(xen, NULL, NULL, ride_xenbus_state);
    }
    return 0;
}

static void __exit
fix_s3_cleanup (void)
{
}

module_init (fix_s3_init);
module_exit (fix_s3_cleanup);
MODULE_LICENSE("GPL");

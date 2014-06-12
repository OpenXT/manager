#
# Copyright (c) 2012 Citrix Systems, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

XENBUS_S3_FIX_VERSION=1.0
obj-m += xenbus-s3-fix.o
KVERSION := $(shell uname -r)

all:
	make -C /lib/modules/${KVERSION}/build M=$(PWD) modules -I$(PWD) EXTRA_CFLAGS="-g -I$(PWD)/include -I$(PWD)"

install:

module_install:
	install -d ${DESTDIR}/lib/modules/${KVERSION}/kernel/driver/xen
	install -m 0644 xenbus-s3-fix.ko ${DESTDIR}/lib/modules/${KVERSION}/kernel/driver/xen
	depmod -a

clean:
	make -C /lib/modules/${KVERSION}/build M=$(PWD) clean

dkms.conf: dkms/dkms.conf.in
control: dkms/control.in
	sed -re 's/%XENBUS_S3_FIX_VERSION%/${XENBUS_S3_FIX_VERSION}/g' $< > $@

dpkg-build: dkms.conf control

dpkg_data="tmp-deb/data"
dpkg_control="tmp-deb/control"
xenbus-s3-fix-dkms_${XENBUS_S3_FIX_VERSION}_all.deb: dpkg-build
	rm -rf tmp-deb
	mkdir -p ${dpkg_data} ${dpkg_control} ${dpkg_data}/usr/src/xenbus-s3-fix-${XENBUS_S3_FIX_VERSION}
	
	cp -r dkms.conf Makefile xenbus-s3-fix.c ${dpkg_data}/usr/src/xenbus-s3-fix-${XENBUS_S3_FIX_VERSION}
	install -m 0644 -D dkms/lintian ${dpkg_data}/usr/share/lintian/overrides/xenbus-s3-fix-dkms
	install -m 0755 -d ${dpkg_data}/usr/share/doc/xenbus-s3-fix-dkms
	(cd ${dpkg_data}/usr/share/doc/xenbus-s3-fix-dkms && ln -s ../../common-licenses/GPL-2 copyright)
	( cd tmp-deb/data && find . -type f -exec md5sum {} \;) | sed -re 's| ./||g' > ${dpkg_control}/md5sums
	tar  --create --owner=root --group=root --gzip --file=tmp-deb/data.tar.gz --directory=tmp-deb/data .
	
	cp dkms/debian-binary tmp-deb
	cp dkms/postinst dkms/prerm ${dpkg_control}
	cp control ${dpkg_control}
	tar  --create --owner=root --group=root --gzip --file=tmp-deb/control.tar.gz --directory=tmp-deb/control .
	
	( cd tmp-deb && ar rcD ../$@ debian-binary control.tar.gz data.tar.gz)
dpkg: xenbus-s3-fix-dkms_${XENBUS_S3_FIX_VERSION}_all.deb

lintian: xenbus-s3-fix-dkms_${XENBUS_S3_FIX_VERSION}_all.deb
	lintian $<

dkms_init: dkms.conf
	sudo rm -rf /usr/src/xenbus-s3-fix*
	sudo mkdir /usr/src/xenbus-s3-fix-${XENBUS_S3_FIX_VERSION}
	sudo cp -r $(PWD)/* /usr/src/xenbus-s3-fix-${XENBUS_S3_FIX_VERSION}

dkms_add:
	sudo dkms add -m xenbus-s3-fix -v ${XENBUS_S3_FIX_VERSION}

dkms_remove:
	sudo dkms remove -m xenbus-s3-fix -v ${XENBUS_S3_FIX_VERSION} --all

dkms_build:
	sudo dkms build -m xenbus-s3-fix -v ${XENBUS_S3_FIX_VERSION}

dkms_mkdeb:
	sudo dkms mkdeb -m xenbus-s3-fix -v ${XENBUS_S3_FIX_VERSION}

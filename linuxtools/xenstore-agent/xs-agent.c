/*
 * Copyright (c) 2013 Citrix Systems, Inc.
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

#include <err.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <xenstore.h>


#define PM_NODE "control/shutdown"

int main(int argc, char **argv)
{
    struct xs_handle *xsh;
    char **vec = NULL;
    char *type;
    unsigned int num;
    unsigned int len;
    xs_transaction_t th;
    int rc, sysr;

    rc = 0;
    xsh = xs_open(0);
    if (!xsh) {
        fprintf(stderr, "error on xs_open\n");
        return 1;   
    }

    if (!xs_watch(xsh, PM_NODE, PM_NODE)) {
        fprintf(stderr, "unable to set the watch\n");
        rc = 1;
        goto clean;
    }
    
    for (;;) {
        
        /* this needs to block */
        vec = xs_read_watch(xsh, &num);
        if (!vec)
            continue; /* FIXME treating errors ? */


        th = xs_transaction_start(xsh);
        if (th == XBT_NULL)
            goto frevec;
        type = xs_read(xsh, th, vec[XS_WATCH_PATH], &len);
        xs_transaction_end(xsh, th, false);
        if (!type)
            goto frevec;
        
        th = xs_transaction_start(xsh);
        if (th == XBT_NULL)
            goto freetype;
        xs_rm(xsh, th, vec[XS_WATCH_PATH]);
        xs_transaction_end(xsh, th, false);


        if (!strcmp(type, "halt"))
            sysr = system("shutdown -h now");

        else if (!strcmp(type, "reboot"))
            sysr = system("reboot");

        else if (!strcmp(type, "s3"))
            sysr = system("pm-suspend");

        else if (!strcmp(type, "hibernate"))
            sysr = system("pm-hibernate");

        else
            fprintf(stderr, "invalid pm event received: %s\n", type);

    freetype:
        free(type);
    frevec:
        free(vec);
    }

    xs_unwatch(xsh, PM_NODE, PM_NODE);

clean:
    xs_close(xsh);
    return rc;  
}

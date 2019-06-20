/*
 * Copyright (c) 2011 Citrix Systems, Inc.
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

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <xen/argo.h>
#include <libargo.h>

int open_argo_socket(domid_t domid, uint32_t port)
{
    int s, r = -1;
    xen_argo_addr_t addr_in = {
        .domain_id = domid,
        .aport = port
    };

    s = argo_socket(SOCK_STREAM);
    if (s < 0) {
        perror("argo_socket");
        return -1;
    }

    while (r) {
        r = argo_connect(s, &addr_in);
        if (r) {
            if ((errno == EINTR) || (errno == EAGAIN)) {
                continue;
            }
            perror("argo_connect");
            close(s);
            return -1;
        }
}

    return s;
}


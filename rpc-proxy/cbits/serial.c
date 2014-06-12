/* XenClient Xen Manager
   Copyright (C) 2010 Citrix Systems, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

int open_serial_raw(const char *dev)
{
    struct termios options;
    int fd, rv;
    size_t written;
    char buf[64];

    fd = open(dev, O_RDWR | O_NOCTTY | O_NDELAY);
    if (fd < 0) {
        char msg[256];
        snprintf(msg, sizeof(msg), "open_serial_raw: Unable to open %s: ", dev);
        perror(msg);
        return fd;
    }
    tcgetattr(fd, &options);
    cfmakeraw(&options);
    options.c_cflag &= ~(CSIZE | CSTOPB | PARENB | CLOCAL | CRTSCTS);
    options.c_cflag |= CLOCAL;
    options.c_cflag |= CS8;
    options.c_cflag |= CREAD;
    options.c_iflag = IGNBRK | IGNPAR;
    options.c_oflag = 0;
    options.c_lflag = 0;
    options.c_cc[VMIN] = 1;
    options.c_cc[VTIME] = 0;
    tcsetattr(fd, TCSAFLUSH, &options);

    /* drain the thing */
    if (fcntl(fd, F_SETFL, FNDELAY) < 0) {
        perror("fcntl");
        return -1;
    }
    for (;;) {
        rv = read(fd, &buf, 1);
        if (rv < 0 && errno != EAGAIN) {
            perror("read");
            return -1;
        } else if (!rv || errno == EAGAIN) {
            break;
        }
    }
    /* set blocking behaviour */
    if (fcntl(fd, F_SETFL, 0) < 0) {
        perror("fcntl");
        return -1;
    }
    return fd;
}

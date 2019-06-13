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

#include "minidbus.h"
#include "minidbus-dispatch.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <xen/argo.h>

#define DEFAULT_HOST_DOMID 0
#define DEFAULT_HOST_PORT  5556

static int wait_x_and_notify(dbus_msg_queue *q)
{
    dbus_msg *notify;
    dbus_sig notify_sig = { DBUS_INVALID };
    struct sockaddr_un name;
    int tries=0;

    /* try to find connection to X on local machine */
    int fd = socket( PF_LOCAL, SOCK_STREAM, 0 );
    if (fd < 0) {
        perror("socket:");
        return -1;
    }
    name.sun_family = AF_LOCAL;
    strcpy(name.sun_path, "/tmp/.X11-unix/X0");
    while ( connect(fd, (struct sockaddr*)&name, sizeof(name)) < 0 ) {
        sleep(1);
        ++tries;
        if (tries>=60) {
            close(fd);
            return -1;
        }
    }

    close(fd);

    /* we have X conn, notify dom0 */
    sleep(3);

    notify = dbus_msg_new_signal(
        dbus_msg_queue_next_serial(q),
        "/", "com.citrix.xenclient.guest",
        "xorg_running" );
    if (!notify) {
        return -1;
    }

    dbus_msg_body_add( notify, 128 );
    dbus_msg_set_signature( notify, &notify_sig );
    dbus_msg_queue_send_later( q, notify );
    return 0;
}

static int io_read(void *priv, void *buf, uint32_t count)
{
    int rv;
    int fd = (int) priv;
    ssize_t num_read = 0;

    printf("expecting to read %d bytes\n", count);
    while ( num_read < count ) {
        rv = read( fd, buf, count - num_read );
        if ( rv < 0 ) {
            perror("io_read");
            if ( errno == EINTR || errno == EAGAIN ) continue;
            return rv;
        }
        printf("%d bytes read\n", rv);
        buf += rv;
        num_read += rv;
    }
    return 0;
}

static int io_write(void *priv, const void *buf, uint32_t count)
{
    int rv, fd = (int) priv;
    ssize_t num_written = 0;

    while ( num_written < count ) {
        rv = write( fd, buf, count - num_written );
        if ( rv < 0 ) {
            perror("io_write");
            if ( errno == EINTR || errno == EAGAIN ) continue;
            return rv;
        }
        printf("%d bytes written\n", rv);
        buf += rv;
        num_written += rv;
    }
    return 0;
}

static int init_argo_dbus_io(domid_t domid, uint32_t port, dbus_io *io)
{
    int fd;

    fd = open_argo_socket(domid, port);
    if (fd < 0) {
        return fd;
    }
    io->priv = (void*) fd;
    io->io_read = io_read;
    io->io_write = io_write;
    return 0;
}

static int read_line(dbus_io *dio, char *buf, int len)
{
    int offset = 0, found = 0, r = 0;
    memset(buf, '\0', len);
    do {
        if (offset == len)
            return -9;
        r = dio->io_read(dio->priv, buf + offset, 1);
        if (r)
            return r;
        if (buf[offset] == '\n')
            found = 1;
        offset++;
    } while (!found);
    return (found) ? 0 : -10;
}

static void chomp( char *str )
{
    int i = strlen(str) - 1;
    while (i >= 0 && (str[i] == '\r' || str[i] == '\n')) {
        str[i--] = 0;
    }
}

static int domain_dbus_auth(dbus_io *dio, char *auth)
{
    char buf[256];
    int len;
    int r = 0;
    char *endp = NULL, *p;
    len = sprintf(buf, "%cAUTH %s\r\n", '\0', auth);
    r = dio->io_write(dio->priv, buf, len);
    if (r)
        return r;

    /* BEGIN command */
    r = read_line(dio, buf, 256);
    if (r != 0) return r;
    r = dio->io_write(dio->priv, "BEGIN\r\n", 7);
    return r;
}

static int notify_agent_started(dbus_io *io, dbus_msg_queue *q)
{
    int rv;
    dbus_msg *msg;
    msg = dbus_msg_new_signal( dbus_msg_queue_next_serial(q),
                               "/",
                               "com.citrix.xenclient.guest",
                               "agent_started" );
    if (!msg) {
        fprintf(stderr, "out of memory\n");
        return -1;
    }
    rv = dbus_msg_send( io, msg );
    if (rv < 0) {
        return rv;
    }
    return 0;
}

static int init_service(dbus_io *io, dbus_msg_queue *q, char *name)
{
    int rv;
    dbus_sig signature;
    dbus_msg *reply = NULL;
    dbus_msg *msg;
    printf("hello dbus!\n");
    msg = dbus_msg_new_method_call( dbus_msg_queue_next_serial(q),
                                   "org.freedesktop.DBus", "/org/freedesktop/DBus",
                                   "org.freedesktop.DBus", "Hello");
    if (!msg) {
        fprintf(stderr, "out of memory\n");
        return -1;
    }
    rv = dbus_msg_send_and_wait_reply(q, io, msg, &reply);
    if (rv < 0) {
        return rv;
    }

    printf("requesting service name %s\n", name);
    msg = dbus_msg_new_method_call( dbus_msg_queue_next_serial(q),
                                    "org.freedesktop.DBus", "/org/freedesktop/DBus",
                                    "org.freedesktop.DBus", "RequestName" );
    if (!msg) {
        fprintf(stderr, "out of memory\n");
        return -1;
    }

    signature.a[0] = DBUS_STRING;
    signature.a[1] = DBUS_UINT32;
    signature.a[2] = DBUS_INVALID;
    dbus_msg_set_signature(msg, &signature);
    dbus_msg_body_add(msg, 4096);
    dbus_msg_body_add_string(msg, name);
    dbus_msg_body_add_uint32(msg, 0x3); // flags = REPLACE_EXISTING,ALLOW_REPLACEMENT
    printf("sending & waiting reply for service name acquisition..\n");
    rv = dbus_msg_send_and_wait_reply(q, io, msg, &reply);
    if (rv < 0) {
        return rv;
    }
    if (reply) {
        free(reply);
    }
    printf("sending start notification\n");
    rv = notify_agent_started( io, q );
    if (rv < 0) {
        fprintf(stderr, "sending start notification FAILED\n");
        return rv;
    }
    return 0;
}

int negotiate_connection( dbus_io *io, dbus_msg_queue *queue )
{
    char *p;
    int rv;
    printf("negotating dbus connection\n");
    if ( domain_dbus_auth(io, "EXTERNAL 30") < 0 ) {
        fprintf(stderr, "FAILED dbus handshake\n");
        return -1;
    }
    return init_service( io, queue, "guest" );
}

static int usage(const char *name)
{
    fprintf(stderr, "Usage: %s [[domid] [port]]\n", name);
    return 1;
}

int main(int argc, char **argv)
{
    dbus_io io;
    dbus_msg_queue *queue = NULL;
    int rv;
    domid_t host = DEFAULT_HOST_DOMID;
    uint32_t port = DEFAULT_HOST_PORT;
    char *endptr;
    queue = dbus_msg_queue_init();

    if (argc == 3) {
        host = strtol(argv[1], &endptr, 10);
        if ((endptr == argv[1]) || (host < 0)) {
            fprintf(stderr, "Invalid domid.\n");
            return 2;
        }
        port = strtol(argv[2], &endptr, 10);
        if ((endptr == argv[2]) || (port < 0)) {
            fprintf(stderr, "Invalid port.");
            return 2;
        }
    } else if (argc > 1) {
        return usage(argv[0]);
    }

    if ( init_argo_dbus_io(host, port, &io) < 0 ) {
        fprintf(stderr, "FAILED to init dbus over argo\n");
        exit( 1 );
    }
    if ( negotiate_connection(&io, queue) < 0 ) {
        fprintf(stderr, "FAILED to negotiate dbus connection\n");
        exit( 1 );
    }
    rv = wait_x_and_notify( queue );
    if ( rv < 0 ) {
        fprintf(stderr, "FAILED to connect to X\n");
        /* lets treat it as non fatal */
    }
    rv = agent( &io, queue );
    if ( rv < 0 ) {
        fprintf(stderr, "FAILED to run agent: %d\n", rv);
        exit( 1 );
    }
    return 0;
}


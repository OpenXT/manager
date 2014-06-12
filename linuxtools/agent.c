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

#include "minidbus.h"
#include "minidbus-dispatch.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

extern int negotiate_connection( dbus_io *io, dbus_msg_queue *queue );

typedef enum {
    RUNNING, SLEEP, HIBERNATE, SHUTDOWN, REBOOT
} power_state;

static power_state target_power_state = RUNNING;

static void transit_power_state();

static
int process( dbus_io *io, dbus_msg_queue *q )
{
    int rv;
    for (;;) {
        /* send outgoing messages */
        printf("sending, %d outgoing messages in queue\n", dbus_msg_queue_get_num_outgoing(q));
        rv = dbus_msg_queue_write( q, io );
        if (rv < 0) {
            fprintf(stderr, "error writing messages to IO: %d\n", rv);
            return rv;
        }
        /* make sure output buffers are flushed */
        fsync((int)io->priv);

        /* transit to desired power state, possibly commiting seppuku */
        transit_power_state( io, q );
        /* read message from transport */
        rv = dbus_msg_queue_read( q, io );
        if (rv < 0) {
            fprintf(stderr, "error reading messages from IO: %d\n", rv);
            return rv;
        }
        /* dispatch read messages */
        do {
            printf("dispatching, %d incoming messages in queue\n", dbus_msg_queue_get_num_incoming(q));
            rv = dbus_msg_queue_dispatch( q );
        } while (rv > 0);
        if (rv < 0) {
            fprintf(stderr, "error dispatching messages: %d\n", rv);
            return rv;
        }
    }
}

static
int sys_power_state( const char *state )
{
    int rv, fd = open( "/sys/power/state", O_WRONLY );
    if (fd < 0) {
        perror("opening /sys/power/state");
        return fd;
    }
    rv = write( fd, state, strlen(state) );
    if (rv < 0) {
        perror("writing to /sys/power/state");
        return rv;
    }
    close( fd );
    return 0;
}

static
void transit_power_state( dbus_io *io, dbus_msg_queue *queue )
{
    int rv = 0;
    switch (target_power_state) {
    case SLEEP:
        fprintf(stderr, "entering s3\n");
        target_power_state = RUNNING;
        /* i feel bad about this */
        sleep(2);
        rv = system("pm-suspend");
        break;
    case HIBERNATE:
        fprintf(stderr, "entering s4\n");
        target_power_state = RUNNING;
        /* i feel bad about this */
        sleep(2);
        rv = sys_power_state( "disk" );
        /* after return from hibernate we need to renegotiate dbus connection */
        negotiate_connection( io, queue );
        break;
    case SHUTDOWN:
        fprintf(stderr, "shutting down\n");
        /* i feel bad about this */
        sleep(2);
        rv = system("shutdown -h now");
        break;
    case REBOOT:
        fprintf(stderr, "rebooting\n");
        /* i feel bad about this */
        sleep(2);
        rv = system("reboot");
        break;
    case RUNNING:
        break;
    default:
        fprintf(stderr, "unknown power state %d\n", target_power_state);
        break;
    }
    if (rv != 0) {
        fprintf(stderr, "error %d trying to change power state to %d\n", rv, target_power_state);
    }
}

static
void make_ack( dbus_msg **ack, dbus_msg_queue *q, dbus_msg *msg )
{
    uint32_t serial = dbus_msg_queue_next_serial( q );
    *ack = dbus_msg_new( serial );
    dbus_msg_set_destination( *ack, msg->sender );
    (*ack)->type = DBUS_TYPE_METHOD_RETURN;
    (*ack)->reply_serial = msg->serial;
}

static
void request_sleep( dbus_msg_queue *q, dbus_msg *msg, dbus_msg **reply )
{
    printf("request s3 received\n");
    target_power_state = SLEEP;
    make_ack( reply, q, msg );
}

static
void request_hibernate( dbus_msg_queue *q, dbus_msg *msg, dbus_msg **reply )
{
    printf("request s4 received\n");
    target_power_state = HIBERNATE;
    make_ack( reply, q, msg );
}

static
void request_shutdown( dbus_msg_queue *q, dbus_msg *msg, dbus_msg **reply )
{
    printf("request shutdown received\n");
    target_power_state = SHUTDOWN;
    make_ack( reply, q, msg );
}

static
void request_reboot( dbus_msg_queue *q, dbus_msg *msg, dbus_msg **reply )
{
    printf("request reboot received\n");
    target_power_state = REBOOT;
    make_ack( reply, q, msg );
}

int agent( dbus_io *io, dbus_msg_queue *q )
{
    printf("setting up handlers\n");
    dbus_msg_queue_set_method_handler( q, "com.citrix.xenclient.guest", "request_sleep", request_sleep );
    dbus_msg_queue_set_method_handler( q, "com.citrix.xenclient.guest", "request_hibernate", request_hibernate );
    dbus_msg_queue_set_method_handler( q, "com.citrix.xenclient.guest", "request_shutdown", request_shutdown );
    dbus_msg_queue_set_method_handler( q, "com.citrix.xenclient.guest", "request_reboot", request_reboot );
    printf("running dispatch queue\n");
    return process( io, q );
}

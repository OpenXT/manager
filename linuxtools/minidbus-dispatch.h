/*
 * Copyright (c) 2010 Citrix Systems, Inc.
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

#ifndef MINIDBUS_DISPATCH_H
#define MINIDBUS_DISPATCH_H

#include "minidbus.h"

struct dbus_msg_queue_;
typedef struct dbus_msg_queue_ dbus_msg_queue;

typedef void (*dbus_method_handler)( dbus_msg_queue *q, dbus_msg *msg, dbus_msg **reply );
typedef void (*dbus_signal_handler)( dbus_msg_queue *q, dbus_msg *msg );
typedef void (*dbus_replyto_handler)( dbus_msg_queue *q, dbus_msg *msg );

/* make message queue */
dbus_msg_queue *dbus_msg_queue_init();

/* free message queue */
void dbus_msg_queue_free(dbus_msg_queue *q);

/* setup dbus method call handler */
int dbus_msg_queue_set_method_handler(dbus_msg_queue *q, const char *interface, const char *member, dbus_method_handler handler);

/* setup dbus signal handler */
int dbus_msg_queue_set_signal_handler(dbus_msg_queue *q, const char *interface, const char *member, dbus_signal_handler handler);

/* fetch message from transport */
int dbus_msg_queue_read(dbus_msg_queue *q, dbus_io *io);

/* write pending messages to transport */
int dbus_msg_queue_write(dbus_msg_queue *q, dbus_io *io);

/* writes outgoing messages to transport, then read one message from transport */
int dbus_msg_queue_read_write(dbus_msg_queue *q, dbus_io *io);

/* dispatches buffered incoming messages to handlers, returns number of messages left to dispatch or negative on error */
int dbus_msg_queue_dispatch(dbus_msg_queue *q);

/* read write dispatch */
int dbus_msg_queue_read_write_dispatch(dbus_msg_queue *q, dbus_io *io);

/* queue message for sending */
int dbus_msg_queue_send_later(dbus_msg_queue *q, dbus_msg *msg);

/* send with reply and block */
int dbus_msg_send_and_wait_reply(dbus_msg_queue *q, dbus_io *io, dbus_msg *msg, dbus_msg **reply);

/* get a new message serial number */
uint32_t dbus_msg_queue_next_serial(dbus_msg_queue *q);

int dbus_msg_queue_get_num_incoming(dbus_msg_queue *q);
int dbus_msg_queue_get_num_outgoing(dbus_msg_queue *q);

#endif

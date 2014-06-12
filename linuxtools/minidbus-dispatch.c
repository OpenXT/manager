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

#include "minidbus-dispatch.h"
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

typedef enum { HANDLER_TYPE_INVALID, METHOD_HANDLER, SIGNAL_HANDLER } handler_type;
typedef enum { MATCHER_TYPE_INVALID, MATCH_METHOD_CALL, MATCH_SIGNAL, MATCH_REPLY_TO } matcher_type;

typedef struct {
    matcher_type type;
    union {
        struct {
            char *interface;
            char *member;
        } match_method_call;
        struct {
            char *interface;
            char *member;
        } match_signal;
        struct {
            uint32_t serial;
        } match_reply_to;
    };
} matcher;

typedef struct {
    handler_type type;
    matcher match;
    union {
        dbus_method_handler method_handler;
        dbus_signal_handler signal_handler;
        dbus_replyto_handler replyto_handler;
    };
} handler_entry;


struct dbus_msg_queue_ {
    uint32_t serial;

    /* incoming messages queued */
    int incoming_count;
    dbus_msg **incoming;

    /* outgoing messages queued */
    int outgoing_count;
    dbus_msg **outgoing;

    /* message handlers */
    int handler_count;
    handler_entry *handler_entries;
};

int dbus_msg_queue_get_num_incoming(dbus_msg_queue *q)
{
    return q->incoming_count;
}

int dbus_msg_queue_get_num_outgoing(dbus_msg_queue *q)
{
    return q->outgoing_count;
}

static
int array_entry_remove( int index, size_t elem_size, void **array_, int *array_count_ )
{
    void *array = *array_;
    int array_count = *array_count_;
    char *dst = array + elem_size*index;
    char *src = array + elem_size*(index+1);

    memmove( array + elem_size*index,
             array + elem_size*(index+1),
             (array_count-index-1)*elem_size );
    array_count--;
    array = realloc( array, array_count*elem_size );
    if (!array && array_count*elem_size > 0) {
        return -ENOMEM;
    }

    *array_ = array;
    *array_count_ = array_count;
    return 0;
}

static
int array_entry_insert( int index, void *elem, size_t elem_size, void **array_, int *array_count_ )
{
    void *array = *array_;
    int array_count = *array_count_;

    array_count++;
    array = realloc( array, array_count*elem_size );
    if (!array) {
        return -ENOMEM;
    }

    memmove( array + elem_size*(index+1),
             array + elem_size*index,
             (array_count-index-1)*elem_size );

    memcpy( array + elem_size*index, elem, elem_size );

    *array_ = array;
    *array_count_ = array_count;
}

static
int matcher_equal(matcher *a, matcher *b)
{
    if (a->type != b->type) {
        return 0;
    }
    switch (a->type) {
    case MATCH_METHOD_CALL:
        return
            strcmp(a->match_method_call.interface, b->match_method_call.interface) == 0 &&
            strcmp(a->match_method_call.member, b->match_method_call.member) == 0;
    case MATCH_SIGNAL:
        return
            strcmp(a->match_signal.interface, b->match_signal.interface) == 0 &&
            strcmp(a->match_signal.member, b->match_signal.member) == 0;
    case MATCH_REPLY_TO:
        return a->match_reply_to.serial == b->match_reply_to.serial;
    default:
        return 0;
    }
}

static
handler_entry *find_handler_entry_for_matcher(dbus_msg_queue *q, matcher *m, int *index)
{
    int i;
    *index = -1;
    for (i = 0; i < q->handler_count; ++i) {
        if (matcher_equal(m, &q->handler_entries[i].match)) {
            *index = i;
            return &q->handler_entries[i];
        }
    }
    return NULL;
}


static
void free_handler_entry( handler_entry *e )
{
    switch (e->match.type) {
    case MATCH_METHOD_CALL:
        free( e->match.match_method_call.interface );
        free( e->match.match_method_call.member );
        break;
    case MATCH_SIGNAL:
        free( e->match.match_signal.interface );
        free( e->match.match_signal.member );
        break;
    default:
        break;
    }
}

static
int queue_incoming(dbus_msg_queue *q, dbus_msg *msg)
{
    return array_entry_insert( q->incoming_count, &msg, sizeof(dbus_msg*), (void**)&q->incoming, &q->incoming_count );
}

static
int queue_outgoing(dbus_msg_queue *q, dbus_msg *msg)
{
    return array_entry_insert( q->outgoing_count, &msg, sizeof(dbus_msg*), (void**)&q->outgoing, &q->outgoing_count );
}

static
handler_entry *add_handler_entry( dbus_msg_queue *q )
{
    handler_entry empty = { 0 };
    array_entry_insert( q->handler_count, &empty, sizeof(empty), (void**)&q->handler_entries, &q->handler_count );
    return &q->handler_entries[q->handler_count-1];
}


static
void remove_handler_entry( dbus_msg_queue *q, int index )
{
    array_entry_remove( index, sizeof(handler_entry), (void**)&q->handler_entries, &q->handler_count );
}

static
handler_entry *put_handler_entry_for_matcher(dbus_msg_queue *q, matcher *m)
{
    int index;
    handler_entry *e = find_handler_entry_for_matcher( q, m, &index );
    if (e) {
        remove_handler_entry( q, index );
    }

    e = add_handler_entry( q );
    memcpy( &e->match, m, sizeof(matcher) );
    return e;
}

static
int dispatch_msg(dbus_msg_queue *q, dbus_msg *msg)
{
    int i, rv;

    for (i = 0; i < q->handler_count; ++i) {
        handler_entry *h = &q->handler_entries[i];
        matcher *m = &h->match;
        if (m->type == MATCH_REPLY_TO && m->match_reply_to.serial == msg->reply_serial) {
            if (h->replyto_handler) {
                h->replyto_handler(q,msg);
            }
        }

        switch (msg->type) {
        case DBUS_TYPE_METHOD_CALL:
            if (m->type == MATCH_METHOD_CALL &&
                msg->interface && strcmp(msg->interface,m->match_method_call.interface)==0 &&
                msg->method && strcmp(msg->method,m->match_method_call.member)==0)
            {
                if (h->method_handler) {
                    dbus_msg *reply = NULL;
                    h->method_handler( q, msg, &reply );
                    if (reply) {
                        /* queue reply to send */
                        rv = queue_outgoing(q, reply);
                        if (rv < 0) {
                            return rv;
                        }
                    }
                }
            }
            break;
        case DBUS_TYPE_SIGNAL:
            if (m->type == MATCH_SIGNAL &&
                msg->interface && strcmp(msg->interface,m->match_signal.interface)==0 &&
                msg->method && strcmp(msg->method,m->match_signal.member)==0)
            {
                if (h->signal_handler) {
                    h->signal_handler( q, msg );
                }
            }
            break;
        default:
            fprintf(stderr, "invalid message type\n");
            return -EINVAL;
        }
    }
    return 0;
}

dbus_msg_queue *dbus_msg_queue_init()
{
    dbus_msg_queue *q = calloc(1, sizeof(dbus_msg_queue) );
    memset( q, 0, sizeof(dbus_msg_queue) );
    q->serial = 1;
    return q;
}

/* free message queue */
void dbus_msg_queue_free(dbus_msg_queue *q)
{
    int i;
    for (i = 0; i < q->incoming_count; ++i) {
        dbus_msg_free( q->incoming[i] );
    }
    free( q->incoming );
    q->incoming_count = 0;
    q->incoming = NULL;

    for (i = 0; i < q->outgoing_count; ++i) {
        dbus_msg_free( q->outgoing[i] );
    }
    free( q->outgoing );
    q->outgoing_count = 0;
    q->outgoing = NULL;

    for (i = 0; i < q->handler_count; ++i) {
        free_handler_entry( &q->handler_entries[i] );
    }
    q->handler_count = 0;
    q->handler_entries = NULL;
}


/* setup dbus method call handler */
int dbus_msg_queue_set_method_handler(dbus_msg_queue *q, const char *interface, const char *member, dbus_method_handler handler)
{
    handler_entry *e = NULL;
    matcher m = { 0 };

    m.type = MATCH_METHOD_CALL;
    m.match_method_call.interface = strdup( interface );
    m.match_method_call.member = strdup( member );
    printf("add method handler %s.%s\n", m.match_method_call.interface, m.match_method_call.member);

    e = put_handler_entry_for_matcher( q, &m );
    if (!e) {
        goto error;
    }

    e->type = MATCH_METHOD_CALL;
    e->method_handler = handler;

    return 0;
error:
    free( m.match_method_call.interface );
    free( m.match_method_call.member );
    return -ENOMEM;
}

/* setup dbus signal handler */
int dbus_msg_queue_set_signal_handler(dbus_msg_queue *q, const char *interface, const char *member, dbus_signal_handler handler)
                                 {
    handler_entry *e = NULL;
    matcher m = { 0 };

    m.type = MATCH_SIGNAL;
    m.match_signal.interface = strdup( interface );
    m.match_signal.member = strdup( member );

    e = put_handler_entry_for_matcher( q, &m );
    if (!e) {
        goto error;
    }

    e->type = MATCH_SIGNAL;
    e->signal_handler = handler;
    return 0;
error:
    free( m.match_signal.interface );
    free( m.match_signal.member );
    return -ENOMEM;
}

int dbus_msg_queue_write(dbus_msg_queue *q, dbus_io *io)
{
    while ( q->outgoing_count > 0 ) {
        int rv = dbus_msg_send( io, q->outgoing[0] );
        if (rv < 0) {
            return rv;
        }
        dbus_msg_free( q->outgoing[0] );
        rv = array_entry_remove( 0, sizeof(dbus_msg*), (void**)&q->outgoing, &q->outgoing_count );
        if (rv < 0) {
            return rv;
        }
    }
    return 0;
}

int dbus_msg_queue_read(dbus_msg_queue *q, dbus_io *io)
{
    dbus_msg *msg;
    int rv = dbus_msg_recv( io, &msg );
    if (rv < 0) {
        return rv;
    }
    return queue_incoming( q, msg );
}

/* writes outgoing messages to transport, then reads message from transport */
int dbus_msg_queue_read_write(dbus_msg_queue *q, dbus_io *io)
{
    int rv = dbus_msg_queue_write( q, io );
    if (rv < 0) {
        return rv;
    }
    return dbus_msg_queue_read( q, io );
}

/* dispatches buffered incoming messages to handlers, returns number of messages left to dispatch or negative on error */
int dbus_msg_queue_dispatch(dbus_msg_queue *q)
{
    dbus_msg *msg = NULL;
    int rv = 0;
    if ( q->incoming_count == 0 ) {
        return 0;
    }
    msg = q->incoming[0];
    rv = array_entry_remove( 0, sizeof(dbus_msg*), (void**)&q->incoming, &q->incoming_count );
    if (rv < 0) {
        fprintf(stderr, "remove element failed\n");
        return rv;
    }
    rv = dispatch_msg( q, msg );
    if (rv < 0) {
        fprintf(stderr, "dispatch_msg failed\n");
        dbus_msg_free( msg );
        return rv;
    }
    dbus_msg_free( msg );
    return q->incoming_count;
}

/* return 1 if should continue to read/write/dispatch, 0 if disconnected, negative on error */
int dbus_msg_queue_read_write_dispatch(dbus_msg_queue *q, dbus_io *io)
{
    int rv = dbus_msg_queue_read_write( q, io );
    if (rv < 0) {
        fprintf(stderr, "read_write failed\n");
        return rv;
    }
    do {
        rv = dbus_msg_queue_dispatch( q );
        if (rv < 0) {
            fprintf(stderr, "dispatch failed\n");
            return rv;
        }
    } while (rv > 0);
    return 1;
}

uint32_t dbus_msg_queue_next_serial(dbus_msg_queue *q)
{
    uint32_t s = q->serial++;
    return s;
}

int dbus_msg_send_and_wait_reply(dbus_msg_queue *q, dbus_io *io, dbus_msg *msg, dbus_msg **reply)
{
    int rv;
    *reply = NULL;
    rv = dbus_msg_send(io, msg);
    if (rv < 0) {
        return rv;
    }
    for (;;) {
        dbus_msg *recv_msg = NULL;
        rv = dbus_msg_recv( io, &recv_msg );
        if (rv < 0) {
            return rv;
        }
        if (recv_msg->reply_serial == msg->serial) {
            *reply = recv_msg;
            return 0;
        }
        rv = queue_incoming( q, recv_msg );
        if (rv < 0) {
            return rv;
        }
    }
}

int dbus_msg_queue_send_later(dbus_msg_queue *q, dbus_msg *msg)
{
    return queue_outgoing( q, msg );
}


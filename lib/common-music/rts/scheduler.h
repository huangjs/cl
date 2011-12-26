/*************************************************************************
 *  Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube  *
 *  This program is free software; you can redistribute it and/or        *
 *  modify it under the terms of the Lisp Lesser Gnu Public License.     *
 *  See http://www.cliki.net/LLGPL for the text of this agreement.       *
 *************************************************************************/

/// $Revision: 1.7 $
/// $Date: 2006/05/30 13:10:24 $ 

enum {
  QENTRY_UNKNOWN,
  QENTRY_PROCESS,
  QENTRY_SEQ,
  QENTRY_OBJECT,
  QENTRY_MESSAGE,
  QENTRY_POINTER
};

enum {
  RTS_STATUS_STOPPED,
  RTS_STATUS_RUNNING,
  RTS_STATUS_PAUSED
};

enum {
  ENQUEUE_OK,
  ENQUEUE_NOROOM,
  ENQUEUE_NOTRUNNING
};

enum {
  LISP_UNKNOWN,
  LISP_CMU,
  LISP_GAUCHE,
  LISP_OPENMCL,
  LISP_SBCL
};

#define TIME_UNIT_USEC 1
#define TIME_UNIT_MSEC 2
#define NOTIME -1

// Gauche GC values above GC_NORMAL = msec threshold for gc

#define GC_DISABLE -1
#define GC_NORMAL 0

#define FALSE 0
// TRUE is already defined in gauch.h
#ifndef HAVE_GAUCHE
#define TRUE 1
#endif

typedef unsigned long qtime_t;
typedef unsigned long qdata_t;
typedef unsigned long qtype_t;

typedef struct qentry_node { 
  qtime_t time;             // current time in queue (usec)
  qdata_t data;             // data associated with entry
  qtype_t typp;             // type of entry
  struct qentry_node *next; // pointer to next queue entry
} qentry_t;

// could have different callbacks to optimize output on the lisp side
// by avoiding method dispatching.

typedef void (* rts_lisp_callback) (qdata_t data, qtype_t typp,
				    qtime_t time );

// rts scheduling

int rts_scheduler_start (int priority, int policy, int qsize, int tunit,
			 int gcmode, rts_lisp_callback cb) ;
int rts_scheduler_stop () ;
void rts_scheduler_reset () ;
int rts_scheduler_flush () ;
int rts_scheduler_pause () ;
int rts_scheduler_continue () ;
int rts_scheduler_state () ;
qtime_t rts_scheduler_time () ;
float rts_scheduler_time_sec () ;
qtime_t rts_scheduler_time_usec () ;
qtime_t rts_scheduler_time_msec () ;
int rts_scheduler_lock_lisp () ;
int rts_scheduler_unlock_lisp () ;
int rts_scheduler_enqueue (qdata_t data, qtype_t type,
			   qtime_t at, int repl) ;
pthread_t rts_current_thread () ;
int rts_thread_p () ;

// qentry nodes static for now
#define QSIZE 1024

void qentry_nodes_init (int size);
qentry_t* qentry_alloc (qdata_t data, qtype_t type, qtime_t time);
qdata_t qentry_data (qentry_t *e);
qtype_t qentry_type (qentry_t *e);
qtime_t qentry_time (qentry_t *e);
qentry_t* qentry_next (qentry_t *e);
void qentry_free (qentry_t *e);
void qentry_print (qentry_t *e);

// scheudling queue
void rts_queue_init (int size);
void rts_queue_free ();
int rts_queue_empty_p ();
qentry_t* rts_queue_peek ();
qentry_t* rts_queue_pop ();
qentry_t* rts_queue_last ();
void rts_queue_add (qentry_t *e);
void rts_queue_prepend (qentry_t *e);
void rts_queue_append (qentry_t *e);

// testing and debugging

void rts_queue_print ();
void rts_queue_test (int n);
int rts_test (int priority, int rate, rts_lisp_callback cb) ;

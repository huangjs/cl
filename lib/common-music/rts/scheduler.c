/*************************************************************************
 *  Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube  *
 *  This program is free software; you can redistribute it and/or        *
 *  modify it under the terms of the Lisp Lesser Gnu Public License.     *
 *  See http://www.cliki.net/LLGPL for the text of this agreement.       *
 *************************************************************************/

/// $Revision: 1.15 $
/// $Date: 2006/05/30 13:10:24 $ 

/*
 * Note bene: This version of the scheduler uses a timer loop rather
 * than cond_timedwait in order to get better timing resolution. to 
 * resurrect the previous working condwait version restore the following
 * revisions:
 * FILE                   REVISION
 * rts.asd                1.6
 * scheduler.h            1.6
 * scheduler.c            1.11
 * cffi-scheduler.lisp    1.10
 * gauche-scheduler.scm   1.9
 * cm-scheduler.scm       1.4
 * cm-scheduler.lisp      1.7
 */

#include <sys/time.h>
#include <stdlib.h>

#ifndef HAVE_GAUCHE
#include <pthread.h>
#endif

#ifdef HAVE_GAUCHE
#include <gauche.h>
#include <gauche/pthread.h>
#include <gc.h>
#endif

#include <stdio.h>
#include "scheduler.h"



static int rts_state=RTS_STATUS_STOPPED;
static int rts_tunit=TIME_UNIT_MSEC;
static int rts_gcval=GC_NORMAL; // -1, 0 or nsec threshold
static int gc_nomore=0; // stop gc flag
static qtime_t rts_start=0;   // usec start time of scheduler

static qentry_t *rts_qhead=NULL;
static qentry_t *rts_qtail=NULL;
static pthread_t rts_thread=NULL;
static rts_lisp_callback rts_callb=NULL;

static FILE *log_file;
static int rts_log=1;

static pthread_mutex_t lisp_lock;
static pthread_mutex_t queue_lock;
static pthread_mutex_t state_lock;
static pthread_cond_t  pause_cond;

int rts_scheduler_lock_lisp () {
  // called by lisp side to lock access mutex
  pthread_mutex_lock(&lisp_lock);
  return 0;
}

int rts_scheduler_unlock_lisp () {
  // called by lisp side to unlock access mutex
  pthread_mutex_unlock(&lisp_lock);
  return 0;
}

void rts_scheduler_reset () {
  rts_state=RTS_STATUS_STOPPED;
  //rts_queue_free();  // where do i do this
  rts_start=0;
  gc_nomore=FALSE;
  rts_gcval=GC_NORMAL;
  rts_tunit=TIME_UNIT_MSEC;
  pthread_mutex_destroy(&state_lock);
  pthread_mutex_destroy(&queue_lock);
  pthread_mutex_destroy(&lisp_lock);
  pthread_cond_destroy(&pause_cond);
  rts_callb=NULL;
}

int rts_scheduler_stop () {
  pthread_mutex_lock(&state_lock);
  rts_state=RTS_STATUS_STOPPED;
  pthread_cond_signal(&pause_cond);
  pthread_mutex_unlock(&state_lock);
  return RTS_STATUS_STOPPED;
}

static struct timeval rts_pause;
static qentry_t *rts_qhold;

int rts_scheduler_pause () {
  pthread_mutex_lock(&state_lock);
  if (rts_state==RTS_STATUS_RUNNING) {
    rts_state=RTS_STATUS_PAUSED;
    gettimeofday(&rts_pause, NULL);
  }
  pthread_mutex_unlock(&state_lock);
  return rts_state;
}

int rts_scheduler_continue () {
  pthread_mutex_lock(&state_lock);
  if (rts_state == RTS_STATUS_PAUSED) {
    struct timeval cont, diff;
    qtime_t pauseusec;
    qentry_t *p;

    gettimeofday(&cont, NULL);
    timersub(&cont,&rts_pause,&diff);
    pauseusec=(qtime_t)((diff.tv_sec*1000000)+diff.tv_usec);
    pthread_mutex_lock(&queue_lock);
    p=rts_queue_peek();
    while (p) {
      p->time += pauseusec;
      p=p->next;
    }
    pthread_mutex_unlock(&queue_lock);
    rts_state=RTS_STATUS_RUNNING;
    pthread_cond_signal(&pause_cond);
    pthread_mutex_unlock(&state_lock);
  }
  return rts_state;
}

int rts_scheduler_state () {
  return rts_state;
}

int rts_scheduler_enqueue (qdata_t data, qtype_t typp, qtime_t at, int repl) {
  qtime_t time;
  qentry_t *p;
  repl=TRUE; // always lock
  if (rts_state != RTS_STATUS_RUNNING) return ENQUEUE_NOTRUNNING;
  time=(rts_tunit == TIME_UNIT_MSEC) ? (at*1000) : at;
  pthread_mutex_lock(&queue_lock);
  p=qentry_alloc(data, typp, time);
  if (p) rts_queue_add(p);
  pthread_mutex_unlock(&queue_lock);
  return ((p == NULL) ? ENQUEUE_NOROOM : ENQUEUE_OK);
}

int rts_scheduler_flush () {
  int empty;
  pthread_mutex_lock(&queue_lock);
  empty=rts_queue_empty_p();
  if (!empty) rts_queue_free();
  pthread_mutex_unlock(&queue_lock);    
  return (!empty);
}
  
float rts_scheduler_time_sec () {
  if (rts_state != RTS_STATUS_RUNNING) return (float)NOTIME;
  return ((float)rts_scheduler_time_usec () / 1000000.0) ;
}

qtime_t rts_scheduler_time_msec () {
  if (rts_state != RTS_STATUS_RUNNING) return NOTIME;
  return (rts_scheduler_time_usec () / 1000) ;
}

qtime_t rts_scheduler_time_usec () {
  struct timeval date;
  if (rts_state != RTS_STATUS_RUNNING) return NOTIME;
  gettimeofday(&date, NULL);
  return (((date.tv_sec*1000000)+date.tv_usec)-rts_start) ;
}

/*
void add_timevals(struct timeval *t1, struct timeval *t2, struct timeval *result) {
  result->tv_sec = t1->tv_sec + t2->tv_sec + ((t1->tv_usec + t2->tv_usec) / 1000000);
  result->tv_usec = (t1->tv_usec + t2->tv_usec) % 1000000;
  return;
}

void find_positive_delta(struct timeval *later, struct timeval *earlier, struct timeval *delta) {
  long sec;
  long usec;
  long udelta;
  
  sec = later->tv_sec - earlier->tv_sec;
  usec = later->tv_usec - earlier->tv_usec;
  
  udelta = sec*1000000 + usec;

  delta->tv_sec = udelta / 1000000;
  delta->tv_usec = udelta - (delta->tv_sec * 1000000);
  
  // if delta is negative set it to 0
  if(delta->tv_sec < 0 || delta->tv_usec < 0) {
    delta->tv_sec = 0;
    delta->tv_usec = 0;
  }
  return;
}
*/

void *rts_scheduler (void *reso) {
  struct timeval currenttime;
  struct timeval targettime;
  struct timeval deltatime;
  struct timeval intervaltime;
  struct timespec waittime;
  qtime_t utime, qtime, etime;
  qentry_t *entry;
  qdata_t edata;
  qtype_t etype;
#ifdef HAVE_GAUCHE
  char tmp[64] = "cm gauche vm";
  ScmVM  *vm;
  ScmVM *parentvm;
  int res = 0;

  parentvm = Scm_VM();
  vm = Scm_NewVM(parentvm, SCM_MAKE_STR_IMMUTABLE(tmp));
  // probably should check to make sure there is no error
  res = Scm_AttachVM(vm);
#endif

  intervaltime.tv_sec = 0;
  intervaltime.tv_usec = (int32_t)reso; // 100=.1 millisecond
  deltatime.tv_sec = 0;
  while (TRUE) {
    pthread_mutex_lock(&state_lock);
    if (rts_state == RTS_STATUS_STOPPED)
      break;
    else 
      while (rts_state == RTS_STATUS_PAUSED)
	pthread_cond_wait(&pause_cond, &state_lock);
    pthread_mutex_unlock(&state_lock);

    gettimeofday(&currenttime, NULL);
    //add_timevals(&currenttime, &intervaltime, &targettime);
    timeradd(&currenttime, &intervaltime, &targettime);
    while (TRUE) {
      pthread_mutex_lock(&queue_lock);
      if (rts_queue_empty_p()) {
	pthread_mutex_unlock(&queue_lock);
	break;
      }
      entry=rts_queue_pop();
      qtime=qentry_time(entry);
      utime=rts_scheduler_time_usec();
      if (qtime>utime) {
	rts_queue_prepend(entry); 
	pthread_mutex_unlock(&queue_lock);
	break;
      }
      // drop entries more than a second late (?)
      if (qtime<(utime-1000000)) {
	qentry_free(entry);
	pthread_mutex_unlock(&queue_lock);
	break;
      }
	
      edata=qentry_data(entry);
      etype=qentry_type(entry);
      //etime=(rts_tunit == TIME_UNIT_MSEC) ? (utime/1000) : utime;
      // use queue time not clock time!
      etime=(rts_tunit == TIME_UNIT_MSEC) ? (qtime/1000) : qtime;

      qentry_free(entry); 
      pthread_mutex_unlock(&queue_lock);
      // queue unlocked during callback, enqueue should always lock
      pthread_mutex_lock(&lisp_lock);
      (*rts_callb) (edata, etype, etime);
      pthread_mutex_unlock(&lisp_lock);
    }
    gettimeofday(&currenttime, NULL);
    //fprintf(log_file, "%f \n", currenttime.tv_usec / 1000.0);
    //find_positive_delta(&targettime, &currenttime, &deltatime);
    timersub(&targettime, &currenttime, &deltatime);
    if (deltatime.tv_usec > 0) {
      waittime.tv_sec = deltatime.tv_sec;
      waittime.tv_nsec = deltatime.tv_usec * 1000L;
      //TIMEVAL_TO_TIMESPEC(&deltatime, &waittime);
      nanosleep(&waittime, NULL);
    }
  }
  rts_scheduler_reset();
  fclose(log_file);
  pthread_exit(NULL);
}

int rts_scheduler_start (int priority, int policy, int reso, int tunit,
			 int lflavor, rts_lisp_callback cb) {
  int rc;
  pthread_attr_t attr;
  struct sched_param parm;

  log_file = fopen("/tmp/rts.log" , "w+");
  fprintf(log_file, "log file for cm rts\n");

  // gag unused vars warnings
  rc=policy;

  pthread_attr_init(&attr);

  if (lflavor != LISP_SBCL) {   // this seems to break sbcl
    pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED);
    pthread_attr_setschedpolicy(&attr, policy);
  }
  parm.sched_priority=priority;
  pthread_attr_setschedparam(&attr, &parm);
 
  pthread_mutex_init(&lisp_lock, NULL);
  pthread_mutex_init(&queue_lock, NULL);
  pthread_mutex_init(&state_lock, NULL);
  pthread_cond_init(&pause_cond, NULL);

  rts_tunit=tunit;
  rts_qhead=NULL;
  rts_qtail=NULL;
  rts_queue_init(QSIZE);
  rts_callb=cb;
  rts_state=RTS_STATUS_RUNNING;
  rts_start=rts_scheduler_time_usec();
  rc = pthread_create(&rts_thread, &attr, rts_scheduler, (void *)reso);
  pthread_attr_destroy(&attr);
  return rc ;
}

pthread_t rts_current_thread () {
  return pthread_self() ;
}

int rts_thread_p () {
  return pthread_equal(rts_thread, pthread_self());
}


//////////////////////////////////////////////////////////////////////////
/// queue entries
//////////////////////////////////////////////////////////////////////////

// allocate block of queue entry nodes with qsize-1 available nodes. head
// node is reserved as a pointer to next available free node, or NULL if
// table is used up. table will be dynamically allocated from a size passed
// to rts_scheduler_start at some point

static struct qentry_node qentry_nodes[QSIZE] ;
static int qcount=0;

void qentry_nodes_init (int size) {
  qentry_t *p;
  int i=size; // gag unused var warning

  // initialize table as a linked list of free nodes where
  // node[0].next always points to next free node in table
  qcount=0;
  p=&qentry_nodes[0];
  for (i=0;i<size-1;i++) {
    p->next=p+1;  // set p.next to adjacent (free) node.
    p=p->next; 
  }
  p->next=NULL;  // last free node points to NULL
}

qentry_t* qentry_alloc (qdata_t data, qtype_t typp, qtime_t time) {
  qentry_t *p=qentry_nodes[0].next;
  if (p->next==NULL) {
    printf("\nQueue table full, the party is over!");
   }
   else {
     qcount++;
     qentry_nodes[0].next=p->next; 
     p->time=time;
     p->data=data;
     p->typp=typp;
     p->next=NULL;
   }
  return p;
}

void qentry_free (qentry_t *p) {
  qcount--;
  p->next=qentry_nodes[0].next;
  qentry_nodes[0].next=p;
}

qdata_t qentry_data (qentry_t *e) {
  return e->data;
}

qtime_t qentry_time (qentry_t *e) {
  return e->time;
}

qtype_t qentry_type (qentry_t *e) {
  return e->typp;
}

qentry_t* qentry_next (qentry_t *e) {
  return e->next;
}

void qentry_print (qentry_t *e) {
  printf("#<qentry_t time=%d type=%d data=%d>", 
	 (int)e->time, (int)e->typp, (int)e->data);
}

void qentry_fprint (qentry_t *e) {
  fprintf(log_file, "#<qentry_t time=%d type=%d data=%d>", 
	 (int)e->time, (int)e->typp, (int)e->data);
}


//////////////////////////////////////////////////////////////////////////
/// scheduing queue
//////////////////////////////////////////////////////////////////////////

static int qdebug=0;

void rts_queue_init (int size) {
  // initialize the queue for scheduling
  // claim all nodes as free
  qentry_nodes_init(size);
}

void rts_queue_free () {
  // release the queue by freeing allocated nodes
  qentry_t *p=rts_queue_pop();
  while (p) {
    qentry_free(p);
    p=rts_queue_pop();
  }
}
  
int rts_queue_empty_p () {
  // true if queue is empty
  if (rts_qhead == NULL) 
   return TRUE;
 return FALSE;
}

qentry_t* rts_queue_peek () {
  // return head node in queue or null
  return rts_qhead;
}

qentry_t* rts_queue_pop () {
  // pop head node from queue
  if (rts_qhead == NULL)
    return NULL;
    else {
      qentry_t *e=rts_qhead;
      rts_qhead=e->next;
      return e;
    }
}

qentry_t* rts_queue_last () {
  // return last node in queue
  return rts_qtail;
}

void rts_queue_prepend (qentry_t *e) {
  // push node onto queue, update tail if first
  if (rts_qhead == NULL) {
    rts_qhead=e;
    rts_qtail=e;
  }
  else {
    e->next=rts_qhead;
    rts_qhead=e;
  }
}

void rts_queue_append (qentry_t *e) {
  // add node to end of queue
  if (rts_qtail == NULL) {
    rts_qhead=e;
    rts_qtail=e;
  }
  else {
    rts_qtail->next=e;
    rts_qtail=e;
  }
}

void rts_queue_add (qentry_t *e) {
  // insert node in sorted queue according to time.
  // node will appear after all nodes with the same time
  if (qdebug) {
    printf("\n\n\nadding e->time=%d", (int)e->time);
    printf("\nqueue before add is:");
    rts_queue_print();
  }

  if (rts_qhead == NULL) {
    // queue empty, prepend
    if (qdebug) printf( "\nqueue is empty: PREPENDING");
    rts_queue_prepend(e);
  }
  else if (e->time < rts_qhead->time) {
    // time is before head, prepend to queue
    if (qdebug) printf( "\ne->time=%d less than head->time=%d: PREPENDING",(int)e->time, (int)rts_qhead->time);
    rts_queue_prepend(e);
  }
  else if (e->time < rts_qtail->time) {
    // insert after head but before tail
    qentry_t *p=rts_qhead;
    qentry_t *l=NULL;
    if (qdebug) {
      printf("\ne->time %d >= head->time=%d and less than tail->time %d: INSERTING", (int)e->time, (int)rts_qhead->time, (int)rts_qtail->time);
      printf("\n starting search e->time=%d p->time=%d", (int)e->time,(int)p->time);
    }
    while (p->time <= e->time) {
      if (qdebug) printf("\nsearching e->time=%d p->time=%d", (int)e->time, (int)p->time);
      l=p;
      p=p->next;
    }
    if (qdebug) printf("\ndone! l->time=%d e->time=%d p->time=%d",(int)l->time, (int)e->time, (int)p->time);
    e->next=l->next;
    l->next=e;
  }
  else {
    // append to end of queue becaue time is <= tail
    if (qdebug) printf("\ne->time=%d greater than tail->time=%d: APPENDING", (int)e->time, (int)rts_qtail->time);
    rts_queue_append(e);
  } 
  if (qdebug) {
    printf("\nqueue after add is:");
    rts_queue_print();
    printf("\n\n\n");
  }
}

//////////////////////////////////////////////////////////////////////////
/// testing and debugging
//////////////////////////////////////////////////////////////////////////

void rts_queue_print () {
  qentry_t *e=rts_qhead;
  int i;
  printf("\n-----------------------------------");
  printf("\nsize=%d used:%d, free=%d", QSIZE, qcount, (QSIZE-qcount));
  pthread_mutex_lock (&queue_lock);
  for (i=0; i<qcount; i++) {
      printf("\n[%i] ", i);
    qentry_print(e);
    e=e->next;
  }
  pthread_mutex_unlock (&queue_lock);
  printf("\n-----------------------------------\n\n\n");
}

void rts_queue_fprint () {
  qentry_t *e=rts_qhead;
  int i;
  fprintf(log_file, "\n-----------------------------------");
  fprintf(log_file, "\nsize=%d used:%d, free=%d", QSIZE, qcount, (QSIZE-qcount));
  pthread_mutex_lock (&queue_lock);
  for (i=0; i<qcount; i++) {
    fprintf(log_file, "\n[%i] ", i);
    qentry_fprint(e);
    e=e->next;
  }
  pthread_mutex_unlock (&queue_lock);
  fprintf(log_file, "\n-----------------------------------\n\n\n");
}

static long testid = 1;
void rts_queue_test (int n) {
  int i;
  long r;
  qentry_t *p;

  if (n == 0) {
    printf("\nQueue initialized.");
    rts_queue_free();
    rts_queue_init(QSIZE);
  }
  else {
    for (i=0;i<n;i++) {
      r=random()&0xF;
      p=qentry_alloc( testid, 0, r);
      if (p) {
	rts_queue_add(p);
	testid++;
      }
      else printf("\nHelp!! No more nodes!");
    }
    printf("\nqueue after add is:");
    rts_queue_print();
    }
  printf("\n\n\n");
}

void qentry_node_test () {
  int i=1;
  qentry_t *p;

  for (i=0;i<QSIZE;i++) {qentry_nodes[i].data=i*100;}
  qentry_nodes_init(QSIZE);
  p=qentry_nodes[0].next;
  i=1;
  while (p) {
    printf("\nqentry_nodes[%d].data=%d", i++, (int)p->data);
    p=p->next;
  }
  printf("\n\n\n");

}


void *rts_callback_test (void *msec) {
  // perform lisp callback at msec rate
  qtime_t t;
  qdata_t i=0;
  qtype_t z=0;
  int mrate=(int) msec;
  struct timespec ts;
  

#ifdef HAVE_GAUCHE
  char tmp[64] = "cm gauche vm";
  ScmVM  *vm;
  ScmVM *parentvm;
  int res = 0;
  parentvm = Scm_VM();
  vm = Scm_NewVM(parentvm, SCM_MAKE_STR_IMMUTABLE(tmp));
  // probably should check to make sure there is no error
  res = Scm_AttachVM(vm);
#endif

  pthread_mutex_init(&queue_lock, NULL);
  //  pthread_cond_init(&pause_cond, NULL);

  if ( mrate < 1000) {
    ts.tv_nsec += mrate*1000000; 
  }
  else {
    ts.tv_sec+=mrate/1000;
    ts.tv_nsec+=(mrate % 1000)*1000000;
  }
  while (rts_state != RTS_STATUS_STOPPED) {
    if (rts_callb != NULL) {
      // lock lisp access mutex
      pthread_mutex_lock(&lisp_lock);
      t=rts_scheduler_time_usec();
#ifdef HAVE_GAUCHE
      SCM_UNWIND_PROTECT {
#endif
	
	(*rts_callb) (++i, z, t) ;  
	
#ifdef HAVE_GAUCHE
      }
      SCM_END_PROTECT;
#endif

      // unlock lisp access mutex
      //pthread_mutex_unlock(&lisp_lock);
   }
    nanosleep(&ts, NULL);
  }
  pthread_exit(NULL);
}

int rts_test (int priority, int rate, rts_lisp_callback cb ) {
  int rc;
  pthread_attr_t attr;
  struct sched_param parm;

  pthread_attr_init(&attr);
  parm.sched_priority=priority;
  pthread_attr_setschedparam(&attr, &parm);
  pthread_mutex_init(&lisp_lock, NULL);
  rts_qhead=NULL;
  rts_qtail=NULL;
  rts_callb=cb;
  rts_state=RTS_STATUS_RUNNING;
  rts_start=rts_scheduler_time_usec();
  rc = pthread_create(&rts_thread, &attr, rts_callback_test, (void *)rate);
  pthread_attr_destroy(&attr);
  return rc ;
}


static int ids=0;

int rts_test2 (int num, int at, int rate) {
  int i;
  qtime_t now=(qtime_t)at;
  for (i=0;i<num;i++) {
    rts_scheduler_enqueue(++ids, 0, now, TRUE);
    now+=rate;
  }
  return num;
}


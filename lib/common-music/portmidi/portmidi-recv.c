/**********************************************************************
 /// Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
 ///
 /// This program is free software; you can redistribute it and/or
 /// modify it under the terms of the Lisp Lesser Gnu Public License.
 /// See http://www.cliki.net/LLGPL for the text of this agreement.
 /// **********************************************************************

 /// $Name:  $
 /// $Revision: 1.5 $
 /// $Date: 2006/05/01 16:30:23 $
 **********************************************************************/



#include "portmidi-recv.h"


static int pm_thread_running = 0;
static pthread_t pm_thread;
static pm_recv_callback pm_callback=NULL;
static int pm_resolution=1;
static PmStream *pm_stream = NULL;
static int pm_input_id=-1;
static int pm_buffer_size=0;
static PmEvent *pm_buffer=NULL;


void init_pm_info() {
  pm_thread_running = 0;
  pm_callback=NULL;
  pm_resolution=1;
  pm_input_id=-1;
  pm_buffer_size=0;
  pm_buffer=NULL;
}



/* sets globals return 0 on success, -1 if pmstream is not valid */
int pm_set_input(PortMidiStream *pmn, int id, int bufsize) {

  init_pm_info();
  
  if(bufsize > 0)
    pm_buffer_size = bufsize;
  else
    return -1;
  
  if( (pmn != NULL) && (id >= 0)) {
    pm_stream = pmn;
    pm_input_id = id;
   
    return 0;
  } else
    {
      pm_input_id = -1;
      return -1;
    }
}

int pm_recv_start(int priority, int resolution, pm_recv_callback cb) {
  pthread_attr_t attr;
  struct sched_param parm;
  int res = 0;

  if(pm_input_id == -1)
    return -1;

  pm_resolution = resolution;
  pm_callback = cb;
  
  pm_buffer = (PmEvent*)malloc(pm_buffer_size * sizeof(PmEvent));
  
  if(!pm_thread_running) {
    pthread_attr_init(&attr);
    parm.sched_priority = priority;
    pthread_attr_setschedparam(&attr, &parm);
    res = pthread_create(&pm_thread, &attr, pm_recv_proc, NULL);
    pthread_attr_destroy(&attr);
  }
  return res;
}


void *pm_recv_proc(void *userData) {
  
  PmError result;
  PmEvent * pmEventsPtr;
  int res, events, i;
  double whole;
  double frac;
  struct timespec ts;

#ifdef HAVE_GAUCHE
  char tmp[64] = "cm recv gauche vm";
  ScmVM  *vm;
  ScmVM *parentvm;

  
  parentvm = Scm_VM();
  vm = Scm_NewVM(parentvm, SCM_MAKE_STR_IMMUTABLE(tmp));
  // probably should check to make sure there is no error
  res = Scm_AttachVM(vm);
#endif
  
    
  if( pm_resolution < 999) {
    ts.tv_sec = 0;
    ts.tv_nsec = ((int32_t)( 10000000 * (pm_resolution / 1000.0)));
  }
  else {
    whole = modf( (double) (pm_resolution / 1000.0), &frac);
    ts.tv_sec = (int32_t)whole;
    ts.tv_nsec = (int32_t)(frac * 10000000);    
  }
  
  pm_thread_running = 1;

  while (pm_thread_running) {
    
    if(pm_stream == NULL)
      return NULL;

    result = Pm_Poll(pm_stream);

    if(result) {
      pmEventsPtr = pm_buffer;
      
      events = Pm_Read(pm_stream, pmEventsPtr, pm_buffer_size);
      
      if(events > 0) {

#ifdef HAVE_GAUCHE
	SCM_UNWIND_PROTECT {
	  for(i=0;i<events;i++) {
	    res = (*(pm_callback))((long) (pmEventsPtr+i)->message);
	  }
	}
	SCM_END_PROTECT;
#else
	for(i=0;i<events;i++) {
	  res = (*(pm_callback))(pmEventsPtr+i);
	}
#endif

      }
    }
    
    nanosleep(&ts, NULL);
  }
  pm_thread_running = 0;
  pthread_exit(NULL);
}

int pm_recv_stop () {
  pm_thread_running = 0;
  free(pm_buffer);
  pthread_cancel(pm_thread);
  return 0;
}

int pm_recv_state () {
  return pm_thread_running;
}

int pm_priority_min() {
  return sched_get_priority_min(SCHED_OTHER);
}

int pm_priority_max() {
  return sched_get_priority_max(SCHED_OTHER);
}

	
	
	



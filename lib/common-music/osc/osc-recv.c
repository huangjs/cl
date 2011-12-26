/**********************************************************************
 /// Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
 ///
 /// This program is free software; you can redistribute it and/or
 /// modify it under the terms of the Lisp Lesser Gnu Public License.
 /// See http://www.cliki.net/LLGPL for the text of this agreement.
 /// **********************************************************************

 /// $Name:  $
 /// $Revision: 1.1 $
 /// $Date: 2006/06/01 23:20:06 $
 **********************************************************************/



#include "osc-recv.h"


static int osc_thread_running = 0;
static pthread_t osc_thread;
static osc_recv_callback osc_callback=NULL;
static int osc_resolution=1;


void init_osc_info() {
  osc_thread_running = 0;
  osc_callback=NULL;
  osc_resolution=1;
}

int osc_recv_start(int priority, int resolution, osc_recv_callback cb) {
  pthread_attr_t attr;
  struct sched_param parm;
  int res = 0;

  osc_resolution = resolution;
  osc_callback = cb;
  
    
  if(!osc_thread_running) {
    pthread_attr_init(&attr);
    parm.sched_priority = priority;
    pthread_attr_setschedparam(&attr, &parm);
    res = pthread_create(&osc_thread, &attr, osc_recv_proc, NULL);
    pthread_attr_destroy(&attr);
  }
  return res;
}


void *osc_recv_proc(void *userData) {

  int res = 0;

#ifdef HAVE_GAUCHE
  char tmp[64] = "cm recv gauche vm";
  ScmVM  *vm;
  ScmVM *parentvm;
  
  
  parentvm = Scm_VM();
  vm = Scm_NewVM(parentvm, SCM_MAKE_STR_IMMUTABLE(tmp));
  // probably should check to make sure there is no error
  res = Scm_AttachVM(vm);
#endif
  
  osc_thread_running = 1;
  
  while (osc_thread_running) {
    
#ifdef HAVE_GAUCHE
    SCM_UNWIND_PROTECT {
#endif
      res = (*(osc_callback))();
#ifdef HAVE_GAUCHE
    }
    SCM_END_PROTECT;
#endif
  }
  osc_thread_running = 0;
  pthread_exit(NULL);
}

int osc_recv_stop () {
  osc_thread_running = 0;
  pthread_cancel(osc_thread);
  return 0;
}

int osc_recv_state () {
  return osc_thread_running;
}

int osc_priority_min() {
  return sched_get_priority_min(SCHED_OTHER);
}

int osc_priority_max() {
  return sched_get_priority_max(SCHED_OTHER);
}

	
	
	



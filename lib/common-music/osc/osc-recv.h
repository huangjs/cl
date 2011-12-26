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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#ifndef HAVE_GAUCHE
#include <pthread.h>
#else
#include <gauche.h>
#include <gauche/pthread.h>
#endif

#include <sched.h>
#include <string.h>

#ifdef HAVE_GAUCHE
typedef int (* osc_recv_callback) (void);
#else
typedef int (* osc_recv_callback) (void);
#endif

void init_osc_info();
int osc_recv_start(int priority, int resolution, osc_recv_callback cb) ;
void *osc_recv_proc(void *userData) ;
int osc_recv_stop () ;
int osc_recv_state () ;
int osc_priority_min();
int osc_priority_max();




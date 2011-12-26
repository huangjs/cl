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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#ifndef HAVE_GAUCHE
#include <pthread.h>
#else
#include <gauche.h>
#include <gauche/pthread.h>
#endif


#include <portmidi.h>
#include <sched.h>
#include <string.h>

#ifdef HAVE_GAUCHE
typedef int (* pm_recv_callback) (long mess);
#else
typedef int (* pm_recv_callback) (void *userData);
#endif



void init_pm_info();
int pm_set_input(PortMidiStream *pmn, int id, int bufsize);
int pm_recv_start(int priority, int resolution, pm_recv_callback cb);
void *pm_recv_proc(void *userData);
int pm_recv_stop();
int pm_recv_state ();
int pm_priority_min();
int pm_priority_max();


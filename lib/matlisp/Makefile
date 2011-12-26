# Generated automatically from Makefile.in by configure.
# Copyright (c) 2000 The Regents of the University of California.
# All rights reserved. 
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in all
# copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
# THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
# PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
# CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
# ENHANCEMENTS, OR MODIFICATIONS.
#
##########################################################################
#
# Process this file with 'make' or 'gmake'.
#
##########################################################################
#
# $Id: Makefile,v 1.6 2001/04/25 17:41:58 rtoy Exp $
#
##########################################################################
# allegro-lisp=/usr/sww/pkg/acl-6.0/alisp
# cmu-lisp=/home/eclair1/shift-uav/bin/cl

all:
	$(MAKE) -f matlisp.mk

#	@echo ================================
#	@echo Type one of the following:
#	@echo 
#	@echo $(MAKE) allegro
#	@echo
#	@echo to build MATLISP for Allegro CL
#	@echo 
#	@echo $(MAKE) cmu
#	@echo
#	@echo to build MATLISP for CMU CL.
#	@echo
#	@echo In either case please check that
#	@echo the allegro-lisp and cmu-lisp
#	@echo variables in the Makefile are
#	@echo set correctly.
#	@echo ================================


#allegro: config.cache lib/libmatlispshared.so
#	$(allegro-lisp) -e '(progn (load "start.lisp"))'

#cmu: config.cache lib/libmatlispstatic.a
#	$(cmu-lisp) -eval '(progn (load "start.lisp"))'

#config.cache: matlisp.mk.in lib/lazy-loader.lisp.in configure
#	\rm -f config.cache
#	./configure --prefix=`pwd`

#lib/libmatlispshared.so: config.cache
#	$(MAKE) -f matlisp.mk shared

#lib/libmatlispstatic.a: config.cache
#	$(MAKE) -f matlisp.mk static

clean:
	$(MAKE) -f matlisp.mk clean
	\rm -f config.cache
	\rm -f bin/*.sparcf
	\rm -f bin/*.x86f
	\rm -f bin/*.fsl
	\rm -f bin/*.fasl
	\rm -f bin/*.err


distclean: clean
	$(MAKE) -f matlisp.mk distclean
	\rm -f logical
	\rm -f matlisp.core
	\rm -f matlisp.dxl
	\rm -f matlisp-acl
	\rm -f matlisp-cmu
	\rm -f lib/libmatlispstatic.a
	\rm -f lib/libmatlispshared.so

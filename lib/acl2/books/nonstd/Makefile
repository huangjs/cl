#; ACL2 Version 3.1 -- A Computational Logic for Applicative Common Lisp
#; Copyright (C) 2006  University of Texas at Austin

#; This version of ACL2 is a descendent of ACL2 Version 1.9, Copyright
#; (C) 1997 Computational Logic, Inc.  See the documentation topic NOTES-2-0.

#; This program is free software; you can redistribute it and/or modify
#; it under the terms of the GNU General Public License as published by
#; the Free Software Foundation; either version 2 of the License, or
#; (at your option) any later version.

#; This program is distributed in the hope that it will be useful,
#; but WITHOUT ANY WARRANTY; without even the implied warranty of
#; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#; GNU General Public License for more details.

#; You should have received a copy of the GNU General Public License
#; along with this program; if not, write to the Free Software
#; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

#; Written by:  Matt Kaufmann               and J Strother Moore
#; email:       Kaufmann@cs.utexas.edu      and Moore@cs.utexas.edu
#; Department of Computer Sciences
#; University of Texas at Austin
#; Austin, TX 78712-1188 U.S.A.

# See ../Makefile (included via "include" below) for details.  The main
# differences here are in the directories that we descend into, together with
# new targets for setting up symbolic links in those directories to files in
# their corresponding "standard" directories, under ../.

# WARNING:  This mechanism isn't currently set up to use ACL2(r) automatically.
# When you execute make, be sure to use a command of the form
# make ACL2=your_saved_acl2r

# Before we include the parent directory's Makefile, we add the following to
# make sure soft links have been set up (including some from nonstd Makefiles
# to corresponding standard Makefiles).  It doesn't work to put these at the
# end of this file; they can then be executed too late.
# ACTUALLY, this doesn't seem to prevent certification from starting on a Mac
# with -j 2 before all links have been set up.  So it's best to execute
# "make links" first before "make -j 2".
all: links
all-plus: links

# Note that the following assumes that we execute make in this directory.
# Also note that the parent directory provides some directory dependencies
# that may need to be re-thought for the nonstd/ case at some point; but as
# long as things are working, we leave well enough alone.
include ../Makefile

# First directories of books to certify (keep in sync with ../Makefile):

DIRS1 = cowles arithmetic meta nsa

# Additional directories, where DIRS3 prunes out those less likely to be of
# use in certification of other books (keep in sync with ../Makefile):.
# Note:  We do not bring textbook or most of case-studies over here at this
# point.

DIRS2_EXCEPT_WK = ordinals data-structures bdd ihs arithmetic-2 misc rtl \
        finite-set-theory powerlists fft sqrt
DIRS2 = $(DIRS2_EXCEPT_WK) workshops

# Directories that have their own contents, and do not need links, or only
# need some links.
FULLDIRS = nsa fft

# Subdirectories of books/nonstd/*/, not including workshops.
SUBDIRS = arithmetic-2/floor-mod arithmetic-2/meta arithmetic-2/pass1 \
	  rtl/rel1 rtl/rel1/lib1 rtl/rel1/lib3 rtl/rel1/support rtl/rel1/support/fadd \
          rtl/rel2 rtl/rel2/lib rtl/rel2/support

WORKSHOPS = workshops/1999 \
	    workshops/1999/calculus workshops/1999/analysis \
	    workshops/1999/calculus/book workshops/1999/calculus/solutions

# Directories that need links:
LINKDIRS = . $(filter-out $(FULLDIRS), $(DIRS1) $(DIRS2) $(SUBDIRS))

.PHONY: links
links:
	@old_dir=`pwd` ; cd .. ; std_dir=`pwd` ; \
	cd $$old_dir ;\
	for book_dir in $(LINKDIRS) ; \
	do \
	if [ -d $$std_dir/$$book_dir ] ; then \
	./link-up.sh $$book_dir $$std_dir ; \
	fi ; \
	done
# Remove any symbolic links that should not have been put there:
	@if [ -h workshops/1999/calculus/book/tree.lisp ] ; then \
	rm workshops/1999/calculus/book/tree.lisp ; \
	fi

clean-links:
	@for book_dir in $(LINKDIRS) ; \
	do \
	if [ -d $$book_dir ] ; then \
	pushd $$book_dir ; \
	for file in `\ls` ; \
	do \
	if [ -h $$file ] ; then \
	rm -f $$file ; \
	fi ; \
	done ; \
	popd ; \
	fi ; \
	done

clean: links clean-subdirs

clean-subdirs:
	@for dir in $(SUBDIRS) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; $(MAKE) clean) ; \
	fi \
	done
	@for dir in $(WORKSHOPS) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; $(MAKE) clean) ; \
	fi \
	done

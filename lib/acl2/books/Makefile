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

# This file certifies all the books that come with the system by asking the
# appropriate subdirectories certify their books.  The subsidiary Makefiles
# take advantage of a makefile, Makefile-generic, in this directory, which is
# derived from one written by Bishop Brock.

# For example, to clean and time book certification (including workshops):
# make clean all-plus

# We do not set variable ACL2 set here, because the value here would be
# overriden anyhow by the values in the subsidiary Makefiles, which get their
# value from file Makefile-generic.  However, ACL2 can be set on the command
# line, e.g.:
# make ACL2=acl2
# make ACL2=/usr/local/bin/acl2 TIME=/usr/bin/time
# make ACL2=/u/acl2/v2-9/acl2-sources/saved_acl2

TIME = time

# Directories go here; first those before certifying arithmetic/top-with-meta,
# then those afterwards.

# NOTE!  This does *not* touch directory nonstd.

# First directories of books to certify:
DIRS1 = cowles arithmetic meta
# Additional directories, where DIRS3 prunes out those not distributed.
# We rely on DIRS3 being a subset of DIRS2 in the dependence of DIRS2 on
# top-with-meta-cert.
DIRS2_EXCEPT_WK = ordinals data-structures bdd ihs arithmetic-2 arithmetic-3 \
	misc proofstyles rtl arithmetic-3/extra make-event \
	finite-set-theory finite-set-theory/osets powerlists textbook \
	finite-set-theory/osets-0.81 finite-set-theory/osets-0.9 defexec symbolic \
	data-structures/memories unicode concurrent-programs/bakery \
	concurrent-programs/german-protocol deduction/passmore
DIRS2 = $(DIRS2_EXCEPT_WK) workshops
DIRS3 =           ordinals data-structures bdd ihs arithmetic-2 arithmetic-3 \
	misc proofstyles rtl make-event arithmetic-3/extra \
	finite-set-theory finite-set-theory/osets powerlists textbook \
	defexec symbolic \
	data-structures/memories unicode concurrent-programs/bakery \
	concurrent-programs/german-protocol deduction/passmore
SHORTDIRS2 = ordinals data-structures bdd

.PHONY: $(DIRS1) $(DIRS2) $(DIRS3)

# Same as all-plus below, using DIRS3 instead of DIRS2.  Much faster!!  Omits
# books less likely to be needed, in particular, under workshops/.
all:
	@date ; $(TIME) $(MAKE) all-aux

# Next, specify all of the directory dependencies.  At this point we do this
# manually by inspecting the Makefiles.
arithmetic: cowles
meta: arithmetic
ordinals: top-with-meta-cert
ihs: arithmetic data-structures
misc: arithmetic top-with-meta-cert ordinals arithmetic-2
make-event: misc arithmetic-3 arithmetic rtl
arithmetic-2: ihs
rtl: arithmetic meta top-with-meta-cert ordinals ihs misc arithmetic-2
# arithmetic-3 has no dependencies
arithmetic-3/extra: arithmetic-3 ihs rtl arithmetic-2 arithmetic-3
finite-set-theory: arithmetic ordinals
powerlists: arithmetic ordinals data-structures
textbook: arithmetic top-with-meta-cert ordinals ihs
defexec: arithmetic misc ordinals
symbolic: arithmetic arithmetic-2 data-structures ihs misc ordinals
data-structures/memories: arithmetic-3 misc
unicode: arithmetic arithmetic-3 ihs
proofstyles: arithmetic-2 ordinals
concurrent-programs/bakery: misc ordinals
concurrent-programs/german-protocol: misc
deduction/passmore: 
# Let us wait for everything else before workshops:
workshops: $(DIRS1) $(DIRS2_EXCEPT_WK)

$(DIRS1):
	@if [ -f $@/Makefile ]; then cd $@ ; $(MAKE) all ; fi

$(DIRS2): top-with-meta-cert
	@if [ -f $@/Makefile ]; then cd $@ ; $(MAKE) all ; fi

all-aux: $(DIRS1) $(DIRS3)

# We should really parallelize the compile targets below (fasl, all-fasl,
# all-o, etc.) as with the .cert targets above.  Any volunteers?

# Keep the following pairs in sync with the two targets just above.
# They will create compiled files for books that may have already been
# certified in another lisp (.fasl files for all-fasl, etc.).
# Of course, the underlying lisp of the ACL2 that is run should agree with
# the desired compiled file extension.

fasl:
	@date ; $(TIME) $(MAKE) fasl-aux ; date

fasl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fasl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-fasl
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fasl ; \
	cd ..) ; \
	fi \
	done

fas:
	@date ; $(TIME) $(MAKE) fas-aux ; date

fas-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fas ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-fas
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fas ; \
	cd ..) ; \
	fi \
	done

sparcf:
	@date ; $(TIME) $(MAKE) sparcf-aux ; date

sparcf-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) sparcf ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-sparcf
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) sparcf ; \
	cd ..) ; \
	fi \
	done

ufsl:
	@date ; $(TIME) $(MAKE) ufsl-aux ; date

ufsl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) ufsl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-ufsl
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) ufsl ; \
	cd ..) ; \
	fi \
	done

x86f:
	@date ; $(TIME) $(MAKE) x86f-aux ; date

x86f-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) x86f ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-x86f
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) x86f ; \
	cd ..) ; \
	fi \
	done

o:
	@date ; $(TIME) $(MAKE) o-aux ; date

o-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) o ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-o
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) o ; \
	cd ..) ; \
	fi \
	done

dfsl:
	@date ; $(TIME) $(MAKE) dfsl-aux ; date

dfsl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) dfsl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-dfsl
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) dfsl ; \
	cd ..) ; \
	fi \
	done

d64fsl:
	@date ; $(TIME) $(MAKE) d64fsl-aux ; date

d64fsl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) d64fsl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-d64fsl
	@for dir in $(DIRS3) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) d64fsl ; \
	cd ..) ; \
	fi \
	done

# Certify all books that need certification.  If you want to get a total time
# for certifying all books, then first do "make clean".
all-plus: $(DIRS1) $(DIRS2)

# Keep the following three pairs in sync with the two targets just above.
# They will create compiled files for books that may have already been
# certified in another lisp (.fasl files for all-fasl, etc.).
# Of course, the underlying lisp of the ACL2 that is run should agree with
# the desired compiled file extension.

all-fasl:
	@date ; $(TIME) $(MAKE) all-fasl-aux ; date

all-fasl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fasl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-fasl
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fasl ; \
	cd ..) ; \
	fi \
	done

all-fas:
	@date ; $(TIME) $(MAKE) all-fas-aux ; date

all-fas-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fas ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-fas
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) fas ; \
	cd ..) ; \
	fi \
	done

all-sparcf:
	@date ; $(TIME) $(MAKE) all-sparcf-aux ; date

all-sparcf-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) sparcf ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-sparcf
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) sparcf ; \
	cd ..) ; \
	fi \
	done

all-ufsl:
	@date ; $(TIME) $(MAKE) all-ufsl-aux ; date

all-ufsl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) ufsl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-ufsl
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) ufsl ; \
	cd ..) ; \
	fi \
	done

all-x86f:
	@date ; $(TIME) $(MAKE) all-x86f-aux ; date

all-x86f-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) x86f ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-x86f
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) x86f ; \
	cd ..) ; \
	fi \
	done

all-dfsl:
	@date ; $(TIME) $(MAKE) all-dfsl-aux ; date

all-dfsl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) dfsl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-dfsl
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) dfsl ; \
	cd ..) ; \
	fi \
	done

all-d64fsl:
	@date ; $(TIME) $(MAKE) all-d64fsl-aux ; date

all-d64fsl-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) d64fsl ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-d64fsl
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) d64fsl ; \
	cd ..) ; \
	fi \
	done

all-o:
	@date ; $(TIME) $(MAKE) all-o-aux ; date

all-o-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) o ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-o
	@for dir in $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) o ; \
	cd ..) ; \
	fi \
	done

.PHONY: top-with-meta-cert
top-with-meta-cert: $(DIRS1)
	cd arithmetic ; $(MAKE) top-with-meta.cert

.PHONY: top-with-meta-o
top-with-meta-o:
	cd arithmetic ; $(MAKE) top-with-meta.o

.PHONY: top-with-meta-fasl
top-with-meta-fasl:
	cd arithmetic ; $(MAKE) top-with-meta.fasl

.PHONY: top-with-meta-fas
top-with-meta-fas:
	cd arithmetic ; $(MAKE) top-with-meta.fas

.PHONY: top-with-meta-sparcf
top-with-meta-sparcf:
	cd arithmetic ; $(MAKE) top-with-meta.sparcf

.PHONY: top-with-meta-ufsl
top-with-meta-ufsl:
	cd arithmetic ; $(MAKE) top-with-meta.ufsl

.PHONY: top-with-meta-x86f
top-with-meta-x86f:
	cd arithmetic ; $(MAKE) top-with-meta.x86f

.PHONY: top-with-meta-dfsl
top-with-meta-dfsl:
	cd arithmetic ; $(MAKE) top-with-meta.dfsl

.PHONY: top-with-meta-d64fsl
top-with-meta-d64fsl:
	cd arithmetic ; $(MAKE) top-with-meta.d64fsl

# Clean all books, not only the "basic" ones.
clean:
	@for dir in $(DIRS1) $(DIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) clean ; \
	cd ..) ; \
	fi \
	done

# Tar up books and support, not including workshops or nonstd stuff.
tar:
	tar cvf books.tar Makefile Makefile-generic Makefile-subdirs README README.html certify-numbers.lisp $(DIRS1) $(DIRS3)

# The following "short" targets allow for a relatively short test, in response
# to a request from GCL maintainer Camm Maguire.

short-clean:
	@rm -f short-test.log
	@for dir in $(DIRS1) $(SHORTDIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) clean ; \
	cd ..) ; \
	fi \
	done

short-test-aux:
	@for dir in $(DIRS1) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) all ; \
	cd ..) ; \
	fi \
	done
	@$(MAKE) top-with-meta-cert
	@for dir in $(SHORTDIRS2) ; \
	do \
	if [ -f $$dir/Makefile ]; then \
	(cd $$dir ; \
	$(MAKE) all ; \
	cd ..) ; \
	fi \
	done

short-test:
	@rm -f short-test.log
	$(MAKE) short-clean
	$(MAKE) short-test-aux > short-test.log 2> short-test.log
	@if [ ! -f short-test.log ] || (fgrep '**' short-test.log > /dev/null) ; then \
	(echo 'Short test failed!' ; exit 1) ; else \
	echo 'Short test passed.' ; fi

# Makefile.in generated by automake 1.6.3 from Makefile.am.
# interfaces/xmaxima/doc/figures/Makefile.  Generated from Makefile.in by configure.

# Copyright 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
# Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.



# versioned installation directories
SHELL = /bin/sh

srcdir = .
top_srcdir = ../../../..

prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/share/info
mandir = ${prefix}/share/man
includedir = ${prefix}/include
oldincludedir = /usr/include
pkgdatadir = $(datadir)/maxima
pkglibdir = $(libdir)/maxima
pkgincludedir = $(includedir)/maxima
top_builddir = ../../../..

ACLOCAL = ${SHELL} /home/huang/.sbcl/lib/maxima/missing --run aclocal-1.6
AUTOCONF = ${SHELL} /home/huang/.sbcl/lib/maxima/missing --run autoconf
AUTOMAKE = ${SHELL} /home/huang/.sbcl/lib/maxima/missing --run automake-1.6
AUTOHEADER = ${SHELL} /home/huang/.sbcl/lib/maxima/missing --run autoheader

am__cd = CDPATH="$${ZSH_VERSION+.}$(PATH_SEPARATOR)" && cd
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_SCRIPT = $(install_sh) -c
INSTALL_SCRIPT = ${INSTALL}
INSTALL_HEADER = $(INSTALL_DATA)
transform = s,x,x,
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
host_alias = 
host_triplet = i686-pc-linux-gnu

EXEEXT = @EXEEXT@
OBJEXT = @OBJEXT@
PATH_SEPARATOR = :
ACL_NAME = lisp
AMTAR = ${SHELL} /home/huang/.sbcl/lib/maxima/missing --run tar
AWK = gawk
CLISP_NAME = clisp
CLISP_RUNTIME = lisp.run
CLISP_RUNTIME_PATH = /usr/lib/clisp/base/lisp.run
CMUCL_NAME = lisp
CMUCL_RUNTIME = lisp
CMUCL_RUNTIME_PATH = 
DEFAULTLISP = clisp
DEPDIR = @DEPDIR@
GCL_NAME = gcl
INSTALL_STRIP_PROGRAM = ${SHELL} $(install_sh) -c -s
OPENMCL_NAME = openmcl
PACKAGE = maxima
POSIX_SHELL = /bin/sh
SBCL_NAME = sbcl
SCL_NAME = scl
SCL_RUNTIME = lisp
SCL_RUNTIME_PATH = 
STRIP = 
VERSION = 5.14.0cvs
WISH = wish
acl_found = false
am__include = @am__include@
am__quote = @am__quote@
clisp_found = true
cmucl_found = false
default_layout_autotools = true
expanded_datadir = /usr/local/share
expanded_exec_prefix = /usr/local
expanded_infodir = /usr/local/share/info
expanded_libdir = /usr/local/lib
expanded_libexecdir = /usr/local/libexec
expanded_top_srcdir = /home/huang/.sbcl/lib/maxima
gcl_found = false
iconv_found = true
install_sh = /home/huang/.sbcl/lib/maxima/install-sh
openmcl_found = false
recode_found = false
sbcl_found = true
scl_found = false
win32 = false

verpkglibdir = $(pkglibdir)/5.14.0cvs
verpkglibexecdir = $(libexecdir)/maxima/5.14.0cvs
verpkgdatadir = $(pkgdatadir)/5.14.0cvs
docdir = $(verpkgdatadir)/doc
dochtmldir = $(docdir)/html
docsharedir = $(docdir)/share
demodir = $(verpkgdatadir)/demo
emacsdir = $(verpkgdatadir)/emacs
sharedir = $(verpkgdatadir)/share
instsrcdir = $(verpkgdatadir)/src
xmaximadir = $(verpkgdatadir)/xmaxima
insttestsdir = $(verpkgdatadir)/tests

FIGURES = xmaxima.gif plot2d.gif plot3d.gif sliders.gif
PDFFIGURES = xmaxima.pdf plot2d.pdf plot3d.pdf sliders.pdf
EXTRA_DIST = $(FIGURES) $(PDFFIGURES)

figuresdir = $(xmaximadir)/html/figures
figures_DATA = $(FIGURES)
subdir = interfaces/xmaxima/doc/figures
mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_CLEAN_FILES =
DIST_SOURCES =
DATA = $(figures_DATA)

DIST_COMMON = Makefile.am Makefile.in
all: all-am

.SUFFIXES:
$(srcdir)/Makefile.in:  Makefile.am $(top_srcdir)/common.mk $(top_srcdir)/configure.in $(ACLOCAL_M4)
	cd $(top_srcdir) && \
	  $(AUTOMAKE) --gnu  interfaces/xmaxima/doc/figures/Makefile
Makefile:  $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe)
uninstall-info-am:
figuresDATA_INSTALL = $(INSTALL_DATA)
install-figuresDATA: $(figures_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(figuresdir)
	@list='$(figures_DATA)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(figuresDATA_INSTALL) $$d$$p $(DESTDIR)$(figuresdir)/$$f"; \
	  $(figuresDATA_INSTALL) $$d$$p $(DESTDIR)$(figuresdir)/$$f; \
	done

uninstall-figuresDATA:
	@$(NORMAL_UNINSTALL)
	@list='$(figures_DATA)'; for p in $$list; do \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " rm -f $(DESTDIR)$(figuresdir)/$$f"; \
	  rm -f $(DESTDIR)$(figuresdir)/$$f; \
	done
tags: TAGS
TAGS:

DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)

top_distdir = ../../../..
distdir = $(top_distdir)/$(PACKAGE)-$(VERSION)

distdir: $(DISTFILES)
	@list='$(DISTFILES)'; for file in $$list; do \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  dir=`echo "$$file" | sed -e 's,/[^/]*$$,,'`; \
	  if test "$$dir" != "$$file" && test "$$dir" != "."; then \
	    dir="/$$dir"; \
	    $(mkinstalldirs) "$(distdir)$$dir"; \
	  else \
	    dir=''; \
	  fi; \
	  if test -d $$d/$$file; then \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      cp -pR $(srcdir)/$$file $(distdir)$$dir || exit 1; \
	    fi; \
	    cp -pR $$d/$$file $(distdir)$$dir || exit 1; \
	  else \
	    test -f $(distdir)/$$file \
	    || cp -p $$d/$$file $(distdir)/$$file \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
check: check-am
all-am: Makefile $(DATA)

installdirs:
	$(mkinstalldirs) $(DESTDIR)$(figuresdir)

install: install-am
install-exec: install-exec-am
install-data: install-data-am
uninstall: uninstall-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am

installcheck: installcheck-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	  INSTALL_STRIP_FLAG=-s \
	  `test -z '$(STRIP)' || \
	    echo "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'"` install
mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)

maintainer-clean-generic:
	@echo "This command is intended for maintainers to use"
	@echo "it deletes files that may require special tools to rebuild."
clean: clean-am

clean-am: clean-generic mostlyclean-am

distclean: distclean-am

distclean-am: clean-am distclean-generic

dvi: dvi-am

dvi-am:

info: info-am

info-am:

install-data-am: install-data-local install-figuresDATA

install-exec-am:

install-info: install-info-am

install-man:

installcheck-am:

maintainer-clean: maintainer-clean-am

maintainer-clean-am: distclean-am maintainer-clean-generic

mostlyclean: mostlyclean-am

mostlyclean-am: mostlyclean-generic

uninstall-am: uninstall-figuresDATA uninstall-info-am uninstall-local

.PHONY: all all-am check check-am clean clean-generic distclean \
	distclean-generic distdir dvi dvi-am info info-am install \
	install-am install-data install-data-am install-data-local \
	install-exec install-exec-am install-figuresDATA install-info \
	install-info-am install-man install-strip installcheck \
	installcheck-am installdirs maintainer-clean \
	maintainer-clean-generic mostlyclean mostlyclean-generic \
	uninstall uninstall-am uninstall-figuresDATA uninstall-info-am \
	uninstall-local


# Support for installation of DATA files in a generic directory
# with subdirectories.
# To use, set genericdir to point to the installation directory.
# Set genericdirDATA to hold the list of files to install.
# genericdirDATA may contain subdirectories. Subdirectories will
# be created if necessary.

install-data-local: install-datafiles
install-datafiles: $(genericdirDATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(genericdir)
	@list='$(genericdirDATA)'; for p in $$list; do \
	  if test -f $(srcdir)/$$p; then \
            if test ! -d `dirname $(DESTDIR)$(genericdir)/$$p`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(genericdir)/$$p`; \
            fi; \
	    echo " $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(genericdir)/$$p"; \
	    $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(genericdir)/$$p; \
	  else if test -f $$p; then \
            if test ! -d `dirname $(DESTDIR)$(genericdir)/$$p`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(genericdir)/$$p`; \
            fi; \
	    echo " $(INSTALL_DATA) $$p $(DESTDIR)$(genericdir)/$$p"; \
	    $(INSTALL_DATA) $$p $(DESTDIR)$(genericdir)/$$p; \
	  fi; fi; \
	done

uninstall-local: uninstall-datafiles
uninstall-datafiles:
	@$(NORMAL_UNINSTALL)
	list='$(genericdirDATA)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(genericdir)/$$p; \
	done
# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

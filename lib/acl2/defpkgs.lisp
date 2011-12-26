; ACL2 Version 3.1 -- A Computational Logic for Applicative Common Lisp
; Copyright (C) 2006  University of Texas at Austin

; This version of ACL2 is a descendent of ACL2 Version 1.9, Copyright
; (C) 1997 Computational Logic, Inc.  See the documentation topic NOTE-2-0.

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Written by:  Matt Kaufmann               and J Strother Moore
; email:       Kaufmann@cs.utexas.edu      and Moore@cs.utexas.edu
; Department of Computer Sciences
; University of Texas at Austin
; Austin, TX 78712-1188 U.S.A.

; This file, defpkgs.lisp, illustrates the idea that defpkg forms
; should be off in files all by themselves.  We require defpkg forms
; to be in files all by themselves to support the compilation of
; files.  Files with defpkg forms should only be loaded, never
; compiled.  Of course, during the compilation of other files, it will
; be necessary for defpkg files to be loaded at the appropriate time.
; The idea of putting defpkg forms in separate files is in the spirit
; of the CLTL2 idea of putting DEFPACKAGE forms in separate files.  By
; keeping package creation separate from compilation, one avoids many
; pitfalls and inconsistencies between Common Lisp implementations.

(in-package "ACL2")

(defpkg "ACL2-USER"
  (union-eq *acl2-exports*
            *common-lisp-symbols-from-main-lisp-package*)

  ":Doc-Section ACL2::Programming

  a package the ACL2 user may prefer~/

  This package imports the standard Common Lisp symbols that ACL2
  supports and also a few symbols from package ~c[\"ACL2\"] that are
  commonly used when interacting with ACL2.  You may prefer to select
  this as your current package so as to avoid colliding with ACL2
  system names.~/

  This package imports the symbols listed in
  ~c[*common-lisp-symbols-from-main-lisp-package*], which contains
  hundreds of CLTL function and macro names including those supported
  by ACL2 such as ~ilc[cons], ~ilc[car], and ~ilc[cdr].  It also imports the symbols in
  ~c[*acl2-exports*], which contains a few symbols that are frequently
  used while interacting with the ACL2 system, such as ~ilc[implies],
  ~ilc[defthm], and ~ilc[rewrite].  It imports nothing else.

  Thus, names such as ~ilc[alistp], ~ilc[member-equal], and ~ilc[type-set], which are
  defined in the ~c[\"ACL2\"] package are not present here.  If you find
  yourself frequently colliding with names that are defined in
  ~c[\"ACL2\"] you might consider selecting ~c[\"ACL2-USER\"] as your current
  package (~pl[in-package]).  If you select ~c[\"ACL2-USER\"] as the
  current package, you may then simply type ~ilc[member-equal] to refer to
  ~c[acl2-user::member-equal], which you may define as you see fit.  Of
  course, should you desire to refer to the ~c[\"ACL2\"] version of
  ~ilc[member-equal], you will have to use the ~c[\"ACL2::\"] prefix, e.g.,
  ~c[acl2::member-equal].

  If, while using ~c[\"ACL2-USER\"] as the current package, you find that
  there are symbols from ~c[\"ACL2\"] that you wish we had imported into
  it (because they are frequently used in interaction), please bring
  those symbols to our attention.  For example, should ~ilc[union-theories]
  and ~ilc[universal-theory] be imported?  Except for stabilizing on the
  ``frequently used'' names from ~c[\"ACL2\"], we intend never to define a
  symbol whose ~ilc[symbol-package-name] is ~c[\"ACL2-USER\"].")


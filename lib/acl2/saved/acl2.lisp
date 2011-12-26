; ACL2 Version 3.1 -- A Computational Logic for Applicative Common Lisp
; Copyright (C) 2006  University of Texas at Austin

; This version of ACL2 is a descendent of ACL2 Version 1.9, Copyright
; (C) 1997 Computational Logic, Inc.  See the documentation topic
; NOTE-2-0.

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

; Acknowledgments:  Bob Boyer contributed crucially to the design and
; implementation of early versions of ACL2.  Many others, largely at CLI, have
; also contributed to the design of certain features.  We especially thank
; Bishop Brock and John Cowles for their contributions.  See the documentation
; topic ACKNOWLEDGMENTS for more information.

; This research was supported in part at Computational Logic, Inc. by the
; Defense Advanced Research Projects Agency (DARPA), ARPA Orders #6082, 9151
; and 7406 and by the Office of Naval Research, contracts numbers
; N00014-88-C-0454, N00014-91-C-0130, and N00014-94-C-0193.  The views and
; conclusions contained in this document are those of the author(s) and should
; not be interpreted as representing the official policies, either expressed or
; implied, of Computational Logic, Inc., Office of Naval Research, DARPA or the
; U.S. Government.

; This file, acl2.lisp, (a) builds the packages for the ACL2 system, (b)
; defines the functions for loading and compiling ACL2 and for running code
; verified using ACL2, and (c) makes a couple of checks concerning minor,
; non-CLTL, assumptions that we make about Common Lisps in which ACL2 is to
; run.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               FILES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is the file acl2.lisp, the first source file for ACL2.  The
; names of the other ACL2 source files are listed under *acl2-files*
; in the file axioms.lisp.

; All of ACL2 is written in Common Lisp and we expect that ACL2 will
; run in any Common Lisp except for those Common Lisps which fail the
; tests we execute upon loading this file, acl2.lisp.  With the
; exception of this file, the file interface-raw.lisp, and those
; constructs after the #-acl2-loop-only passim only (especially in the
; file axioms.lisp), ACL2 is written in the applicative Common Lisp
; for which ACL2 is a verification system.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          LISP BUGS AND QUIRKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We do not yet support GCL ANSI.
#+(and gcl ansi-cl)
(error
 "FATAL ERROR: ACL2 does not yet support GCL ANSI.  Please use a non-ANSI GCL.")

; To use ACL2 under Lispworks 3.2.0, execute the following to work around a
; bug.

; #+lispworks
; (setf (svref ccl::*funny-symbol-char-table* (char-code #\.)) t)

; Next, we correct *features* in a Windows version of GCL.  In fact there is no
; reason to restrict to GCL; we assume that any Lisp image with feature WINNT
; is intended for a Windows system.

#+(and (or winnt win32) (not mswindows))
(setq *features*
      (cons :mswindows *features*))

#+(and (or mswindows winnt) unix)
(setq *features*
      (delete :unix *features*
              :test
              (function (lambda (x y)
                          (equal (symbol-name x) (symbol-name y))))))

; Speed up I/O in Allegro 6 and beyond:
#+(and allegro allegro-version>= (version>= 6 0))
(setf (stream-external-format *terminal-io*)
      (excl::find-external-format
       #+unix :latin1-base
       #-unix :latin1))

; Turn off automatic declaration of special variables, in particular since we
; do not want state declared special; see the comment above
; (eval '(setq state *the-live-state*)) in load-acl2.
#+cmu
(setq ext:*top-level-auto-declare* nil)

; Turn off compiler verbosity going to error stream, since even >& does not
; seem to redirect that stream to a file.
#+(or cmu sbcl)
(setq *compile-verbose* nil)
#+(or cmu sbcl)
(setq *compile-print* nil)

; Turn off gc verbosity (same reason as above).
#+cmu
(setq ext:*gc-verbose* nil)

; See later in this file for with-warnings-suppressed (after we introduce and
; enter the ACL2 package).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            CLTL1/CLTL2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For the most part, we program in a subset both of CLTL1 and CLTL2.
; However, there are a few places, notably the name of the main
; package for Lisp symbols, where this is impossible.  So we use the
; read conditional #+CLTL2.  If one wishes to run ACL2 under CLTL2,
; execute the following form before commencing compiling or loading:

;        (push :CLTL2 *features*)

; For example, for Allegro and lispworks (respectively) we have the following.

#+(or ansi-cl draft-ansi-cl-2 lispworks clisp openmcl)
(push :CLTL2 *features*)

; We use IN-PACKAGE in a way that is consistent with both CLTL1 and
; CLTL2, namely as a macro (i.e., whose argument is not evaluated) for
; switching *package* to an existing package, value ignored.

(in-package #-CLTL2 "USER" #+CLTL2 "COMMON-LISP-USER")


; Changes Made in Support of CMU Lisp

; (0) Cmulisp doesn't support EVAL-WHEN.  This meant that in the #+cmu case
;     I had to put down special code to try to do what other lisps do.
;     Generally, this involved just not checking for certain errors (compiling
;     files that weren't supposed to be compiled) in #+cmu that were checked
;     in other lisps.  In one case, namely the initialization of 
;     current-acl2-world, it involved a little more.

; (1) cmulisp switches from *standard-input* to *terminal-io* when the input
;     stream reaches eof; our other lisps all exit to the operating system.
;     This means all the batch jobs we submit via make have to be arranged to
;     exit from cmulisp before eof.  This required changing the top-level
;     makefile and the makefiles of all the books.  I generally put a
;     `:q #+cmu (lisp::quit)' at the end of each workxxx.
;     These could be replaced simply by `:q (acl2::exit-lisp)'.

; (2) Cmulisp checks type assertions at runtime.  I found two of our assertions
;     violated by actual use.  In fmt-char we mistakenly claimed the string's
;     length was less than 101.  This was a typo -- elsewhere in the same
;     function we claimed it was just a fixnum -- apparently caused by 
;     copying a type-declaration and failing to edit it thoroughly.  (Another
;     variable was correctly limited to 101.)

;     In maximal-elements1, used in the selection of induction candidates,
;     we claimed (by using int=) that the scores for an induction candidate
;     are integers when in fact they are rationals.

; (3) Evidently, all functions in cmulisp pass the compiled-function-p test.
;     If you defun foo and immediately get its symbol-function you get
;     an object like #<Interpreted function ...>.  If you ask whether
;     this object is a compiled-function-p you get t.  If you compile
;     foo the symbol-function changes to an object like #<Function
;     ...>, which also passes the test.
    
;     The fact that everything in a symbol-function field looks like a compiled
;     function killed us in an interesting way.  Most locally, it made
;     compile-uncompiled-*1*-defuns write out an empty file of functions to
;     compile, because everything looked compiled already.  But where that 
;     really got us was that we use that function to create TMP1.lisp during
;     the bootstrapping.  TMP1.lisp, recall, contains the mechanically
;     generated executable counterparts of logic mode functions defined in
;     axioms.lisp.  By not generating these we built an image in which the
;     relevant functions were undefined.  Because of the rugged way we invoke
;     them, catching errors and producing a HIDE term if we can't eval them,
;     we survived the certification of many books before we finally got to a
;     proof that couldn't be done without running one of those functions.
;     The proof, in the bdd books, required evaling (nth -1 nil), which
;     according to the axioms is nil but which we couldn't deduce because
;     ACL2_*1*_COMMON-LISP::NTH was undefined.

;     My fix was to define and use compiled-function-p! instead of the standard
;     compiled-function-p.  The new function has a #+/-cmu split in it.  In the
;     cmu case I ask

;     (not (eq (type-of (symbol-function fn)) 'eval:interpreted-function))

;     So I say fn is compiled if its symbol-function is not an object like
;     #<Interpreted function ...>.

; (4) CMU Lisp does not allow us to "undefine" a macro-function.  That is,
;     one is not permitted to store a nil into the macro-function
;     field because nil is not a function.  We do this when we
;     implement undo.  We handle it by storing a dummy function
;     instead, and change the notion of when a symbol is virgin to
;     recognize the case that its symbol-function or macro-function is
;     the dummy.

; (5) We made a few fixes and cleanups in order to avoid compiler warnings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                              PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We never intend that this file should be compiled, and hence we do
; not need to obey the CLTL1 strictures about putting all IN-PACKAGE
; forms near the front.

(let ((lisp-pkg (find-package "LISP")))
  (if lisp-pkg
      (let ((cl-pkg (find-package "COMMON-LISP")))
        (cond
         ((and cl-pkg (eq cl-pkg lisp-pkg))

; Then all is well.

          )
         #+cmu

; Starting with CMUCL 19a, lisp-pkg and cl-pkg are no longer the same.  We view
; CMUCL as CLTL2; see (push :CLTL2 *features*) above, noting that :ANSI-CL is
; in *features*.  So in this case, we simply ignore lisp-pkg.  Probably we can
; do the same for most other lisps, except for GCL where LISP is populated but
; COMMON-LISP might not be to any significant extent.

         (t nil)
         #-cmu
         (t
          (when cl-pkg ; but by the test above, cl-pkg is not lisp-pkg
            #-gcl

; Perhaps we can just add the present lisp to the #+cmu above.

            (error "This Lisp is unsuitable for ACL2, because the ~
                    COMMON-LISP~% package is defined but is not the LISP ~
                    package.")
            #+gcl

; Early versions of GCL 2.4.3/2.5.0 had a "COMMON-LISP" package that was
; initially populated only with LISP::T and LISP::NIL.  It seems safe to move
; any GCL COMMON-LISP package out of the way before we make "COMMON-LISP" a
; nickname for "LISP".

            (rename-package "COMMON-LISP" "COMMON-LISP-renamed"))
          (let ((old-name (package-name lisp-pkg)) ; reuse old name, nicknames
                (old-nicknames (package-nicknames lisp-pkg)))
            (rename-package "LISP"
                            old-name
                            (cons "COMMON-LISP" old-nicknames))))))))

#-cmu
(eval-when #-cltl2 (compile)
           #+cltl2 (:compile-toplevel)
           (error "The file acl2.lisp should never be compiled."))

(dolist
 (p (list-all-packages))
 (cond ((equal 4 (string< "ACL2" (package-name p)))
        (format t
                "~%~%Warning:  There is already a package with the ~
                 name ~a. ~%The ACL2 implementation depends upon ~
                 having complete knowledge of ~%and control over any ~
                 packge whose name begins with the ~%four letters ``ACL2'', ~
                 so ACL2 may not work in this Lisp." (package-name p))
        (cond ((package-use-list p)
               (format t "~%~%Warning:  The package with name ~a ~
                   USES the packages in the list ~a.  ACL2 will not work ~
                   in state of affairs."
                       (package-name p) (package-use-list p)))))))

(or (find-package "ACL2")
    (make-package "ACL2" :use nil))

; The definition of defconst appears just below because we use it
; early in axioms.lisp.  But first, we define the constant
; *the-live-state* because it is used below in the definition of
; defconst and cmulisp warns if we use it before it has been defined.

; And, in order to define *the-live-state* we need the ACL2_INVISIBLE
; package, which we define here.  This package is used for a few odd
; objects that are to be hidden from the ACL2 user.

(let ((inv "ACL2_INVISIBLE"))
  (or (find-package inv)
      (make-package inv :use nil)))

; The value of the constant *the-live-state* is actually just a
; symbol, but that symbol is the unique representative of the one
; single active, global, real-time state of ACL2, which is represented
; both in real-time (e.g., characters not yet typed) and also rather
; efficiently, using typical von Neumann storage techniques.
; Functions that wish to access the global state must have received
; *the-live-state* as an arg.  Functions that modify this global state
; must receive it as an arg and return it.

(defconstant acl2::*the-live-state* 'acl2_invisible::|The Live State Itself|)

; (Defconst *var* term) is essentially just (defparameter *var* term).
; But things are made complicated by the desire not to evaluate term
; unnecessarily.  Suppose (defconst *var* term) appears in a certified
; book, say "book.lisp".  Then when the events in "book.lisp" are
; processed, a CLTL-COMMAND is executed which causes (defconst *var*
; term) to be evaluated in the underlying raw lisp, assigning a value
; to *var* in Lisp.  But then the compiled file, say "book.o", is
; loaded.  If defconst were just defparameter, term would be evaluated
; again (yielding a presumably EQUAL value) and the first setting of
; *var* would be discarded in favor of the second.

; We avoid this in the code below by saving, on the raw Lisp property
; list of *var*, under the key 'REDUNDANT-RAW-LISP-DISCRIMINATOR, a
; triple, (DEFCONST term . val), giving the term we evaluated to
; produce the setting of the var and the value, val, produced.  When a
; defconst (defconst *var* term) is evaluated, we ask whether *var*
; has a value.  If not, we just proceed normally.  But if it has a
; value, we insist that the discriminator be present and appropriate or
; else we cause a hard error.  By appropriate in this case we mean
; that it be a DEFCONST with the same (EQUAL) term to evaluate and
; that the value produced last time be EQ to the current setting of
; *var*.  The last check is meant to provide some safety if the user
; has manually set *var* in raw lisp, as with setq, since the last
; defconst to it.

; We anticipate that redundant-raw-lisp-discriminator may be used by
; the support functions for other events, e.g., defstobj.  So the
; value of that property is not necessarily (DEFCONST term . val) but
; may depend on the kind of event that stored it.  The reason we put
; the discriminator on the raw lisp property list of *var* rather than
; looking in the world of *the-live-state*, where we could in
; principle find an event-landmark, is that we execute many defconsts
; in axioms.lisp, before the world processing functions, like getprop,
; are defined and so the defmacro below must expand to code that can
; be executed in a partially booted ACL2.

(defmacro acl2::defconst (name term &rest rst)
  (declare (ignore rst)) ; e.g., documentation
  `(defparameter ,name
     (cond ((boundp ',name)

; Even though ',name is bound, we refer to its value with
; (symbol-value ',name) rather than simply an in-line ,name, to avoid
; compiler warnings.

            (let ((d (get ',name 'acl2::redundant-raw-lisp-discriminator)))
              (cond
               ((and (consp d)
                     (eq (car d) 'acl2::defconst)
                     (consp (cdr d))
                     (equal (car (cdr d)) ',term)
                     (eq (cdr (cdr d)) (symbol-value ',name)))
                (symbol-value ',name))
               ((acl2::raw-mode-p acl2::*the-live-state*)

; In this case we allow redeclaration of the constant; this is, after all, raw
; mode, where there are no guarantees.

                ,term)
               (t 
                (error "Illegal attempt to redeclare the constant ~s."
                       ',name)))))

; If ',name is not bound, we must evaluate ,term.  Note that we do so
; outside of all local bindings, so as not to disturb variables in
; term.  (There should be none -- this is supposed to be a constant,
; but you never know!)  We may want to enforce that this code is only executed
; during the boot-strap; see the Essay on Guard Checking.

           (t (let* ((val ,term)
                     (d (list* 'acl2::defconst ',term val)))
                (setf (get ',name 'acl2::redundant-raw-lisp-discriminator)
                      d)
                (cdr (cdr d)))))))

; We now get our imports for package ACL2, putting them into the
; variable acl2::*common-lisp-symbols-from-main-lisp-package*.

; We use different variables for common-lisp-symbols-from-main-lisp-package*,
; *acl2-version*, and *acl2-files*, in order to avoid compiler warnings for
; redefined variables.  Actually, *acl2-version* no longer exists starting with
; Version_2.9.1, but we keep the name below nonetheless.

(defvar acl2::*copy-of-common-lisp-symbols-from-main-lisp-package*)
(defvar acl2::*copy-of-common-lisp-specials-and-constants*)
(defvar acl2::*copy-of-acl2-version*)
(defvar acl2::*copy-of-acl2-files*)

; CLISP version 2.30 (along with perhaps other versions) locks the LISP
; package.  That causes problems when we try to read the second form in
; axioms.lisp, which defines
; acl2::*common-lisp-symbols-from-main-lisp-package*, when CLISP tries to read
; some of the symbols in that list (e.g., CALL-METHOD) into the COMMON-LISP
; package.  (Specifically, INTERN breaks.)  We use eval below to avoid any
; chance (we hope) of getting an "undefined function" warning with earlier
; CLISP versions.

#+clisp
(if (fboundp 'package-lock)
    (eval '(setf (package-lock "LISP") nil)))

; CLISP version 2.33 defines the symbol custom::*suppress-check-redefinition*,
; but version 2.30 does not.  We temporarily unlock that package if necessary
; in order to define this variable.  While we are at it we make the variable
; special, in order to avoid potential compiler warnings in 2.30 when we bind
; it but never use it.

#+clisp
(if (not (find-symbol "*SUPPRESS-CHECK-REDEFINITION*" "CUSTOM"))
    (if (fboundp 'package-lock)
        (let ((old-lock (package-lock "CUSTOM")))
          (eval '(setf (package-lock "CUSTOM") nil))
          (let ((sym (intern "*SUPPRESS-CHECK-REDEFINITION*" "CUSTOM")))
            (eval `(defvar ,sym nil)))
          (eval `(setf (package-lock "CUSTOM") ',old-lock)))
      (let ((sym (intern "*SUPPRESS-CHECK-REDEFINITION*" "CUSTOM")))
        (eval `(defvar ,sym nil)))))

(with-open-file
 (fl "axioms.lisp" :direction :input)

;  Get into the main lisp package in order to read in
;  *common-lisp-symbols-from-main-lisp-package*.

 (let ((*package* (find-package #-CLTL2 "LISP" #+CLTL2 "COMMON-LISP")))

;  Skip the in-package

   (read fl)

;  Do the defconst for *common-lisp-symbols-from-main-lisp-package*.

   (setq acl2::*copy-of-common-lisp-symbols-from-main-lisp-package*
         (eval (caddr (read fl))))
   (import acl2::*copy-of-common-lisp-symbols-from-main-lisp-package* "ACL2")

   (setq acl2::*copy-of-acl2-version*
;  Keep this in sync with the value of acl2-version in *initial-global-table*.
         (concatenate 'string
                      "ACL2 Version 3.1"
                      #+non-standard-analysis
                      "(r)"
                      #+(and mcl (not openmcl))
                      "(mcl)"))

;  Do the defconst for *acl2-files*.

   (setq acl2::*copy-of-acl2-files*
         (eval (caddr (read fl))))

;  Do the defconst for *common-lisp-specials-and-constants*.

   (setq acl2::*copy-of-common-lisp-specials-and-constants*
         (eval (caddr (read fl))))))

(in-package "ACL2")

(defparameter *global-package-prefix* "ACL2_GLOBAL_")

(defparameter *1*-package-prefix* "ACL2_*1*_")

(defun make-global-package (x)
  (let* ((n (concatenate 'string *global-package-prefix* x))
         (p (find-package n)))
    (cond (p
           (do-symbols (sym p)
                       (makunbound sym)))
          (t (make-package n :use nil)))))

(defun make-*1*-package (x)
  (let* ((n (concatenate 'string *1*-package-prefix* x)))

; Unlike make-global-package, here we really don't have to worry about bound
; (or fbound) symbols.  Presumably ``bound'' is irrelevant, and ``fbound'' is
; taken care of by our undoing mechanisms.

    (make-package n :use nil)))

(or (find-package "ACL2-INPUT-CHANNEL")
    (make-package "ACL2-INPUT-CHANNEL" :use nil))

(or (find-package "ACL2-OUTPUT-CHANNEL")
    (make-package "ACL2-OUTPUT-CHANNEL" :use nil))


; Next we define the initial global and *1* packages.

; Keep the following in sync with *main-lisp-package-name*.
(make-global-package "COMMON-LISP")
(make-global-package "KEYWORD")
(make-global-package "ACL2")
(make-global-package "ACL2-INPUT-CHANNEL")
(make-global-package "ACL2-OUTPUT-CHANNEL")

; Keep the following in sync with *main-lisp-package-name*.
(make-*1*-package "COMMON-LISP")
; Functions cannot be defined in the keyword package, so we do not do so.
;  (make-*1*-package "KEYWORD")
(make-*1*-package "ACL2")
(make-*1*-package "ACL2-INPUT-CHANNEL")
(make-*1*-package "ACL2-OUTPUT-CHANNEL")

; Common Lisp does not require that the symbol-package of the basic
; Lisp symbols be the basic LISP or COMMON-LISP package, but merely
; that those symbols be exported from that package.

(defparameter acl2::*initial-lisp-symbol-mark*
  'acl2_invisible::initial-lisp-symbol-mark)

(dolist (sym *copy-of-common-lisp-symbols-from-main-lisp-package*)
        (setf (get sym *initial-lisp-symbol-mark*) "COMMON-LISP"))

(defconstant *acl2-package* (find-package "ACL2"))

(dolist (x *features*)
        (cond ((or (equal "AKCL-SET-MV"
                          (symbol-name x))
                   (equal "ACL2-LOOP-ONLY"
                          (symbol-name x))
                   (equal "ACL2-METERING"
                          (symbol-name x)))
               (format t "~%~%Warning:  This Common Lisp may be ~
                          unsuitable for ACL2 because a symbol with~
                          ~%the name \"ACL2-LOOP-ONLY\", ~
                          \"AKCL-SET-MV\" or \"ACL2-METERING\" is ~
                          a member of *FEATURES*."))))

#+akcl
(if (fboundp 'si::set-mv)
    (push :akcl-set-mv *features*)
  (error "Use a version of ACKL after 206"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                CHECKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We need the following macro in order to turn off style warnings in sbcl.
; Perhaps we will find it useful in other lisps as well.  It is defined here
; instead of the "LISP BUGS AND QUIRKS" section because we want to define this
; in the ACL2 package.
(defmacro with-warnings-suppressed (&rest forms)

; Keep this in sync with the handler-bind call in init.lisp.  Thanks to Juho
; Snellman for the SBCL form.

  #+sbcl
  `(handler-bind
    (#+ansi-cl
     (style-warning (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'muffle-warning))))
    ,@forms)
  #+cmucl
  `(progn (setq ext:*gc-verbose* nil) ,@forms)
  #+lispworks
  `(let ((compiler::*redefinition-action* :QUIET)) ,@forms)
  #-(or cmucl sbcl lispworks)
  (if (cdr forms) `(progn ,@forms) (car forms)))

(defconstant acl2::*acl2-status-file*
  (make-pathname :name "acl2-status"
                 :type "txt"))

(defun acl2::check-suitability-for-acl2 ()

; As of version 18a, cmulisp spews gc messages to the terminal even when
; standard and error output are redirected.  So we turn them off.

  (with-warnings-suppressed
   (or (not (probe-file *acl2-status-file*))
       (delete-file *acl2-status-file*))
   (load "acl2-check.lisp")
   (with-open-file (str *acl2-status-file*
                        :direction :output)
                   (format str
                           "~s"
                           :checked))
   t))

(defun note-compile-ok ()
  (progn (or (not (probe-file *acl2-status-file*))
             (delete-file *acl2-status-file*))
         (with-open-file (str *acl2-status-file*
                              :direction :output)
                         (format str
                                 "~s"
                                 :compiled))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       COMPILING and LOADING, PART 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To compile ACL2, invoke (load "acl2.lisp") and then invoke
; (acl2::compile-acl2).  Having compiled ACL2, to build ACL2, invoke
; (load "acl2.lisp") and then invoke (acl2::load-acl2).  To run ACL2
; verified code, one may either load all of ACL2 as above, or
; alternatively, having complied ACL2, invoke (load "acl2.lisp") and
; invoke (acl2::load-acl2-execution-environment).  The top-level user
; functions for ACL2 are in package "ACL2", so invoke (in-package
; "ACL2") to obviate typing package names.

; NOTE:  In order to compile ACL2, checks must first be run on the suitability
; of the underlying Common Lisp implementation, by executing
; (check-suitability-for-acl2).  If the Common Lisp is suitable, this form will
; write the file acl2-status.txt with the symbol :CHECKED.  Successful
; compilation should write out that same file with the symbol :COMPILED.

(defvar *lisp-extension* "lisp")

(defparameter *compiled-file-extension*

; Note that books/Makefile-generic deals with compiled file extensions as well
; in its "clean" target.
; Thanks to Gary Byers for suggested the following approach for #+ansi-cl,
; which however appears to work for all Lisps supported as of early 2006.

  (pathname-type (compile-file-pathname "foo.lisp")))

; The user is welcome to modify the following declaim form.

(declaim (optimize #+(or cmu sbcl lucid lispworks) (compilation-speed 0)
; The following may allow more tail recursion elimination (from "Lisp
; Knowledgebase" at lispworks.com); might consider for Allegro CL too.
                   #+lispworks (debug 0)
                   #+cmu (extensions:inhibit-warnings 3)
                   #+sbcl (sb-ext:inhibit-warnings 3)
                   (speed 3)
                   (space #+cmu 1 #-cmu 0) ; no longer necessary?

; WARNING:  Do not proclaim (cl-user::fixnum-safety 0) for Lispworks.  Any
; fixnum-safety less than 3 expects all integers to be fixnums!

; Consider using (safety 3) if there is a problem with Lispworks.  It enabled
; us to see a stack overflow involving collect-assumptions in the proof of
; bitn-lam0 from books/rtl/rel2/support/lop3.lisp.  (See save-acl2-in-lispworks
; for how we have eliminated that stack overflow.)

                   (safety 0)))

#+sbcl ; turn off compiler notes (noisy)
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

#+allegro
(eval (read-from-string "
  (SETQ COMPILER::DECLARED-FIXNUMS-REMAIN-FIXNUMS-SWITCH
        #'(LAMBDA (X Y Z #+(VERSION>= 4 1) D) NIL)) "))

; The following appears to allow tail recursion elimination for functions
; locally defined using LABELS.  This is important for efficiency since we
; use LABELS in defining executable counterparts (see e.g. oneify-cltl-code).
#+allegro
(setq compiler:tail-call-non-self-merge-switch t)

; Lispworks Version 4.2.0 has issued the following complaint during compilation
; until the next form was executed:
; **++++ Error in NTH-UPDATE-REWRITER1: 
;   Function size 73824 is too large.
#+lispworks
(cl-user::toggle-source-debugging nil)

(defmacro our-with-compilation-unit (form)

; In fact, with-compilation-unit is only defined in dpANS, not CLtL2.  But MCL
; and OpenMCL do seem to support it, so we allow it with #+cltl2 and #+openmcl.
; We also have noticed that while :CLTL2 belongs to *features* in Version
; :CMU17 (presumably 17f), it does not belong in a cmucl version 18d that we
; obtained for Linux, even though with-compilation-unit continues to be
; defined.

  #+(or cltl2 cmu openmcl)
  `(common-lisp::with-compilation-unit
    ()
    ,form)
  #-(or cltl2 cmu)
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           BASIC FUNCTIONS TO BE COMPILED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  Functions for proclaiming and for defining ACL2's Implementation of the
;  Backquote Readmacro are to be compiled, so we do not include them in the
;  present file.  Instead we put them in acl2-fns.lisp, after defining the
;  following constant.  (We could put this defconstant in acl2-fns.lisp, but
;  CLISP would warn unless we made it conditional on a (not (boundp ..))
;  check.  That sort of fix has gotten us into trouble with Allegro, so we take
;  the simpler solution of putting the defconstant in this file, which is
;  loaded only once (there is no compiled version of this file to load).

(defconstant *acl2-read-character-terminators*
  '(#\Tab #\Newline #\Page #\Space #\" #\' #\( #\) #\; #\` #\,))

(our-with-compilation-unit
 (let ((object-file (make-pathname :name "acl2-fns"
                                   :type *compiled-file-extension*)))

; At one time we avoided recompiling and had the following code inside a COND:
   #|
   ((and (probe-file object-file)
         (<= (file-write-date "acl2-fns.lisp")
             (file-write-date object-file)))
    (load object-file))
|#
; But for example, if we compile with Allegro under SunOS and then later build
; the system with Allegro under Linux, the same ".fasl" extension could fool us
; into thinking that recompilation is not necessary.

   (load "acl2-fns.lisp") ;we like to load before compiling
   (compile-file "acl2-fns.lisp")

; Note that load-compiled is not used below, but on the other hand we are still
; using the original readtable here so that's not a problem.

   (load object-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           ACL2-READTABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *acl2-readtable*
  (copy-readtable nil)
  "*acl2-readtable* is the readtable we use (a) to prevent the use
#. to cause evaluation during READing (b) and to define our own version
of backquote.")

(defun turn-off-sharp-dot ()
  (set-dispatch-macro-character
   #\#
   #\.
   #'(lambda (stream char n)

; MCL noticed that stream, char, and n are not used:

       (declare (ignore stream char n))

; We used to cause the following hard error in response to #. so pre-empting
; its meaning for abort is no real loss to the ACL2 user.
;       (error "ACL2 does not support #. syntax.")

       (throw 'local-top-level :abort))))

(defun modify-acl2-readtable (do-all-changes)
  (let ((*readtable* *acl2-readtable*))

; Backquote

    (set-macro-character
     #\`
     #'(lambda (stream char)
         (declare (ignore char))
         (let ((*backquote-counter* (1+ *backquote-counter*)))
           (backquote (read stream t nil t)))))

; Comma

    (set-macro-character
     #\,
     #'(lambda (stream char)
         (declare (ignore char))
         (let ((*backquote-counter* (1- *backquote-counter*)))
           (cond ((< *backquote-counter* 0)
                  (clear-input stream)
                  (error "Illegal comma encountered by READ.")))
           (case (peek-char nil stream t nil t)
             (#\@ (read-char stream t nil t)
              (list *comma-atsign* (read stream t nil t)))
             (#\. (error ",. not allowed in ACL2 backquote forms."))
             (otherwise (list *comma* (read stream t nil t)))))))

;  Turn off #.

;  Actually, we do not turn it off but make it the all-powerful abort signal.
;  That is, if at any time the user types #. while under the *acl2-readtable*
;  the effect is to abort all the way out to the top-most call of LD.  See
;  LD-FN for some comments.  We avoid doing this if do-all-changes is nil (as
;  it is for CLISP; in that case we handle #. later).

    (when do-all-changes
      (turn-off-sharp-dot))

;  Turn off #,

    (set-dispatch-macro-character
     #\#
     #\,
     #'(lambda (stream char n)

; MCL noticed that stream, char, and n are not used:

         (declare (ignore stream char n))
         (error "ACL2 does not support #, syntax.")))

;  Keep control of character reader.  However, we do not need to keep such
;  control when reading in a .fas file for CLISP, and in fact, the set-theory
;  book's use of (code-char 255) generates
;  #\LATIN_SMALL_LETTER_Y_WITH_DIAERESIS in set-theory.fas.  We see no reason
;  to restrict reading of such characters in .fas files.

    (when do-all-changes
      (set-dispatch-macro-character
       #\#
       #\\
       #'acl2-character-reader))))

#+cmu
(modify-acl2-readtable t)
#-cmu
(eval-when
 #-cltl2
 (load eval compile)
 #+cltl2
 (:load-toplevel :execute :compile-toplevel)
 #-clisp
 (modify-acl2-readtable t)
 #+clisp
 (progn (modify-acl2-readtable nil)
        (defparameter *acl2-readtable-clisp-fas*
          (copy-readtable *acl2-readtable*))
        (let ((*readtable* *acl2-readtable*))
          (turn-off-sharp-dot)
          (set-dispatch-macro-character
           #\#
           #\\
           #'acl2-character-reader))))

(defmacro load-compiled (&rest args)
  #+clisp
  `(let ((*readtable* *acl2-readtable-clisp-fas*))
     (load ,@args))
  #-clisp
  `(load ,@args))

#|
                      Remarks on *acl2-readtable*


Because read-object$ calls the Common Lisp function read, read-object$
is a function of the values of the Common Lisp symbols (a)
*readtable*, (b) *package*, and (c) *features*.  In ACL2, the user can
specify the package to use, via in-package, which sets the global
current-package.  The user of ACL2 has no (legitimate) control over the
readtable, which is set above and discussed below.

As for *features*, we currently permit full use of the *features*
facility of the Common Lisp reader, with this caveat: it is an
official part of the ACL2 story that *features* should have the same
setting throughout all phases of ACL2 usage.  That is, the user must
set up *features* at the beginning, before starting to use ACL2 and
the user must then leave *features* alone (even though the
implementors of ACL2 put :acl2-loop-only onto *features* during
boot-strapping).  One bad consequence of our *features* policy is that
verified files will not in general be verifiable or useable in other
Lisp implementations or installations unless the settings of
*features* relevant to one's usages of the #+ and #- are the same in
the two Lisp implementations.  One simple way to obtain peace of mind
on this topic is for the user not to use #+ or #- at all!  It might be
cleaner for us simply to prohibit the use of the #+ and #- readmacros
all together in ACL2.  This could be done at the cost of replacing the
many uses of #+ and #- in axioms.lisp, and a few other places, with
some sort of regular macro.

Below is a detailed examination of the default Common Lisp readtable
from the perspective of ACL2.  We want to make sure that when we read,
we do not have side effects (e.g. via #.) of ACL2 but will merely
either (a) cause an error or (b) generate a Lisp object, which we then
will check with bad-lisp-object before handing it to ACL2 functions.

All of the standard Common Lisp characters are either white space or
constituent, with the following exceptions, which are read macros:

  "  quote
  #  number sign
  '  quote
  () parentheses
  ,  comma
  ;  comment
  \  single escape
  `  backquote
  |  multiple escape

With the exception of number sign, backquote and comma, we certainly
want all of these readmacros to have their usual Common Lisp
semantics.

We have defined our own backquote and discussed it above.

We now review the # situation:

  ## and #= are for reading possibly circular stuff
         bad-lisp-object may run forever
  #'  reads as function
         won't hurt anything
  #(  vector
         will be rejected by bad-lisp-object
  #)  signals an error
         enough said
  #*  bit vector
         will be rejected by bad-lisp-object
  #,  load-time evaluation
         we shut it off
  #0-#9 used for infix arguments
         ok
  #:  uninterned symbol
         will be rejected by bad-lisp-object
  #<  signals an error
         enough said
  #\  character object
         will be checked by bad-lisp-object; see also below
  #|  start comment, ended by |#
         ok
  #<backspace | tab | newline | linefeed | page | return | space>
      signals an error -- ok
  #+  and #-
      see the discussion of *features* above
  #.  read time evaluation
         we shut it off
  #A, #a  arrays
         will be checked by bad-lisp-object
  #B, #b  binary rational
         ok
  #C, #c complex
         ok (rejected by bad-lisp-object except for rational components)
  #O, #o octal
         ok
  #P, #p pathname
         will be checked by bad-lisp-object
  #R, #r radix-n
         fine
  #S, #s structure
         will be rejected by bad-lisp-object
  #X, #x hex
         ok

Eventually, it will be best to define a read function for ACL2 solely
in terms of ACL2 character reading primitives.  Common Lisp read is
ambiguous.  There is the ambiguity of backquote described above.
There is the abiguity of which tokens get read as numbers.  To make
matters a little more scary, there is nothing that prevents a Common
Lisp implementation from adding, for example, a new # readmacro option
that would provide something as potentially catastrophic as #.  One
obstacle to doing a read within ACL2 this is efficiency.  For example,
ACL2 does not now support unread-char.  And given the requirement that
whatever is specified in a guard must be checkable, it is unclear now
how best to add unread-char since Common Lisp does permit one to
detect whether a stream is in a ``just unread'' state.  ACL2 could
enable one to answer such a question, but at the cost of having to
store the information every time that a character was unread.

|#

;          ACL2's Implementation of the character reader

; We have decided to take full charge of everything that # reader
; does, which is just a part of the way towards writing READ totally
; from scratch.  And we are pretty conservative about what #\ does
; accept; one can always get the characters one wants by using
; code-char.  Notice for example that if we're not careful, then ACL2
; could be potentially unsound when we have 8-bit characters, because
; it's conceivable that
; 
; (equal (char-code "#\fifth-control-character") 5)
; 
; is a theorem in one Common Lisp and yet
; 
; (equal (char-code "#\fifth-control-character") 6)
; 
; is a theorem in another.  Bizarre, but apparently not ruled out by
; dpANS.
; 
; So, we manage this simply by modifying the character reader so that
; the #\ notation only works for single characters and for Space, Tab,
; Newline, Page, and Rubout; an error is caused otherwise.

; Our algorithm for reading character objects starting with #\ is
; quite simple.  We accumulate characters until encountering a
; whitespace character or a terminating macro character, from the list
; *acl2-read-character-terminators*.  The result must be either a
; single standard character or else one of the names (up to case,
; which we ignore in the multiple-character case) SPACE, TAB, NEWLINE,
; PAGE, and RUBOUT.  Otherwise we cause an error.  Note that if we do
; NOT cause an error, then any dpANS-compliant Common Lisp
; implementation's character reader would behave the same way, because
; dpANS says (in the section ``Sharpsign Backslash'') the following.

;    .....  After #\ is read, the reader backs up
;    over the slash and then reads a token, treating the initial slash
;    as a single escape character (whether it really is or not in the
;    current readtable).

; The rather involved description from dpANS in the section ``Reader
; Algorithm'' implies that when a token is terminated without error,
; it must be upon first encountering a whitespace character or a
; terminating macro character.

; Finally, here is an argument that we cannot reasonably allow ACL2 to
; accept character notations of the sort akcl allows, such as #\\112
; for #\J for example.  (By the way, 112 is octal for 74, which is
; (char-code #\J).)  This is sad, because it would have been nice to
; provide a way of reading arbitrary 8-bit characters in ACL2.

; In the following, we refer to documentation from Bill Schelter's
; info version of dpANS.

; So, assume that #\J parses the same as #\\112 (and we'll derive a
; ``contradiction'' of sorts).  The documentation from ``Sharpsign
; Backslash'' below implies that #\\112 parses as the character whose
; name is [STRING-UPCASE applied to] the 4-character string \112.  So
; if #\\112 parses as #\J, then the name of the character #\J is
; "\\112".  Then, the documentation for ``char-name'' (part of which
; is also below) implies that CHAR-NAME returns the character name,
; and hence (CHAR-NAME #\J) must be "\\112".  But probably this isn't
; true of the implementation (it's not true for akcl or allegro, for
; example).  And, it seems really dangerous for us to redefine
; CHAR-NAME in ACL2.

; What's worse, if we apply the first part of this argument to
; #\Newline, we see that the name of the character #\Newline is
; "\\12", which directly contradicts the final passage below from
; dpANS.

; In what follows we'll quote from dpANS (an emacs Info version).  We
; quote here from three sections, separated by ++++++++++++++++++.

#|

Sharpsign Backslash
...................

Syntax: #\<<x>>

[[[ text omitted ]]]

When the token x is more than one character long, the x must have the
syntax of a symbol with no embedded package markers.  In this case, the
sharpsign backslash notation parses as the character whose name is
(string-upcase x); see *See Character Names::.

++++++++++++++++++

char-name                                                        [Function]
---------------------------------------------------------------------------

`char-name'  character =>  name

Arguments and Values::
......................

character--a character.

name--a string or nil.

Description::
.............

Returns a string that is the name of the character, or nil if the
character has no name.

++++++++++++++++++

Character Names
---------------

The following character names must be present in all conforming
implementations:

Newline

|# ; |

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       COMPILING and LOADING, PART 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-acl2 (&optional use-acl2-proclaims)

  (with-warnings-suppressed

   #+sbcl
   (declaim (optimize (safety 0) (space 0) (speed 3) (debug 0)))

; Here is a natural place to put compiler options.  In fact, we put
; them above, globally.

; (declaim (optimize (safety 0) (space 0) (speed 3)))

; However, Juho Snellman points out that SBCL resets the compiler policy on
; entry to LOAD / COMPILE-FILE, and restores the old value once the file has
; been loaded / compiled; thus the global declaim is no longer in effect once
; COMPILE-ACL2 gets called.

; Note on Illegal Instructions:  If ACL2 causes an illegal instruction
; trap it is difficult to figure out what is happening.  Here is a
; brute force way to do it.
; (0) create an events file or book that will recreate the
;     the state up to the event that causes the illegal instruction.
; (1) Copy saved_acl2 to old_saved_acl2
; (2) replace the declaim above with
;     (declaim (optimize (safety 3) (space 0) (speed 0)));
; (3) make full init
;     this will fail because of stack overflows during booting.
; (4) fire up old_saved_acl2
; (5) load the saved events file or book, recreating the state.
; (6) :q from (LP)
; (7) (progn (setq old-world-pair (get 'current-acl2-world 'acl2-world-pair))
;            t)
; (8) (dolist (name *copy-of-acl2-files*)
;           (or (equal name "defpkgs")
;               (load source)))   ; load the (safety 3) .o files
;     Actually, the last time I did this I loaded each file manually
;     and I did not load ld or interface-raw.  I think they can be
;     loaded here though.
; (9) (progn (f-put-global 'current-acl2-world
;                          (car old-world-pair) *the-live-state*)
;            t)
;(10) (si::use-fast-links nil)
;(11) enter (LP) and execute the offending event.
;(12) once the bug is fixed, be sure to change the declaim
;     back to (safety 0) (speed 3) before recompiling.

; Note on loading before compiling.  We load each ACL2 source file
; before compiling it to make sure that the functions needed to
; execute macros have been defun-ed before they are called.  Normal
; Common Lisp compilation does not do this.  So we cause all forms to
; be executed before we start the compilation.  This guarantees that
; when macros run, the functions they call have been defined.

; In general, and for the same reason, all ACL2 user checked files
; are also be loaded before they are compiled.

   #-acl2-mv-as-values
   (when use-acl2-proclaims
     (return-from compile-acl2 nil))

; As of version 18a, cmulisp spews gc messages to the terminal even when
; standard and error output are redirected.  So we turn them off.

   (cond
    ((or (not (probe-file *acl2-status-file*))
         (with-open-file (str *acl2-status-file*
                              :direction :input)
                         (not (eq (read str nil)
                                  :checked))))
     (check-suitability-for-acl2)))
   (our-with-compilation-unit
    (let ((*readtable* *acl2-readtable*)
          #+akcl

; AKCL compiler note stuff.  We have so many tail recursive functions
; that the notes about tail recursion optimization are just too much
; to take.

          (compiler:*suppress-compiler-notes* t))
      (when use-acl2-proclaims
        (load "acl2-proclaims.lisp"))
      (dolist (name *copy-of-acl2-files*)
        (or (equal name "defpkgs")
            (let ((source (make-pathname :name name
                                         :type *lisp-extension*)))
              (load source)
              (or (equal name "proof-checker-pkg")
                  (progn
                    (when (not use-acl2-proclaims)

; So, we have not loaded acl2-proclaims.lisp.

                      (proclaim-file source))
                    (compile-file source)
                    (load-compiled
                     (make-pathname :name name
                                    :type *compiled-file-extension*)))))))
      (note-compile-ok)
      ))))

(defun no-object-file-or-out-of-date-object-file (fl)
  (or (null (probe-file
             (make-pathname :name fl :type *compiled-file-extension*)))
      (> (file-write-date
          (make-pathname :name fl :type *lisp-extension*))
         (file-write-date
          (make-pathname :name fl :type *compiled-file-extension*)))))

(defun quick-compile-acl2 (&optional very-fast use-acl2-proclaims)

  (with-warnings-suppressed

; Here is a natural place to put compiler options.

; (declaim (optimize (safety 0) (space 0) (speed 3)))

; As of version 18a, cmulisp spews gc messages to the terminal even when
; standard and error output are redirected.  So we turn them off.

   #+cmu
   (setq extensions::*gc-verbose* nil)

   (cond
    ((or (not (probe-file (make-pathname :name "acl2-status"
                                         :type "txt")))
         (with-open-file (str (make-pathname :name "acl2-status"
                                             :type "txt")
                              :direction :input)
                         (not (eq (read str nil)
                                  :checked))))
     (check-suitability-for-acl2)))
   (our-with-compilation-unit
    (let ((compile-rest-flg nil)
          (*readtable* *acl2-readtable*)
          #+akcl
          (si:*notify-gbc* nil)
          #+akcl

; AKCL compiler note stuff.  We have so many tail recursive functions
; that the notes about tail recursion optimization are just too much
; to take.

          (compiler:*suppress-compiler-notes* t)
          (files (remove "defpkgs" *copy-of-acl2-files* :test #'equal)))
      (cond
       ((some #'no-object-file-or-out-of-date-object-file files)
        (when use-acl2-proclaims
          (load "acl2-proclaims.lisp"))
        (dolist
          (fl files)
          (let ((source (make-pathname :name fl :type *lisp-extension*))
                (object (make-pathname :name fl :type *compiled-file-extension*)))
            (cond
             ((equal fl "proof-checker-pkg")
              (load source))
             ((or compile-rest-flg (no-object-file-or-out-of-date-object-file fl))
              (load source)
              (proclaim-file source)
              (when (not very-fast)
                (setq compile-rest-flg t))
              (compile-file source)
              (load-compiled object))
             (t (load-compiled object)
                (proclaim-file source))))))
       (t "Nothing to do."))))
   (note-compile-ok)))

#+gcl
(defvar user::*fast-acl2-gcl-build* nil)

(defun load-acl2 (&optional fast)

  #-akcl (declare (ignore fast)) ; fast only avoids slow growth during gcl init

  (with-warnings-suppressed

; If we are in the first pass of two passes because of acl2-mv-as-values, then
; don't waste time doing the slow build for GCL (where we compile all *1*
; functions as we go through initialization).

   #+(and gcl acl2-mv-as-values)
   (when fast
     (setq user::*fast-acl2-gcl-build* t))

   #+akcl

; We grow the image slowly, since we now do allocation on start-up.  We are
; assuming that people will be using load-acl2 only as part of the process of
; building a saved image, and hence that this slow growth policy will be undone
; by the function save-acl2-in-akcl.  If we are 

   (when (not fast)
     (sloop::sloop
      for type in
      '(cons fixnum symbol array string cfun sfun

; In akcl, at least some versions of it, we cannot call allocate-growth on the
; following two types.

             #+gcl contiguous
             #+gcl relocatable
             )
      do
      (cond
       ((or (boundp 'si::*gcl-major-version*) ;GCL 2.0 or greater
            (and (boundp 'si::*gcl-version*) ;GCL 1.1
                 (= si::*gcl-version* 1)))
        (si::allocate-growth type 1 10 50 2))
       (t (si::allocate-growth type 1 10 50)))))
   (cond
    ((or (not (probe-file *acl2-status-file*))
         (with-open-file (str *acl2-status-file*
                              :direction :input)
                         (not (member (read str nil)
                                      '(:compiled :initialized)))))
     (error "Please compile ACL2 using ~s or~%~
            ~s, which will write the~%~
            token :compiled to the file acl2-status.txt."
            '(compile-acl2)
            '(quick-compile-acl2 t))))
   (let ((*readtable* *acl2-readtable*))
     (dolist (name *copy-of-acl2-files*)
       (or (equal name "defpkgs")
           (if (equal name "proof-checker-pkg")
               (load "proof-checker-pkg.lisp")
             (load-compiled (make-pathname :name name
                                           :type *compiled-file-extension*)))))
     (load "defpkgs.lisp")
     (in-package "ACL2")

; Do not make state special, as that can interfere with tail recursion removal.
; The following form is provided merely as a convenience to the user, who may
; want to execute ACL2 forms in raw Lisp.  The use of set instead of setq is to
; avoid getting a warning in cmulisp that state is undefined.

     (set 'state *the-live-state*)
     "ACL2")))

(defun load-acl2-execution-environment ()
  (let ((*readtable* *acl2-readtable*))
    (load-compiled (make-pathname :name "axioms"
                                  :type *compiled-file-extension*))
    (load "defpkgs.lisp")
    (in-package "ACL2")
    "ACL2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            DECLARATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We use XARGS in DECLARE forms.  By making this proclamation, we
; suppress compiler warnings.

(declaim (declaration xargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            EXITING LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exit-lisp (&optional (cmd '"(GOOD-BYE)"))
  #-(or openmcl clisp lispworks sbcl)
  (declare (ignore cmd))

; We do not yet know why the appropriate exit command does not cause an exit
; for each of the indicated lisps while inside the ACL2 loop.  So we print ba
; informative message instead if we are in the loop.

  #+(or openmcl clisp lispworks sbcl)
  (if (and (boundp '*ld-level*)
           (not (eql 0 (symbol-value '*ld-level*))))
      (progn
        #-openmcl
        (format t
                "To exit both ACL2 and lisp, exit the ACL2 loop with :q~%~
                 and then: type ~a again, or ~s, then <RETURN>.~%"
                cmd
                #+clisp
                '(user::exit)
                #+lispworks ; Version 4.2.0; older versions have used bye
                '(lisp::quit)
                #+sbcl
                '(sb-ext:quit)
                )
        #+openmcl
        (progn (format t
                       "Quitting ACL2 and lisp; this may take a moment.  For~%~
                        faster response time, exit the ACL2 loop with :q~%~
                        and then: type ~a or ~s, then <RETURN>.~%"
                       cmd
                       '(ccl::quit))
               (ccl::quit))

; Return t here so that (good-bye) does not cause an error.

             (return-from exit-lisp t))
    #+clisp
    (user::exit)
    #+lispworks ; Version 4.2.0; older versions have used bye
    (lisp::quit)
    #+sbcl
    (sb-ext:quit)
    )
  #+akcl
  (lisp::bye)
  #+lucid
  (lisp::exit)
  #+openmcl
  (ccl::quit)
  #+cmu
  (common-lisp-user::quit t)
  #+allegro
  (user::exit 0 :no-unwind t)
  #+(and mcl (not openmcl))
  (cl-user::quit)

; Return nil if we cannot exit lisp.  The caller of this function should
; complain if Lisp survives the call.

  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Allegro 6.0 (and probably later versions) warns when the following constants
; axioms.lisp, even with boundp checks (as in ACL2 Version_2.5).  The warning
; is a result of evaluating the defconstant twice: when loading the source file
; and when subsequently loading the compiled file.  So starting with
; Version_2.6 we put the constants here, since this file (acl2.lisp) is not
; compiled and hence is only loaded once.

; A slight complication is that *slashable-array* uses *slashable-chars*, which
; in turn is used in the definition of function some-slashable.  (There are
; other such examples, but we'll focus on that one.)  So, defconst
; *slashable-chars* needs to be defined in the ACL2 loop even though, as of
; Version_2.5, it was used in the definition of *slashable-array*, which we
; want to include in the present file.  So we inline the defconsts below.

(defconstant *slashable-array*
  (let ((ar (make-array 256 :initial-element nil)))
    (dolist (ch

; Inline *slashable-chars*; see the comment above.

             '(#\Newline #\Page #\Space #\" #\# #\' #\( #\) #\, #\. #\: #\; #\\ #\`
               #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p
               #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\|))
            (setf (aref ar (char-code ch))
                  t))
    ar))

(defconstant *suspiciously-first-numeric-array*
  (let ((ar (make-array 256 :initial-element nil)))
    (dolist (x

; Inline *suspiciously-first-numeric-chars*; see the comment above.

             '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\. #\^ #\_))
            (setf (aref ar (char-code x))
                  t))
    ar))

(defconstant *char-code-backslash* (char-code #\\))

(defconstant *char-code-double-gritch* (char-code #\"))

(defconstant *read-object-eof* (cons nil nil))

; The following constant was originally in translate.lisp, but CLISP warned
; that it was being redefined.  This seems to be the same problem as mentioned
; above for Allegro above, so we define it here.

(defconstant *big-n-special-object* '(nil . nil))

(defconstant *number-of-return-values*

; Keep this in sync with related code in translate11.

  32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            STATISTICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; See the comment in *rewrite-depth-max* about rewrite stack depth:
; (push :acl2-rewrite-meter *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            PROMPTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; New ACL2 users sometimes do not notice that they are outside the ACL2
; read-eval-print loop when in a break.  See the discussion of "PROMPTS" in
; interface-raw.lisp for how we deal with this.  For GCL, we currently (as of
; GCL version 2.6.6) need a patch for built-in function si::break-level.  This
; requires a package change, so we put that patch in a file that is not
; compiled; the present file serves nicely.

#+gcl
(in-package "SYSTEM")
#+gcl
(progn
(defvar *debug-prompt-suffix* "")
; See comment about ACL2 for how the following is patched from si::break-level.
(defun break-level-for-acl2 (at &optional env)
  (let* ((*break-message* (if (stringp at) at *break-message*))
         (*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
         (*quit-tag* (cons nil nil))
         (*break-level* (if (not at) *break-level* (cons t *break-level*)))
         (*ihs-base* (1+ *ihs-top*))
         (*ihs-top* (1- (ihs-top)))
         (*current-ihs* *ihs-top*)
         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*break-env* nil)
         (be *break-enable*)
         (*break-enable*
          (progn 
            (if (stringp at) nil be)))
                                        ;(*standard-input* *terminal-io*)
         (*readtable* (or *break-readtable* *readtable*))
         (*read-suppress* nil)
         (+ +) (++ ++) (+++ +++)
         (- -)
         (* *) (** **) (*** ***)
         (/ /) (// //) (/// ///)
         )
                                        ; (terpri *error-output*)
    (unless (or be (not (stringp at)))
      (simple-backtrace)
      (break-quit (length (cdr *break-level*))))
    (catch-fatal 1)
    (setq *interrupt-enable* t)
    (cond ((stringp at) (set-current)(terpri *error-output*)
           (setq *no-prompt* nil)
           )
          (t (set-back at env)))
      (loop 
       (setq +++ ++ ++ + + -)
       (cond (*no-prompt* (setq *no-prompt* nil))
             (t ; ACL2 patch is in the following form, only
              (format *debug-io* "~&~a~a~a>~{~*>~}"
                      (if (stringp at) "" "dbl:")
                      (if (eq *package* (find-package 'user)) ""
                        (package-name *package*))
                      *debug-prompt-suffix*
                      *break-level*)))
       (force-output *error-output*)
       (when
        (catch 'step-continue
        (catch *quit-tag*
          (setq - (locally (declare (notinline read))
                           (dbl-read *debug-io* nil *top-eof*)))
          (when (eq - *top-eof*) (lisp::bye -1))
          (let* ( break-command
                 (values
                  (multiple-value-list
                  (LOCALLY (declare (notinline break-call evalhook))
                           (if (keywordp -)(setq - (cons - nil)))
                           (cond ((and (consp -) (keywordp (car -)))
                                  (setq break-command t)
                                  (break-call (car -) (cdr -) 'si::break-command))
                                 (t (evalhook - nil nil *break-env*)))))))
            (and break-command (eq (car values) :resume )(return))
            (setq /// // // / / values *** ** ** * * (car /))
            (fresh-line *debug-io*)
            (dolist (val /)
                    (locally (declare (notinline prin1)) (prin1 val *debug-io*))
                    (terpri *debug-io*)))
          nil))
        (terpri *debug-io*)
        (break-current))))))
#+gcl
(in-package "ACL2")

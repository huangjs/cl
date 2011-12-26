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

(in-package "ACL2")

; This file, interface-raw.lisp, contains parts of ACL2 which we
; cannot code in ACL2 because they require constructs not in ACL2, such
; as calling the compiler.

;          EVALUATION

; Essay on Evaluation in ACL2

; This essay is structured as follows.  Terminology is explained below.

; A. Introduction
; B. Specification of the problem
; C. Sketch of correctness proof
; D. Why safe mode is necessary
; E. The template for oneification of function definitions
; F. Remarks

; Let us begin.

; A. Introduction

; Evaluation in ACL2, which takes place in the ACL2 loop and during theorem
; proving, is based on the way evaluation was done in Nqthm.  The idea is to
; "oneify" the body of a definition by replacing functions by their so-called
; "executable counterparts," sometimes called "*1* functions."  The primitives
; have *1* functions that reflect their logical definitions, so that for
; example (*1*car x), or more precisely (acl2_*1*_lisp::car x), returns nil
; when x is an atom -- except that an error occurs if we are checking guards
; (or are in so-called safe mode, as explained below).  Defined functions have
; *1* function counterparts that are defined, roughly speaking, by replacing
; each function call in their bodies by a call of the corresponding *1*
; function.

; The evaluation mechanism in ACL2 changed radically in v1-8, when guards were
; removed from the logic.  It has changed again in Version_2.6, due to a hole
; in the existing mechanism, as we explain in Part D of this Essay, below.

; B. Specification of the problem

; Our specification begins with the introduction of three notions of evaluation
; and three notions of macroexpansion.  Evaluation is relative to an (implicit)
; environment, which binds variables to ACL2 values, and operates on ACL2 terms
; that are free of macro calls.  If we want to discuss evaluation of terms that
; involve macro calls, we will compose macroexpansion and evaluation.  This
; composition represents the way that both Common Lisp and the ACL2 loop
; evaluate forms.  We also assume that there is no distinction between
; evaluation of compiled and interpreted code.  Finally, we assume that for all
; of these sorts of evaluation and expansion, macros are expanded before
; function and macro bodies are stored; this is how things are done in the ACL2
; logic and with *1* functions, and it had better be equivalent to how a Common
; Lisp does its evaluation and expansion.

; We extend classes of function symbols to classes of terms in the obvious way:
; a :logic mode term is one whose function symbols are all :logic mode function
; symbols, and similarly for the notion of :common-lisp-compliant.

; Here then are the notions promised above.

;   ACL2 logical evaluation: This procedure is an interpreter that computes
;   using definitions in the logic and obvious properties of primitives (e.g.,
;   (stringp "abc") returns t).

;   ACL2 loop evaluation: This is the procedure used in the ACL2 loop, using
;   so-called *1* functions (and higher-level routines such as raw-ev-fncall).

;   Common Lisp evaluation: As the name implies, this procedure is the one used
;   by Common Lisp.

;   ACL2 logical macroexpansion: This is any procedure that carries out the
;   usual macroexpansion algorithm (outside-in), but entirely using ACL2
;   definitions, including those of :program mode functions.  We assume that
;   macros have already been similarly expanded in function bodies, before
;   evaluation begins.  Macro bodies are evaluated using ACL2 logical
;   evaluation.  This procedure is embodied in the ACL2 definition of the
;   function translate.

;   ACL2 loop macroexpansion: This is the procedure that ACL2 actually applies
;   in order to create terms from user input.  Ideally this procedure returns
;   the same results as does ACL2 logical macroexpansion; the distinction here
;   is between what an appropriate interpreter would return (ACL2 logical
;   macroexpansion) and how ACL2 actually translates a term (ACL2 loop
;   macroexpansion).  ACL2 loop macroexpansion always takes place in safe mode.

;   Common Lisp macroexpansion: This is how Common Lisp (actually, an arbitrary
;   but fixed implementation) macroexpands forms.

; As an aside, we note the following fact that is useful in establishing the
; guarantees below, but whose proof we omit here.

;   (*) If a :common-lisp-compliant function is applied to arguments that
;   satisfy its guard (using Common Lisp evaluation), without error, then the
;   result agrees with that produced by ACL2 logical evaluation.

; Now our top-level guarantees about evaluation and macroexpansion are as
; follows, where for brevity, "evaluation" of a given type is the composition
; of macroexpansion and evaluation for that type.

;   (1) If ACL2 evaluates a :logic mode form without error, then the value
;   returned equals the result of ACL2 logical (macroexpansion and) evaluation
;   of that form.

;   (2) If furthermore that evaluation in done with guard-checking on and the
;   result of ACL2 logical macroexpansion is a :common-lisp-compliant term,
;   then any non-erroneous Common Lisp evaluation returns that same value.

; C. Sketch of correctness proof

; We now outline a proof of these guarantees by breaking them into the
; following sequence of claims.  We write "weakly implements" to mean that two
; procedures give equivalent results on given inputs when they both return
; without error, and we write "implements" if the condition can be weakened to
; assume only that the first procedure returns without error.  That is, proc1
; implements proc2 iff proc1 weakly implements proc2 and whenever proc1 returns
; without error, then so does proc2.  Above, "equivalent" means identical
; except as explained otherwise below.  Implicit in this notion is that the
; input is appropriate for the procedures; for example, our notions of
; evaluation assume that all function symbols in the input are either ACL2
; primitive functions or have been defined as functions (not macros) in ACL2.

; A more rigorous argument would proceed by induction on the length of
; histories, showing that the properties in question hold when one extends the
; history with new function and macro definitions.

;   (1a) ACL2 loop evaluation implements ACL2 logical evaluation on :logic mode
;   terms and, provided safe mode is used, on arbitrary terms.

;   (1b) ACL2 loop macroexpansion implements ACL2 logical macroexpansion.

;   (2a) ACL2 loop evaluation in safe mode weakly implements Common Lisp
;   evaluation.  The same claim holds if the assumption of safe mode is
;   replaced by the assumption that guard-checking is on, provided that the
;   input form expands to a :common-lisp-compliant term.

;   (2b) ACL2 loop macroexpansion weakly implements Common Lisp macroexpansion,
;   where results r1 (from ACL2 loop macroexpansion) and r2 (from Common Lisp
;   macroexpansion) are considered equivalent if for any environment, the ACL2
;   loop evaluation of r1 with guard-checking on returns the same result as the
;   Common Lisp evaluation of r2, provided both evaluations return without
;   error.

; Sketch of proof that guarantees hold.  Clearly (1) follows from (1a) and
; (1b), while (2) follows from (1b) and (2b).  (1a) follows by inspection of
; the template presented below, using (*) above.  (1b) follows from (1a) by
; computational induction on the macroexpansion, because ACL2 loop
; macroexpansion and ACL2 logical macroexpansion differ only in the use of loop
; or logical evaluation of macro bodies.  The first part of (2a) is argued
; similarly to (1a), while the second part is actually quite trivial by
; inspection of the template below.  Finally, (2b) follows from (2a) by a
; computational induction just as (1b) follows from (1a), with a bit of
; complication.  When we encounter a call of a macro first introduced in ACL2
; (either during the boot-strap or by a user), then we evaluate the same macro
; body for ACL2 loop evaluation as for Common Lisp evaluation, except that this
; body has first been macroexpanded using ACL2 loop macroexpansion and Common
; Lisp macroexpansion, respectively.  But these may be viewed as equivalent by
; the inductive hypothesis (where for purposes of this proof we pretend that
; macroexpansion of the body takes place as part of the process).  In the other
; case, the macro already has a Common Lisp definition (as a function or
; macro), and we have arranged that (2) holds.  For example, the ACL2 loop
; macroexpansion of (append x y z) is (binary-append x (binary-append y z)),
; and Common Lisp evaluation of the former clearly agrees with ACL2 loop
; evaluation of the latter.  Q.E.D.

; D. Why safe mode is necessary

; The soundness of ACL2 potentially rests on the principle of not calling raw
; Lisp counterparts of functions with arguments outside their intended domains,
; as specified by their guards.  Here we give three examples illustrating why
; we introduced safe mode in Version_2.6.  The third one is a proof of nil!

; Example 1.  In our first example below, the defun of bar should fail.  It
; does indeed fail starting with Version_2.6, but not in Version_2.5 or (we
; believe) several versions before that.  We discuss below how this can lead 
; to unsoundness.

#|
(defmacro foo (x) (car x))
(set-guard-checking nil)
(defun bar (y)
  (declare (xargs :verify-guards t))
  (cons (foo y) y))
:q
(trace bar)
(lp)
|#

; Now, the result of evaluating (bar 3) looks as shown below.  Notice that the
; Common Lisp function bar is called.  If the Common Lisp evaluation of the
; form (car 'y) had returned 1 or 2 (say) instead of breaking, then the Common
; Lisp evaluation of (bar 3) would have returned (cons 1 3) or (cons 2 3),
; respectively.  This evaluation could be reflected in theorems (equal (bar 3)
; i) [i=1,2] proved in books certified in two different Common Lisp
; implementations of ACL2.  We could then prove nil by including both books
; into the same session.  Lest one think that one needs different Lisp
; implementations to expose unsoundness, imagine a single Lisp in which (car
; 'y) sometimes returns 1 and sometimes returns 2.

#|
ACL2 >(bar 3)
  1> (ACL2_*1*_ACL2::BAR 3)>
    2> (BAR 3)>

Error: Y is not of type LIST.
Fast links are on: do (si::use-fast-links nil) for debugging
Error signalled by CAR.
Broken at COND.  Type :H for Help.
ACL2>>
|#

; Here is what ACL2 Version_2.6 prints in an attempt to define function bar,
; above, with guard-checking off.

#|
ACL2 >(defun bar (y) (foo y))


ACL2 Error in ( DEFUN BAR ...):  The guard for the function symbol
CAR, which is (OR (CONSP X) (EQUAL X NIL)), is violated by the arguments
in the call (CAR 'Y).  The guard is being checked because this function
is a primitive and a "safe" mode is being used, perhaps for macroexpansion.



ACL2 Error in ( DEFUN BAR ...):  In the attempt to macroexpand the
form (FOO Y), evaluation of the macro body caused an error.


Summary
Form:  ( DEFUN BAR ...)
Rules: NIL
Warnings:  None
Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)

******** FAILED ********  See :DOC failure  ******** FAILED ********
ACL2 >
|#

; Example 2.  Unlike the previous example, this one causes problems even when
; guard-checking is on.  (Thanks to Pete Manolios for helping to construct this
; example, which is simpler than an earlier one we had.)

#|
(defun my-endp-0 ()
  (declare (xargs :mode :program))
  (endp 0))
(defmacro bad-macro ()
  (my-endp-0))
:q
(trace my-endp-0 endp)
(lp)
(thm (equal (bad-macro) 1))
|#

; Now look at the following Version_2.5 trace.  It highlights a behavior of 
; Version_2.5: when a :program mode function (here, my-endp-0) is 
; called on arguments satisfying its guard (here, implicitly t), the
; corresponding raw Lisp function is invoked.  Thus guards are not checked on
; its subroutines (here, endp).  In this example, endp is being called on an 
; argument not satisfying its guard.  In the abstract, this is problematic
; because we use guards to restrict primitives to arguments for which the
; result is implementation independent.  If the result of (endp 0) can depend
; on the implementation, we could prove nil as described in the preceding
; example.

#|
ACL2 !>(thm (equal (bad-macro) 1))
  1> (ACL2_*1*_ACL2::MY-ENDP-0)>
    2> (MY-ENDP-0)>
      3> (ENDP 0)>

Error: 0 is not of type LIST.
Fast links are on: do (si::use-fast-links nil) for debugging
Error signalled by SYSTEM::TRACE-CALL.
Broken at COND.  Type :H for Help.
ACL2>>
|#

; The example above may seem contrived (because it is!).  However, our foray
; into this territory began on a rather similar but real example.  In Allegro
; 6.0, the character (code-char (+ 128 65)) is upper case; in particular it is
; not equal to the result of applying char-downcase to it.  However, this is
; not the case for Allegro 5.0.1.  Since the result is
; implementation-dependent, it is natural to restrict the application of
; code-char to standard characters, using ACL2's guard mechanism.  But the
; example above show that we can bypass such restrictions by using macros.

; Example 3.  We can prove nil in Version_2.5 by certifying the following two
; books. The only cheats are that the first book needs to be certified after
; executing the following in the ACL2 loop:

; (set-guard-checking nil)

; First book, call it "ex":
#|
(in-package "ACL2")

(defun my-eq (x y)
  (declare (xargs :guard t ; "bad" guard
                  :mode :program))
  (eq x y))

(defmacro bad-macro ()
  (my-eq '(a b) '(a b)))

(set-verify-guards-eagerness 0)

(local (verify-termination my-eq))

(defun bad-fn ()
  (bad-macro))

(defthm bad-thm
  (bad-fn)
  :rule-classes nil)
|#

; Second book, which includes the one above::
#|
(in-package "ACL2")

(local (include-book "ex"))

(defthm very-bad
  nil
  :hints (("Goal" :use bad-thm))
  :rule-classes nil)
|#

; In Version_2.6 we get an error when we try to certify the first book above
; ("ex"):

#|
ACL2 Error in ( DEFUN BAD-FN ...):  The guard for the function symbol
EQ, which is (IF (SYMBOLP X) T (SYMBOLP Y)), is violated by the arguments
in the call (EQ '(A B) '(A B)).  The guard is being checked because
this function is a primitive and a "safe" mode is being used, perhaps
for macroexpansion.



ACL2 Error in ( DEFUN BAD-FN ...):  In the attempt to macroexpand the
form (BAD-MACRO), evaluation of the macro body caused an error.
|#

; As the first message just above suggests, in Version_2.6 we prevent the bad
; behavior illustrated by the examples above by introducing a "safe mode" for
; use during macroexpansion, in which guards are checked on built-in functions.

; Finally, note that we do not attempt to fix the following "problem."  That
; is, the behavior for the example below is unchanged from Version_2.5 to
; Version_2.6.  The point is that for macroexpansion to behave properly, we 
; really need only guarantee consistency between the logic and Common Lisp; it
; is acceptable if in some modes we get errors even when errors are not 
; necessary.

#|
(defun mac-fn (x) (declare (xargs :guard (consp x))) x)
(defmacro mac (x) (mac-fn x))
(defun bar (x) (mac x)) ; fails
:set-guard-checking nil
(defun bar (x) (mac x)) ; succeeds
|#

; E. The template for oneification of function definitions

; Before we present this template, we give a bit of history and show an
; example.

; The following example shows how *1* functions are handled in Version_2.5 and
; before. The ACL2 definition is:

#|
(defun foo (x)
  (declare (xargs :mode :logic :guard (true-listp x)))
  (if (endp x) 3 (+ 1 (foo (cdr x)))))
|#

; Here is the executable counterpart in Version_2.5, in gcl:

#|
ACL2>(symbol-function 'ACL2_*1*_ACL2::FOO) ; in gcl, ACL2 Version_2.5
(LISP:LAMBDA-BLOCK ACL2_*1*_ACL2::FOO (X)
  (LABELS ((ACL2_*1*_ACL2::FOO (X)
               (IF (ACL2_*1*_LISP::ENDP X) '3
                   (ACL2_*1*_ACL2::BINARY-+ '1
                       (ACL2_*1*_ACL2::FOO (ACL2_*1*_LISP::CDR X))))))
    (LET ((ACL2_*1*_ACL2::FOO (SYMBOL-CLASS 'FOO (W *THE-LIVE-STATE*)))
          (GUARD-CHECKING-ON
              (F-GET-GLOBAL 'GUARD-CHECKING-ON *THE-LIVE-STATE*)))
      (COND
        ((LET ((*HARD-ERROR-RETURNS-NILP*
                   (OR *HARD-ERROR-RETURNS-NILP*
                       (NOT GUARD-CHECKING-ON))))
           (IF (EQ ACL2_*1*_ACL2::FOO :IDEAL)
               (ACL2_*1*_ACL2::TRUE-LISTP X) (TRUE-LISTP X)))
         (IF (EQ ACL2_*1*_ACL2::FOO :IDEAL) (ACL2_*1*_ACL2::FOO X)
             (FOO X)))
        (GUARD-CHECKING-ON
            (THROW-RAW-EV-FNCALL
                (LIST 'EV-FNCALL-GUARD-ER 'FOO (LIST X) '(TRUE-LISTP X)
                      '(NIL))))
        (T (ACL2_*1*_ACL2::FOO X))))))
|#

; Notice the inefficiency of needlessly checking guards in Version_2.5 in the
; :ideal case when guard-checking is off.  We fix that problem in Version_2.6,
; but more importantly, we implement a "safe mode" to be used during
; macroexpansion, so that we can trust that ACL2 and Common Lisp agree when
; they are supposed to, thus avoiding the sort of problem illustrated above
; (function bar and macro mac).  We make this idea precise in our discussion of
; "Guarantees", above.

#|
ACL2>(symbol-function 'ACL2_*1*_ACL2::FOO) ; in gcl, ACL2 Version_2.6
(LISP:LAMBDA-BLOCK ACL2_*1*_ACL2::FOO (X)
  (LABELS ((ACL2_*1*_ACL2::FOO (X)
               (IF (ACL2_*1*_LISP::ENDP X) '3
                   (ACL2_*1*_ACL2::BINARY-+ '1
                       (ACL2_*1*_ACL2::FOO (ACL2_*1*_LISP::CDR X))))))
    (COND
      ((TRUE-LISTP X) (RETURN-FROM ACL2_*1*_ACL2::FOO (FOO X)))
      ((F-GET-GLOBAL 'GUARD-CHECKING-ON *THE-LIVE-STATE*)
       (RETURN-FROM ACL2_*1*_ACL2::FOO
         (THROW-RAW-EV-FNCALL
             (LIST 'EV-FNCALL-GUARD-ER 'FOO (LIST X) '(TRUE-LISTP X)
                   '(NIL))))))
    (ACL2_*1*_ACL2::FOO X)))
|#

; Next, we present a basic template (outline, really) for defining executable
; counterparts.  Note that as in the code for Version_2.5, we may optimize away
; consideration of the guard when the guard is t (either implicitly or
; explicitly).  Furthermore, we do some optimization when the symbol-class of
; the function is :common-lisp-compliant, as we do in Version_2.5 for :program
; vs. :logic mode.

; The template below uses some abbreviations <...>, which are defined below the
; template.  See also oneify-cltl-code for more details, special cases, and
; optimizations.  There we also handle the guarded-primitive-p case, which
; pertains to built-in defined functions that need to be responsible for
; checking their guards in safe-mode.  That does not however deal with true
; primitives, which are not defined.  For those, safe-mode is handled with
; calls of gv in their defun-*1* definitions.

#|
(defun <*1*fn> <formals>

; This "template" is only approximagte, intended to give a sense of how
; oneify-cltl-code works.  But you may be better off simply by tracing
; oneify-cltl-code (hiding the wrld argument, e.g. in raw Lisp, (trace$
; (oneify-cltl-code :entry (butlast si::arglist 1))), or else looking at
; (symbol-function (*1*-symbol 'foo)), after executing each of the following
; test cases.

#|
 (defun foo (x)
   (declare (xargs :guard t))
   (if (natp x) (if (zp x) 0 (* x (foo (1- x)))) 0))
 (defun foo (x)
   (declare (xargs :guard t :verify-guards nil))
   (if (natp x) (if (zp x) 0 (* x (foo (1- x)))) 0))
 (defun foo (x)
   (declare (xargs :guard t :mode :program))
   (if (natp x) (if (zp x) 0 (* x (foo (1- x)))) 0))

 (defun foo (x)
   (declare (xargs :guard (natp x)))
   (if (zp x) 0 (* x (foo (1- x)))))
 (defun foo (x)
   (declare (xargs :guard (natp x) :mode :program))
   (if (zp x) 0 (* x (foo (1- x)))))
 (defun foo (x)
   (declare (xargs :guard (natp x) :verify-guards nil))
   (if (zp x) 0 (* x (foo (1- x)))))

 (defun foo (x)
   (if (zp x) 0 (* x (foo (1- x)))))
|#

  <wormhole-test-for-functions-with-user-stobjs>
  (let ((<class> (symbol-class '<fn> (w *the-live-state*))))
    (cond ((eq <class> :common-lisp-compliant)
           (cond
            ((or (equal <guard> *t*)
                 (not (eq <guard-checking-on> :none))
                 (acl2-system-namep name wrld))
             (cond (<guard> ; avoid <*1*guard> since guard is known compliant
                    (cond (<live-stobjp-test> ; test can often be omitted
                           (return-from <*1*fn> (<fn> . <formals>)))))
                   (<guard-checking-on> <fail_guard>))

; Otherwise fall through to final call of *1* function.

           )

; The next case is not needed for our guarantees.  Rather, it ensures that
; evaluation with guard checking on really does check guards at each function
; call.

          ((and <guard-checking-on>
                (not <*1*guard>))
           <fail_guard>)
          ((or (eq <guard-checking-on> :all)
               (and <safe>
                    (eq <class> :program)))
           (return-from <*1*fn> *1*body))
          ((eq <class> :program)
           (return-from <*1*fn> (<fn> . <formals>)))

; If we fall through to here, then we compute in the logic, avoiding further
; guard checks in recursive calls (where a "special" declaration will take
; care of this if we are in a mutual-recursion nest).

          (maybe-warn-for-guard <fn>)))

; In the case (eq <class> :program), we conclude by laying down the call (<fn>
; . <formals>).  Otherwise, we lay down the following code.

  (labels

; The following local definition of <*1*fn> executes calls of <fn> in the
; logic, without guard-checking (except for primitives under safe-mode; see
; below).  Note that it is always legitimate for this local function to cause
; an error, so if we want to save space we can fail here, in particular idea
; for :program mode functions encountered during the boot-strap, at least
; outside axioms.lisp -- although that would presumably eliminate the ability
; to call those functions in macro definitions.

   ((<*1*fn>
     <formals>

; Certain functions can take the live state as an argument, and yet do
; not ``properly'' handle it in their logic code.  Consider for
; example (princ$ '(a b) *standard-co* state).  With guard-checking
; off, and a live state, this form used to cause a hard error.  The
; problem was that the logical body of princ$ (actually, its
; oneification) was being executed.  Consider calling a function such
; as open-output-channels, which is really a call of nth, on a live
; state!  We believe that our problem is solved by disallowing calls
; of certain built-in functions on a live state argument, when passing
; to their oneified bodies.  These functions are those in
; *super-defun-wart-stobjs-in-alist*, since these are the ones that
; pass the state on as though it were a legitimate state object, i.e.,
; to functions that take non-state values as arguments.

; Other functions, such as those defined under defstobj, may have a stobj
; name as an argument but do not have an appropriate 'stobjs-in
; setting in the world, because we have not yet declared that the
; stobj name is a stobj.  These latter functions are characterized by
; having a non-nil stobj-flag, said flag being the stobj name.  We
; compute here the appropriate stobjs-in.

     <fail_if_live_stobj> ; laid down only in cases as described above
     *1*body))
   (*1*fn . formals)))))

WHERE:

<*1*guard>
 =
oneification of guard

<formals>
 =
list of formals, e.g., (x1 ... xn)

<guard-checking-on>
 =
(f-get-global 'guard-checking-on *the-live-state*)

<guard>
 =
[guard of the function being defined]

<fail_guard>
 =
(throw-raw-ev-fncall
 (list 'ev-fncall-guard-er '<fn> (list x) '<guard> '(nil) nil))

<fail_safe>
 =
(throw-raw-ev-fncall
 (list 'ev-fncall-guard-er '<fn> (list x) '<guard> '(nil) t))

<class>
 =
<*1*fn>

<*1*fn>
 =
(*1*-symbol <fn>)

<fn>
 =
[function symbol being defined]

<safe>
 =
(f-get-global 'safe-mode *the-live-state*)

<fail_if_live_stobj>
 = 
code for protecting against executing <*1*body> on live stobjs

<live-stobjp-test>
 =
test that all of the stobj parameters to the function are indeed the "live"
stobjs.  This is required because the Common Lisp code for stobj access and
update functions assumes, starting in v2-8, that user-defined stobj parameters
are live, a restriction enforced by the corresponding *1* functions before
passing to Common Lisp.

<wormhole-test-for-functions-with-user-stobjs>
  =
a test that is generated to check if one is evaluating a function with
user-defined stobjs in a wormhole (no wormhole test is performed if the
function does not take user-defined stobjs as arguments).  Only proper updates
on state are allowed inside wormholes since the wormhole can properly "undo"
these side effects upon completion.  No such mechanism exists for user-defined
stobjs and thus the test.  Before v2-8, this wormhole test was performed in the
stobj update primitives directly, but it is now performed in the *1* function
as a matter of efficiency.  The exclusion of read access of user-defined stobjs
in wormholes simplifies the code to generate the *1* body and while technically
unnecessary, does not seem to be a relevant over-restriction in practice.

|#

; F. Remarks

; Remark 1.  Notice that safe mode does not by itself force guard-checking in
; all cases, nor does soundness of safe mode require guard-checking as long as
; we do check guards when evaluating calls of functions that are built into
; Common Lisp.  We ensure this in the macro gv, which is changed in Version_2.6
; to cause an error when in safe mode.

; Remark 2.  Consider, in the body of *1*fn, the case that <guard-checking-on>
; holds.  If we were to replace it with (or guard-checking-on program) then we
; would always check guards when in program mode, which would give backward
; compatability: this scheme would behave exactly as the scheme from
; Version_2.5 for and before did when the new scheme is used in other than safe
; mode.  But we have decided that starting with Version_2.6, we will no longer
; check guards for :program mode functions when 'guard-checking-on has value
; nil (though starting with Version_3.0 you can get this effect when
; 'guard-checking-on has value :none).  After all, even with guard-checking on
; in Version_2.5 you can get nasty Lisp breaks, since we slip directly into
; Common Lisp when a guard holds even though guards cannot be verified for
; :program mode functions.

; End of Essay on Evaluation in ACL2

;          ONEIFICATION

(defconst *oneify-primitives*

;;;; Some day we should perhaps remove consp and other such functions from this
;;;; list because of the "generalized Boolean" problem.

; Add to this list whenever we find a guardless function in #+acl2-loop-only.

  '(if equal cons not consp atom acl2-numberp characterp integerp rationalp
       stringp symbolp

; We want fmt-to-comment-window (which will arise upon macroexpanding calls of
; cw) to be executed always in raw Lisp, so we add it to this list in order to
; bypass its *1* function.

       fmt-to-comment-window

; When we oneify, we sometimes do so on code that was laid down for constrained
; functions.  Therefore, we put throw on the list.

       throw-raw-ev-fncall

; The next group may be important for the use of safe-mode.

       makunbound-global
       trans-eval ev ev-lst ev-fncall
       fmt-to-comment-window
       sys-call-status
;      pstack-fn
       untranslate ; no-hack
       untranslate-lst
       with-error-trace-fn trace$-fn untrace$-fn
       set-w acl2-unwind-protect
       ))

; Here are the *1* functions.  They should be kept in sync with
; *primitive-formals-and-guards*.  We could probably get by with avoiding
; defining those in the list *oneify-primitives*, but why bother?  Well, we
; want to avoid calling "slow" functions, e.g., *1*cons instead of cons.
; Indeed, we define *1*if to cause an error, because we insist that the
; oneification of if remain lazy.  However, in the absence of a strong feeling
; about this, we prefer to leave things as they currently are.  We follow the
; rule that every function has a *1* counterpart, which is easy to remember.
; And we only give special treatment to if.

; Keep these in sync with the -completion axioms in axioms.lisp.

(defun-*1* acl2-numberp (x)
  (numberp x))

(defun-*1* binary-* (x y)
  (the number
       (if (numberp x)
           (if (numberp y)
               (* x y)
             (gv binary-* (x y) 0))
         (gv binary-* (x y) 0))))

(defun-*1* binary-+ (x y)
  (the number
       (if (numberp x)
           (if (numberp y)
               (+ (the number x) (the number y))
             (gv binary-+ (x y) x))
         (gv binary-+ (x y)
             (if (numberp y)
                 y
               0)))))

(defun-*1* unary-- (x)
  (the number
       (if (numberp x)
           (- x)
         (gv unary-- (x) 0))))

(defun-*1* unary-/ (x)
  (the number
       (if (and (numberp x) (not (= x 0)))
           (/ x)
         (gv unary-/ (x) 0))))

(defun-*1* < (x y)

; If one regards (gv op args val) simply as val, then we can prove that
; the body below is equivalent to the let-expression used for val.  Put
; another way, if we use << as the "familiar" less-than on the rationals
; then this definition of < is equivalent to
; (< x y) = (let ((x1 (if (acl2-numberp x) x 0))
;                 (y1 (if (acl2-numberp y) y 0)))
;            (or (<< (realpart x1) (realpart y1))
;                (and (= (realpart x1) (realpart y1))
;                     (<< (imagpart x1) (imagpart y1)))))
; The consideration of the case where both x and y are rational is just
; an optimization.

  (if (and (rationalp x)
           (rationalp y))
      (< (the rational x) (the rational y))
    (gv < (x y)
        (let ((x1 (if (numberp x) x 0))
              (y1 (if (numberp y) y 0)))
          (or (< (realpart x1) (realpart y1))
              (and (= (realpart x1) (realpart y1))
                   (< (imagpart x1) (imagpart y1))))))))
              

(defun-*1* apply (x y)
  (error "We have called apply on ~s and ~s, but we thought we were rid of apply."
         x y))

(defun-*1* bad-atom<= (x y)
  (cond ((and (bad-atom x) (bad-atom y))

; The following should never happen.

         (error "We have called (the executable counterpart of) bad-atom<= on ~
                 ~s and ~s, but bad-atom<= has no Common Lisp definition."
                x y))
        (t (gv bad-atom<= (x y) nil))))

(defun-*1* car (x)
  (cond
   ((consp x)
    (car x))
   ((null x)
    nil)
   (t (gv car (x) nil))))

(defun-*1* cdr (x)
  (cond
   ((consp x)
    (cdr x))
   ((null x)
    nil)
   (t (gv cdr (x) nil))))

(defun-*1* char-code (x)
  (if (characterp x)
      (char-code x)
    (gv char-code (x) 0)))

(defun-*1* characterp (x)
  (characterp x))

(defun-*1* code-char (x)
  (if (and (integerp x)
           (>= x 0)
           (< x 256))
      (code-char x)
    (gv code-char (x) (code-char 0))))
                   
(defun-*1* complex (x y)
  (complex (the rational (if (rationalp x) x (gv complex (x y) 0)))
           (the rational (if (rationalp y) y (gv complex (x y) 0)))))

(defun-*1* complex-rationalp (x)
  (complexp x))

;; RAG - I added this function to recognize the complex numbers.

#+:non-standard-analysis
(defun-*1* complexp (x)
  (complexp x))

(defun-*1* coerce (x y)
  (cond
   ((equal y 'list)
    (if (stringp x)
        (coerce x 'list)
      (gv coerce (x y) nil)))
   ((character-listp x)
    (if (equal y 'string)
        (coerce x 'string)
      (gv coerce (x y) (coerce x 'string))))
   (t
    (gv coerce (x y)
        (coerce (make-character-list x) 'string)))))

(defun-*1* cons (x y)
  (cons x y))

(defun-*1* consp (x)
  (consp x))

(defun-*1* denominator (x)
  (if (rationalp x)
      (denominator x)
    (gv denominator (x) 1)))

(defun-*1* equal (x y)
  (equal x y))

;; RAG - I added this function to evaluate the special floor1
;; function, which computes floor with a modulus of 1.

#+:non-standard-analysis
(defun-*1* floor1 (x)
  (if (rationalp x)
      (floor x 1)
    (gv floor1 (x) 0)))

(defun-*1* if (x y z)
  (error "We just can't stand having a non-lazy IF around.  But we attempted ~%~
          to call the executable counterpart of IF on argument list ~s."
         (list x y z)))

(defun-*1* imagpart (x)
  (if (numberp x)
      (imagpart x)
    (gv imagpart (x) 0)))

(defun-*1* integerp (x)
  (integerp x))

(defun-*1* intern-in-package-of-symbol (x y)
  (if (and (stringp x)
           (symbolp y))
      (let ((ans (intern x (symbol-package y))))
; See comment in intern-in-package-of-symbol for an explanation of this trick.
        ans)
    (gv intern (x y) nil)))

(defun-*1* pkg-witness (pkg)
  (if (stringp pkg)
      (if (find-non-hidden-package-entry
           pkg (known-package-alist *the-live-state*))
          (let ((ans (intern *pkg-witness-name* pkg)))
; See comment in intern-in-package-of-symbol for an explanation of this trick.
            ans)
        (throw-raw-ev-fncall (list 'pkg-witness-er pkg)))
    (gv pkg-witness (pkg) (intern *pkg-witness-name* "ACL2"))))

(defun-*1* numerator (x)
  (if (rationalp x)
      (numerator x)
    (gv numerator (x) 0)))

(defun-*1* rationalp (x)
  (rationalp x))

;; RAG - I added realp to recognize real numbers.

#+:non-standard-analysis
(defun-*1* realp (x)
  (realp x))

(defun-*1* realpart (x)
  (if (numberp x)
      (realpart x)
    (gv realpart (x) 0)))

(defun-*1* stringp (x)
  (stringp x))

(defun-*1* symbol-name (x)
  (if (symbolp x)
      (symbol-name x)
    (gv symbol-name (x) "")))

(defun-*1* symbol-package-name (x)
  (if (symbolp x)
      (symbol-package-name x)
    (gv symbol-package-name (x) "")))

(defun-*1* symbolp (x)
  (symbolp x))

;; RAG - I added *1*-defns for the non-standard predicates.  Note,
;; however, that the non-standard predicates do NOT have an executable
;; counterpart.  (Actually, that's too hasty.  Standard-part could be
;; defined as "fix" and standard-numberp could be "acl2-numberp".
;; Nothing can be done about i-large-integer, though.)  So, these
;; functions simply throw an error [added by Matt...: -- or, they did
;; at one time.  For efficiency it was useful to allow these to compute
;; on valid ACL2 objects (see bad-lisp-objectp); actually Ruben already
;; had made such changes].

#+:non-standard-analysis
(progn

(defun standard-numberp (x)
  (acl2-numberp x))

(defun-*1* standard-numberp (x)
  (acl2-numberp x))

(defun standard-part (x)
  (fix x))

(defun-*1* standard-part (x)
  (if (acl2-numberp x)
      x
    (gv symbol-package-name (x) 0)))

(defun i-large-integer ()
  (throw-raw-ev-fncall '(ev-fncall-null-body-er i-large-integer)))

(defun-*1* i-large-integer ()
  (throw-raw-ev-fncall '(ev-fncall-null-body-er i-large-integer)))

)

(defconst *macros-for-nonexpansion-in-raw-lisp*

; If a symbol, sym, is on this list then the form (sym a1 ... ak) is oneified
; to (sym a1' ... ak') where ai' is the oneification of ai.  Thus, conditions
; for sym being put on this list include that it is defined as a function or
; macro in raw lisp and that it is "applied" to a list of terms.  Another
; condition is that it not have a guard, because if a guard is present it is
; likely that Common Lisp will cause an error when we run the oneified version
; on inappropriate inputs.

; The value of this list should be a subset of
; (sloop::sloop for x in (w state) when (eq (cadr x) 'macro-body) collect (car x))
; Below we exhibit the value of the sloop above and comment out the macros we
; do not want on it.  The macros commented out will be translated away in
; oneified code.  

; When in doubt, comment it out!

  '(f-decrement-big-clock  ; we leave these two in oneified code because they
    f-big-clock-negative-p ; are handled by our raw lisp
;   make-list
;   f-put-global
;   f-get-global
;   f-boundp-global
;   mv-let                 ; not of the right shape so special-cased in oneify
    mv         

; The following are not in *primitive-event-macros* (which is handled directly
; in oneify-cltl-code).

; Note that safe-mode for make-event will require addition of the following four:
;   certify-book make-event defpkg in-package

;   acl2-unwind-protect
;   pprogn
;   the
    list*

;   rest tenth ninth eighth seventh sixth fifth fourth third second first cddddr
;   cdddar cddadr cddaar cdaddr cdadar cdaadr cdaaar cadddr caddar cadadr cadaar
;   caaddr caadar caaadr caaaar cdddr cddar cdadr cdaar caddr cadar caadr caaar
;   cddr cdar cadr caar

;   case progn mutual-recursion

;   / * >= > <=   ; guarded
;   let* cond
;   + -           ; guarded
    or and list 
;   local
;   defdoc
    f-put-global f-get-global f-boundp-global
    theory-invariant    
    ))

(defun-one-output macroexpand1! (x)
  (mv-let (erp val state)
          (macroexpand1 x 'oneify *the-live-state*)
          (declare (ignore erp state))
          val))

(defvar *acl2-gentemp-counter* 0)
(defun-one-output acl2-gentemp (root)
  (let ((acl2-pkg (find-package "ACL2")))
    (loop
     (let ((name (coerce (packn1 (list root *acl2-gentemp-counter*)) 'string)))
       (if (not (find-symbol name acl2-pkg))
           (return (let ((ans (intern name acl2-pkg)))
; See comment in intern-in-package-of-symbol for an explanation of this trick.
                     ans))
         (incf *acl2-gentemp-counter*))))))

(mutual-recursion

(defun-one-output oneify (x w)

; Keep this function in sync with translate11.  Errors have generally
; been removed, since we know they can't occur.

  (cond
   ((or (atom x) (eq (car x) 'quote))
    (cond ((keywordp x)
           (kwote x))
          ((symbolp x)

; At one time we returned (defined-constant x w) here in the case that
; (legal-constantp1 x).  But when the constant is replaced by its value, we can
; wind up with slow array accesses, since the constant and the written-out
; value need not be EQ.

           x)
          ((atom x) (kwote x))
          (t x)))
   ((not (symbolp (car x)))
    (oneify
     (list* 'let (listlis (cadr (car x))
                          (cdr x))
            (cddr (car x)))
     w))
   ((or (member-eq (car x) *oneify-primitives*)

; Note that safe-mode for make-event will require addition of the following two
; lines:
;       (member-eq (car x) *primitive-event-macros*)
;       (assoc-eq (car x) *super-defun-wart-table*)

        (member-eq (car x) *macros-for-nonexpansion-in-raw-lisp*))
    (let ((args (oneify-lst (cdr x) w)))
      (cons (car x) args)))
   ((eq (car x) 'mv-let)
    (let ((value-form (oneify (caddr x) w))
          (body-form (oneify (car (last x)) w)))
      `(mv-let ,(cadr x)
               ,value-form

; We leave the DECLAREs in place so that the compiler can see the
; IGNOREs at least.

               ,@(butlast (cdddr x) 1)
               ,body-form)))

#|  Feb 8, 1995.  Once upon a time we had the following code here:
   ((eq (car x) 'the)
    (let ((value-form (oneify (caddr x) w)))
      `(the ,(cadr x) ,value-form)))
   But that led to garbage for a user defined function like
   (defun foo (x) (declare (xargs :verify-guards nil)) (the integer x))
   because (foo 3) = 3 but (foo t) would cause a hard error.  We now
   just macroexpand the just like we would any other macro.  We are not
   sure why we ever thought we could handle it any other way.
|#

   ((eq (car x) 'translate-and-test)
    (oneify (caddr x) w))
   ((eq (car x) 'with-local-stobj)
    (mv-let (erp st mv-let-form creator)
            (parse-with-local-stobj (cdr x))
            (declare (ignore erp)) ; should be nil
            (mv-let-for-with-local-stobj mv-let-form st creator w)))
   ((getprop (car x) 'macro-body nil 'current-acl2-world w)
    (oneify (macroexpand1! x) w))
   ((eq (car x) 'let)
    (let ((value-forms
           (oneify-lst (strip-cadrs (cadr x)) w))
          (body-form (oneify (car (last x)) w)))
      `(let ,(listlis (strip-cars (cadr x))
                      value-forms)
         ,@(butlast (cddr x) 1)
         ,body-form)))
   ((eq (car x) 'must-be-equal) ; (must-be-equal logic exec)

; We avoid the extra needless evaluation when guard-checking is off, for
; example, during proofs.

    (let ((name (acl2-gentemp "XXX")))
      `(let ((,name ,(oneify (cadr x) w)))
         (if (member-eq (f-get-global 'guard-checking-on *the-live-state*)
                        '(nil :none))
             ,name
           (,(*1*-symbol 'must-be-equal)
            ,(oneify (caddr x) w)
            ,name)))))
   ((eq (car x) 'prog2$)

; We need to avoid dropping values in the multiple-value case when
; #+acl2-mv-as-values.  But this seems like a reasonable step to take in
; general.

    (let ((args (oneify-lst (cdr x) w)))
      (cons 'progn args)))
   ((eq (car x) 'time$)
    (list 'time (oneify (cadr x) w)))
   ((eq (car x) 'with-prover-time-limit)
    (list 'with-prover-time-limit (oneify (cadr x) w) (oneify (caddr x) w)))
   (t
    (let ((arg-forms (oneify-lst (cdr x) w)))
      (cons (*1*-symbol (car x)) arg-forms)))))

(defun-one-output oneify-lst (lst w)
  (cond ((atom lst) nil)
        (t (let ((x (oneify (car lst) w))
                 (y (oneify-lst (cdr lst) w)))
             (cons x y)))))

)

(defun-one-output select-stobj (name stobjs terms)
  (cond ((endp stobjs) nil)
        ((eq name (car stobjs)) (car terms))
        (t (select-stobj name (cdr stobjs) (cdr terms)))))

(defun-one-output super-defstobj-wart-stobjs-in (formals stobj-flag)
  (cond ((endp formals) nil)
        ((eq (car formals) stobj-flag)
         (cons stobj-flag
               (super-defstobj-wart-stobjs-in (cdr formals) stobj-flag)))
        (t (cons nil
                 (super-defstobj-wart-stobjs-in (cdr formals) stobj-flag)))))

(defun-one-output oneify-fail-form (er-type fn formals guard super-stobjs-in wrld safep)
  `(throw-raw-ev-fncall
    (list ',er-type
          ',fn
          (list ,@formals)
          ',guard
          ',(or super-stobjs-in
                (stobjs-in fn wrld)
                (if formals
                    (er hard 'oneify-cltl-code
                        "I didn't think this could happen, but for fn = ~x0 ~
                         stobjs-in is nil and formals isn't."
                        fn)
                  nil))
          ,@(and safep '(t)))))

(defun-one-output get-declared-stobjs (edcls)

; Keep this in sync with get-declared-stobj-names (which does checking and
; returns a value triple).

  (if (endp edcls)
      nil
    (union-eq (and (eq (caar edcls) 'xargs)
                   (let ((stobjs (cadr (assoc-keyword :STOBJS (cdar edcls)))))
                     (cond ((symbol-listp stobjs) stobjs)
                           ((and stobjs (symbolp stobjs)) (list stobjs))
                           (t nil))))
              (get-declared-stobjs (cdr edcls)))))

(defun-one-output create-live-user-stobjp-test (stobjs)
  (if (endp stobjs)
      t
    (let* ((stj (car stobjs))
           (rst (create-live-user-stobjp-test (cdr stobjs)))
           (tst `(the-live-stobjp ,stj)))
      (cond ((eq stj 'state) rst)
            ((eq rst t) tst)
            (t `(and ,tst ,rst))))))

(defun-one-output warn-for-guard-body (fn)
  (assert$ (boundp '*raw-guard-warningp*)
           (setq *raw-guard-warningp* nil))
  (let ((state *the-live-state*))
    (warning$ 'top-level "Guards"
              "Guard-checking will be inhibited on recursive calls of the ~
               executable counterpart (i.e., in the ACL2 logic) of ~x0.  To ~
               check guards on all recursive calls:~%  (set-guard-checking ~
               :all)~%To leave behavior unchanged except for inhibiting this ~
               message:~%  (set-guard-checking :nowarn)"
              fn)))

(defun-one-output oneify-cltl-code (defun-mode def stobj-flag wrld)

; This function is called when add-trip encouters a 'cltl-command triple, which
; is laid down by install-event after the triple for the symbol-class is laid
; down.  So, for example, the symbol-class for the function at hand has already
; been stored.

; See the template above for detailed comments.

  (if (null defun-mode)

; The function is non-executable.  We rely on the fact that such a function will
; always do a throw; so, it is safe for its executable counterpart simply to
; call that function.

      (return-from oneify-cltl-code
                   `(,(*1*-symbol (car def))
                     ,(cadr def) ; formals
                     (,(car def) ,@(cadr def)))))

  (if (and stobj-flag (null (cadr def)))

; We want to know if (car def) is a stobj creator, but it is premature to call
; stobj-creatorp using wrld because the necessary properties have not yet been
; laid down.  So we use the test above.

      (return-from oneify-cltl-code
                   `(,(*1*-symbol (car def)) nil
                     (throw-raw-ev-fncall ; as in oneify-fail-form
                      (list 'ev-fncall-creator-er ',(car def))))))

  (let* ((dcls (append-lst
                (strip-cdrs (remove-strings (butlast (cddr def) 1)))))
         (guards (get-guards1 dcls wrld))
         (guard (cond ((null guards) t)
                      ((null (cdr guards)) (car guards))
                      (t (cons 'and guards))))
         (guard-is-t (or (eq guard t)
                         (equal guard *t*)))
         (fn (car def))
         (*1*fn (*1*-symbol fn))
         (cl-compliant-p
          (and (eq (symbol-class fn wrld) :common-lisp-compliant)
               (assert$ (eq defun-mode :logic) t)))
         (formals (cadr def))
         boot-strap-p)
    (cond
     ((or (and guard-is-t
               cl-compliant-p)
          (and (setq boot-strap-p
                     (global-val 'boot-strap-flg (w *the-live-state*)))
               (member-eq fn
                          '(thm-fn
                            make-event-fn
                            certify-book-fn
; Keep the following in sync with *primitive-event-macros*.
                            defun-fn
                            ;; #+:non-standard-analysis
                            ;; defun-std ; defun-fn
                            defuns-fn ; mutual-recursion
                            ;; defuns ; calls defuns-fn, above
                            defthm-fn
                            ;; #+:non-standard-analysis
                            ;; defthm-std ; calls defthm-fn, above
                            defaxiom-fn
                            defconst-fn
                            defstobj-fn
                            defpkg-fn
                            deflabel-fn
                            defdoc-fn
                            deftheory-fn
                            defchoose-fn
                            verify-guards-fn
                            defmacro-fn
                            in-theory-fn
                            in-arithmetic-theory-fn
                            push-untouchable-fn
                            remove-untouchable-fn
                            reset-prehistory-fn
                            set-body-fn
                            table-fn
                            progn-fn
                            encapsulate-fn
                            include-book-fn
                            add-include-book-dir-fn
                            delete-include-book-dir-fn
                            comp-fn
                            verify-termination-fn
                            ;; add-match-free-override ; should be fast enough

; Theory-invariant is included in *macros-for-nonexpansion-in-raw-lisp*.  The
; remaining members of *primitive-event-macros*, after theory-invariant, are
; handled well enough already since we included table-fn above.
                            ))))

; Optimization in a common case: avoid labels function.  Note that if the guard
; is t then there are no stobjs except for the recognizer, whose raw Lisp code
; can handle non-live stobjs.

      `(,*1*fn
        ,formals
        ,(cons fn formals)))
     (t
      (let* ((*1*guard (oneify guard wrld))

; We throw away most declararations and the doc string, keeping only ignore and
; ignorable declarations.  Note that it is quite reasonable to ignore
; declarations when constructing ``slow'' functions.

             (body (car (last def)))
             (*1*body (oneify body wrld))
             (super-stobjs-in ; At a "leaf" of a stobj-based computation?
              (if stobj-flag

; Then we are looking at a function introduced by a defstobj event.

                  (let ((temp (super-defstobj-wart-stobjs-in formals
                                                             stobj-flag)))
                    (cond ((find-first-non-nil temp)
                           temp)
                          (t nil)))

; Else see if we are looking at a function that takes state but has logic code
; that does not handle a live state properly (and not just because of calls to
; lower-level functions with that problem).

                (cdr (assoc-eq fn *super-defun-wart-stobjs-in-alist*))))
             (ignore-vars

; If super-stobjs-in is non-nil, then we will lay down the code
; fail_if_live_stobj (defined below), which refers to all the formals; hence
; ignore-vars should be nil if super-stobjs-in is non-nil.

              (and (not super-stobjs-in) (ignore-vars dcls)))
             (ignorable-vars (ignorable-vars dcls))
             (guard-checking-on-form

; Functions in the ev-rec nest have a gc-off parameter that we generally assume
; to correspond with the state global guard-checking-on used here, so that the
; logic-only and raw lisp code agree.  See the comment in *ev-shortcut-okp*.

              '(f-get-global 'guard-checking-on *the-live-state*))
             (guard-checking-is-really-on-form
              (if (or boot-strap-p (eq defun-mode :program))
                  `(not (member-eq ,guard-checking-on-form '(nil :none)))

; An optimization here: otherwise the context should have ruled out :none.  See
; the comments in the binding of early-exit-code-and-warning below, and note
; that all uses of guard-checking-is-really-on-form support that binding.

                guard-checking-on-form))
             (fail_guard ; form for reporting guard failure
              (oneify-fail-form 'ev-fncall-guard-er fn formals guard
                                super-stobjs-in wrld nil))
             (fail_safe ; form for reporting guard or safe mode failure
              (oneify-fail-form 'ev-fncall-guard-er fn formals guard
                                super-stobjs-in wrld
                                `(member-eq ,guard-checking-on-form
                                            '(nil :none))))
             (safe-form

; Functions in the ev-rec nest have a safe-mode parameter that we generally
; assume to agree with the state global safe-mode, so that the logic-only and
; raw lisp code agree.  See the comment in *ev-shortcut-okp*.

              '(f-get-global 'safe-mode *the-live-state*))
             (fail_if_live_stobj
              (and super-stobjs-in ; optimization
                   (oneify-fail-form 'ev-fncall-live-stobj-guard-er fn formals
                                     guard super-stobjs-in wrld nil)))
             (super-stobjs-chk
              (if stobj-flag
                  (let ((first-non-nil (find-first-non-nil super-stobjs-in)))
                    `(the-live-stobjp ,first-non-nil))
                `(live-state-p
                  ,(select-stobj 'state super-stobjs-in formals))))
             (declared-stobjs (if stobj-flag
                                  (list stobj-flag)
                                (get-declared-stobjs dcls)))
             (user-stobj-is-arg (and declared-stobjs
                                     (not (equal declared-stobjs '(state)))))
             (live-stobjp-test (create-live-user-stobjp-test declared-stobjs))
             (declare-stobj-special

; Without a special declaration for the live stobj, a defstobj event will
; introduce *1* functions in add-trip, via a defuns trip, before the defstobj
; trip introduces the live stobj variable as special.  This might not be a big
; deal unless we compile, by which time (at the end of processing the defstobj
; trip) the live stobj variable has been introduced with defparameter, thus
; globally declaring it special.  However, openmcl complains because
; compilation is done when the *1* function is first introduced.  It seems
; appropriate to declare the live stobj variable special as soon as it is
; referenced, in such *1* functions, even though openmcl might be the only Lisp
; that could need this done.

              (and stobj-flag
                   `(declare (special ,(the-live-var stobj-flag)))))
             (guarded-primitive-p

; We want to check guards on the "leaves" of a computation in safe-mode, for
; example, on a call of EQ.  Evaluation in the ACL2 logic can only diverge from
; evaluation in (raw) Common Lisp when a guard is violated on a function that
; is already defined in Common Lisp.  A function considered here that is at
; risk for such divergence has a non-T guard, is being defined in the
; boot-strap, and is not in the ACL2 package (which is unknown to Common Lisp).
; So as we generate code here, we restrict the additional guard-check in
; safe-mode to such functions.

              (and (not guard-is-t) ; we are trusting guards on the primitives!
                   boot-strap-p
                   (not (member-equal (symbol-package-name fn)
                                      '("ACL2" "ACL2-PC")))))
             (*1*form
              `(labels ((,*1*fn
                         ,formals
                         ,@(and declare-stobj-special
                                (list declare-stobj-special))
                         ,@(and ignore-vars
                                `((declare (ignore ,@ignore-vars))))
                         ,@(and ignorable-vars
                                `((declare (ignorable ,@ignorable-vars))))
                         ,@(and super-stobjs-in
                                `((when ,super-stobjs-chk
                                    ,fail_if_live_stobj)))
                         ,*1*body))
                       (,*1*fn ,@formals)))
             (cl-compliant-code-guard-not-t
              (and
               (not guard-is-t) ; optimization: only used in guard-is-t case

; NOTE: we have to test for live stobjs before we evaluate the guard, since the
; guard is in Common Lisp and may assume all stobjs are live.

               `(cond (,guard
                       ,(cond ((not (eq live-stobjp-test t))
                               (assert$

; No user-stobj-based functions are primitives for which we need to give
; special consideration to safe-mode.

                                (not guarded-primitive-p)
                                `(cond (,live-stobjp-test
                                        (return-from ,*1*fn
                                                     (,fn ,@formals))))))
                              (t `(return-from ,*1*fn (,fn ,@formals)))))
                      ,@(let ((guard-checking-is-on-form-optimized
                               (cond (boot-strap-p
                                      guard-checking-is-really-on-form)
                                     (t

; The function will be :common-lisp-compliant in this context, so we can rule
; out :program mode and thus guard-checking-is-really-on-form reduces to 
; guard-checking-on-form

                                      guard-checking-on-form))))
                          (cond (guarded-primitive-p
                                 `(((or ,guard-checking-is-on-form-optimized
                                        ,safe-form)
                                    ,fail_safe)))
                                (t
                                 `((,guard-checking-is-on-form-optimized
                                    ,fail_guard))))))))
             (logic-recursive-p
              (and (eq defun-mode :logic)
                   (ffnnamep fn (body fn nil wrld))))
             (early-exit-code
              (if cl-compliant-p
                  (assert$ (not guard-is-t)
                           cl-compliant-code-guard-not-t)
                (let ((cond-clauses
                       `(,@(and (not (eq defun-mode :program))

; If the guard is t, then we want to execute the raw Lisp code in the
; :common-lisp-compliant case even if guard-checking is :none.  This
; early-exit-code is only executed when guard-checking is not :none, so
; we need to handle that special case (:none, guard t) elsewhere, and
; we do so in *1*-body-forms below.

                                (not guard-is-t)
                                `(((eq (symbol-class ',fn (w *the-live-state*))
                                       :common-lisp-compliant)
                                   ,(assert$ (not guard-is-t)
                                             cl-compliant-code-guard-not-t))))
                           ,@(and (not guard-is-t)
                                  (cond
                                   (guarded-primitive-p
                                    `(((and (or ,safe-form
                                                ,guard-checking-is-really-on-form)
                                            (not ,*1*guard))
                                       ,fail_safe)))
                                   (t
                                    `(((and ,guard-checking-is-really-on-form
                                            (not ,*1*guard))
                                       ,fail_guard)))))
                           ,@(cond ((eq defun-mode :program)

; Keep the following comment in sync with the final paragraph of :doc
; set-guard-checking.

; In the boot-strap world we have functions whose definitions are different in
; raw Lisp from the logic, such as ev and get-global.  If we allow :all or
; :none to serve their purposes for those functions, we can wind up with
; unpleasant guard violations.  For example, wet expands to a call of
; with-error-trace-fn, and if the idea of :all is applied then we get the
; following sequence of calls in the logic (i.e., using their *1* functions):
; with-error-trace-fn, trans-eval, ev, ev-rec, ev-fncall-rec,
; ev-fncall-rec-logical, w-of-any-state, and finally global-table.  The last of
; these calls causes a guard violation since *the-live-state* is not a true
; list.

; Also, we want to make sure that built-in :program mode functions run fast,
; for example, defthm-fn.

; And finally, this is where we finish handling of safe-mode for
; guarded-primitive-p functions, specifically those in :program mode since if
; such a function is in :logic mode then it is :common-lisp-compliant (see
; check-none-ideal), so it is handled above.

                                    (cond (boot-strap-p
                                           `((,safe-form
                                              (return-from ,*1*fn ,*1*body))))
                                          (t

; We will not be laying down a labels call, so we go ahead and stay in the *1*
; world here in the case of safe-mode.

                                           `(((or (member-eq ,guard-checking-on-form
                                                             '(:none :all))
                                                  ,safe-form)
                                              (return-from ,*1*fn ,*1*body))))))
                                   (logic-recursive-p

; If logic-recursive-p is nil, then we know that fn doesn't occur in the body,
; so we can avoid this code and fall through to the labels call (which will be
; there because we are not in :program mode in this case).  We don't worry
; about safe-mode when making the *1*fn call here because we stay "safe" as
; long as we are calling *1* functions.

                                    `(((eq ,guard-checking-on-form :all)
                                       (return-from ,*1*fn ,*1*body))))))))
                  (and cond-clauses
                       (cons 'cond cond-clauses)))))
             (early-exit-code-and-warning-boot-strap
              (cond ((and logic-recursive-p
                          (not guard-is-t))

; Note that we do not print the warning for :program mode functions, though we
; easily could.  It seems that this warning, which might be printed during
; macroexpansion (where we use safe-mode), may cause more distraction than it
; is worth.

                     (list (assert$ early-exit-code
                                    early-exit-code)
                           `(when *raw-guard-warningp*
                              (warn-for-guard-body ',fn))))
                    (early-exit-code (list early-exit-code))))
             (early-exit-code-and-warning
              (cond
               ((or boot-strap-p
                    (eq defun-mode :program))
; Thus, we can use guard-checking-is-really-on-form in the following code.
                early-exit-code-and-warning-boot-strap)
               (early-exit-code-and-warning-boot-strap
                `((when (not (eq ,guard-checking-on-form :none))
; Thus, by ruling out :none just above, we can use
; guard-checking-is-really-on-form in the following code.
                    ,@early-exit-code-and-warning-boot-strap)))))
             (main-body-before-*1*-call

; This is the code that is executed before we fall through: to the final labels
; function in the :logic mode case, but to the non-*1* call in the :program
; mode case.

              (append
               (and user-stobj-is-arg
                    `((cond (*wormholep*
                             (wormhole-er (quote ,fn) (list ,@formals))))))
               early-exit-code-and-warning))
             (*1*-body-forms
              (cond
               ((eq defun-mode :program)
                (append main-body-before-*1*-call
                        `((,fn ,@formals))))
               ((not guard-is-t)
                (append main-body-before-*1*-call
                        (list *1*form)))
               (t ; :ideal, guard-is-t
                (assert$
                 (not cl-compliant-p) ; would be handled above
                 (cons
                  `(when (eq (symbol-class ',fn (w *the-live-state*))
                             :common-lisp-compliant)
                     (return-from ,*1*fn (,fn ,@formals)))
                  (append main-body-before-*1*-call
                          (list *1*form))))))))
    `(,*1*fn
      ,formals
      ,@(and declare-stobj-special (list declare-stobj-special))

; At one time we attempted to do some code-sharing using a macro call, by using
; *1*body-call in place of *1*body in the code above, where *1*body-call was
; defined as shown below.  But with an ACL2 image built on Allegro, for (ld
; "books/rtl/rel4/support/merge.lisp") with (set-inhibit-output-lst '(prove
; proof-tree)) after (prof:start-profiler), it took 127.5 seconds to run such a
; modification of oneify-cltl-code, as opposed to 103.5 seconds.  Granted, we
; chose this file because it was shown in some earlier experiments with
; macrolet to have a particularly bad slowdown over previous versions without
; macrolet.  But still, we suffer the extra code for recursive :ideal-mode
; functions rather than generate macrolet forms.  Below are the relevant
; bindings used in a previous version of this code, in case we decide to
; revisit this approach.

#|
         (*1*body-call-shared-p

; We want to keep code size down by using macrolet to share the *1*body
; expression, but preferably not otherwise, to avoid overhead that we seem to
; have observed, at least in Allegro CL, for expanding (uncompiled) macrolet
; calls.  The expression below should thus agree with the governing conditions
; for the occurrences of *1*body-call outside the labels function that will
; also occur in a corresponding labels function.  The latter rules out the case
; (eq defun-mode :program).

          (ffnnamep fn (body fn nil wrld)))
         (*1*body-macro (and *1*body-call-shared-p
                             (acl2-gentemp "*1*BODY-MACRO")))
         (*1*body-call (if *1*body-call-shared-p
                           `(,*1*body-macro)
                         *1*body))

;;; end of let* bindings .... and here is the replacement for ,@*1*-body-forms
;;; below:

        ,@(if *1*body-call-shared-p
              `((macrolet ((,*1*body-macro () ',*1*body))
                          ,@*1*-body-forms))
            *1*-body-forms)
|# ; |

      ,@*1*-body-forms))))))


;          PROMPTS

; New ACL2 users sometimes do not notice that they are outside the ACL2
; read-eval-print loop when in a break.  We modify the prompts when we see how
; to do so, so that in a break we see "[RAW LISP]" in the prompt.  For most
; lisps, this seems to require changing the prompt at the top-level too, not
; just in a break.

(defvar *saved-raw-prompt* nil)
(defvar *saved-raw-prompt-p* nil)

#+allegro
(progn

(defun-one-output install-new-raw-prompt ()
  (cond ((not *saved-raw-prompt-p*)
         (setq *saved-raw-prompt* tpl:*prompt*)
         (setq tpl:*prompt*
               (concatenate 'string *saved-raw-prompt* "[RAW LISP] "))
         (setq *saved-raw-prompt-p* t))))

(defun-one-output install-old-raw-prompt ()
  (cond (*saved-raw-prompt-p*
         (setq tpl:*prompt* *saved-raw-prompt*)
         (setq *saved-raw-prompt-p* nil)
         (setq *saved-raw-prompt* nil) ; no longer needed; free storage
         t))))

#+clisp
(progn

(defun-one-output install-new-raw-prompt ()
  (cond ((not *saved-raw-prompt-p*)
         (setq *saved-raw-prompt* custom::*prompt-body*)
         (setq custom::*prompt-body* ; attempt to mimic clisp 2.33
               (function
                (lambda ()
                  (if (equal system::*home-package* *package*)
                      (format nil "[RAW LISP]")
                    (format nil "~a [RAW LISP]" (package-name *package*))))))
         (setq *saved-raw-prompt-p* t))))

(defun-one-output install-old-raw-prompt ()
  (cond (*saved-raw-prompt-p*
         (setq custom::*prompt-body* *saved-raw-prompt*)
         (setq *saved-raw-prompt-p* nil)
         (setq *saved-raw-prompt* nil) ; no longer needed; free storage
         t))))

#+cmu
(progn

(defun-one-output install-new-raw-prompt ()
  (setq debug:*debug-prompt*
        (function (lambda ()
                    (debug::debug-prompt)
                    (format t "[RAW LISP] ")
                    (force-output t)))))

(defun-one-output install-old-raw-prompt ()
  (setq debug:*debug-prompt*
        (function debug::debug-prompt))))

#+openmcl
(progn

(defun-one-output install-new-raw-prompt ()
   (cond ((not *saved-raw-prompt-p*)
          (setq *saved-raw-prompt*
                (symbol-function 'ccl::print-listener-prompt))
          (let ((ccl:*warn-if-redefine-kernel* nil))
            (setf (symbol-function 'ccl::print-listener-prompt)
                  (lambda (stream &rest args)
                    (declare (ignore stream))
                    (apply *saved-raw-prompt* *debug-io* args)
                    (format *debug-io* "[RAW LISP] "))))
          (setq *saved-raw-prompt-p* t))))

(defun-one-output install-old-raw-prompt ()
  (cond (*saved-raw-prompt-p*
         (let ((ccl:*warn-if-redefine-kernel* nil))
           (setf (symbol-function 'ccl::print-listener-prompt)
                 *saved-raw-prompt*))
         (setq *saved-raw-prompt-p* nil)
         (setq *saved-raw-prompt* nil) ; no longer needed; free storage
         t))))

#+gcl
(progn

(defun-one-output install-new-raw-prompt ()
  (cond ((not (and (eql si::*gcl-major-version* 2)
                   (eql si::*gcl-minor-version* 6)))
         (cond (*lp-ever-entered-p*
                (er hard 'install-new-raw-prompt
                    "Install-new-raw-prompt is only supported in GCL 2.6 and ~
                     its sub-versions.  This appears to be a GCL ~s0.~s1."
                    si::*gcl-major-version*
                    si::*gcl-minor-version*))
               (t (setq *saved-raw-prompt-p* t))))
        ((not *saved-raw-prompt-p*)
         (setq si::*debug-prompt-suffix* "[RAW LISP]")
         (setf *saved-raw-prompt* (symbol-function 'si::break-level))
         (setf (symbol-function 'si::break-level)
               (symbol-function 'si::break-level-for-acl2))
         (setq *saved-raw-prompt-p* t))))

(defun-one-output install-old-raw-prompt ()
  (cond (*saved-raw-prompt-p*
         (setq si::*debug-prompt-suffix* "")
         (setf (symbol-function 'si::break-level)

; Since we set si::*debug-prompt-suffix*, we really don't have to revert
; (symbol-function 'si::break-level) -- unless our patch,
; 'si::break-level-for-acl2 is out of sync with the current GCL's
; 'si::break-level.  So we play it safe and revert.

               *saved-raw-prompt*)
         (setq *saved-raw-prompt-p* nil)
         (setq *saved-raw-prompt* nil) ; no longer needed; free storage
         t))))

#-(or allegro clisp cmu openmcl gcl)
(progn

(defun-one-output install-new-raw-prompt ()
  nil)

(defun-one-output install-old-raw-prompt ()
  nil)

)

;          INITIALIZATION OF CURRENT ACL2 WORLD

; Once upon a time (pre-V2.2) we had the following defvar here:

; (defvar *current-acl2-world-key* (make-symbol "*CURRENT-ACL2-WORLD-KEY*"))

; But compiling under cmulisp showed us that we refer to the value
; of this var earlier in the initialization process.  So I have
; moved the defvar to axioms.lisp.

#+cmu
(cond ((not (boundp (global-symbol 'current-acl2-world)))
       (f-put-global 'current-acl2-world nil *the-live-state*)
       (setf (get 'current-acl2-world 'acl2-world-pair)
             (cons nil *current-acl2-world-key*))))
#-cmu
(eval-when #-cltl2 (load eval) #+cltl2 (:load-toplevel :execute)
           (f-put-global 'current-acl2-world nil *the-live-state*)
           (setf (get 'current-acl2-world 'acl2-world-pair)
                 (cons nil *current-acl2-world-key*)))

; EXTENDING AND RETRACTING PROPERTY LIST WORLDS

; We here sketch the entire world management scheme before diving into
; the details.  The software archeologist might think these summaries
; were written just for his use but that is wrong.  In fact, these are
; design sketches and refresher courses to bring to mind the salient
; details before getting back down to work.  This particular one
; represents the attempt to get back into this frame of mind after
; several days of Christmas preparations, 1990.  (Note: This essay has
; been updated since, to track changes such as the adoption, in April,
; 1994, of the restriction that reincarnated undone defpkgs must
; import only a subset of the old imports.  That attack on the
; "unintern problem" was sketched as the "Alternative Design Proposal"
; in the December, 1990 essay but rejected as unnecessary as it was
; then thought that we handled reincarnation correctly by uninterning
; all symbols except in abort recovery.  But :oops and the second pass
; of include books, etc., exposed the lie.)

; A property list "world" is a list of triples as created by putprop.
; Each triple is of the form (symb key . val).  Such a list is
; accessed by getprop, which, logically speaking, scans down it
; looking for a given symb and key.  Practically however, we allow a
; given world to be "installed" under any given symbolic name.  What
; installation does is assemble into an alist all of the properties of
; each symb in the world and put that alist on the property list of
; the symbol, under some special key that is associated with the name
; of the installed world.

; If name has an 'acl2-world-pair property then name is the name of an
; installed world.  The value of the property will be a pair, (alist .
; world-key), where alist is the (eq) world alist installed and
; world-key is a unique symbol associated with this world name and
; under which each symb's property alist is stored.

; The functions extend-world and retract-world will extend and retract
; a named world.  Logically speaking, these two functions are identity
; functions.  But practically speaking they smash Common Lisp property
; lists.  Extend-world must be given a name and a world that is an
; extension (eq) of the one currently installed under the name and
; will install the new properties.  An analogous remark holds for
; retract-world.  We make these functions available to the ACL2
; programmer.

; We store our own property list world under the name 'current-acl2-
; world.  How do we prevent the ACL2 programmer from smashing our
; properties?  Well, extend-world (which is logically a no-op all the
; time) is even a no-op practically on the name 'current-acl2-world.
; To smash property lists you must call extend-world1 (not an ACL2
; function) and that function works on any name.  Our ACL2 function
; set-w, which installs the current-acl2-world, calls extend-world1 in
; its #-acl2-loop-only code.  Set-w is, of course, untouchable.

; We include special support for retraction, which of course is the
; basis of undoing.  It would suffice, for extension and for getprop,
; if we could expedite the retrieval of the most recently put value of
; every symbol and key.  Suppose the world in question is w, named
; name, and suppose it is installed under the property name world-key.
; Suppose the only three triples on w about symb are (symb key1 . b),
; (symb key1 . a), and (symb key2 . c), occurring in that order on w.
; Then for purposes of extension and getprop alone, we could store
; '((key1 . b) (key2 . c)) under symb's world-key property.  But now
; suppose we wanted to retract back to where (symb key1 . a) was most
; recent.  Then we would need to change the alist stored under symb's
; world-key to '((key1 . a) (key2 . c)) and to find the newly exposed
; value for key1 we would have to search w.  This is what we did for
; the first 18 months of ACL2's development.  This made :ubt suffer
; because when we undid a property -- especially a property on some
; symbol like binary-+ or cons -- we would have to scan all the back
; down the world to the primordial putprops to recover the newly
; exposed values.  This was bad not so much because of the scan time
; but because of the swap time: the world is big and rarely
; referenced, so it tends to get paged out and then when you scan it
; you have to page it back in.  This can take a minute or more.

; To avoid this we actually store a stack for each key.  The stack is
; the list of all past values of the key, topped by the current value.
; An empty stack indicates that no putprop has occurred for that key
; (or, more accurately, that we have retracted back past the first
; putprop for that key).

; There is another twist to this scheme.  To support the execution and
; compilation of ACL2 functions in raw Common Lisp, we interpret a
; certain putprop symb key, namely CLTL-COMMAND GLOBAL-VALUE, as a
; directive to smash the symbol-function, macro-function, or constant
; definition of certain symbols contained in the value.  We only do
; this if we are installing 'current-acl2-world, of course.  To
; support undoing of these smashes we maintain a stack of the past
; settings of those fields.  This is the *undo-stack* of the symb.
; The situation here is complicated and more fully explained in the
; code below.

; The installation of worlds and error recovery are intimately con-
; nected to the problem of uninterning symbols on behalf of undone or
; reincarnated packages.  When the CLTL-COMMAND defpkg is encountered,
; the program defpkg is called to create the package.  Consider what
; would happen if defpkg were coded so as to unintern the symbols in
; the existing package and set the import list as per the new defini-
; tion (as, indeed, we once did, allowing the reincarnation of undone
; packages).  In particular, consider the effect this would have on
; triples yet-to-be installed: if they mentioned symbols in the new
; package then those symbols would suddenly become uninterned.  We
; once thought this was ok because symbols in newly defined packages
; couldn't yet exist in the yet-to-be installed world.  But that is a
; bogus claim: if we are reinstalling a world after an error abort or
; even an :oops the world might contain symbols in the "just defined"
; package.  This is what eventually drove us to implement the restric-
; tion described in :DOC package-reincarnation-import-restrictions.

; Because of the possiblity of user interrupts, it is possible that we
; can have effected some but not all of changes necessary to achieve a
; new state and then have the computation aborted.  To handle this,
; extend-world1 and retract-world1 both save the current world alist
; before they begin to make any changes.  If they are interrupted, the
; original configuration can be recovered by retracting back to nil
; and then extending to the saved current world.  This is admittedly
; inefficient -- all 20,000 properties of a typical current-acl2-world
; might have to be stored again because we didn't bother to remember
; how much of the extension we had done when we were interrupted.  On
; the other hand, it is truly simple and elegant and only comes into
; play with aborts during installation.

; Inspection of the lisp code for defpkg will reveal that it is
; sensitive to abort recovery in one other aspect.  If we are in abort
; recovery and the "dual package" (the one used to house the lisp
; equivalents of state global variables) already exists, we do not
; unbind all the variables in it but simply leave it untouched.  Since
; neither extending nor retracting changes state globals, the state
; global settings at the time of an abort are what they were when *w0*
; was saved.  Hence, by doing nothing to the dual package we keep the
; installed world and the state globals in the same relationship.

; So much for the sketch of the world management business.  We now get
; down to brass tacks.

; CMU Lisp won't let us undefine a macro because when we try to store
; nil into the macro-function we get a type violation because nil
; isn't a function.  So we have defined the useless function
; unbound-cmu-macro-function and we store it in the macro-function
; cell.  When we see this function in a macro-function cell, we act as
; though the symbol has no macro-function.

#+cmu
(defmacro unbound-cmu-macro-function (x y)
  (declare (ignore x y))
  '(er hard 'cmu
      "The function UNBOUND-CMU-MACRO-FUNCTION should never be called!"))

#+cmu
(defconst *unbound-cmu-macro-function*
  (macro-function 'unbound-cmu-macro-function))

(defun-one-output fmakunbound! (name)
  (fmakunbound name)

; The following appears not to be needed any longer in CMUCL 19b, for example.
; But this is working so we keep this code for robustness.

  (if (macro-function name)
      (setf (macro-function name)
            #+cmu
            *unbound-cmu-macro-function*
            #-cmu
            nil)))

(defmacro maybe-untrace (fn)
  `(when #+openmcl (ccl::advisedp ',fn)
         #-openmcl (member-eq ',fn
                              #-allegro (trace)
                              #+allegro excl::*advised-functions*)
     (untrace ,fn)))

(defun-one-output maybe-push-undo-stack (fn name &optional ignorep)

; See add-trip below for context.  Fn is one of the raw Lisp function
; names secretly spawned by CLTL-COMMAND forms, e.g., DEFUN, DEFMACRO,
; DEFCONST, or DEFPKG.  Name is the symbol or string that is being
; defined.

; Whenever we smash a CLTL cell we first save its current contents to
; permit redefinition and undoing.  Toward this end we maintain a
; stack for each defined symbol, called the *undo-stack* property of
; the symbol.  Very roughly speaking, the stack contains the previous
; values of the cells in question.  Add-trip will push the old value
; onto the stack before storing the new and undo-trip will pop the
; stack and restore that old value.  Ah, were it only that simple...

; There are complications.  First, DEFPKG doesn't have a symbol
; associated with it explicitly, so we have to manufacture one for the
; *undo-stack*.  We use the ``base symbol'' of the package (see
; chk-acceptable-defpkg).  If the symbol-package-name string is "name"
; then the base symbol is the symbol ACL2::name-PACKAGE.  (We use that
; symbol as a rule name associated with the defpkg axiom and so we
; already check that the name is new.)  Second, DEFPKG makes the
; notion of "current contents" highly abstract because it not only
; creates a package but imports various symbols into it.  So rather
; than use the *undo-stack* to save the "current contents" we use the
; stack to save a form that when evaluated will recreate the "current
; contents" of the cell in question.  When a new value is stored (and
; the cell is already in use) we will manufacture a suitable form for
; recreating the old value and push it.

; Third, ignorep is either nil, 'reclassifying or '(defstobj . stobj).
; When it is 'reclassifying, we only save the *1* def for name.
; Otherwise, we save both defs.

  (case fn
        ((defun defmacro)

; In Common Lisp, a symbol can be either a macro or function, but the
; symbol-function cell is used in both cases to store the associated
; code.  Therefore, if we are about to smash the symbol-function cell,
; e.g., in response to a DEFUN event, then we are obliged to remember
; whether it was previously defined as a macro.

; Notice that :inlined stobj functions are handled fine here, since
; in such cases, fn will be defun and the code below is fine for
; macros (even if defined with defabbrev).  We rely on this fact in
; undo-trip; see the comment there.

         (cond
          ((macro-function name)
           (push `(setf (macro-function ',name)
                        ',(macro-function name))
                 (get name '*undo-stack*)))
          ((fboundp name)
           (let ((oneified-name (*1*-symbol name)))
             (push `(progn
                      (maybe-untrace ,name)
                      ,@(if (eq ignorep 'reclassifying)
                            `((setf (symbol-function ',oneified-name)
                                    ',(symbol-function oneified-name)))
                          `((setf (symbol-function ',name)
                                  ',(symbol-function name))
                            ,(cond
                              ((fboundp oneified-name)
                               `(setf (symbol-function ',oneified-name)
                                      ',(symbol-function oneified-name)))
                              (t `(fmakunbound! ',oneified-name))))))
                   (get name '*undo-stack*))))
          (t (push `(progn (maybe-untrace ,name)
                           (fmakunbound! ',name)
                           (fmakunbound! ',(*1*-symbol name)))
                   (get name '*undo-stack*)))))
        (defconst

; Note: defstobj events use maybe-push-undo-stack with fn = 'defconst
; to save the values of the name, the live name and also of
; '*user-stobj-alist*!

          (cond
           ((boundp name)
            (push `(setf (symbol-value ',name)
                         ',(symbol-value name))
                  (get name '*undo-stack*)))
           (t (push `(makunbound ',name)
                    (get name '*undo-stack*)))))

        (defpkg
          (let ((temp (find-non-hidden-package-entry
                       name
                       (known-package-alist *the-live-state*))))
            (cond
             (temp
              (push `(defpkg ,name ',(package-entry-imports temp))
                    (get (packn (cons name '("-PACKAGE"))) '*undo-stack*))))))
        (otherwise
         (er hard 'maybe-push-undo-stack
             "Unrecognized CLTL-COMMAND spawn ~x0"
             fn))))

(defun-one-output maybe-pop-undo-stack (name)

; See maybe-push-undo-stack.

  (let* ((name (if (symbolp name)
                   name
                 (packn (cons name '("-PACKAGE")))))
         (stk (get name '*undo-stack*)))
    (cond
     ((null stk) nil)
     (t (eval (car stk))
        (setf (get name '*undo-stack*) (cdr stk))))))

(defun-one-output flush-undo-stack (name)

; We completely wipe out the undo-stack of name, after returning
; the relevant cell to its initial configuration.

  (let* ((name (if (symbolp name) name (intern name "ACL2")))
         (stk (get name '*undo-stack*)))
    (cond (stk (eval (car (last stk)))))
    (remprop name '*undo-stack*)))

; Now we define the two programs that manage the stacks of old
; property values.

; We start with pushing a new value onto the stack for a given key.
; Complicating things is our decision to order the keys in the alists by (a
; priori) frequency of access.  The aim is to speed up getprop.  We record
; the results of many experiments below.

; Recall that the current-acl2-world is implemented so that the logical
; properties are stored in an alist which is obtained via a raw lisp get of the
; property *current-acl2-world-key*.  That alist is then searched with assoc
; :test #'eq.  Of interest then are both the order of the properties
; encountered by the raw lisp get and the order of the keys encountered by the
; assoc :test #'eq.

; The basic experiment addressed one particular proof in the Nqthm package.  To
; set the stage, the Nqthm package was loaded and then undone back through
; NQTHM-COUNT-SYMBOL-IS-COUNT-FN-UNPACK, a theorem whose reported proof time is
; 35.23 by the current Version 1.8.  Then that theorem was proved again while a
; patch was in place inside of fgetprop.  The patch collected together an alist
; recording the calls of fgetprop.  In particular the alist entries were of the
; form (symb (key1 . cnt1) ... (keyk . cntk)) indicating that (fgetprop symb
; keyi <some-default> <current-acl2-world>) was called cnti times during the
; proof.  We then wrote and compiled a program that swept the alist and
; repeated every call of fgetprop simply to allow us to measure the total time
; spent in fgetprop.  There were a total of 102781 calls.  To sweep the alist
; with a no-op function of the same arity as fgetprop required 0.25 seconds.
; We therefore consider that to be the overhead of the sweep itself.  To sweep
; with fgetprop required 0.75 seconds, indicating that a "net" 0.50 seconds
; were actually spent in fgetprop on the actual calls in the sample theorem.
; (We will use "net" henceforth to mean the measured time minus 0.25.)  This
; gives an expected "per call" time of 4.86E-6.

; For what it is worth, a noop that calls get has an overhead of 0.267 for
; a net of 0.017 or a per call time of 1.65E-7 seconds.  Thus an fgetprop
; is about 30 times slower than a get (with the orderings created by the
; current Version 1.8).

; However, we have noticed that *current-acl2-world-key* is not always the
; first property encountered by the raw lisp get.  Other properties sometimes
; covering it up include *UNDO-STACK*, *PREDEFINED* and SYSTEM:PNAME.  We
; therefore moved *current-acl2-world-key* to the front of every symbol-plist.
; The net sweep time was then 0.30 (for a per call time of 18 gets).

; We now move on to ordering the keys seen by assoc :test #'eq.  In prior
; experiments we had determined the frequency with which the various keys are
; accessed (during the entire Nqthm package proof).  For what it is worth, here
; is the key list, in order from most frequently accessed to least:

#|
  '(COARSENINGS GLOBAL-VALUE CONGRUENCES SYMBOL-CLASS TYPE-PRESCRIPTIONS
    LEMMAS RUNIC-MAPPING-PAIRS MULTIPLICITY STATE-IN
    RECURSIVEP DEF-BODIES CONSTRAINEDP LINEAR-LEMMAS
    FORMALS MACRO-BODY FORWARD-CHAINING-RULES STATE-OUT TABLE-ALIST
    GUARD MACRO-ARGS ELIMINATE-DESTRUCTORS-RULE CONST LEVEL-NO
    UNNORMALIZED-BODY THEOREM REDEFINED INDUCTION-MACHINE JUSTIFICATION
    INDUCTION-RULES CONTROLLER-ALIST QUICK-BLOCK-INFO
    ABSOLUTE-EVENT-NUMBER PRIMITIVE-RECURSIVE-DEFUNP TABLE-GUARD)|#

; We therefore reordered the alist so that the keys were stored with the most
; frequently accessed ones first.  We added nil COARSENINGS and CONGRUENCES
; properties (and later, as described below, RECURSIVEP) to those function
; symbol property lists for which the value of the property was nil but the
; property was unrecorded.  (This saves the time of cdring through the entire
; list to compute the most frequently seen two properties.)  Technically, we
; extended and reordered the alist found in (get symb
; *current-acl2-world-key*), for each symbol with a *current-acl2-world- key*
; property and that property was always first on the symbol-plist.

; We then repeated the sweep in a net time of 0.22 seconds (per call = 13 gets).

; We then reversed the "optimal" ordering on the property lists and measured a
; net time of 0.31 (down from 0.30 from the random order of Version 1.8).

; Finally, we perturbed the property lists by adding 10 new property keys and
; values to the front of every (get symb *current-acl2-world-key*) and measured
; a net time of 0.50.

; From this experiment one can make the following conclusions: (a) In this
; theorem, fgetprop is reponsible for less than 2% of the proof time.  Making
; fgetprop instantaneous would reduce the 35.23 seconds to 34.73 seconds.

; By ordering the properties (in both senses) we can speed fgetprop up from
; about 30 gets to about 13 gets, more than doubling its speed.

; The rest of this essay on experimental results discusses some detailed
; investigations that led to virtually no further improvement (see stats at the
; end of the essay).  The lesson learned is that it may not be worth mucking
; around further with *current-acl2-world-key-ordering*.

; In July 2002, during the development of Version_2.7, we modifed the use of
; the fnstack (specifically, being-openedp) so that for recursive functions we
; look up the representative of a clique, thus avoiding the need to look
; through all members every clique for the function at hand.  (A
; mutual-recursion nest with 4,786 defuns at AMD prompted this change.)  As a
; result we saw a 1.8% slowdown in the regression suite, reduced to 0.9% with
; some optimizations.  Presumably the slowdown was due to the more frequest use
; of the RECURSIVEP property.  So we ran experiments using
; books/certify-numbers.lisp and books/rtl/rel2/support/cert.lsp, though we
; aborted the latter partway through lop3.lisp (during the proof of BITN-LAM0,
; which seemed to be bogging down).  The results using analyze-fgetprop-stats
; were as follows.

#|
books/certify-numbers.lisp:

GLOBAL-VALUE                                        2474980
COARSENINGS                                         2332094
TYPE-PRESCRIPTIONS                                  1162730
RUNIC-MAPPING-PAIRS                                  979110
CONGRUENCES                                          769460
RECURSIVEP                                           676128
TABLE-ALIST                                          675429
SYMBOL-CLASS                                         415118
LEMMAS                                               381015
MACRO-BODY                                           356823
STOBJS-OUT                                           303906
FORMALS                                              213447
STOBJS-IN                                            161261
STOBJ                                                101845
GUARD                                                 75749
MACRO-ARGS                                            75221
BODY ; changed later to def-bodies                    68867
CONSTRAINEDP                                          50190
FORWARD-CHAINING-RULES                                49839
CONST                                                 25601
ELIMINATE-DESTRUCTORS-RULE                            19922
THEOREM                                                9234
LINEAR-LEMMAS                                          9102
...

books/rtl/rel2/support/cert.lsp (aborted as explained above):

COARSENINGS                                        30087445
GLOBAL-VALUE                                       28366962
CONGRUENCES                                        27187188
RUNIC-MAPPING-PAIRS                                13934370
TYPE-PRESCRIPTIONS                                 12058446
RECURSIVEP                                         10080678
TABLE-ALIST                                         4644946
SYMBOL-CLASS                                        2742519
LEMMAS                                              1978039
STOBJS-OUT                                          1943646
MACRO-BODY                                          1837674
FORMALS                                             1185024
STOBJS-IN                                            781274
BODY ; changed later to def-bodies                   585696
STOBJ                                                509394
GUARD                                                390584
MACRO-ARGS                                           389694
CONSTRAINEDP                                         332418
FORWARD-CHAINING-RULES                               211225
CONST                                                145628
ABSOLUTE-EVENT-NUMBER                                 93259
LINEAR-LEMMAS                                         34780
...
|#

; As a result, we revised the ordering of keys.  We also noticed that although
; GLOBAL-VALUE is high on the list, most of that is accounted for by looking it
; up for symbols RECOGNIZER-ALIST and UNTOUCHABLES, which do not have other
; properties:

#|
books/certify-numbers.lisp:

RECOGNIZER-ALIST                               2056058
 GLOBAL-VALUE                                       2056058
UNTOUCHABLES                                    261297
 GLOBAL-VALUE                                        261297

books/rtl/rel2/support/cert.lsp (aborted as explained above):

RECOGNIZER-ALIST                              26193957
 GLOBAL-VALUE                                      26193957
UNTOUCHABLES                                   1359647
 GLOBAL-VALUE                                       1359647
|#

; The user times (in seconds) for running the regression suite using an Allegro
; 6.0 Linux development Version_2.7 were as follows, with successive
; "improvements" shown.

; 15359.38 ; original time
; 15637.45 ; 1.81% slowdown: first cut at new approach to fnstack for mutrec
; 15496.32 ; 0.89% slowdown: optimizations in being-openedp (made a macro)
; 15497.46 ; 0.90% slowdown: new *current-acl2-world-key-ordering*
; 15481.14 ; 0.79% slowdown: always put recursivep property on function symbols

; March 2006: Here are some new numbers, listing in each case down to about 2
; orders of magnitude below the most-used property.  All were obtained with all
; outpu inhibited.

#|
============================================================

stats0 (books/certify-numbers.lisp):

COARSENINGS                                         2527582
GLOBAL-VALUE                                        2224181
RUNIC-MAPPING-PAIRS                                 1188675
TYPE-PRESCRIPTIONS                                  1074218
CONGRUENCES                                          730666
DEF-BODIES                                           685868
TABLE-ALIST                                          642459
SYMBOL-CLASS                                         400157
LEMMAS                                               362209

============================================================

stats1 (books/workshops/1999/compiler/proof1):

COARSENINGS                                         1137397
DEF-BODIES                                           705063
GLOBAL-VALUE                                         587267
TABLE-ALIST                                          360303
TYPE-PRESCRIPTIONS                                   196192
CONGRUENCES                                          194726
SYMBOL-CLASS                                         177363
LEMMAS                                               167682
RUNIC-MAPPING-PAIRS                                   75828
STOBJS-OUT                                            13381
MACRO-BODY                                            10245

============================================================

stats2 (:mini-proveall):

COARSENINGS                                           87020
GLOBAL-VALUE                                          58987
RUNIC-MAPPING-PAIRS                                   54106
TABLE-ALIST                                           32902
DEF-BODIES                                            26496
TYPE-PRESCRIPTIONS                                    24822
CONGRUENCES                                           20367
LEMMAS                                                17938
SYMBOL-CLASS                                          15271
FORWARD-CHAINING-RULES                                 4820
FORMALS                                                1278
MACRO-BODY                                             1216
STOBJS-OUT                                             1199
ELIMINATE-DESTRUCTORS-RULE                              962

============================================================

stats3 (osets/map):

DEF-BODIES                                           288073
RUNIC-MAPPING-PAIRS                                  262004
COARSENINGS                                          235573
GLOBAL-VALUE                                         171724
FORMALS                                               84780
TABLE-ALIST                                           76462
UNNORMALIZED-BODY                                     61718
TYPE-PRESCRIPTIONS                                    56193
LEMMAS                                                54533
CONSTRAINEDP                                          52642
SYMBOL-CLASS                                          43824
CONGRUENCES                                           36786
MACRO-BODY                                            30206
STOBJS-OUT                                            27727
THEOREM                                               15714

============================================================

stats4 (rtl/rel5/support/drnd):

COARSENINGS                                        20881212
GLOBAL-VALUE                                       10230404
RUNIC-MAPPING-PAIRS                                 7726914
TYPE-PRESCRIPTIONS                                  4177523
DEF-BODIES                                          2732746
SYMBOL-CLASS                                         705776
STOBJS-OUT                                           671763
TABLE-ALIST                                          664941
CONGRUENCES                                          497120
LEMMAS                                               376371
MACRO-BODY                                           294016

============================================================

stats5 (rtl/rel2/support/cert.lsp):

COARSENINGS                                        21792912
GLOBAL-VALUE                                       15497700
RUNIC-MAPPING-PAIRS                                 8088313
TYPE-PRESCRIPTIONS                                  6554966
DEF-BODIES                                          5365470
TABLE-ALIST                                         2641304
SYMBOL-CLASS                                        1873984
CONGRUENCES                                         1562924
LEMMAS                                              1220873
STOBJS-OUT                                           420330
MACRO-BODY                                           364583
FORMALS                                              248019
FORWARD-CHAINING-RULES                               245442

============================================================

|#

; End of Experimental Results.

; Below we list the most important property keys according to the results
; above.  Keys are stored in alists in this order, i.e., keys occurring earlier
; in this list are stored earlier in the alists.  When a key not occurring in
; this list is added to the alist it is as though it occurred at the very end
; of this list, i.e., it gets a low priority.  Not all keys used by the current
; system are in this list (see below).

(defparameter *current-acl2-world-key-ordering*
  '(COARSENINGS
    GLOBAL-VALUE ; mostly looked up for RECOGNIZER-ALIST and UNTOUCHABLES,
                 ; which do not have other properties
    RUNIC-MAPPING-PAIRS
    DEF-BODIES
    TYPE-PRESCRIPTIONS
    TABLE-ALIST
    CONGRUENCES
    SYMBOL-CLASS
    LEMMAS
    STOBJS-OUT
    MACRO-BODY
    FORMALS
    FORWARD-CHAINING-RULES

; Note: As of this writing there are many properties not included above, all of
; which fall into the low priority category.  We have omitted keys simply to
; keep the list shortened and thus to speed up the insertion program
; (merge-into-alist, on behalf of destructive-push-assoc) a little.  This is an
; unanalyzed "optimization".

    ))

(defun-one-output key-lesseqp (key1 key2 ordering)

; We return t if key1 occurs weakly before key2 in the ordering.

  (cond ((null ordering) t)
        ((eq key1 (car ordering)) t)
        ((eq key2 (car ordering)) nil)
        (t (key-lesseqp key1 key2 (cdr ordering)))))

(defun-one-output merge-into-alist (key val alist)

; Alist is a symbol alist, key is a symbol that is not bound in alist.  We wish
; to create the alist that is logically equivalent under assoc-eq to (cons
; (cons key val) alist) but we actually place the new pair in the proper place
; according to the *current-acl2-world-key-ordering*.

  (cond ((null alist) (list (cons key val)))
        ((key-lesseqp key (caar alist) *current-acl2-world-key-ordering*)
         (cons (cons key val) alist))
        (t (cons (car alist) (merge-into-alist key val (cdr alist))))))

(defun-one-output destructive-push-assoc (key value alist world-key)

; We push value onto the stack associated with key in alist.  If key has no
; value in alist, we pretend it has the empty stack.  E.g., if alist is '((a .
; (1))) and we push 2 on 'a we get '((a . (2 1))) and if we then push 0 on 'b
; we get '((b . (0)) (a . (2 1))).  This function is maximally destructive on
; the cons structure of alist and the stacks, but doesn't touch the cons
; structure of the values.  We keep the alists in sorted order iff the
; world-key is our special one, *current-acl2-world-key*.

  (let ((temp (assoc key alist :test #'eq)))
    (cond (temp (setf (cdr temp) (cons value (cdr temp)))
                alist)
          ((eq world-key *current-acl2-world-key*)
           (merge-into-alist key (list value) alist))
          (t (cons (cons key (list value)) alist)))))

(defun-one-output destructive-pop-assoc (key alist)
  (let ((temp (assoc key alist :test #'eq)))
    (cond (temp (setf (cdr temp) (cdr (cdr temp)))
                alist)
          (t alist))))

(defun-one-output remove-current-acl2-world-key (plist)
  (cond ((null plist) plist)
        ((eq (car plist) *current-acl2-world-key*)
         (cddr plist))
        (t (cons (car plist)
                 (cons (cadr plist)
                       (remove-current-acl2-world-key (cddr plist)))))))

(defun-one-output add-trip (world-name world-key trip)

; Add-trip is the function that moves a triple, (symb key .  val) from
; a property list world into the von Neumann space of Common Lisp.
; World-name is the name of the world being installed.  World-key is
; the property being used to hold the installed properties of that
; world (i.e., the cdr of its 'acl2-world-pair).

; First we set the properties for the global-symbol and *1*-symbol, so that
; these will ultimately be behind the world-key property (as guaranteed at the
; end of the code for this function).

  (global-symbol (car trip))
  (*1*-symbol (car trip))

; Our next step is to push val onto the key stack in (get symb world-key).

  (setf (get (car trip) world-key)
        (destructive-push-assoc (cadr trip) (cddr trip)
                                (get (car trip) world-key)
                                world-key))

; Now, in the case that we are messing with 'current-acl2-world and
; symb is 'CLTL-COMMAND and key is 'GLOBAL-VALUE, we smash the
; symbol-function or symbol-value cell of the appropriate name, first
; saving the old value (form) on the undo-stack.

  (cond
   ((and (eq world-name 'current-acl2-world)
         (eq (car trip) 'cltl-command)
         (eq (cadr trip) 'global-value)
         (consp (cddr trip)))
    (let ((boot-strap-flg
           (global-val 'boot-strap-flg
                       (w *the-live-state*)))
          #+(and allegro (not acl2-loop-only))

; The following bindings, guarded by the readtime conditional above, should
; turn off redefinition warnings in Allegro and CLISP (2.33 and beyond, at
; least).  We trust that add-trip knows what it is doing.  Without this code,
; for example, :oops can cause many screens of such warnings.

          (excl:*redefinition-warnings* nil)
          #+(and clisp (not acl2-loop-only))
          (custom::*suppress-check-redefinition* t))
      (case (car (cddr trip))
        (defuns

; (cddr trip) is of the form (defuns defun-mode ignorep def1 ... defn).
; Defun-mode non-nil is stored by DEFUNS and defun-mode nil by :non-executable
; DEFUNS and by ENCAPSULATE when it is defining the constrained fns.
; Oneify-cltl-code relies on the fact that functions with defun-mode nil do a
; THROW.

; Observe that we sometimes use oneify-cltl-code to modify the actual Common
; Lisp code.  Why don't we modify the defi before storing the cltl-command
; tuple?  Because we want to make it easy on ourselves to recover from the
; world the actual defi used to define :program mode functions.  See
; verify-termination.

; Recall that ignorep is non-nil if we are to AVOID storing the
; symbol-functions for names.  If ignorep is non-nil, then it is either
; reclassifyingp -- meaning we are reclassifying a symbol from :program
;                   to :logic mode.  We don't want to overwrite its 
;                   symbol-function since that might be ACL2 source code.
;                   We still write a *1* definition in this case.
; (defstobj . stobj)
;                -- meaning the names being introduced are actually being
;                   defun'd under (defstobj stobj ...).  We don't want
;                   to store the code generated by defun for these
;                   names because defstobj will generate a
;                   CLTL-COMMAND containing the made-to-order raw
;                   defs.  We also do not store the *1* definition in this
;                   case, because in openMCL (at least) this would cause a
;                   problem since the *1* code calls the raw Lisp function,
;                   which has not yet been defined and in the :inline case is
;                   actually a macro.  (See also the comment in
;                   defstobj-functionsp.)

; Why do we need the stobj name in the case of ignorep = '(defstobj
; . stobj)?  The reason is that when we generate the *1* code for the
; function, fn, we must generate a throw to handle a guard violation
; and the argument to that throw is an object which includes, among
; other things, the stobjs-in of fn so we will know how to print them.
; You might think we would get the stobjs-in of fn from the world.
; But we can't because this defun is being done under, and as part of,
; a defstobj and the defstobj will later declare stobj to be a stobj
; name.  So the stobjs-in of fn in the world right now is wrong.  The
; stobjs-in we need is built into the object thrown and so won't be
; overwritten when defstobj gets around to declaring stobj a stobj.
; So oneify-cltl-code, called below, takes the stobj name as its input
; and computes the appropriate stobjs-in from the formals.  This is a
; problem analogous to the one addressed by the super-defun-wart
; table.

          (let ((ignorep (caddr (cddr trip)))
                (defun-mode (cadr (cddr trip)))
                (new-defs

; We avoid potential "undefined" warnings by holding off on compilation until
; all the functions have been defined.  Moreover, in the case of OpenMCL we
; need to hold off even on defining the functions.  So we collect up the
; definitions that need to be made in Common Lisp, proclaiming as we go
; (although proclaiming may be a no-op in most Lisps), then make all the
; definitions, and finally do the compilation as appropriate.

                 nil))
            (dolist
              (def (cdddr (cddr trip)))
              (cond ((and boot-strap-flg
                          (not (global-val 'boot-strap-pass-2
                                           (w *the-live-state*))))

; During the first pass of initialization, we insist that every function
; defined already be defined in raw lisp.  During pass two we can't expect this
; because there may be LOCAL defuns that got skipped during compilation and the
; first pass.

                     (or (fboundp (car def))

; Note that names of macros are fboundp, so we can get away with symbols that
; are defined to be macros in raw Lisp but functions in the logic (e.g.,
; must-be-equal).

                         (interface-er "~x0 is not fboundp!"
                                       (car def)))

; But during the first pass of initialization, we do NOT assume that every (or
; any) function's corresponding *1* function has been defined.  So we take care
; of that now.

                     (let ((*1*def (cons 'defun
                                         (oneify-cltl-code
                                          defun-mode
                                          def

; The if below returns the stobj name being introduced, if any.

                                          (if (consp ignorep)
                                              (cdr ignorep)
                                            nil)
                                          (w *the-live-state*)))))
                       #-acl2-mv-as-values

; We create an appropriate declaim form only with ACL2's notion of mv.  If mv
; is treated as Common Lisp values, then we are not sure that we trust our
; algorithm for creating that declaim form, which relies on output arities
; being correctly stored in the stobjs-out field of the original function (see
; output-type-for-declare-form).  But perhaps with a little thought we could
; do something useful here even in that case.

                       (eval (make-defun-declare-form (car def) *1*def))
                       (setq new-defs
                             (cons *1*def new-defs))))
                    ((and (not ignorep)
                          (equal *main-lisp-package-name*
                                 (symbol-package-name (car def))))
                     (interface-er "It is illegal to redefine a function in ~
                                    the main Lisp package, such as ~x0!"
                                   (car def)))
                    ((and (consp ignorep)
                          (eq (car ignorep) 'defstobj))

; We wait for the cltl-command from the defstobj (which is laid down last by
; defstobj-fn, using install-event) before defining/compiling the *1*
; functions, in order to avoid potential "undefined" warnings and, more
; importantly, to avoid defining *1* functions in terms of undefined macros
; (for the :inline case), which confuses openMCL as described in a comment in
; defstobj-functionsp.  We still save the existing values (if any) of the
; current def and the current *1* def; see the next comment about ignorep.

                     (maybe-push-undo-stack 'defun (car def) ignorep))
                    (t (maybe-push-undo-stack 'defun (car def) ignorep)

; Note: If ignorep is '(defstobj . stobj), we save both the current def and the
; current *1* def.  If ignorep is 'reclassifying, we save only the *1* def.
; The former behavior means that in defstobj, when the defun runs for each
; name, we will save both symbol-function cells, even though we store into
; neither.  The code for installing a defstobj CLTL-COMMAND doesn't bother to
; do undo-stack work, because it knows both cells were saved by the defun.

                       (let ((def (cons 'defun def))
                             (*1*def (cons 'defun
                                           (oneify-cltl-code
                                            defun-mode
                                            def

; The if below returns the stobj name being introduced, if any.

                                            (if (consp ignorep)
                                                (cdr ignorep)
                                              nil)
                                            (w *the-live-state*)))))
                         (or ignorep
                             (progn (eval (make-defun-declare-form (cadr def)
                                                                   def))
                                    (setq new-defs
                                          (cons def new-defs))))
                         #-acl2-mv-as-values
                         (eval (make-defun-declare-form (cadr def) *1*def))
                         (setq new-defs
                               (cons *1*def new-defs))))))
            (dolist (def new-defs)

; Remove the documentation string potentially stored in raw Lisp, if a copy is
; already around in our documentation database, just to save space.

              (cond ((doc-stringp (documentation (car def) 'function))
                     (setf (documentation (car def) 'function) nil)))
              (eval def))
            (cond ((or

; It seems critical to compile as we go in CMUCL 18e during the boot-strap, in
; order to avoid stack overflows.  This seems to cut about 20% off the
; regression time for Allegro builds, so we go ahead and do this in all Lisps.
; See also the long comment for the case (eq fns :some) in
; compile-uncompiled-*1*-defuns.  It is tempting to avoid this on-the-fly
; compilation for GCL, where we have seen build time shrink from over 22 to
; under 7 minutes and have seen roughly a percent or slightly less degradation
; in regression time, probably because of the lack of compilation in that case
; of *1* functions for built-in :program mode functions.  But we have decided,
; at least for now, to keep the code simple by doing the same thing for all
; lisps and be happy with even that small improvement in regression time for
; GCL.  (Note that by using make with
;   LISP='gcl -eval "(defparameter user::*fast-acl2-gcl-build* t)"'
; one can get a faster build, without this on-the-fly compilation, with very
; little performance penalty at runtime.  Something like this could be done
; with any Common Lisp, but there is only a point in GCL; see above.)

                    (and #+gcl (not user::*fast-acl2-gcl-build*)
                         boot-strap-flg) ; delete for build speedup (see above)
                    (and
                     (not (eq (f-get-global 'ld-skip-proofsp
                                            *the-live-state*)
                              'include-book))
                     (default-compile-fns
                       (w *the-live-state*))))
                   (dolist (def new-defs)
                     (eval `(compile ',(cadr def))))))))
        (defstobj

; (cddr trip) is of the form 

; (DEFSTOBJ name the-live-name init raw-defs template axiomatic-defs).

; Init is a form to eval to obtain the initial setting for the live variable.
; Each def in raw-defs and in axiomatic-defs is of the form (name args dcl
; body), where dcl may be omitted.  We defun each raw-def and the oneification
; of each axiomatic-def.

          (let ((name (nth 1 (cddr trip)))
                (the-live-name (nth 2 (cddr trip)))
                (init (nth 3 (cddr trip)))
                (raw-defs (nth 4 (cddr trip)))
                (template (nth 5 (cddr trip)))
                (ax-defs (nth 6 (cddr trip)))
                (new-defs

; We avoid "undefined function" warnings by Allegro during compilation by
; defining all the functions first, and compiling them only after they have all
; been defined.  But we go further; see the comment in the binding of new-defs
; in the previous case.

                 nil))
            (maybe-push-undo-stack 'defconst the-live-name)
            (maybe-push-undo-stack 'defconst '*user-stobj-alist*)

; See the comment below, just above where we formerly set the symbol-value of
; name.  If we re-install that code, then the next line of code also needs to
; be re-installed.

;               (maybe-push-undo-stack 'defconst name)
            (eval `(defparameter ,the-live-name ,init))

; As with defconst we want to make it look like we eval'd this defstobj
; in raw lisp, so we set up the redundancy stuff:

            (setf (get the-live-name 'redundant-raw-lisp-discriminator)
                  (list* 'defstobj (car template) (cadr template)
                         (caddr template)))

; At one point we executed the following form.  But now we see that this is not
; necessary, since trans-eval binds stobj names anyhow using *user-stobj-alist*
; and even acl2-raw-eval uses *user-stobj-alist* to bind stobj names.  If this
; code is re-installed, then also re-install the code (maybe-push-undo-stack
; 'defconst name) above.
;               (setf (symbol-value name) (symbol-value the-live-name))

; The following assignment to *user-stobj-alist* is structured to keep
; new ones at the front, so we can more often exploit the optimization
; in put-assoc-eq-alist.

            (setq *user-stobj-alist*
                  (cond ((assoc-eq name *user-stobj-alist*)

; This is a redefinition!  We'll just replace the old entry.

                         (put-assoc-eq name
                                       (symbol-value the-live-name)
                                       *user-stobj-alist*))
                        (t (cons (cons name (symbol-value the-live-name))
                                 *user-stobj-alist*))))

; We eval and compile the raw lisp definitions first, some of which may be
; macros (because :inline t was supplied), before dealing with the *1*
; functions.

            (dolist
              (def raw-defs)
              (cond ((and boot-strap-flg
                          (not (global-val 'boot-strap-pass-2
                                           (w *the-live-state*))))

; During the first pass of initialization, we insist that every function
; defined already be defined in raw lisp.  During pass two we can't expect this
; because there may be LOCAL defuns that got skipped during compilation and the
; first pass.

                     (or (fboundp (car def))
                         (interface-er "~x0 is not fboundp!"
                                       (car def))))
                    ((equal *main-lisp-package-name*
                            (symbol-package-name (car def)))
                     (interface-er
                      "It is illegal to redefine a function in ~
                           the main Lisp package, such as ~x0!"
                      (car def)))

; We don't do maybe-push-undo-stack for defuns (whether inlined or not) under
; the defstobj CLTL-COMMAND, because we did it for their defuns.

                    (t
                     (let ((def (if (member-equal *stobj-inline-declare* def)

; We now handle the case where we are going to inline the function calls by
; defining the function as a defabbrev.  Note that this is allowed for
; access/update/array-length functions for stobjs, but only for these, where
; speed is often a requirement for efficiency.

                                    (cons 'defabbrev
                                          (remove-stobj-inline-declare def))
                                  (cons 'defun def))))
                       #-acl2-mv-as-values
                       (eval (make-defun-declare-form (cadr def) def))
                       (setq new-defs (cons def new-defs))))))
            (dolist
              (def ax-defs)
              (let ((*1*def (cons 'defun
                                  (oneify-cltl-code
                                   :logic
                                   def
                                   name
                                   (w *the-live-state*)))))
                #-acl2-mv-as-values
                (eval (make-defun-declare-form (cadr def) *1*def))
                (setq new-defs (cons *1*def new-defs))))
            (dolist (def

; We reverse new-defs because we want to be sure to define the *1*
; defs after the raw Lisp defs (which may be macros, because of :inline).

                     (reverse new-defs))
              (eval def))
            (cond ((and (not (eq (f-get-global 'ld-skip-proofsp
                                               *the-live-state*)
                                 'include-book))
                        (default-compile-fns (w *the-live-state*)))
                   (dolist (def new-defs)

; CMUCL 18e cannot seem to compile macros at the top level.  Email from Raymond
; Toy on June 9, 2004 suggests that this appears to be a bug that exists in
; CMUCL 18e sources.

                     #+cmu (cond ((not (eq (car def) 'defabbrev))
                                  (eval `(compile ',(cadr def)))))
                     #-cmu (eval `(compile ',(cadr def))))))))
        (defpkg
          (maybe-push-undo-stack 'defpkg (cadr (cddr trip)))
          (eval (cons 'defpkg (cdr (cddr trip)))))
        (defconst

; Historical remark on defconstant.

; In the beginning we supported defconstant.  We changed to
; defparameter and then changed to defconst.  As things stand now,
; ACL2 supports defconst, which has the same effect at the raw lisp
; level (i.e., the cltl-command) as defparameter, and in addition
; causes proclaim-file to exectute an appropriate proclamation for the
; parameter, knowing as we do that it is really constant.  Here are
; some historical remarks that explain why we have gone down this
; path.

; "Currently we turn defconstants into defparameters at the raw Lisp
; level (that is, the cltl-command for defconstant is a defparameter).
; However, we have begun to contemplate alternatives, as we now
; explain.

; We have run into the following problem with defconstant:  the
; compiler won't let us compile certified books containing defconstant
; forms because it thinks that constants are special variables
; (because that is what the defparameter cltl-command does).  What can
; we do about this problem?  One idea was to temporarily redefine
; defconstant to be defparameter (during the compilation done by
; certify-book), but macrolet has only lexical scope, and anyhow Boyer
; says that it's illegal to redefine a Common Lisp function (as we did
; using setf, macro-function, and unwind-protect).

; Another possibilty is to change defconstant-fn so that it really
; does create defconstants.  But the reason we use defparameter now is
; that when we undo we need to unbind (because we're always checking
; to see if something is already bound), and we can't unbind a
; constant.

; Why not just eliminate defconstant in favor of defparameter
; everywhere?  This is very appealing, especially because defconstant
; is inherently not amenable to undoing.  But, Boyer thinks that when
; you defconstant something to a value that is a fixnum, then the
; compiler knows it's a fixnum.  This could be very important for
; speed in type-set reasoning.  Without the consideration of
; arithmetic, Schelter thinks that we're only paying the price of two
; memory references for defparameter vs. one for defconstant; but a
; factor of 80 or so seems like too high a price to pay.

; So, how about allowing both defconstant and defparameter, but not
; allowing any undoing back past a defconstant?  After all, we already
; have a notion of not undoing into the system initialization, so
; we're just talking about a very reasonable extension of that
; protocol.  One problem with this approach is that certify-book
; currently does an include-book after a ubt, and this ubt would
; probably fail.  But perhaps we can force this to work.  The user
; could then develop his work using defparameter, but certify the
; final "toothbrush" book using defconstant.  Perhaps defconst would
; be a convenient macro that could be redefined so as to be one or the
; other of defparameter or defconstant.  With this approach it would
; probably be useful to require answering a query in order to execute
; a defconstant.

; Another option would be to have acl2::defconstant be distinct from
; lisp::defconstant, but as Boyer points out, this violates our desire
; to have such Lisp primitives available to the user that he can count
; on.  Or, we could define a new package that's just like the acl2
; package but doesn't import defconstant.  But note that
; (symbol-package 'defconstant) would create different answers in the
; ACL2 package than in this package -- ouch!"

; Note: (cddr trip) here is (defconst var form val).

          (cond (boot-strap-flg
                 (or (boundp (cadr (cddr trip)))
                     (interface-er "~x0 is not boundp!"
                                   (cadr (cddr trip)))))
                ((equal *main-lisp-package-name*
                        (symbol-package-name (cadr (cddr trip))))
                 (interface-er "It is illegal to redefine a defconst in ~
                                    the main Lisp package, such as ~x0!"
                               (cadr (cddr trip))))
                (t (maybe-push-undo-stack 'defconst (cadr (cddr trip)))

; We do not want to eval (defconst var form) here because that will recompute
; val.  But we make raw Lisp look like it did that.

                   (setf (get (cadr (cddr trip))
                              'redundant-raw-lisp-discriminator)
                         (list* 'defconst
                                (caddr (cddr trip)) ; form
                                (cadddr (cddr trip)))) ; val
                   (eval `(defparameter ,(cadr (cddr trip))
                            ',(cadddr (cddr trip))))))
          (cond ((doc-stringp (documentation (cadr (cddr trip)) 'variable))
                 (setf (documentation (cadr (cddr trip)) 'variable)
                       nil))))
        (defmacro
          (cond (boot-strap-flg
                 (or (fboundp (cadr (cddr trip)))
                     (interface-er "~x0 is not fboundp!"
                                   (cadr (cddr trip)))))
                ((equal *main-lisp-package-name*
                        (symbol-package-name (cadr (cddr trip))))
                 (interface-er "It is illegal to redefine a macro in the ~
                                    main Lisp package, such as ~x0!"
                               (cadr (cddr trip))))
                (t (maybe-push-undo-stack 'defmacro (cadr (cddr trip)))
                   (eval (cddr trip))))
          (cond ((doc-stringp (documentation (cadr (cddr trip)) 'function))
                 (setf (documentation (cadr (cddr trip)) 'function)
                       nil))))))))

; Finally, we make sure always to leave the *current-acl2-world-key* as the
; first property on the symbol-plist of the symbol.
                                                
  (let ((temp (get (car trip) *current-acl2-world-key*))
        (plist (symbol-plist (car trip))))
    (cond ((and temp (not (eq (car plist) *current-acl2-world-key*)))
           (setf (symbol-plist (car trip))
                 (cons *current-acl2-world-key*
                       (cons temp
                             (remove-current-acl2-world-key
                              plist))))))))

(defun-one-output undo-trip (world-name world-key trip)

; Undo-trip is the function that removes from the ``real'' Common Lisp
; the things installed by add-trip.  It works merely by popping the
; appropriate stacks.

  (setf (get (car trip) world-key)
        (destructive-pop-assoc (cadr trip) (get (car trip) world-key)))
  (cond
   ((and (eq world-name 'current-acl2-world)
         (eq (car trip) 'cltl-command)
         (eq (cadr trip) 'global-value)
         (consp (cddr trip)))
    (case (car (cddr trip))
          (defuns

; Note that :inlined stobj functions are processed by eval-event-lst
; as though they are ordinary defuns.  We are relying on the fact that
; maybe-push-undo-stack handled defun and defmacro the same, so that
; the form eveluated by maybe-pop-undo-stack will be appropriate even
; though the "function" is actually a macro (defined by defabbrev).

            (dolist (tuple (cdddr (cddr trip)))
                    (maybe-pop-undo-stack (car tuple))))
          (defstobj
            (let ((name (nth 1 (cddr trip)))
                  (the-live-name (nth 2 (cddr trip))))
              (maybe-pop-undo-stack name)
              (maybe-pop-undo-stack '*user-stobj-alist*)
              (maybe-pop-undo-stack the-live-name)))
          (defpkg nil)
          (defconst
            (maybe-pop-undo-stack (cadr (cddr trip))))
          (defmacro
            (maybe-pop-undo-stack (cadr (cddr trip))))
          (otherwise nil)))))

(defun-one-output flush-trip (name world-key trip)
  (remprop (car trip) world-key)
  (cond ((and (eq name 'current-acl2-world)
              (eq (car trip) 'cltl-command)
              (eq (cadr trip) 'global-value)
              (consp (cddr trip)))
         (case (car (cddr trip))
          (defuns

; Note that :inlined stobj functions are handled properly here; see the
; corresponding comment in undo-trip.

            (dolist (tuple (cdddr (cddr trip)))
                    (flush-undo-stack (car tuple))))
          (defstobj
            (let ((name (nth 1 (cddr trip)))
                  (the-live-name (nth 2 (cddr trip))))
              (flush-undo-stack name)
              (flush-undo-stack '*user-stobj-alist*)
              (flush-undo-stack the-live-name)))
          (defpkg nil)
          (defconst
            (flush-undo-stack (cadr (cddr trip))))
          (defmacro
            (flush-undo-stack (cadr (cddr trip))))
          (otherwise nil)))))

(defvar *bad-wrld*)

(defun check-acl2-world-invariant (wrld old-wrld)

; Old-wrld is the world currently installed under 'current-acl2-world.
; Wrld is a world we are trying to install there.  We check that
; old-world is in fact the current global value of 'current-acl2-
; world.  We have gotten out of sync on this once or twice.  It is
; cheap to check and pernicious to track down.

  (cond ((not (eq old-wrld
                  (w *the-live-state*)))
         (setq *bad-wrld* wrld)
         (interface-er
          "Extend-world1 or rollback-world1 has been asked to install ~
           a world at a moment when the current global value of ~
           'current-acl2-world was not the installed world!  The ~
           world we were asked to install may be found in the variable ~
           *bad-wrld*."))))

(defparameter *known-worlds* nil)

(defun-one-output extend-world1 (name wrld)

; Wrld must be a world that is an extension of the world currently
; installed under name.

; Warning: Even though this program does not take state as an
; argument, it has the effect of smashing the value of the live state
; global 'current-acl2-world if name is 'current-acl2-world.  In
; particular, we maintain the invariant that the live global value of
; 'current-acl2-world is always the world installed under that name.
; If you don't want these changes to occur to your state, don't call
; this program!

  (let ((pair (get name 'acl2-world-pair)) old-wrld world-key new-trips)
    (cond
     ((null pair)
      (setq pair (cons nil (if (eq name 'current-acl2-world)
                               *current-acl2-world-key*
                               (gensym))))
      (pushnew name *known-worlds*)
      (cond ((eq name 'current-acl2-world)
             (f-put-global 'current-acl2-world nil *the-live-state*)))
      (setf (get name 'acl2-world-pair) pair)))
    (setq old-wrld (car pair))
    (setq world-key (cdr pair))

; Pair is of the form (old-wrld . world-key) and means that the world
; currently installed under name is old-wrld and its properties are
; stored at world-key.

    (cond ((eq name 'current-acl2-world)
           (check-acl2-world-invariant wrld old-wrld)))

; We now scan down the about-to-be-installed world and push onto the
; temporary new-trips the triples that constitute the extension.  If
; we fail to find the old world, we will cause a hard error.  It may look
; like we are doing this scan to guarantee that wrld is an extension.
; Were that the reason, we would do this as we installed the properties.
; No, the real reason we do this scan is so that we can collect, in reverse
; order, the triples we must install.  The order in which we push the
; values into the property lists is important!

    (do ((tl wrld (cdr tl)))
        ((eq tl old-wrld))
        (cond
         ((null tl)
          (setq *bad-wrld* wrld)
          (er hard 'extend-world1
              "Extend-world1 was called upon to ``extend'' ~x0.  But ~
               the world supplied to extend-world1, which is now the ~
               value of the Lisp global *bad-wrld*, is not an ~
               extension of the current ~x0.  The alist corresponding ~
               to the current ~x0 may be obtained via ~x1.  No ~
               properties were modified -- that is, the symbol-plists ~
               still reflect the pre-extend-world1 ~x0."
              name
              `(car (get ',name 'acl2-world-pair))))
         (t (push (car tl) new-trips))))
    (let ((state *the-live-state*))

; We bind state only so our use of acl2-unwind-protect below isn't so odd
; looking.  Logically the body never signals an error, but if an abort
; occurs, we will do recover-world for cleanup.

      (acl2-unwind-protect
       "extend-world1"
       (value
        (#-openmcl
         progn
         #+openmcl
         let #+openmcl ((ccl::*suppress-compiler-warnings* t))

; Observe that wrld has recover-world properties (a) and (b).  (a) at
; the time of any abort during this critical section, every symbol
; that may have a world-key property is in wrld (because the only
; symbols with a world-key property initially are those in old-wrld,
; wrld is an extension of old-wrld, and during the critical section we
; add world-key properties just to symbols in wrld); and (b): every
; symbol in old-wrld is a symbol in wrld (because wrld is an extension
; of old-wrld).  (Of course, by "symbol in" here we mean "symbol
; occuring as the car of an element".)

          (dolist (trip new-trips)
            (add-trip name world-key trip))
          (setf (car pair) wrld)
          (cond ((eq name 'current-acl2-world)
                 (f-put-global 'current-acl2-world wrld *the-live-state*)
                 (install-global-enabled-structure wrld state)
                 (recompress-global-enabled-structure
                  'global-arithmetic-enabled-structure
                  wrld)
                 (recompress-stobj-accessor-arrays
                  (strip-cars *user-stobj-alist*)
                  wrld)))))
       (recover-world 'extension name old-wrld wrld nil)

; Observe that wrld has recover-world properties (a) and (b).  (a) at
; the time of any abort during this critical section, every symbol
; that may have a world-key property is in wrld (because the only
; symbols with a world-key property initially are those in old-wrld,
; wrld is an extension of old-wrld, and during the critical section we
; add world-key properties just to symbols in wrld); and (b): every
; symbol in old-wrld is a symbol in wrld (because wrld is an extension
; of old-wrld).  (Of course, by "symbol in" here we mean "symbol
; occuring as the car of an element".)

       state)

; The acl2-unwind-protect returns (mv nil x *the-live-state*), for some x.
; All three values are ignored.

      wrld)))

(defun-one-output retract-world1 (name wrld)

; Wrld must be a world that is a retraction of the world currently installed
; under name.

; Warning: Even though this program does not take state as an argument, it has
; the effect of smashing the value of the live state global 'current-acl2-world
; if name is 'current-acl2-world.  In particular, we maintain the invariant
; that the live global value of 'current-acl2-world is always the world
; installed under that name.  We also maintain the invariant that the binding
; of 'current-package is a known package, by setting 'current-package to "ACL2"
; if we have to.  If you don't want these changes to occur to your state, don't
; call this program!

  (let ((pair (get name 'acl2-world-pair)) old-wrld world-key)
    (cond
     ((null pair)
      (setq pair (cons nil (if (eq name 'current-acl2-world)
                               *current-acl2-world-key*
                               (gensym))))
      (pushnew name *known-worlds*)
      (cond ((eq name 'current-acl2-world)
             (f-put-global 'current-acl2-world nil *the-live-state*)))
      (setf (get name 'acl2-world-pair) pair)))
    (setq old-wrld (car pair))
    (setq world-key (cdr pair))

; Pair is of the form (old-wrld . world-key) and means that the world currently
; installed under name is old-wrld and its properties are stored at world-key.

    (cond ((eq name 'current-acl2-world)
           (check-acl2-world-invariant wrld old-wrld)))
    (let ((state *the-live-state*)
          (pkg (current-package *the-live-state*)))

      (acl2-unwind-protect
       "retract-world1"
       (value
        (progn

; We now scan down old-wrld until we get to wrld, doing a pop for each triple
; in the initial segment of old-wrld.  Note that we do not do the pops in the
; reverse order (as we did the pushes).  It doesn't matter.  All that matters
; is that we one pop for each push that was done.

          (do ((tl old-wrld (cdr tl)))
              ((eq tl wrld))
              (cond
               ((null tl)
                (setq *bad-wrld* wrld)
                (er hard 'retract-world1
                    "Retract-world1 was called upon to ``retract'' ~
                    ~x0.  But the world supplied to retract-world1, ~
                    which is now the value of the Lisp global ~
                    variable *bad-wrld*, is not a retraction of the ~
                    currently installed ~x0.  The alist corresponding ~
                    to the current ~x0 may be obtained via ~x1.  ~
                    Unfortunately, this problem was not discovered ~
                    until all of the properties in ~x0 were removed.  ~
                    Those properties can be restored via ~
                    (recover-world)."
                    name
                    `(car (get ',name 'acl2-world-pair))))
               (t (undo-trip name world-key (car tl)))))
          (setf (car pair) wrld)
          (cond ((eq name 'current-acl2-world)
                 (f-put-global 'current-acl2-world wrld *the-live-state*)
                 (cond ((not (find-non-hidden-package-entry
                              (current-package *the-live-state*)
                              (known-package-alist *the-live-state*)))

; Note: Known-package-alist returns the new known packages because of the setf
; above!

                        (f-put-global 'current-package "ACL2" *the-live-state*)))
                 (install-global-enabled-structure wrld state)
                 (recompress-global-enabled-structure
                  'global-arithmetic-enabled-structure
                  wrld)
                 (recompress-stobj-accessor-arrays
                  (strip-cars *user-stobj-alist*)
                  wrld)))))
       (recover-world 'retraction name old-wrld old-wrld pkg)

; Note that old-wrld has recover-world properties (a) and (b).  (a) At the time
; of any abort during this critical section, every symbol that may possibly
; have the world-key property occurs as a car of some element of old-wrld (at
; the beginning of this operation the only symbols with the world-key property
; are in old-wrld and during the critical section we may only remove the
; property). (b) Every symbol in old-wrld is in old-wrld.  Note that it is not
; important whether wrld is actually a retraction, because we are just removing
; world-key properties.

       state)
      wrld)))

(defun-one-output recover-world1 (wrld ans)

; This is like (revappend wrld ans) except that we cons successive
; tails of wrld onto ans instead of successive elements.  Thus, if you
; start with wrld = '(trip3 trip2 trip1) you end up with ans =
; '((trip1) (trip2 trip1) (trip3 trip2 trip1)).  By scanning ans you
; will see the successive cdrs of world, starting with the shortest.

  (cond ((null wrld) ans)
        (t (recover-world1 (cdr wrld) (cons wrld ans)))))

(defun-one-output recover-world (op name old-wrld universe pkg)

; If this function is called then an extension or retraction operation (op) was
; aborted.  The arguments tell us how to restore the world to the configuration
; it had when the aborted operation was initiated.  Toward that end, the two
; operations execute this form on the cleanup1 side of acl2-unwind-protects,
; after saving in locals the name and world before they start making changes to
; the symbol- plists.  Our recovery mechanism has two steps.  First, we throw
; away all properties stored under the name in question.  Second, we extend
; that empty world to the saved one.  The extension part is easy.

; But how do we "throw away all properties" stored under a given name?  One way
; is to get the world key, world-key, associated with the name, and then visit
; each CLTL symbol, sym, and do (remprop sym world-key).  But sweeping through
; all symbols seems to be slow.  So instead we enforce the discipline of having
; a "universal list" of all the symbols which might, remotely, have the
; world-key property.  This list is actually a list of triples (it is, in fact,
; always one of the two worlds involved in the aborted operation) and we visit
; the symbol in each car of each triple.

; So much for spoon-fed background information.  Name is the name of the world
; whose installation was aborted.  We know that the cdr of the 'acl2-world-pair
; property of name is some uninterned symbol, world-key, at which all the
; properties for this world are stored.  Old-wrld is the alist of triples that
; was actually installed at the time the aborted operation was initiated -- and
; is thus the world we are going to re-install.  And universe is a list of
; pairs (triples actually) with the following two properties:

; (a) at the time of any abort during the critical section, each symbol having
; the world-key property occurs as the car of some pair in universe, and (b)
; each symbol occuring as the car of some pair in old-wrld occurs as the car of
; a pair in universe.

; The latter property is necessary to ensure that we can recover from an
; aborted attempt to recover.

; In addition, if operation is 'retraction, then pkg is the current- package at
; the inception of the operation.  If operation is not 'retraction, pkg is
; irrelevant.

  (let* ((pair (get name 'acl2-world-pair))
         (world-key (cdr pair))
         (*in-recover-world-flg* t))

; The *in-recover-world-flg* is used by the raw lisp implementation of defpkg.
; How is defpkg called from within this function?  Add-trip, above, is the
; program used to add the triples in old-wrld to the raw property lists.  It
; notes CLTL-COMMAND triples and executes the appropriate Common Lisp to cause
; the raw defuns, defmacros, constants, and packages to come into existence.
; So we execute defpkg when we store a CLTL-COMMAND property with val (DEFPKG
; ...).  Normally, defpkg unbinds all the symbols in the dual package (where
; state globals are actually stored).  But we can't afford to do that if we
; are recovering the world (because state globals are unaffected by world
; operations).

    (cond ((null pair)
           (er hard 'recover-world
               "There is no acl2-world-pair stored on ~x0, which is ~
                the name of the world we are supposed to recover."
               name))
          (t (fmt1 "Flushing current installed world.~%"
                   nil 0 (standard-co *the-live-state*) *the-live-state*
                   nil)
             (dolist (trip universe) (flush-trip name world-key trip))
             (cond ((eq name 'current-acl2-world)
                    (f-put-global 'current-acl2-world nil
                                  *the-live-state*)))
             (setf (car pair) nil)               
             (fmt1 "Reversing the new world.~%" nil 0
                   (standard-co *the-live-state*) *the-live-state* nil)
             (let ((rwtls (recover-world1 old-wrld nil))
                   #+openmcl
                   (ccl::*suppress-compiler-warnings* t))
               (fmt1 "Installing the new world.~%" nil 0
                     (standard-co *the-live-state*) *the-live-state* nil)
               (do ((tl rwtls (cdr tl)))
                   ((null tl))
                   (add-trip name world-key (caar tl))
                   (cond ((eq name 'current-acl2-world)
                          (f-put-global 'current-acl2-world (car tl)
                                        *the-live-state*)))
                   (setf (car pair) (car tl))))
             (cond ((eq name 'current-acl2-world)
                    (cond ((eq op 'retraction)
                           (f-put-global 'current-package pkg
                                         *the-live-state*)))
                    (install-global-enabled-structure old-wrld
                                                       *the-live-state*)
                    (recompress-global-enabled-structure
                     'global-arithmetic-enabled-structure
                     old-wrld)
                    (recompress-stobj-accessor-arrays
                     (strip-cars *user-stobj-alist*)
                     old-wrld)))))))

;                              VIRGINITY

; We do not want the ACL2 user to define any symbol that already has
; any sort of definition.

(defmacro special-form-or-op-p (name)

; This odd macro deals with the problem that the notion of special operator
; is defined differently in different lisps.

; Even though cmu lisp purports to be CLTL2, in fact it does not
; contain a defun for special-form-p and does contain a defun of
; special-operator-p.

; SBCL makes no claim to being CLTL2.  Unfortunately acl2.lisp helpfully
; pushes :CLTL2 to *FEATURES* anyway (for all :ANSI-CL implementations).

  #+(and (not DRAFT-ANSI-CL-2) CLTL2
         (not cmu) (not sbcl) (not clisp) (not openmcl))
  `(common-lisp::special-form-p ,name)
  #+openmcl
  `(common-lisp::special-operator-p ,name)
  #+(and (not DRAFT-ANSI-CL-2) (not CLTL2)
         (not cmu) (not sbcl) (not clisp) (not openmcl))
  `(lisp::special-form-p ,name)
  #+(or (and DRAFT-ANSI-CL-2 (not openmcl)) cmu sbcl clisp)
  `(special-operator-p ,name))

(defun-one-output virginp (name new-type)
  (and (symbolp name)
       (if (member-eq new-type '(function macro constrained-function t))
           (not (or (and (fboundp name)
                         #+cmu
                         (or (eq name 'unbound-cmu-macro-function)
                             (not (eq (macro-function name)
                                      *unbound-cmu-macro-function*))))
                    (and (macro-function name)
                         #+cmu
                         (not (eq (macro-function name)
                                  *unbound-cmu-macro-function*)))
                    (special-form-or-op-p name)))
         t)
       (if (member-eq new-type '(const stobj stobj-live-var t))
           (not (boundp name))
         t)))

(defun-one-output chk-virgin2 (name new-type wrld)
  (cond ((virginp name new-type) nil)
        ((global-val 'boot-strap-flg wrld)
         (setf (get name '*predefined*) t))

; A name regains its true virginity the moment we decide to give it a
; 'redefined property, which only happens just after the user has said that
; it's OK to redefine it.

        ((getprop name 'redefined nil 'current-acl2-world wrld)
         nil)
        (t
         (let ((reason
                (cond ((not (symbolp name)) "it is not a symbol")
                      ((member-eq new-type
                                  '(function macro constrained-function t))
                       (cond
                        ((special-form-or-op-p name) "it is a special form")
                        ((macro-function name) "it has a macro definition")
                        ((fboundp name) "it has a function definition")
                        (t "there is an inconsistency in the definition of ~
                            virginp and chk-virgin2")))
                      ((member-eq new-type '(const stobj stobj-live-var t))
                       (cond
                        ((boundp name) "it has a top level binding")
                        (t "there is an inconsistency in the definition of ~
                            virginp and chk-virgin2")))
                      (t "there is an inconsistency in the definition of ~
                          virginp and chk-virgin2")))
               (suggestion
                (let ((str "  If earlier you accidentally made this ~
                            definition or assignment outside the ACL2 ~
                            loop, then you might consider exiting the ~
                            ACL2 loop and executing appropriate ~
                            Common Lisp to erase those mistakes.  ~
                            This is a potentially perilous act unless ~
                            you are sure these names were introduced ~
                            by you and are not involved in any ~
                            logical code. ~#3~[~/To erase a function ~
                            or macro definition use (fmakunbound! ~
                            '~x0).  ~]~#4~[~/To erase a variable ~
                            setting use (makunbound '~x0). ~]"))
                  (cond ((not (symbolp name)) "")
                        ((member-eq new-type
                                    '(function macro constrained-function t))
                         (cond
                          ((special-form-or-op-p name) "")
                          (t str)))
                        (t ; (member-eq new-type '(const stobj
;                       stobj-live-var t))
                         str)))))
           (interface-er
            "It is illegal to define ~x0 because ~@1 in raw Common Lisp.~@2"
            name
            reason
            suggestion
            (if (member-eq new-type
                           '(function macro constrained-function t))
                1
              0)
            (if (member-eq new-type '(const stobj stobj-live-var t))
                1
              0))))))


;                           PACKAGE VIRGINITY

(defun-one-output chk-package-reincarnation-import-restrictions2 (name proposed-imports)

; This routine returns nil or causes a hard error.  It is used in the
; implementation of the logical function chk-package-reincarnation-import-
; restrictions, which axiomatically always returns t but may cause this hard
; error (which can be thought of as a resource error).

; Note: The "2" in the name stems from chk-virgin2, which plays a similar role
; in chk-virgin.  Chk-virgin1 has been lost in the mist of time, but
; chk-package-reincarnation-import-restrictions1 has never existed!

  (let ((pkg (find-package name))
        (temp (find-package-entry name *ever-known-package-alist*)))
    (cond
     (pkg
      (cond
       (temp
        (cond
         ((equal proposed-imports (package-entry-imports temp))

; The package has already been built in Common Lisp and the imports are
; identical.  This is ok.

          nil)
         (t

; The package has already been built in Common Lisp but with the wrong imports.
; There is nothing we can do.

          (interface-er
           "We cannot reincarnate the package ~x0 with imports ~x1 because it ~
            was previously defined with imports ~x2.  See :DOC ~
            package-reincarnation-import-restrictions."
           name
           proposed-imports
           (package-entry-imports temp)))))
       (t

; The package has been built in this Common Lisp but not by defpkg.

        (interface-er
         "It is illegal to defpkg ~x0 because a package of that name already ~
          exists in this lisp."
         name))))
     (t

; The package has never been built in this Common Lisp.

      nil))))

;                     ACL2 INITIALIZATION ROUTINES

#+gcl
(defvar user::*acl2-keep-tmp-files* nil)

(defun-one-output enter-boot-strap-mode (distributed-books-dir operating-system)

; If we interrupted an earlier initialization, the following form will undo it.
; This will set the *acl2-unwind-protect-stack* to nil because *ld-level* is
; 0 at the top.

  (acl2-unwind *ld-level* nil)

; Now provide a frame into which the set-w can push its acl2-unwind-protect
; forms.  An abort of the set-w will leave the forms in the frame so that the
; above acl2-unwind will undo them upon the next initialization attempt.  But
; if the set-w is successful, it will leave the then empty frame on the stack.

  (push nil *acl2-unwind-protect-stack*)
  (set-w 'retraction nil *the-live-state*)
  (do-symbols (sym (find-package "ACL2_GLOBAL_ACL2"))
              (makunbound sym))
  (do-symbols (sym (find-package
                    (concatenate 'string
                                 *global-package-prefix*
                                 *main-lisp-package-name*)))
              (makunbound sym))
  (do-symbols (sym (find-package "ACL2_GLOBAL_KEYWORD"))
              (makunbound sym))
  (dolist (pair *initial-global-table*)
          (f-put-global (car pair) (cdr pair) *the-live-state*))

; The following patch for save-gprof.lsp may be more heavy-handed than
; necessary, because maybe we don't need to keep all TMP* files.  See also
; "Note: temporary files" in save-gprof.lsp.

; If we want to let the user set other variables, we could replace
; user::*acl2-keep-tmp-files* with a variable whose value associates state
; global symbol names with initial values.  But then we will need to check that
; none is untouchable, and we will need to make a corresponding change in
; save-gprof.lsp.

  #+gcl
  (f-put-global 'keep-tmp-files user::*acl2-keep-tmp-files* *the-live-state*)
  (let ((distributed-books-dir
         (cond (distributed-books-dir
                (let ((dir (cond
                            ((symbolp distributed-books-dir)
                             (symbol-name distributed-books-dir))
                            ((stringp distributed-books-dir)
                             distributed-books-dir)
                            (t (er hard 'initialize-acl2
                                   "Unable to complete initialization,because ~
                                    the supplied distributed books directory, ~
                                    ~x0, is not a string."
                                   distributed-books-dir)))))
                  (if (and (< 0 (length dir))
                           (eql (char dir (1- (length dir))) #\/))
                      dir
                    (concatenate 'string dir "/"))))
               (t (concatenate 'string
                               (namestring (truename ""))
                               "books/")))))
    (f-put-global 'distributed-books-dir
                  distributed-books-dir
                  *the-live-state*))
  (f-put-global 'global-enabled-structure
                (initial-global-enabled-structure "ENABLED-ARRAY-")
                *the-live-state*)
  (f-put-ld-specials *initial-ld-special-bindings* *the-live-state*)

; The next set-w will avail itself of the empty frame left above.

  (set-w 'extension
         (primordial-world operating-system)
         *the-live-state*)

; Inhibit proof-tree output during the build, including pass-2 if present.

  (f-put-global 'inhibit-output-lst '(proof-tree) *the-live-state*)

; We now pop the empty frame.  There is no way this acl2-unwind will do any
; real work because only an abort would leave stuff in the frame and an abort
; would prevent us from getting here.  Note that in the scheme of things,
; namely within the control structure of initialize-acl2, it is strictly
; unnecessary for us to pop this empty frame: each LD called by initialize-acl2
; will unwind the stack back to nil.  But I do it here out of politeness: the
; stack started empty and ends that way.

  (acl2-unwind *ld-level* nil))

(defun-one-output move-current-acl2-world-key-to-front (wrld)

; This program sweeps the world and makes sure that the current acl2 world key
; is the first property on every property list upon which it is found.  We try
; to maintain that invariant in set-w where we always move the property up when
; we mess with a symbol's plist.  Of course, one must then wonder why this
; program is ever useful.  The reason is that in some lisps, e.g., AKCL, when
; you ask for the symbol-name of a symbol it has the side-effect of storing the
; string on the plist for future use.  Thus, for example, during booting of
; ACL2 we keep the world key at the front but then when we print the name of
; the event we accidentally cover the world key up with a SYSTEM:PNAME
; property.  This doesn't happen for every name.  Apparently for most we access
; the symbol-name during the processing and cause the property to come into
; existence and then store our world key in front of it.  But for some names we
; don't, apparently, ever access the symbol-name during processing and then our
; world key is covered up the first time the name is printed by ACL2.  Among
; the names so covered up by system properties are the names such as |Make
; RECOGNIZER-TUPLE record| INCLUDE-BOOK-ALIST, CERTIFICATION-TUPLE,
; TYPE-SET-INVERTER-RULES, PROVED-FUNCTIONAL-INSTANCES-ALIST, GENERALIZE-RULES,
; WELL-FOUNDED-RELATION-ALIST, WORLD-GLOBALS, and CHK-NEW-NAME-LST.  It is my
; theory that these names are simply never set a second time in booting and
; their initial setting is made before they are first printed.

; In any case, the following sweep takes only a second or two at the end of a
; boot and will make our world key the first property.  We hope to keep it that
; way in add-trip, but we cannot guarantee it, since the Lisp system might
; legally at anytime cover it up with some system property.

  (cond ((null wrld) nil)
        ((eq (car (symbol-plist (caar wrld))) *current-acl2-world-key*)
         (move-current-acl2-world-key-to-front (cdr wrld)))
        (t (let ((temp (get (caar wrld) *current-acl2-world-key*)))
             (cond (temp
                    (setf (symbol-plist (caar wrld))
                          (cons *current-acl2-world-key*
                                (cons temp
                                      (remove-current-acl2-world-key
                                       (symbol-plist (caar wrld)))))))))
           (move-current-acl2-world-key-to-front (cdr wrld)))))

(defun-one-output exit-boot-strap-mode ()

; We need not unwind the *acl2-unwind-protect-stack* because it must be nil for
; us to have gotten here; had an abort occurred, leaving some frames on the
; stack, we would not get here.  The frame we push below is used by the set-w
; and then popped unless an abort occurs.

  (push nil *acl2-unwind-protect-stack*)
  (set-w 'extension
         (end-prehistoric-world (w *the-live-state*))
         *the-live-state*)
  (acl2-unwind *ld-level* nil)
  (f-put-global 'inhibit-output-lst nil *the-live-state*)

; This is where to start up proof trees if we ever decide that this should be
; the default.  But probably we don't want to do it in MCL.

  (stop-proof-tree-fn *the-live-state*)
  (f-put-global 'ld-skip-proofsp nil *the-live-state*)
  (move-current-acl2-world-key-to-front (w *the-live-state*)))

(defun-one-output ld-alist (standard-oi ld-skip-proofsp ld-error-action)
  `((standard-oi . ,standard-oi)
    (standard-co . ,*standard-co*)
    (proofs-co . ,*standard-co*)
    (current-package . "ACL2")
    (ld-skip-proofsp . ,ld-skip-proofsp)
    (ld-redefinition-action . nil)
    (ld-prompt . ,(if ld-skip-proofsp nil t))
    (ld-keyword-aliases . nil)
    (ld-pre-eval-filter . :all)
    (ld-pre-eval-print . ,(if ld-skip-proofsp nil t))
    (ld-post-eval-print . :command-conventions)
    (ld-evisc-tuple .

; In order to avoid printing huge forms during initialization when ld-ing files
; that are among the pass two files:

                    ,(default-evisc-tuple *the-live-state*))
    (ld-error-triples . t)
    (ld-error-action . ,ld-error-action)
    (ld-query-control-alist . t)
    (ld-verbose . t)))

(defun enter-boot-strap-pass-2 ()
; We must provide set-w a frame on which to push its undo forms.
  (push nil *acl2-unwind-protect-stack*)  
  (set-w 'extension
         (global-set 'boot-strap-pass-2 t (w *the-live-state*))
         *the-live-state*)
  (acl2-unwind *ld-level* nil)

; We use an explicit call of LD-fn to change the defun-mode to :logic just to
; lay down an event in the pre-history, in case we someday want to poke around
; at the boundary.

  (ld-fn (ld-alist '((logic))
                   t
                   :error)
         *the-live-state* nil))

; The following constant should be changed when we add to the collection of
; files to be processed in :logic default-defun-mode.

(defconst *acl2-pass-2-files*
  '("axioms"))

(defun check-built-in-constants ()

; Certain defconsts are problematic because they build in values that one
; cannot know until the system is built!  Getting their values right requires
; some thought or experiment and even then subsequent changes to the system can
; render the values incorrect.  To guard against incorrect (obsolete) values
; for these contants, this function causes an error if doesn't like what it
; sees.  We document only one such constant, *force-xnume*, which more or less
; describes the situation suffered by all of them.

; Our Code requires that *force-xnume* be the nume of (:EXECUTABLE-COUNTERPART
; FORCE).  It would be nice to be able to write: (defconst *force-xnume*
; (fn-rune-nume 'force t t (w state))).  But that suffers TWO problems.  The
; first is that defconst disallows the use of any variable in the initial value
; form.  Thus, state is illegal above.  We tried fixing that, in a hypocritical
; way, by allowing it to ourselves in boot-strap even though we denied it to
; the user.  But even that doesn't work because of the second problem: The
; first time the defconst is processed, no numes are being allocated because
; everything is done in the :program defun-mode.  You might think, therefore,
; we ought to delay the execution of this defconst until after we've done the
; second pass and created the rune in question.  But we cannot do that because
; we use *force-xnume* in our code (namely, in ok-to-force) and that function
; can't be defined until *force-xnume* is defined.  Thus, this seemingly
; hackish way of doing it, where we actually specify ahead of time which nume
; will be assigned, is not as outlandish as it might at first seem.  We check
; that the actual assignment is correct (using this function) after booting.

  (cond
   ((not (equal *force-xnume* (fn-rune-nume 'force t t (w *the-live-state*))))
    (interface-er
     "The defconst of *force-xnume* is ~x0 but should be ~x1.  To fix the ~
      error, change the offending defconst to the number indicated and rebuild ~
      the system.  To understand why the code is written this way, see the ~
      comment in check-built-in-constants.  You may also need to change the ~
      defconst of *immediate-force-modep-xnume*."
     *force-xnume*
     (fn-rune-nume 'force t t (w *the-live-state*)))))
  (cond
   ((not
     (equal *immediate-force-modep-xnume*
            (fn-rune-nume 'immediate-force-modep t t (w *the-live-state*))))
    (interface-er
     "The defconst of *immediate-force-modep-xnume* is ~x0 but should be ~x1.  ~
      To fix the error, change the offending defconst to the number indicated ~
      and rebuild the system.  To understand why the code is written this way, ~
      see the comment in check-built-in-constants."
     *immediate-force-modep-xnume*
     (fn-rune-nume 'immediate-force-modep t t (w *the-live-state*)))))
  (cond
   ((not
     (and (equal
           *min-type-set*
           #-:non-standard-analysis -8192 #+:non-standard-analysis -65536)
          (equal
           *max-type-set*
           #-:non-standard-analysis 8191 #+:non-standard-analysis 65535)))
    (interface-er
     "The minimal and maximal type-sets are incorrectly built into ~
      the definition of type-alist-entryp.  These type-sets get ~
      generated by the call of def-basic-type-sets in type-set-a.lisp ~
      are must be mentioned, as above, in axioms.lisp.  Evidently, a ~
      new type-set bit was added.  Update type-alist-entryp.")))
  (let ((good-lst (chk-initial-built-in-clauses *initial-built-in-clauses*
                                                (w *the-live-state*) nil nil)))
    (cond
     (good-lst
      (interface-er
       "The defconst of *initial-built-in-clauses* is bad because at least one ~
        of the records supplies an :all-fnnames that is different from that ~
        computed by all-fnnames-subsumer.  The correct setting is shown below. ~
        To preserve the comments in the source file it might be best to ~
        compare -- with EQUAL -- the elements below with the corresponding ~
        elements in the current source file and just replace the ones that ~
        have changed.  Here is a correct setting in 1:1 correspondence with ~
        the current setting: ~X01."
       `(defconst *initial-built-in-clauses*
          (list ,@good-lst))
       nil)))))

(defun-one-output check-none-ideal (trips acc)
  (cond
   ((null trips)
    (cond ((null acc) nil)
          (t (er hard 'check-none-ideal
                 "The following are :ideal mode functions.  We ~%rely in ~
                  oneify-cltl-code on the absence of :ideal mode functions in ~
                  ~%the boot-strap world (see the comment on check-none-ideal ~
                  there).  These functions should ~%have their guards ~
                  verified: ~&0."
                 acc))))
   (t
    (let ((trip (car trips)))
      (cond ((and (eq (car trip) 'event-landmark)
                  (true-listp trip)
                  (eq (cadr trip) 'global-value)
                  (eq (nth 4 trip) 'defun)

; We need to rule out triples such as the following (but for :ideal mode)
#|
 (EVENT-LANDMARK GLOBAL-VALUE 5054
                 (DEFUN EVENS . :COMMON-LISP-COMPLIANT)
                 DEFUN EVENS (L)
                 (DECLARE (XARGS :GUARD (TRUE-LISTP L)))
                 (COND ((ENDP L) NIL)
                       (T (CONS (CAR L) (EVENS (CDDR L))))))
|#

                  (symbolp (nth 5 trip))
                  (eq (symbol-class (nth 5 trip)
                                    (w *the-live-state*))
                      :ideal)
                  #+:non-standard-analysis
                  (not (member-eq (nth 5 trip)
                                  '(I-SMALL I-CLOSE I-LARGE)))
                  )
             (check-none-ideal (cdr trips) (cons (nth 5 trip) acc)))
            (t (check-none-ideal (cdr trips) acc)))))))

(defun check-state-globals-initialized ()
  (let (bad)
    (do-symbols
     (sym (find-package "ACL2_GLOBAL_ACL2"))
     (when (boundp sym)
       (let ((acl2-sym (intern (symbol-name sym) "ACL2")))
         (when (not
                (or (assoc acl2-sym *initial-global-table* :test 'eq)
                    (assoc acl2-sym *initial-ld-special-bindings* :test 'eq)))
           (push (cons acl2-sym (symbol-value sym))
                 bad)))))
    (when bad
      (error
       "The following symbols, perhaps with the values shown, need to~%~
        be added to *initial-global-table*:~%~s~%"
       bad))))

(defun-one-output check-acl2-initialization ()
  (check-built-in-constants)
  (check-out-instantiablep (w *the-live-state*))
  (check-none-ideal (w *the-live-state*) nil)
  (check-state-globals-initialized))

(defun initialize-acl2 (&optional (pass-2-ld-skip-proofsp 'include-book)
                                  (acl2-pass-2-files *acl2-pass-2-files*)
                                  distributed-books-dir
                                  skip-comp-exec)

; Note: if distributed-books-dir is supplied, it should be a Unix-style
; pathname (either absolute or not [doesn't matter which]).

; This function lds all of the *acl2-files* (except "interface-raw") in
; default-defun-mode :program (which is the default default-defun-mode).  It
; then re-loads the files in acl2-pass-2-files in :logic.

; During the first pass, ld-skip-proofsp is 'initialize-acl2, which is
; like the setting t (doing syntactic checks but skipping proofs and
; LOCALs) but omits a few of the checks so the bootstrapping can be
; done.  During the second pass, ld-skip-proofsp is as specified by
; the &optional parameter above.  It defaults to 'include-book, which
; means we skip LOCALs, all syntactic checks, and proofs.  By calling
; this function with pass-2-ld-skip-proofsp nil you can arrange for it
; to try to prove its way through the second pass.  However, see
; below.

; Why Two Passes?  By doing things in two passes we make it possible
; to use all system functions in hints and other proof commands.  In
; the one-pass initialization we used to use, it was impossible to use
; theory expressions in the defthms in axioms.lisp because the
; necessary theory functions were not yet defined and so trans-eval
; balked on them.

  (with-warnings-suppressed

; Interactive Proofs: Many times, (initialize-acl2 nil) -- which
; causes the system to prove the admissibility of everything done in
; the second pass -- will fail because insufficient lemmas are
; available to handle new functions added since the last such proof.
; But it will leave you in a state so that you can continue to develop
; proofs.  In particular, if you have changed some of the proved code,
; e.g., axioms.lisp, and you wish to re-verify it, you can
; proceed as follows.  First, fire up akcl.  Then do
; (acl2::load-acl2).  Finally do (initialize-acl2 nil) and wait for
; the first proof to fail.  When it fails you will be returned to
; lisp.  There, in raw lisp, you should execute
#|
  (let ((state *the-live-state*))
   (reset-ld-specials t)
;  (set-ld-redefinition-action '(:doit! . :overwrite) state) ;see below
   )
|#
; This will set the ld-specials to their conventional post-boot-strap
; setting, except (possibly) for ld-redefinition-action which will
; allow redefinition.  (We discuss this more below.)  Then enter the
; loop with (LP!), which will set *features* to include
; :acl2-loop-only in case read conditionals are present in the
; sources you wish to develop.  Once in the loop, you should be able
; to continue as normal with proof development.

; If the initialization did not get beyond pass one, :q is undefined
; and the only way to exit is to do (mv nil :q state).  You will also note
; that other keyword commands, e.g., :pc, are unavailable.  You can always
; execute the appropriate form in raw lisp, e.g.,

; (let ((state *the-live-state*)) (pc 'fn))

; If you did get beyond pass one, things will be pretty normal looking
; except that inspection will show that both (global-val
; 'boot-strap-flg (w state)) and (global-val 'boot-strap-pass-2 (w
; state)) are t.  This will manifest itself in some perhaps surprising
; ways during interactive proofs, e.g., redundant deflabels are
; admitted during the second pass.

; Is Redefinition Permission Necessary?  Not always.  If you are
; re-verifying the sources the chances are you've changed something.
; Suppose some event is no longer admissible.  If you have to change a
; function definition to admit it, i.e., one of your new events is
; incorrectly written, then redefinition permission is necessary
; because what you are trying to do is NOT just a simple reclassifying
; (you're changing the definition).  If on the other hand you are not
; changing definitions but adding them, you need not perform the
; set-ld-redefinition-action shown above.  A conservative thing to do
; is to leave the redefinition action nil and not set it until
; necessary.

; How do I Add a New Function Definition?  If you try to add to the
; sources a new function definition while in pass one of the
; initialization it will fail because we insist that all functions
; defined logically already have raw lisp definitions.  You should
; define the function first in raw lisp (by exiting the LP!) and then
; in the loop.  If you discovered this problem by provoking the hard
; error: ACL2 Error in ACL2-INTERFACE: fn is not fboundp! then you
; must first exit the lisp break with :q.  This will revert the world
; and throw you out of the LP!.  The world is as it was before the
; offensive definition, so you should define the function in raw lisp,
; reenter LP! and proceed as intended.

; The Special State Discussion: We bind state to *the-live-state*
; below because the macro expansion of ld introduces the variable
; state.  Once upon a time we declared state special with
; (defparameter state *the-live-state*) in this file.  But that had
; the effect of preventing tail-recursion removal in situations like
; (pprogn (princ$ ...) (recursive-call ...)) because pprogn macro
; expands to a binding of state and you can't jump out of a binding of
; a special in tail recursion (it is only allowed if it is lexical).
; Thus, we got the curious and frustrating problem that if we
; recompiled system functions (like print-doc-string-part1) then they
; would no longer have tail recursions removed, even though the
; recursion would be so removed when we made the system from scratch.
; (Reiterating the reason: between the two compilations of the
; function, state became special.  Had we declared state special in
; axioms.lisp instead of here in interface-raw.lisp, none of
; tail-recursion removal on state changing functions would have been
; done!)  We no longer make state special and hence must bind it to
; *the-live-state* lexically when its use is unavoidable.  In point
; of fact we now evaluate (setq state *the-live-state*) in
; (load-acl2), without making state special, and hence state may be
; used in raw Lisp after the system is loaded as long as the form
; using state is not compiled.

; Finally:  another way to prove your way through axioms.lisp is
; to invoke (acl2::load-acl2) and (initialize-acl2), then
; save the system (e.g., in akcl execute
; (si::save-system "my-saved_acl2")), and now each time you invoke
; that saved image first execute

; (defconst *acl2-pass-2-files* '())

; in raw Lisp (or, execute this before saving the image), and then
; after executing (LP!) submit the form

; (set-w 'extension (global-set 'boot-strap-pass-2 t (w state)) state)

; to ACL2 in order to allow system DEFLABELs to be considered
; redundant.

; Save perhaps 12K in image.

   (makunbound '*copy-of-common-lisp-symbols-from-main-lisp-package*)
   (let* ((*features* (cons :acl2-loop-only *features*))
          #+akcl

; AKCL compiler note stuff.  We have so many tail recursive functions
; that the notes about tail recursion optimization are just too much
; to take.

          (compiler:*suppress-compiler-notes* t)
          (state *the-live-state*)
          pass-2-alist)
     (let ((operating-system
            #+unix :unix
            #+apple :apple
            #+mswindows :mswindows
            #-(or unix apple mswindows) nil))
       (enter-boot-strap-mode distributed-books-dir operating-system))

; Rockwell Addition:  Here we initialize the nu-rewriter memo cache.

     (initialize-nu-memos 65534)
     (setq pass-2-alist
           (let ((ans nil))
             (dolist
               (fl acl2-pass-2-files)
               (mv-let (erp val state)

; Warning.  Because of the read-file here, we have to be careful not to define
; any packages in the pass-2 files that contain symbols mentioned in those
; files.  The read-file will break in any such case; the DEFPKG in such a file
; must be processed first.

                 (read-file (coerce
                             (append (coerce fl 'list)
                                     (cons #\. (coerce *lisp-extension*
                                                       'list)))
                             'string)
                            *the-live-state*)
                 (declare (ignore state))
                 (cond (erp (interface-er "Unable to read file ~x0!"
                                          fl))
                       (t (push (cons fl val) ans)))))
             ans))
     (dolist
       (fl (remove "interface-raw" *acl2-files* :test #'equal))
       (mv-let (er val st)
         (ld-fn
          (ld-alist (or (cdr (assoc fl pass-2-alist :test #'equal))
                        (coerce
                         (append (coerce fl 'list)
                                 (cons #\. (coerce *lisp-extension*
                                                   'list)))
                         'string))
                    'initialize-acl2
                    :error)
          *the-live-state*
          nil)
         (declare (ignore val st))
         (cond (er

; The load caused an error.  We abort quietly without doing anything
; else so we are in the same state.

                (return-from initialize-acl2 nil)))))
     (enter-boot-strap-pass-2)
     (dolist
       (fl acl2-pass-2-files)
       (mv-let (er val st)
         (ld-fn
          (ld-alist (or (cdr (assoc fl pass-2-alist :test #'equal))
                        (interface-er "Did not find ~x0 in pass-2-alist."
                                      fl))
                    pass-2-ld-skip-proofsp
                    :error)
          state
          nil)
         (declare (ignore val st))
         (cond (er

; The load caused an error.  We abort quietly without doing anything
; else so we are in the same state.

                (return-from initialize-acl2 nil)))))

; It is important not to wait to write out TMP1.lisp until after we leave the
; boot-strap.  By doing so before that time, we ensure that the guard-check
; under safe mode is made for primitives (see oneify-cltl-code), and that
; compile-uncompiled-*1*-defuns will not encounter 'exit-boot-strap-mode and
; thus stop finding functions to compile.  We use a call of ld here to make
; possible the subsidiary uses of state-global-let* on behalf of macroexpand1
; (see the comment in comp-fn for more information).

     (if (not skip-comp-exec)

; Optimization: Skip this compile for generate-acl2-proclaims.

         (ld '((comp-fn :exec nil "1" state))))
     (exit-boot-strap-mode)
     (initialize-pc-acl2 *the-live-state*)

; We now set up the ld specials as we wish them for the user's first
; invocation of LP.  The ld specials were previously initialized (in
; enter-boot-strap-mode) to the same values used below (because the
; LDs above make reference to some of them so they must be bound).
; But the LD above changes them so we now initialize them again.

     (f-put-ld-specials *initial-ld-special-bindings* *the-live-state*)

; We now check certain invariants, for example, that we have defined certain
; built-in constants correctly.

     (or (not acl2-pass-2-files)

; The check for check-built-in-constants in check-acl2-initialization, for one,
; will fail if we do not make a second pass through axioms.lisp.  That is
; because all (or at least most) of the 'level-no properties are still 0 before
; then, so chk-initial-built-in-clauses (called in check-built-in-constants)
; will fail, because its subroutine all-fnnames-subsumer does not behave
; properly until the 'level-no properties are set.

         (check-acl2-initialization))
     (cond
      ((or (not (probe-file *acl2-status-file*))
           (delete-file *acl2-status-file*))
       (with-open-file (str *acl2-status-file*
                            :direction :output)
                       (format str
                               "~s"

; The next token, :INITIALIZED, is used in GNUmakefile; keep in sync.

                               :INITIALIZED))))
; If you want the final image to have infixp = t, then put the following
; form here:
;    (f-put-global 'infixp t *the-live-state*)
    
     t)))


;                                  LP

; Lp is the function that an ACL2 user invokes to get into the ACL2
; read-eval-print loop.

; Essay on Pathnames

; We use Unix-style pathnames everywhere in ACL2 except when interfacing with
; the operating system.  Functions defined in this file, interface-raw.lisp,
; generally use real pathname strings for the host operating system.
; (Exceptions are clearly labeled, including compile-uncompiled-defuns and
; compile-uncompiled-*1*-defuns.)  Functions defined outside this file
; (interface-raw.lisp) pass around ACL2 (Unix-style) pathname strings.  Here
; are all the functions that take pathnames whose form is based on (os (w
; state)) rather than on Unix.

; acl2-compile-file [but see comment there]
; compile-file
; convert-book-name-to-compiled-name
; delete-file
; delete-compiled-file
; load
; load-compiled-file-if-more-recent [but see comment there]
; probe-file
; proclaim-file
; unfound-compiled-file [but see comment there]

(defun lp (&rest args)

; This function can only be called from within raw lisp, because no
; ACL2 function mentions it.  Thus, we assume we are in raw lisp.
; This is the top-level entry to ACL2.  Note that truename can cause an error
; in some Common Lisps when the given file or directory does not exist.  Hence,
; we sometimes call truename on "" rather than on a file name.

  (let ((state *the-live-state*)
        (raw-p
         (cond
          ((null args) nil)
          ((equal args '(raw)) 'raw)
          (t (error "LP either takes no args or a single argument, 'raw.")))))
    (cond
     ((> *ld-level* 0)
      (when (raw-mode-p *the-live-state*)
        (fms "You have attempted to enter the ACL2 read-eval-print loop from ~
              within raw mode.  However, you appear already to be in that ~
              loop.  If your intention is to leave raw mode, then execute:  ~
              :set-raw-mode nil.~|"
             nil (standard-co *the-live-state*) *the-live-state* nil))
      (return-from lp nil))
     (*lp-ever-entered-p*
      (f-put-global 'standard-oi
                    (if (and raw-p (not (raw-mode-p state)))
                        (cons '(set-raw-mode t)
                              *standard-oi*)
                      *standard-oi*)
                    *the-live-state*)
      (with-warnings-suppressed
       (ld-fn (f-get-ld-specials *the-live-state*)
              *the-live-state*
              nil)))
     (t (in-package "ACL2")

; Acl2-default-restart isn't enough in Allegro, at least, to get the new prompt
; when we start up:

        (let ((system-dir (getenv$-raw "ACL2_SYSTEM_BOOKS")))
          (when system-dir
            (f-put-global 'distributed-books-dir system-dir *the-live-state*)))
        (install-new-raw-prompt)
        (setq *lp-ever-entered-p* t)
        #+(and (not acl2-loop-only) acl2-rewrite-meter)
        (setq *rewrite-depth-alist* nil)

; Without the following call, it was impossible to read and write with ACL2 I/O
; functions to *standard-co* in CLISP 2.30.  Apparently the appropriate Lisp
; streams at the time of the build were closed when the ACL2 image was brought
; up.  So we "refresh" the appropriate property lists with the current such
; Lisp streams.

        (setup-standard-io)

; The following applies to CLISP 2.30, where charset:iso-8859-1 is defined, not to
; CLISP 2.27, where charset:utf-8 is not defined.  It apparently has to be
; executed in the current Lisp session.  We tried executing the following form
; before saving an image, but the value of custom:*default-file-encoding* at
; startup was #<ENCODING CHARSET:ASCII :UNIX>.

        #+(and clisp unicode)
        (setq custom:*default-file-encoding* charset:iso-8859-1)
        #+mswindows
        (cond
         ((null (f-get-global 'mswindows-drive *the-live-state*))
          (let* ((str (namestring (truename "")))
                 (posn (position #\: str)))
            (cond
             ((null posn)
              (er soft 'LP
                  "We are unable to determine the drive using ~
                   (namestring (truename \"\")), which evaluates to ~p0."
                  str)
              (return-from lp nil)))
            (f-put-global 'mswindows-drive
                          (subseq str 0 (1+ posn))
                          *the-live-state*))))
        (cond ((f-get-global 'connected-book-directory *the-live-state*) nil)
              ((null *initial-cbd*)
               (setq *initial-cbd*
                     (pathname-os-to-unix
                      (namestring (truename

; See the comment in save-acl2-in-allegro, which mentions a comment present
; before Version_2.5 that was present here as well.

                                   ""))
                      *the-live-state*))

; In openmcl, it seems that *initial-cbd* as computed above could give a string
; not ending in "/".  We fix that here.

               (cond ((and (stringp *initial-cbd*)
                           (not (equal *initial-cbd* ""))
                           (not (eql (char *initial-cbd*
                                           (1- (length *initial-cbd*)))
                                     #\/)))
                      (setq *initial-cbd*
                            (concatenate 'string *initial-cbd* "/"))))
               (cond ((not (absolute-pathname-string-p
                            *initial-cbd*
                            t
                            (os (w *the-live-state*))))
                      (er soft 'LP
                          "Our guess for the initial setting of cbd, ~x0, ~
                           which was generated by (pathname-os-to-unix ~
                           (namestring (truename \"\")) *the-live-state*), ~
                           is not a legal directory!  Before entering ACL2, ~
                           please setq *initial-cbd* to a nonempty string ~
                           that represents an absolute ACL2 (i.e., ~
                           Unix-style) pathname.  Sorry for the inconvenience."
                          *initial-cbd*)
                      (return-from lp nil)))
               (f-put-global 'connected-book-directory *initial-cbd*
                             *the-live-state*))
              ((not (absolute-pathname-string-p *initial-cbd*
                                                t
                                                (os (w *the-live-state*))))
               (er soft 'LP
                   "The current setting of *initial-cbd*, ~x0, is ~
                    not a directory.  Before entering ACL2, please ~
                    setq *initial-cbd* to a nonempty string that ~
                    represents the absolute ACL2 (i.e., Unix-style) ~
                    pathname of a directory. See :DOC cbd."
                   *initial-cbd*
                   *directory-separator*)
               (return-from lp nil))
              (t
               (f-put-global 'connected-book-directory *initial-cbd*
                             *the-live-state*)))
        (let ((customization-full-file-name
               (let* ((cb1 (our-merge-pathnames
                            (f-get-global 'connected-book-directory
                                          *the-live-state*)
                            "acl2-customization"))
                      (cfb1 (string-append cb1 ".lisp")))
                 (if (probe-file (pathname-unix-to-os cfb1 *the-live-state*))
                     cfb1
                   (let* ((cb2

; There is not a true notion of home directory for Windows systems, as far as
; we know.  We may provide one at a future point, but for now, we simply act as
; though ~/acl2-customization.lisp does not exist on such systems.

                           #+mswindows
                           nil
                           #-mswindows
                           (our-merge-pathnames

; The call of pathname-os-to-unix below may seem awkward, since later we apply
; pathname-unix-to-os before calling probe-file.  However, our-merge-pathnames
; requires Unix-style pathname arguments, and we prefer not to write an
; analogous function that takes pathnames for the host operating system.

                            (pathname-os-to-unix
                             (namestring

; MCL does not seem to handle calls of truename correctly on logical pathnames.
; We should think some more about this, but for now, let's solve this problem
; by brute force.

                              #+(and mcl (not openmcl))
                              (truename
                               (common-lisp::translate-logical-pathname
                                (user-homedir-pathname)))
                              #-(and mcl (not openmcl))
                              (truename (user-homedir-pathname)))
                             *the-live-state*)
                            "acl2-customization"))
                          (cfb2 (and cb2 (string-append cb2 ".lisp"))))
                     (if (and cfb2
                              (probe-file (pathname-unix-to-os
                                           cfb2 *the-live-state*)))
                         cfb2
                       nil))))))
          (cond
           ((and customization-full-file-name
                 (not (global-val 'boot-strap-flg (w state))))

; If the file "acl2-customization.lisp" on the current directory exists (and we
; are not booting) and it hasn't been included yet, we include it first.  If it
; does not exist, but it exists on the user's home directory, then we include
; that.  Because of the ld-skip-proofsp setting we use, no warning is printed
; if "acl2-customization.lisp" is uncertified.  But it will prevent the
; production of truly certified books in any session in which it has been
; included, so we check it explicitly below and warn the user.

            (fms "Customizing with ~x0.~%"
                 (list (cons #\0 customization-full-file-name))
                 *standard-co*
                 state
                 nil)
            (let ((old-infixp (f-get-global 'infixp *the-live-state*)))
              (f-put-global 'infixp nil *the-live-state*)
              (with-warnings-suppressed
               (ld-fn (put-assoc-eq
                       'standard-oi
                       (if (and raw-p (not (raw-mode-p state)))
                           (cons '(set-raw-mode t)
                                 customization-full-file-name)
                         customization-full-file-name)
                       (put-assoc-eq
                        'ld-error-action :return
                        (f-get-ld-specials *the-live-state*)))
                      *the-live-state*
                      nil))
              (f-put-global 'infixp old-infixp *the-live-state*))))
          (f-put-global 'standard-oi
                        (if (and raw-p (not (raw-mode-p state)))
                            (cons '(set-raw-mode t)
                                  *standard-oi*)
                          *standard-oi*)
                        *the-live-state*)
          (f-put-global 'ld-error-action :continue *the-live-state*)
          (with-warnings-suppressed
           (ld-fn (f-get-ld-specials state)
                  *the-live-state*
                  nil)))))
    (fms "Exiting the ACL2 read-eval-print loop.  To re-enter, execute (LP)."
         nil *standard-co* *the-live-state* nil)
    (values)))

(defmacro lp! (&rest args)
  `(let ((*features* (add-to-set-eq :acl2-loop-only *features*)))
     (lp ,@args)))

;                   COMPILING, SAVING, AND RESTORING

(defun convert-book-name-to-compiled-name (file &optional lsp-not-lisp)

; If lsp-not-lisp is true, then the file ends in .lsp, not .lisp.

  (let ((rev-filename-list (reverse (coerce file 'list))))
    (coerce (append (reverse (if lsp-not-lisp
                                 (cdddr rev-filename-list)
                               (cddddr rev-filename-list)))
                    (coerce *compiled-file-extension* 'list))
            'string)))

(defun acl2-compile-file (file os-file &optional lsp-not-lisp)

; If lsp-not-lisp is true, then the file ends in .lsp, not .lisp.  Otherwise,
; we assume file is a book-name and that it has been certified.  To compile it,
; we need to make sure that uses in the file of backquote and comma conform in
; meaning to those that were in effect during certification.  Os-file is the
; name of the file corresponding to book-name in the syntax of the host
; operating system.

  (mv-let
   (erp val state)
   (if lsp-not-lisp
       (mv nil nil *the-live-state*)
     (chk-book-name file file 'acl2-compile-file *the-live-state*))
   (declare (ignore val))

; Even though state is not explicitly used below it is introduced
; by macros (such as WARNING) and thus it is not ignored.  It will
; be bound to *the-live-state* since that is the state chk-book-name
; returns.  See the Special State Discussion above.

; Be sure to call this function only after the defconst and defmacro forms in
; the given file have already been evaluated, because this function calls
; proclaim-file.

; Warning: The given file should already have been loaded in order that macros
; have been defined.  In raw lisp we have another reason to load first: to
; catch make-event errors.

   (cond
    (erp (error "ACL2 Halted"))
    (t
     (let ((*readtable* *acl2-readtable*)
           (ofile (convert-book-name-to-compiled-name os-file lsp-not-lisp))
           (stream (get (proofs-co *the-live-state*) *open-output-channel-key*)))

; It is tempting to evaluate (proclaim-file os-file).  However, all functions
; in the file were presumably already proclaimed, as appropriate, during add-trip.

       (compile-file os-file :output-file ofile)
       (load-compiled ofile)
       (terpri stream)
       (prin1 ofile stream)
       (terpri stream)
       (terpri stream)
       (let ((file-date (file-write-date os-file))
             (ofile-date (file-write-date ofile)))
         (cond ((and file-date
                     ofile-date)
                (cond ((>= ofile-date file-date)
                       ofile)
                      (t (er hard 'acl2-compile-file
                             "The compiled file we created, ~x0, has ~
                              an earlier write date than the source ~
                              file, ~x1!"
                             ofile
                             os-file))))
               (t (terpri stream)
                  (warning$ 'acl2-compile-file nil
                           "We cannot get file write dates for both ~
                            ~x0 and ~x1.  This will prevent ~
                            include-book from loading the compiled ~
                            file automatically.~%"
                           os-file ofile)
                  ofile))))))))

(defun-one-output delete-compiled-file (file)
  (let ((ofile (convert-book-name-to-compiled-name file)))
    (cond ((probe-file ofile)
           (cond ((delete-file ofile) nil)
                 (t (er hard 'delete-compiled-file
                        "We have just recertified ~x0.  You asked us ~
                         not to compile it.  But a compiled file of ~
                         the corresponding name, ~x1, exists and ~
                         cannot be deleted with Common Lisp's ~
                         delete-file.  The compiled file was not ~
                         produced by the ongoing certify-book and we ~
                         do not know that it corresponds to the just ~
                         certified ~x0.  If ~x1 exists at the time of ~
                         an (include-book ~x0), it might be ~
                         erroneously loaded, possibly causing ~
                         inconsistency."
                        file ofile))))
          (t nil))))

(defun unfound-compiled-file (ctx new-fns-exec load-compiled-file file os-file
                                  ofile expansion-alist ev-lst str)

; We return a non-nil value if and only if a file is compiled, in which case it
; is also loaded and proclaimed.

  (let ((state *the-live-state*))
    (case load-compiled-file
          (:warn
           (warning$ ctx "Compiled file"
                     (concatenate
                      'string
                      "The compiled file for ~x0 was not loaded because "
                      str
                      "  See :DOC include-book.")
                     os-file
                     ofile)
           nil)
          (:try nil)
          ((:comp :comp!)
           (when (not *inside-include-book-fn*)
             (load file)) ; see the Warning in compile-certified-file
           (let (expansion-filename)
             (when (or (eq load-compiled-file :comp!)
                       new-fns-exec
                       expansion-alist)
               (setq expansion-filename (expansion-filename file))
               (when (or (not (probe-file expansion-filename))
                         (and (or (eq load-compiled-file :comp!)
                                  (<= (file-write-date expansion-filename)
                                      (file-write-date file)))
                              (progn (delete-file expansion-filename)
                                     t)))
                 (write-expansion-file new-fns-exec expansion-filename
                                       expansion-alist ev-lst ctx state)))
             (compile-certified-file new-fns-exec expansion-filename file
                                     expansion-alist state))
           t)
          ((nil)
           (er hard ctx
               "Please contact the implementors of ACL2.  This case in ~
                unfound-compiled-file should never arise."))
          (otherwise ; t
           (er hard ctx
               (concatenate
                'string
                "The compiled file for ~x0 was not loaded because "
                str
                "  See :DOC include-book.")
               os-file
               ofile)))))

(defun load-compiled-file-if-more-recent (ctx new-fns-exec load-compiled-file
                                              file directory-name
                                              expansion-alist ev-lst)

; File is the ACL2 pathname of file os-file (the latter being in the syntax of
; the host operating system).

; If load-compiled-file is not nil, then we load the compiled version of file
; if it is more recent than file.  We call this with load-compiled-file = nil
; under the call of include-book-fn in certify-book-fn.

; If expansion-alist is nil, then ev-lst is a don't-care.

; We return a non-nil value if and only if a compiled file is loaded.

; At one time we called proclaim-file.  But we believe that all necessary
; proclaims have already been done by add-trip.

  (let ((*connected-book-directory* directory-name)
        (os-file (pathname-unix-to-os file *the-live-state*))
        (load-existing nil))
    (or
     (and
      load-compiled-file
      (let ((ofile (convert-book-name-to-compiled-name os-file)))
        (cond ((probe-file ofile)
               (when (eq load-compiled-file :comp!)
                 (delete-file ofile))
               (let ((file-date (file-write-date os-file))
                     (ofile-date (file-write-date ofile)))
                 (cond ((not file-date)
                        (unfound-compiled-file
                         ctx new-fns-exec load-compiled-file file os-file ofile
                         expansion-alist ev-lst
                         "(file-write-date ~x0) is NIL."))
                       ((not ofile-date)
                        (unfound-compiled-file
                         ctx new-fns-exec load-compiled-file file os-file ofile
                         expansion-alist ev-lst
                         "(file-write-date ~x1) is NIL."))
                       ((not (> ofile-date file-date))
                        (unfound-compiled-file
                         ctx new-fns-exec load-compiled-file file os-file ofile
                         expansion-alist ev-lst
                         "the write-date of ~x1 is not greater than that of ~
                          ~x0."))
                       (t (load-compiled ofile)
                          (setq load-existing t)
                          nil))))
              (t (unfound-compiled-file
                  ctx new-fns-exec load-compiled-file file os-file ofile
                  expansion-alist ev-lst
                  "the compiled file ~x1 was not found.")))))
     load-existing)))

(defun compile-uncompiled-defuns (file &optional (fns :some) gcl-flg
                                       &aux (state *the-live-state*))

; File should be given in Unix-style syntax.  Hence for example, "TMP" is
; relative to the current directory, even though on a Macintosh this might
; appear to be an absolute pathname for a file.

; Compile-uncompiled-defuns compiles the non-built-in defuns that are not
; currently compiled if FNS is :some.  Otherwise, FNS should be a list of
; functions to compile.

  (when (and (not (symbol-listp fns))
             (not (eq fns :some)))
    (er hard 'compile-uncompiled-defuns
        "The argument to compile-uncompiled-defuns must be either a true list ~
         of symbols or the keyword :some.  The argument ~x0 is thus illegal."
        fns))
  (cond
   ((null fns)
    (warning$ 'compile-uncompiled-defuns nil
              "No functions to compile.")
    (return-from compile-uncompiled-defuns file)))
  (let ((*package* (find-package (current-package state)))
        (*read-base* 10)
        (*print-base* (acl2-print-base))
        (*print-radix* (acl2-print-radix))
        (*print-circle* nil)
        *print-pretty*
        *print-level*
        *print-length*
        (*readtable* *acl2-readtable*)
        (*print-case* :upcase)
        (seen (make-hash-table :test 'eq))
        #+allegro (excl:*redefinition-warnings* nil)
        #+clisp (custom::*suppress-check-redefinition* t)
        (fns (cond ((eq fns :uncompiled)
                    :some)
                   ((eq fns t)
                    :all)
                   (t fns)))
        (fn-file (format nil "~a.lisp" file))
        (os-file (pathname-unix-to-os file state)))
    (mv-let
     (chan state)
     (open-output-channel fn-file :character state)
     (pprogn
      (mv-let (col state)
              (fmt1 "; This file is automatically generated, to be ~
                     compiled.~|; Feel free to delete it after compilation.~|"
                    nil 0 chan state nil)
              (declare (ignore col))

; We print (in-package "...") but we do it this way to guarantee that the
; symbol 'in-package is printed correctly.

              (fms! "~x0"
                    (list (cons #\0 (list 'in-package
                                          (current-package state))))
                    chan state nil))
      (progn
        (dolist (trip (w state))
          (cond ((and (eq fns :some)
                      (eq (car trip) 'command-landmark)
                      (eq (cadr trip) 'global-value)
                      (equal (access-command-tuple-form (cddr trip))
                             '(exit-boot-strap-mode)))
                 (return))
                ((and (eq (car trip) 'cltl-command)
                      (eq (cadr trip) 'global-value)
                      (consp (cddr trip))
                      (eq (caddr trip) 'defuns)

; The next test asks whether the ignorep field of the defuns tuple is
; '(defstobj . stobj).  If it is, this triple didn't actually make
; those definitions.

                      (not (and (consp (caddr (cddr trip)))
                                (eq (car (caddr (cddr trip))) 'defstobj))))
                 (dolist (x (cdddr (cddr trip)))
                   (cond ((and (not (gethash (car x) seen))
                               (not (member-eq (car x) *non-exec-fns*))
                               (or (eq fns :some)
                                   (member-eq (car x) fns)))
                          (setf (gethash (car x) seen) t)
                          (when (not (compiled-function-p! (car x)))
                            (fms! "~x0"
                                  (list (cons #\0 (cons 'defun x)))
                                  chan state nil))))))
                ((and (eq (car trip) 'cltl-command)
                      (eq (cadr trip) 'global-value)
                      (consp (cddr trip))
                      (eq (caddr trip) 'defstobj))
                 (dolist (x (car (cddddr (cddr trip))))

; (cddr trip) is of the form 
; (DEFSTOBJ name the-live-name init raw-defs template)
; and x here is one of the raw-defs.

                   (cond ((and (not (gethash (car x) seen))
                               (not (member-equal *stobj-inline-declare* x))
                               (or (eq fns :some)
                                   (member-eq (car x) fns)))
                          (setf (gethash (car x) seen) t)
                          (when (not (compiled-function-p! (car x)))
                            (fms! "~x0"
                                  (list (cons #\0 (cons 'defun x)))
                                  chan state nil))))))))
        (newline chan state)
        (close-output-channel chan state))))
    (when (not (eq fns :some))
      (let (missing)
        (dolist (fn fns)
          (when (not (gethash fn seen))
            (push fn missing)))
        (when missing
          (format t
                  "~%Warning:  The following functions have not been ~
                   compiled.~%  ~s~%Perhaps you have not defined them inside ~
                   the ACL2 command loop.~%"
                  missing))))
    (cond
     (gcl-flg
      #+gcl
      (compile-file
       (namestring (truename (pathname-unix-to-os fn-file state)))
       :c-file t :h-file t)
      #-gcl
      (er hard 'compile-uncompiled-defuns
          "The gcl-flg argument to compile-uncompiled-defuns is only legal ~
           when running under GCL."))
     (t
      (let ((lisp-file (namestring (truename (pathname-unix-to-os fn-file state)))))
        (compile-file lisp-file)
        (when (not (keep-tmp-files state))
          (delete-file lisp-file)
          #+clisp
          (delete-file (concatenate 'string os-file ".lib"))))))
    (load-compiled os-file)
    (if (not (keep-tmp-files state))
        (delete-file (concatenate 'string os-file "." *compiled-file-extension*)))
    os-file))

(defun compile-uncompiled-*1*-defuns (file &optional (fns :some) gcl-flg chan0
                                           &aux
                                           (state *the-live-state*)
                                           (wrld (w *the-live-state*)))

; This is similar to compile-uncompiled-defuns, but for *1* functions.
; However, an optional chan0 may be supplied; if non-nil, then it is an open
; output channel of type :character, which is closed by this function.

; File should be given in Unix-style syntax.  Hence for example, "TMP" is
; relative to the current directory, even though on a Macintosh this might
; appear to be an absolute pathname for a file.

; If chan0 is not nil, then unlike compile-uncompiled-defuns, we write out all
; relevant *1* function definitions, even those that are currently compiled.

  (when (and (not (symbol-listp fns))
             (not (eq fns :some)))
    (er hard 'compile-uncompiled-*1*-defuns
        "The argument to compile-uncompiled-*1*-defuns must be either a true ~
         list of symbols or the keyword :some.  The argument ~x0 is thus ~
         illegal."
        fns))
  (cond
   ((and (null fns) (null chan0))
    (warning$ 'compile-uncompiled-defuns nil
              "No functions to compile.")
    (return-from compile-uncompiled-*1*-defuns file)))
  (let ((*package* (find-package (current-package *the-live-state*)))
        (*read-base* 10)
        (*print-base* (acl2-print-base))
        (*print-radix* (acl2-print-radix))
        (*print-circle* nil)
        *print-pretty*
        *print-level*
        *print-length*
        (*readtable* *acl2-readtable*)
        (*print-case* :upcase)
        (seen (let ((tbl (make-hash-table :test 'eq)))
                (when (not (eq fns :some))
                  (dolist (fn fns)
                    (setf (gethash fn tbl) :init)))
                tbl))
        #+allegro (excl:*redefinition-warnings* nil)
        #+clisp (custom::*suppress-check-redefinition* t)
        (fns (cond ((eq fns :uncompiled)
                    :some)
                   ((eq fns t)
                    :all)
                   (t fns)))
        (fn-file (format nil "~a.lisp" file))
        (os-file (pathname-unix-to-os file state))
        (not-boot-strap-p (null (global-val 'boot-strap-flg wrld))))
    (mv-let
     (chan state)
     (cond (chan0 (mv chan0 state))
           (t (open-output-channel fn-file :character state)))
     (progn
       (cond ((null chan0) ; new output file
              (mv-let (col state)
                      (fmt1 "; This file is automatically generated, to be ~
                            compiled.~|; Feel free to delete it after ~
                            compilation.~|"
                            nil 0 chan state nil)
                      (declare (ignore col))

; We print (in-package "...") but we do it this way to guarantee that the
; symbol 'in-package is printed correctly.

                      (fms! "~x0"
                            (list (cons #\0 (list 'in-package
                                                  (current-package state))))
                            chan state nil)))
             (t state))
       (dolist (trip wrld)
         (cond ((and (eq fns :some)
                     (eq (car trip) 'command-landmark)
                     (eq (cadr trip) 'global-value)
                     (equal (access-command-tuple-form (cddr trip))
                            '(exit-boot-strap-mode)))

; If we are compiling while building the system, then we will never see
; 'exit-boot-strap-mode, which allows us to explore the entire world.  But when
; a user executes (comp t), thus calling this function with argument fns equal
; to :some, the exploration should only consider user-defined events.

                (return))
               ((and (eq (car trip) 'cltl-command)
                     (eq (cadr trip) 'global-value)
                     (consp (cddr trip))
                     (eq (caddr trip) 'defuns))
                (dolist
                  (x (cdddr (cddr trip)))
                  (let ((*1*fn (*1*-symbol (car x))))
                    (cond ((and (fboundp *1*fn)
                                (cond
                                 ((eq fns :some)
                                  (and (not (gethash (car x) seen))

; We have seen during development of v2-9 that in Allegro CL, when compiling
; *1* functions on the fly during boot-strap (because of code in add-trip), the
; image size increases from 29.3M to 36.9M if we instead use the following
; code, which avoids writing *1* definitions to TMP1.lisp for compiled :logic
; mode functions at the end of the boot-strap.
;                                  (and (not (compiled-function-p! *1*fn))
;                                       (or not-boot-strap-p
;                                           (not (eq (cadr (cddr trip))
;                                                    :program))))
; That is, when we wrote out a TMP1.lisp for all :logic mode functions at the
; end of initialization and compiled it, we saved 7.6M.  This result remains a
; mystery, but we prefer the smaller image so we keep the code below.  The
; resulting increase in wall-clock build time was only about 3 seconds.  See
; also the corresponding comment mentioning compile-uncompiled-*1*-defuns in
; add-trip.

                                       (if not-boot-strap-p
                                           (not (compiled-function-p! *1*fn))

; We have noticed about a corresponding 0.6% to 1.2% slowdown in the regression
; suite when we avoid compiling :program mode *1* functions for GCL during the
; build and also at the end, when TMP1.lisp is written, as discussed in the
; comment above "boot-strap-flg ; delete for build speed-up (see above)" in
; add-trip.  With that mod, we have tried the following additional mod so that
; for GCL we still compile built-in :program mode *1* functions after all, by
; writing out a huge TMP1.lisp file (8.3 MB instead 0.4 MB).

; #+gcl t #-gcl

; But this made things worse.  Here are examples for v2-9-1 (on the way to
; v2-9-2):
;
; During the build, compile :program mode functions on the fly (as usual):
; 9763.160u 146.760s 2:51:33.10 96.2%   0+0k 0+0io 13673004pf+0w
; 
; During the build, do not compile :program mode functions at all:
; 9827.570u 149.730s 2:52:29.27 96.4%   0+0k 0+0io 14549410pf+0w
;
; During the build, do not compile :program mode functions until the end
; (creating very large TMP1.lisp file):
; 9893.870u 150.240s 2:54:22.62 95.9%   0+0k 0+0io 14528555pf+0w
;
; Moroever, the saved_acl2.gcl file went from 43 MB, for the first two, to 104
; MB for the last.  So let's not write :program mode *1* functions to
; TMP1.lisp.  See the long comment about *fast-acl2-gcl-build* in add-trip.

                                         (not (eq (cadr (cddr trip))
                                                  :program)))
                                       (setf (gethash (car x) seen) t)))
                                 ((eq (gethash (car x) seen) :init)
                                  (setf (gethash (car x) seen) t)
                                  (or chan0
                                      (not (compiled-function-p! *1*fn))))))
                           (let ((*1*def
                                  (cons 'defun
                                        (oneify-cltl-code
                                         (cadr (cddr trip)) ; defun-mode
                                         x
                                         (getprop (car x)
                                                  'stobj-function
                                                  nil
                                                  'current-acl2-world
                                                  wrld)
                                         wrld))))
                             (fms! "~x0"
                                   (list (cons #\0 *1*def))
                                   chan state nil)))))))))
       (newline chan state)
       (cond (chan0 (return-from compile-uncompiled-*1*-defuns os-file))
             (t (close-output-channel chan state)))))
    (when (not (eq fns :some))
      (let (missing)
        (dolist (fn fns)
          (when (not (eq (gethash fn seen) t))
            (push fn missing)))
        (when missing
          (format t
                  "~%Warning:  The following executable-counterpart functions ~
                   have not been compiled.~%  ~s~%Perhaps you have not ~
                   defined them inside the ACL2 command loop.~%"
                  missing))))
    (cond
     (gcl-flg
      #+gcl
      (compile-file
       (namestring (truename (pathname-unix-to-os fn-file state)))
       :c-file t :h-file t)
      #-gcl
      (er hard 'compile-uncompiled-defuns
          "The gcl-flg argument to compile-uncompiled-*1*-defuns is only legal ~
           when running under GCL."))
     (t
      (let ((lisp-file (namestring (truename (pathname-unix-to-os fn-file state)))))
        (compile-file lisp-file)
        (when (not (keep-tmp-files state))
          (delete-file lisp-file)
          #+clisp
          (delete-file (concatenate 'string os-file ".lib"))))))
    (load-compiled os-file)
    (if (not (keep-tmp-files state))
        (delete-file (concatenate 'string os-file "." *compiled-file-extension*)))
    os-file))

(defun compile-certified-file (new-fns-exec expansion-filename full-book-name
                                            expansion-alist state)

; Warning: File full-book-name should already have been loaded in order that
; macros have been defined (so we do just that in unfound-compiled-file).  In
; raw lisp we have another reason to load first: to catch make-event errors.

; Expansion-filename should already have been written, if it is used below.

  (let* ((os-full-book-name (pathname-unix-to-os full-book-name state))
         (os-full-book-name-compiled
          (convert-book-name-to-compiled-name os-full-book-name)))
    (when (probe-file os-full-book-name-compiled)
      (delete-file os-full-book-name-compiled))
    (cond
     ((and (null expansion-alist)
           (null new-fns-exec))
      (acl2-compile-file full-book-name os-full-book-name))
     (t
      (let ((os-expansion-filename
             (assert$ expansion-filename
                      (pathname-unix-to-os expansion-filename
                                           state))))
        (acl2-compile-file expansion-filename os-expansion-filename t)

; It is tempting to avoid the following rename-file, instead modifying
; include-book to look first for the compilation of the expansion file before
; the given book file.  This is tempting because it seems cleaner to maintain
; the correspondence between source filenames and compiled filenames at the
; level of Common Lisp.  However, this would mess with the all-o target (for
; example) in books/Makefile.  We may revisit this decision some day.

        (rename-file (convert-book-name-to-compiled-name
                      os-expansion-filename
                      t)
                     os-full-book-name-compiled)))))
  state)


;                            MISCELLANEOUS

(defun-one-output enabled-structurep (x)

; This function is basically a hack.  We return t if x is probably an
; enable-structure.  This is just part of the test of recognizing
; something we don't want to print out when tracing.  See below.
; Without something like this, it is just too uncomfortable to trace
; many ACL2 functions because too much output is printed since
; enabled-structures typically take hundreds of lines to print.

; WARNING:  Keep this in sync with enabled-structure.

  (case-match x
              (((index-of-last-enabling . theory-array)
                (array-name . array-length)
                array-name-root . array-name-suffix)
               (and (integerp index-of-last-enabling)
                    (symbolp array-name)
                    (array1p array-name theory-array)
                    (integerp array-length)
                    (character-listp array-name-root)
                    (integerp array-name-suffix)))
              (& nil)))

(defun-one-output rcnstp (x)

; This is another function in the spirit of enabled-structurep, above.

; WARNING: Keep this in sync with rewrite-constant.

  (case-match x
              (((current-enabled-structure)
                (& & . &)
                (& . &)
                (& . &)
                .
                &)
               (enabled-structurep current-enabled-structure))
              (& nil)))

(defvar *trace-alist*
  (list (cons 'state '|*the-live-state*|)))

(defun-one-output assoc-eq-trace-alist (val alist)
  (cond
   ((endp alist) nil)
   ((and (boundp (caar alist))
         (eq val (symbol-value (caar alist))))
    (car alist))
   (t (assoc-eq-trace-alist val (cdr alist)))))

(defun-one-output stobj-print-symbol (x user-stobj-alist-tail)

; Finds the (first) name of a pair (name . val) in user-stobj-alist-tail such
; that x is the symbol-value of that name's live var, and returns the symbol to
; print when encountering x during tracing.

  (and user-stobj-alist-tail
       (if (eq x (symbol-value (the-live-var (caar user-stobj-alist-tail))))
           (intern-in-package-of-symbol
            (stobj-print-name (caar user-stobj-alist-tail))
            (caar user-stobj-alist-tail))
         (stobj-print-symbol x (cdr user-stobj-alist-tail)))))

(defun-one-output trace-hide-world-and-state (l)

; This function intuitively belongs over in init.lisp but it is here so
; that it will get compiled so we won't get stack overflow when
; tracing large objects.  It is used to replace certain offensive
; objects by less offensive ones before trace prints the args and
; results of traced functions.  It may not work well with local stobjs.

  (let* ((stobj-pair (rassoc l *user-stobj-alist*))
         (l (cond
             (stobj-pair
              (intern-in-package-of-symbol
               (stobj-print-name (car stobj-pair))
               (car stobj-pair)))
             (t ; consider local stobjs
              (or (and (arrayp l)
                       (stobj-print-symbol l *user-stobj-alist*))
                  l))))
         (pair (assoc-eq-trace-alist l *trace-alist*)))
    (cond (pair (cdr pair))
          ((atom l) l)
          ((eq l (w *the-live-state*))
           '|current-acl2-world|)
          ((rcnstp l) '|some-rcnst|)
          ((enabled-structurep l) '|some-enabled-structure|)
          ((and (consp l)
                (or (eq (car l) 'event-index)
                    (eq (car l) 'command-index))
                (consp (cdr l))
                (eq (car (cdr l)) 'global-value))
           (list* (car l) 'global-value '|some-index|))

#| I have been known to put this in here

          ((and (consp l)
                (consp (car l))
                (symbolp (car (car l)))
                (consp (cdr (car l)))
                (eq (car (cdr (car l))) 'global-value))
           '|some-other-world-perhaps|)
|#

          (t (cons (trace-hide-world-and-state (car l))
                   (trace-hide-world-and-state (cdr l)))))))

; The following would create warnings in MCL 4.2, presumably because this file
; is compiled in that Lisp; so we avoid it for MCL.  It was originally in
; acl2-init.lisp, but cmulisp warned that *open-output-channel-key*,
; print-idate, and idate were undefined.
#-(and mcl (not openmcl))
(defun-one-output saved-build-date-string ()
  (with-output-to-string
   (str)
   (setf (get 'tmp-channel *open-output-channel-key*)
         str)
   (print-idate (idate)
                'tmp-channel
                *the-live-state*)
   (remprop 'tmp-channel *open-output-channel-key*)
   str))

#+(or akcl openmcl)
(defun-one-output get-stobjs-out-for-declare-form (fn)

; This function is used in acl2-fns.lisp.

; Here we open-code stobjs-out, except that we allow for the possibility that
; fn is defined in raw Lisp.

  (if (eq fn 'cons)
; We call this function on cons so often we optimize it.
      '(nil)
    (let ((w (w *the-live-state*)))
      (or (getprop fn 'stobjs-out nil 'current-acl2-world w)
          (and (getprop fn 'symbol-class nil 'current-acl2-world w)
               '(nil))))))

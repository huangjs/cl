;;;-*- Mode: Lisp; Package: LISP-CRITIC -*-

#|
Copyright (c) 1997-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; Rules for the Lisp Critic.
;;; Author: Chris Riesbeck
;;; 
;;; Update history:
;;;
;;; 2/4/07 added DEFMACRO to SETS-GLOBALS [CKR]
;;; 2/15/06 added NEEDLESS-SHIFTF, rewrote COND-WITHOUT-DEFAULT [CKR]
;;; 2/13/06 added GREATER-DIFFERENCE-0 [CKR]
;;; 2/9/06 modified MAKE-PATHNAME-FILE critique [CKR]
;;; 2/8/06 added EXPORT-KEYWORD, RETURN-FROM-BLOCK-DEFUN [CKR]
;;; 2/4/06 added LAMBDA for IDENTITY [CKR]
;;; 2/4/06 fixed VERBOSE-GENERIC-VAR-NAME to exempt keywords [CKR]
;;; 2/2/06 expanded comment in EQL-ON-NUMBERS [CKR]
;;; 2/1/06 added SETF-INCF-VALUE and SETF-DECF-VALUE [CKR]
;;; 1/31/06 added CONS-CONS-LIST* [CKR]
;;; 1/25/06 added WHEN-IN-DO [CKR]
;;; 1/24/06 added COPY-ARRAY and FORMAT-CONSTANT-STRING, remove NULL-FOR-ENDP [CKR]
;;; 1/13/06 fixed LET-ATOMS format bug [CKR]
;;; 1/11/06 replaced RPLACD-OR-RPLACA with separate rules [CKR]
;;; 1/11/06 fixed comment in LENGTH=NUM [CKR]
;;; 1/11/06 added IF-NO-ELSE [CKR]
;;; 1/6/06 added TYPEP-PRIMITIVE [CKR]
;;; 1/6/06 added QUOTE-TRUE [CKR]
;;; 09/18/05 replaced Academic Free License with MIT Licence [CKR]
;;; 08/30/05 added license notice [CKR]
;;; 3/15/05 added NEEDLESS-COND, NEEDLESS-COND-NOT [CKR]
;;; 3/11/05 added NOT-ATOM [CKR]
;;; 3/7/05 added MULTIPLE-VALUE-LIST [CKR]
;;; 3/7/05 fixed LET-ATOMS to also catch (LET ((X)) ...) [CKR]
;;; 1/31/05 fixed NESTED-COND-ELSE-COND [CKR]
;;; 1/24/05 Added NESTED-AND-OR [CKR]
;;; 1/22/05 Edited various critique texts [CKR]
;;; 1/20/05 fixed USE-EQL to test for '(...) arguments [CKR]
;;; 1/20/05 fixed COND-INSTEAD-OF-CASE to only match EQL [CKR]
;;; 1/7/03 fixed X-PLUS-1 and X-MINUS-1 to ignore numbers [CKR]
;;; 1/5/03 fixed require/use-package code  [CKR]
;;; 1/3/03 merged DEFINE-LISP-PATTERN and DEFINE-RESPONSE [CKR]
;;; 3/9/02 added NEEDLESS-PUSH, fixed NEEDLESS-SETF [CKR]
;;; 2/7/02 changed NEEDLESS-AND, NEEDLESS-OR [CKR]
;;; 12/28/01 changed package to CS325-USER [CKR]
;;; 3/1/01 fixed AND -> ?AND in IF-FOR-NOT [CKR]
;;; 3/1/01 added missing ?NOT NIL to IF->OR and COND->OR [CKR]
;;; 2/23/01 added DEFMACRO to FUNCTION-TOO-LONG [CKR]
;;; 2/6/01 added DO*-SINGLE-VAR, IF->OR, COND->OR [CKR]
;;; 1/30/01 added VERBOSE-GENERIC-VAR-NAME [CKR]
;;; 1/29/01 added ?-FOR-PREDICATE [CKR]
;;; 1/22/01 changed LENGTH=NUM response to distinguish vectors from lists [CKR]
;;; 1/22/01 added top-level constraint to SETS-GLOBALS and NEEDLESS-SETF [CKR]
;;; 1/22/01 added UNUSED-MAPCAR-VALUE [CKR]
;;; 1/22/01 added DOLIST and DOTIMES to PROGN-IN-WHEN [CKR]
;;; 1/17/01 changed SETF-INCF and SETF-DECF to include SETQ [CKR]
;;; 1/15/01 added SETF-INCF and SETF-DECF [CKR]
;;; 1/15/01 added NULL-THEN-LISTP, modified LISTP-FOR-CONSP [CKR]
;;; 1/12/01 added NEEDLESS-AND-T and NEEDLESS-OR-NIL [CKR]
;;; 11/25/98 added CAR-CDR [CKR]
;;; 11/25/98 added LET to PROGN-IN-LAMBDA [CKR]
;;; 11/22/98 changed nested conditional handling to use
;;;          NESTED-COND-ELSE-COND and NESTED-COND-ELSE-COND [CKR]
;;; 11/11/98 added LET*-SINGLE [CKR]
;;; 10/28/98 added NESTED-ELSE-COND [CKR]
;;; 10/28/98 fixed DO-WITH-BODY to handle multiexpression DO-bodies [CKR]
;;; 10/28/98 added plurals to MISSPELLED-OCCURRENCE [CKR]
;;; 10/26/98 added APPEND-LIST2-LIST [CKR]
;;; 10/26/98 added PUSH to SETF-IN-DO-INC [CKR]
;;; 10/25/98 added PROGN-IN-WHEN, PROGN-IN-DEFUN, PROGN-IN-LAMBDA [CKR]
;;; 10/14/98 added more misspellings to MISSPELLED-OCCURRENCE [CKR]
;;; 10/14/98 generalized NESTED-IF's to check for COND's [CKR]
;;; 10/1/98 fixed QNIL (missing ?*'s) [CKR]
;;; 9/29/98 added PROGN-IN-COND, SETF-IN-DO-INC [CKR]
;;; 12/4/97 added QUOTE-KEYWORD [CKR]
;;; 12/2/97 added FLOOR-WITH-/ [CKR]
;;; 12/2/97 added keywords and longer message to the EOF rule [CKR]
;;; 12/2/97 added COND-ELSE-NO-EXP [CKR]
;;; 11/30/97 turned off QNIL inside DEFMACRO [CKR]
;;; 11/24/97 added APPLY-FOR-FUNCALL [CKR]
;;; 11/21/97 added = to LENGTH=NUM [CKR]
;;; 11/18/97 added LET-ATOMS [CKR]
;;; 11/18/97 fixed COND-INSTEAD-OF-CASE [CKR]
;;; 11/16/97 added FIND-MEMBER-FOR-ASSOC [CKR]
;;; 11/15/97 fixed responses for WHEN-FOR-UNLESS [CKR]
;;; 11/14/97 partially fixed COND-INSTEAD-OF-CASE [CKR]
;;; 11/14/97 added several IF and COND patterns [CKR]
;;; 11/14/97 removed erroneous mode line [CKR]
;;; 11/11/97 fixed X-PLUS-1 to do (+ 1 X) [CKR]
;;; 11/7/97 fixed EQL-WITH-NULL and APPEND-LIST-LOOP [CKR]
;;; 11/4/97 fixed 2 EOF rules with same name [CKR]
;;; 10/17/97 fixed progn-in-if pattern [CKR]
;;; 10/17/97 made do-with-body and do-setf smarter [CKR]
;;; 10/14/97 added append-list-list and nested-defuns [CKR]
;;; 10/11/97 added append-list-recursion [CKR]
;;; 10/2/97 added require and in-package [CKR]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cs325-user)

(require "lisp-critic")
(use-package '#:lisp-critic)


;;; Should add:
;;;   Catch (format t "..." arg) where "..." takes no arguments
;;;   Catch all nested conditional combinations between IF and COND

(define-lisp-pattern copy-array
    (copy-array (?))
  "There is no COPY-ARRAY in Standard Common Lisp. You have to create ~
   an empty array and fill it as needed.")

(define-lisp-pattern function-too-long
  (?and  ((?or defun defmacro) (?*)) (?too-long how-much))
  "Definition is ~A too long! A \"little\" is probably OK, \"somewhat\" ~
   might be OK, if this is a really complicated problem, but code that is ~
   \"too long\" or \"way too long\" can almost certainly be improved."
  (? how-much))

(define-lisp-pattern nested-defuns
 (defun (?*)
   (?contains (defun (?*)))
   (?*))
  "DEFUN's don't nest in CL like they do in Scheme. 
   They're always top-level. FLET and LABELS can define local 
   functions, but you don't need them here.")

(define-lisp-pattern sets-globals
 (?top-level ((?or defun defmacro) (?*)) (?sets-free-vars vars))
  "GLOBALS!! Don't use global variables, i.e.,~{ ~S~}"
  (? vars))

(define-lisp-pattern sets-parameters
 (defun (?) ((?*) (? var) (?*))
   (?*)
   (?contains ((?or setq setf incf decf) (? var) (?*)))
   (?*))
  "It's bad style to reassign input parameters like ~S ~
   -- and often useless."
  (? var))

(define-lisp-pattern eql-on-numbers
 (?and ((?eql-pred eql) (? arg1) (? arg2))
  (?or (?match (?is numberp) (? arg1))
   (?match (?is numberp) (? arg2))))
  "Don't use (~S ~S ~S) to compare numbers, use =. = handles floating point numbers ~
   correctly, and signals an error if passed a non-number."
  (? eql) (? arg1) (? arg2))

(define-lisp-pattern equal-with-nil
 (?and ((?eql-pred eql) (? arg1) (? arg2))
  (?or (?match (? arg1) nil) (?match (? arg2) nil)))
  "Don't use ~S to compare ~S with ~S. Use NULL"
  (? eql) (? arg1) (? arg2))

(define-lisp-pattern typep-primitive
    (typep (?) (quote (?or integer number string cons atom)))
  "For basic types, specific predicates, such as integerp and so on, are simpler than typep.")

(define-lisp-pattern cond-without-default
 (cond (?*) ((?not t) (?*)))
  "If the return value of a COND is being used, then be sure to ~
   have an ELSE branch, i.e., (T ...).")

(define-lisp-pattern cond-test-no-exp
 (cond (?*) ((? test)) (?) (?*))
  "Try to avoid COND branches with tests and no actions. ~
They're easy to misread. Try (OR ~S ...) instead."
  (? test))

(define-lisp-pattern cond-else-no-exp
 (cond (?*) ((? test)))
  "Try to avoid COND branches with tests and no actions. ~
They're easy to misread. Try (T ~S) instead."
  (? test))


(define-lisp-pattern setq-in-do
 (do ((?*) ((? var) (? init)) (?*))
     (?)
   (?contains (setq (? var) (? incr))))
  "Most SETQ's of a DO variable, like ~S, are better done ~
with (DO (... (~S ~S ~S)...) ...)"
  (? var) (? var) (? init) (? incr))

(define-lisp-pattern setf-in-do-inc
 (do ((?*)
      ((? var) (?) ((?and (?or setq setf incf decf push) (? setf))
                    (?*)))
      (?*))
     (?*))
  "~S is unnecessary in the increment part of a DO clause. Why? What should ~
you write?"
  (? setf))

(define-lisp-pattern if-no-else
    (if (?) (?))
  "You have an IF with no else branch. If the return value of the IF ~
matters, you should explicitly say what the else returns, e.g., NIL. ~
If the return value doesn't matter, use WHEN or UNLESS.")

(define-lisp-pattern nested-and-or 
    ((?and (?or and or) (? fn)) (?*) ((? fn) (?*)) (?*))
  "Why nest an ~S inside an ~S?" (? fn) (? fn))

(define-lisp-pattern nested-ifs
 (if (?) ((?or cond if) (?*)) (?*))
  "Avoid nested IF's. Use AND, if possible, or a single COND.")

(define-lisp-pattern nested-cond-else-cond
 (cond (?*) (t ((?and (?or cond if) (? form)) (?*))))
  "Why nest a ~S in the ELSE branch of a COND when one flat COND ~
   will work instead?"
  (? form))

(define-lisp-pattern nested-if-else-cond
 (if (?) (?) ((?and (?or cond if) (? form)) (?*)))
  "Why nest a ~S in the ELSE branch of an IF when one flat COND ~
   will work instead?"
  (? form))

(define-lisp-pattern needless-cond
    (cond ((?) t) (t nil))
  "There's an unnecessary COND here.")

(define-lisp-pattern needless-cond-not
    (cond ((?) nil) (t t))
  "There's a COND here that can be replaced with something simpler (not IF).")

(define-lisp-pattern needless-if
 (?and (?or (if (?) t nil) (if (?) t)) (? form))
  "~S is silly. Use either just the test, or (NOT (NULL ...)) if you want T instead of not NIL."
  (? form))

(define-lisp-pattern if->or
 (if (?) t (?not nil))
  "Instead of (IF test T else), just write (OR test else)"
  )

(define-lisp-pattern cond->or
 (cond ((?) t) (t (?not nil)))
  "Instead of (COND (test T) (T else)), just write (OR test else)"
  )

(define-lisp-pattern needless-and-t
 (and (?*) t)
  "Why do you think you need that T at the end of the AND?")

(define-lisp-pattern needless-or-nil
 (or (?*) nil)
  "Why do you think you need that NIL at the end of the OR?")

(define-lisp-pattern needless-and
 (and (?))
  "Why do you think you need that AND?")

(define-lisp-pattern needless-or
 (or (?))
  "Why do you think you need that OR?")

(define-lisp-pattern if-for-not
 (?and (if (?) nil t) (? form))
  "No need for ~S. Just use (NOT ...))."
  (? form))

(define-lisp-pattern if-for-unless
 (if (not (? test)) (?))
  "Instead of (IF (NOT ~S) ...) use (UNLESS ~S ...)."
  (? test) (? test))

(define-lisp-pattern progn-in-if
 (if (? test) (?*) (progn (?*)) (?*))
  "Don't use IF and PROGN, use COND")

(define-lisp-pattern progn-in-cond
 (cond (?*) ((? test) (?*) (progn (?*)) (?*)) (?*))
  "You never need a PROGN after the test in a COND branch.")

(define-lisp-pattern progn-in-defun
 ((?and (?or defun defmacro) (? fn)) (?) (?) (progn (?*)))
  "You never need a PROGN at the start of the body of a ~S." (? FN))

(define-lisp-pattern progn-in-lambda
 ((?and (?or lambda let) (? fn)) (?) (progn (?*)))
  "You never need a PROGN at the start of the body of a ~S."
  (? fn))

(define-lisp-pattern progn-in-when
 ((?and (?or when unless dotimes dolist) (? fn)) (?) (progn (?*)))
  "You never need a PROGN at the start of the body of a ~S" (? FN))

(define-lisp-pattern setf-push
 (setf (? x) (cons (? y) (? x)))
  "Instead of (SETF ~S (CONS ...)), use PUSH."
  (? x))

(define-lisp-pattern cond-instead-of-case
 (cond (?repeat ((eql (? var) '(?)) (?*)) 2)
       (?optional (t (?*))))
  "Don't use COND with repeated (EQL ~S ...) branches, use CASE."
  (? var))

(define-lisp-pattern lambda-for-identity
    (lambda ((? x)) (? x))
  "You don't need that LAMBDA. Common Lisp has a function that does the same thing.")

(define-lisp-pattern let-atoms
 (let ((?*) (?or (?and (?is atom) (? var)) ((? var))) (?*)) (?*))
  "Always initialize LET variables like ~S with (~S NIL), not just ~S or (~S). It's too ~
easy to misread what's being initialized to what."
  (? var) (? var) (? var) (? var))

(define-lisp-pattern let*-single
 (let* ((?)) (?*))
  "There's no need for LET* here. Use LET unless you can't.")

(define-lisp-pattern do-with-body
 (do (?)
     (?)
   (?*)
   (?contains
    (?or ((?and (?or setq setf incf decf) (? fn)) 
          (?and (?is symbolp) (? var))
          (?*))
     ((?and push (? fn)) (?) (and (?is symbolp) (? var)))))
   (?*))
  "Don't use a DO body to collect values. 
Incorporate the body into the DO variable update list.")

(define-lisp-pattern do*-single-var
 (do* ((?)) (?*))
  "DO* says \"later variable clauses depend on earlier ones.\" Clearly that's not true here so use DO."
  )

(define-lisp-pattern format-constant-string
    (format t (?or "~A" "~a") (?is stringp))
  "There's a simpler way to print a constant string.")

(define-lisp-pattern qnil
 (defun (?) (?) (?*) (?contains '(?and (?or t nil) (? const))) (?*))
  "Don't quote ~S.  ~:*~S is a constant and doesn't need quoting. [If you ~
wrote '() to initialize a list, that's OK. It's impossible for the ~
Lisp Critic to distinguish '() from 'NIL internally.]"
  (? const))

(define-lisp-pattern qnumber
 '(?and (?is numberp) (? n))
  "Don't quote numbers like ~S. Numbers are constants and don't need quoting."
  (? n))

(define-lisp-pattern quote-keyword
 '(?and (?is keywordp) (? key))
  "Don't quote keywords like ~S. Keywords are constants and don't need quoting."
  (? key))

(define-lisp-pattern using-print
 ((?or princ prin1 print) (?*))
  "In general, FORMAT is used for most printing, because it's more flexible.")

(define-lisp-pattern car-cdr
 (car (cdr (?)))
  "Use CADR (or CADDR or ...) or SECOND (or THIRD or ...), not (CAR (CDR ...)).")

(define-lisp-pattern nth-for-cdr
 (nth 1 (?))
  "(NTH 1 ...) is clumsy. Use REST or CDR.")

(define-lisp-pattern nth-for-cdr
 (nth (?not (?is numberp)) lst)
  "(NTH ...) is expensive. Lists are not arrays.~%~
   Hint: use FIRST, REST, and/or a pointer to access elements of a list")

(define-lisp-pattern dolist-setf
 (dolist (?)
   (?contains
    (?or ((?and (?or setq setf incf decf) (? fn)) 
          (?and (?is symbolp) (? var))
          (?*))
     ((?and push (? fn)) (?) (and (?is symbolp) (? var))))))
  "Don't use ~S inside DOLIST to accumulate values for ~S.~%~
   Use DO. Make ~S a DO variable and don't use SETQ etc at all."
  (? fn) (? var) (? var))

(define-lisp-pattern substitute-use
 (substitute (?*))
  "Because SUBSTITUTE creates new lists, it is expensive and used rarely.")

(define-lisp-pattern rplaca
    (rplaca (? var) (?))
  "Instead of (RPLACA ~S ...) use (SETF (CAR ~S) ...)."
  (? var) (? var))

(define-lisp-pattern rplacd
    (rplacd (? var) (?))
  "Instead of (RPLACD ~S ...) use (SETF (CDR ~S) ...)."
  (? var) (? var))

(define-lisp-pattern misspelled-occurrence
 (?or occurance occurrance occurence occurances occurrances occurences)
  "You must be a real computer scientist. None of them can spell occurrence.")

(define-lisp-pattern cons-with-nil
 (cons (? x) nil)
  "~S is silly -- ~
     what's the right way to make a list of ~S?"
  (cons (? x) nil) (? x))

(define-lisp-pattern append-list-loop
 (do ((?*) 
      ((?) (?) (?contains (append (? x) (list . (? y))))) 
      (?*))
     (?*) )
  "Avoid ~S in loops. It takes N squared CONSes to build a list ~
     N long when only N are needed.~%~
     Hint: build ~S backwards with CONS and then REVERSE."
  (append (? x) (list . (? y))) (? x))

(define-lisp-pattern append-list-recursion
 (defun (? fn) (?)
        (?*)
        (?and (?contains (? fn))
              (?contains (append (? x) (list (? y)))))
        (?*))
  "Avoid ~S in recursive loops. It takes N squared CONSes to build a list ~
     N long when only N are needed."
  (append (? x) (list (? y))))

(define-lisp-pattern append-list-list
 (append (list (? x)) (? y))
  "~S is silly. What's the right way to add something
   to the front of a list?"
  (append (list (? x)) (? y)))

(define-lisp-pattern append-list2-list
 (append (list (? x) (?) (?*)) (? y))
  "(APPEND (LIST ~S ...) ~S) is inefficient. It makes a list then copies it. ~
Instead, do (CONS ~S (CONS ... ~S)) or (LIST* ~S ... ~S)."
  (? x) (? y) (? x) (? y) (? x) (? y))

(define-lisp-pattern cons-cons-list*
    (cons (?) (cons (?) (?)))
  "When you have nested CONSes, it might be simpler to use LIST*.")

(define-lisp-pattern use-eql
 ((?and (?or equal equalp eq) (? equal)) (?not '((?*))) (?not '((?*))))
  "Unless something special is going on, use EQL, not ~S."
  (? equal))

(define-lisp-pattern x-plus-1
 (?or (+ (?and (?not (?is numberp)) (? var)) 1) (+ 1 (?and (?not (?is numberp)) (? var))))
  "Don't use ~S, use ~S for its value or ~S to change ~S, ~
   whichever is appropriate here."
  (+ (? var) 1) (1+ (? var)) (incf (? var)) (? var))

(define-lisp-pattern x-minus-1
  (- (?and (?not (?is numberp)) (? var)) 1)
  "Don't use ~S, use ~S for its value or ~S to change ~S, ~
   whichever is appropriate here."
  (- (? var) 1) (1- (? var)) (decf (? var)) (? var))

(define-lisp-pattern floor-with-/
  (floor (/ (?) (?)))
  "You don't need FLOOR and /. FLOOR with two arguments does a divide already.")

(define-lisp-pattern quote-false
 'false
  "Don't use 'FALSE for NIL. Believe it or not, 'FALSE is true in Lisp!")

(define-lisp-pattern quote-true
 'true
  "Don't use 'TRUE for true. Just use T (lowercased). That's the normal default true value.")

(define-lisp-pattern return-done
 'done
  "Don't return 'DONE. Return values aren't for people, they're ~
   for Lisp code, and DONE means nothing to Lisp code. Just return T ~
   or NIL.")

(define-lisp-pattern apply-for-funcall
 (apply (? fn) (list (?*)))
  "(APPLY ~S (LIST ...)) makes a list for no reason. How can you call ~
~S with those arguments directly?"
  (? fn) (? fn))

(define-lisp-pattern optionals
 (defun (? fun-name) ((?*) &optional (?) (?) (?*)) (?*))
  "Multiple optional arguments get confusing. Use &KEY for ~S."
  (? fun-name))

(define-lisp-pattern length=num
    (?or 
     ((?and (?or (?eql-pred) = < <= > >=) (? pred))
      (length (? exp))
      (?and (?is numberp) (? n)))
     ((?and (?or (?eql-pred) = < <= > >=) (? pred))
      (?and (?is numberp) (? n))
      (length (? exp)))
      )
  "If ~S is a list, not a vector, don't use ~S and LENGTH. LENGTH has to ~
   CDR down the entire list. Use (NULL (CDR ...)) with the appropriate number of ~
   CDR's. That will run in constant time, independent of list length."
  (? exp) (? pred))

(define-lisp-pattern null-then-listp
 (cond (?*)
       ((null (? exp)) (?*))
       (?*)
       ((listp (? exp)) (?*))
       (?*))
  "Testing for LISTP ~S after testing for NULL ~S is like testing ~
   for X <= Y after testing for X = Y. It's redundant and misleading."
  (? exp) (? exp))

(define-lisp-pattern listp-for-consp
 (cond (?*)
       ((listp (? exp))
        (?*)
        (?contains ((?and (?or car first cdr rest) (? fn)) (? exp)))
        (?*))
       (?*))
  "LISTP is not sufficient to guarantee that ~S of ~S is legal."
  (? fn) (? exp))

(define-lisp-pattern when-in-do
    (do ((?*) ((?) (?) ((?or when unless) (?*))) (?*)) (?*))
  "WHEN and UNLESS are not appropriate when the return value is needed. ~
   WHEN and UNLESS are for when you conditionally want to do some actions.")

(define-lisp-pattern when-for-unless
 (when (not (? x)) (?*))
  "(UNLESS ~S ...) is better than (WHEN (NOT ~S) ...)."
  (? x) (? x))

(define-lisp-pattern unless-for-when
 (unless (not (? x)) (?*))
  "(WHEN ~S ...) is better than (UNLESS (NOT ~S) ...)."
  (? x) (? x))

(define-lisp-pattern find-member-for-assoc
 ((?and (?or find member) (? fn)) (?*) :key (?or 'car #'car))
  "When working with lists of pairs, use ASSOC, not ~S and CAR. That's what ~
ASSOC was built for!"
  (? fn))

(define-lisp-pattern constant-bad-eof
 (read (?) nil (?and (?or nil t '(?) (?is keywordp)) (? eof)) (?*))
  "~S is a bad end of file marker. Any constant expression might be ~
in the file and cause a premature exit. You need to generate a unique marker ~
at run-time."
  (? eof))

(define-lisp-pattern constant-bad-eof-var
 (let ((?*) ((? var) (?and (?or nil t '(?)) (? eof))) (?*))
   (?contains (read (?) nil (? var) (?*))))
  "~S is a bad end of file marker (and being in a variable doesn't ~
   help any). What if ~:*~S is in the file?"
  (? eof))

(define-lisp-pattern uses-open
 (open (?*))
  "Don't use OPEN. If an error occurs, the file will never be ~
   closed. Use WITH-OPEN-FILE -- simpler and safer.")

(define-lisp-pattern with-open-close
 (with-open-file ((? stream) (?*)) (?contains (close (? stream))))
  "Don't close a stream open with WITH-OPEN-FILE. That will be ~
   done automatically.")

(define-lisp-pattern make-pathname-file
 (defun (?) ((?*) (? file) (?*))
   (?contains (make-pathname :name (? file))))
  "Careful! (MAKE-PATHNAME :NAME ~S ...) is only correct if ~S ~
is just the file name and you're constructing a full path. If ~S can be ~
a full pathname, like c:/foo/baz.lisp, ~
MAKE-PATHNAME may create file with those characters in its name! Normally ~
you should just use the pathname passed in."
  (? file) (? file) (? file))

(define-lisp-pattern evil-eval
 (eval (?*))
  "EVAL may not always be evil, but it's almost certainly not the best answer.")

(define-lisp-pattern uses-prog
 (prog (?*))
  "PROG is obsolete. There are other ways to code.")

(define-lisp-pattern needless-setf
 (?top-level
  (defun (?*)
    (?or ((?and (?or setf incf decf) (? fn)) 
          (?and (?is symbolp) (? var))
          (?*))
         (cond (?*)
               ((?*) ((?and (?or setf incf decf) (? fn)) 
                      (?and (?is symbolp) (? var))
                      (?*)))
               (?*))
         (if (?)
           (?*)
           ((?and (?or setf incf decf) (? fn)) 
            (?and (?is symbolp) (? var))
            (?*))
           (?*))
         (let (?)
           (?*)
           (?or ((?and (?or setf incf decf) (? fn))
                 (?and (?is symbolp) (? var))
                 (?*))
                (cond (?*)
                      ((?*) ((?and (?or setf incf decf) (? fn)) 
                             (?and (?is symbolp) (? var))
                             (?*)))
                      (?*))
                (if (?)
                  (?*)
                  ((?and (?or setf incf decf) (? fn)) 
                   (?and (?is symbolp) (? var))
                   (?*))
                  (?*))))
         )
    ))
  "Why do you think you need that ~S on ~S?" (? FN) (? VAR))

(define-lisp-pattern needless-push
 (?top-level
  (defun (?*)
    (?or ((?and (?or push pushnew) (? fn)) 
          (?)
          (?and (?is symbolp) (? var)))
         (cond (?*)
               ((?*) ((?and (?or push pushnew) (? fn)) 
                      (?)
                      (?and (?is symbolp) (? var))))
               (?*))
         (if (?)
           (?*)
           ((?and (?or push pushnew) (? fn)) 
            (?)
            (?and (?is symbolp) (? var)))
           (?*))
         (let (?)
           (?*)
           (?or ((?and (?or push pushnew) (? fn)) 
                 (?)
                 (?and (?is symbolp) (? var)))
                (cond (?*)
                      ((?*) ((?and (?or push pushnew) (? fn)) 
                             (?)
                             (?and (?is symbolp) (? var))))
                      (?*))
                (if (?)
                  (?*)
                  ((?and (?or push pushnew) (? fn)) 
                   (?)
                   (?and (?is symbolp) (? var)))
                  (?*))))
         )
    ))
  "Why do you think you need that ~S on ~S?" (? FN) (? VAR))

(define-lisp-pattern needless-shiftf
  (shiftf (?) (?))
  "There's no need for SHIFTF when there's just 2 arguments.")

(define-lisp-pattern setf-incf
  ((?and (?or setq setf) (? fn)) (? exp) (1+ (? exp)))
  "Instead of ~S plus 1+, just use INCF." (? FN))

(define-lisp-pattern setf-decf
  ((?and (?or setq setf) (? fn)) (? exp) (1- (? exp)))
  "Instead of ~S plus 1-, just use DECF." (? FN))

(define-lisp-pattern setf-incf-value
    ((?and (?or setq setf) (? fn)) (? var)
     (?or (+ (? var) (? val)) (+ (? val) (? var))))
  "INCF would be simpler to add ~S to ~S than ~S" (? val) (? var) (? FN))

(define-lisp-pattern setf-decf-value
    ((?and (?or setq setf) (? fn)) (? var)
     (- (? var) (? val)))
  "DECF would be simpler to subtract ~S from ~S than ~S" (? val) (? var) (? FN))

    

;;; Name related critiques

(define-lisp-pattern verbose-generic-var-name
    ((?or defmacro defmethod defun) (?) (?optional (?))
     ((?*) (?and (?or number input-list output-list data) (? var)) (?*))
     (?*))
  "~S is very generic. If this variable really is that generic, ~
   use a standard short name like L for a list, I for a counter, ~
   N for integer, and X and Y for real numbers. If this variable ~
   holds a value with more semantics, use a more specific name."
  (? var))

(define-lisp-pattern misspells-occurrences
  (?or (?name-contains "occurances")
       (?name-contains "occurences"))
  "You misspelled 'occurrences.'")

(define-lisp-pattern ?-for-predicate
  (?name-ends-with "?")
  "In Common Lisp, use -p to end predicate names, not ? as in Scheme.'")

(define-lisp-pattern unused-mapcar-value
  (let (?*) (mapcar (?*)) (?) (?*))
  "MAPCAR is building a list that's not used. Use MAPC instead.")

(define-lisp-pattern multiple-value-list
    (multiple-value-list (?))
  "Multiple values were invented to avoid the need to cons lists. ~
   So most uses of multiple-value-list to make a list are a mistake. ~
   Use multiple-value-bind (or possibly multiple-value-setq) instead. ~
   They do not cons.")

(define-lisp-pattern not-atom
    (not (atom (?)))
  "Instead of (NOT (ATOM ...)) just write (CONSP ...).")

(define-lisp-pattern export-keyword
    (:export (?is keywordp) (?*))
  "Don't specify symbols to export using keywords. This puts copies of those symbols ~
   into the keyword package as well, wasting space.")

(define-lisp-pattern return-from-block-defun
    (defun (? fn) (?)
      (?contains (block (? label)
                   (?*) (?contains (return-from (? label) (?))) (?*))))
  "It may be simpler to just RETURN-FROM ~S directly, rather than creating ~
   nested BLOCK." (? fn))

(define-lisp-pattern greater-difference-0
    (> (- (? x) (? y)) 0)
  "Instead of (> (- x y) 0), just write (> x y).")

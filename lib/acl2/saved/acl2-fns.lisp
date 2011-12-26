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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            SUPPORT FOR NON-STANDARD ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Amazingly, some lisps do not have a definition for realp.  In those
; implementations (apparently including at least one early version of GCL), we
; will use rationalp as a poor substitute which however suffices for ACL2
; objects.

#+:non-standard-analysis
(defun acl2-realp (x)                                                   
  (rationalp x))                                                        
                                                                        
#+(and :non-standard-analysis CLTL2)
(if (not (fboundp 'common-lisp::realp))
    (setf (symbol-function 'common-lisp::realp) (symbol-function 'acl2-realp)))

#+(and :non-standard-analysis (not CLTL2))
(if (not (fboundp 'lisp::realp))
    (setf (symbol-function 'lisp::realp) (symbol-function 'acl2-realp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            PROCLAIMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defun-one-output (&rest args)

; Use this for raw Lisp functions that are known to return a single value in
; raw Lisp, since make-defun-declare-form uses that assumption to make an
; appropriate declaration.

  (cons 'defun args))

(defun macroexpand-till (form sym)

; In order to find the THEs that we want to find to do automatic
; proclaiming of the output types of functions, we need to do
; macroexpansion at proclaim-time.  It is possible that a given
; implementation of Common Lisp will macroexpand THE forms further.
; Hence we gently do the macroexpansion we need, one expansion at a
; time, looking for the THE we want to find.

  (loop (cond ((and (consp form) (eq (car form) sym))
               (return form))
              (t (multiple-value-bind
                  (new-form flg)
                  (macroexpand-1 form)
                  (cond ((null flg) (return form))
                        (t (setq form new-form))))))))

(defun get-type-from-dcls (var dcls)
  (cond ((endp dcls) t)
        ((and (consp (car dcls))
              (eq 'type (caar dcls))
              (member var (cddr (car dcls))))
         (cadr (car dcls)))
        (t (get-type-from-dcls var (cdr dcls)))))

(defun arg-declarations (formals dcls)
  (cond ((endp formals) nil)
        (t (cons (get-type-from-dcls (car formals) dcls)
                 (arg-declarations (cdr formals) dcls)))))

(defun collect-types (l)
  (cond ((null (cdr l)) nil)
        ((stringp (car l))
         (collect-types (cdr l)))
        ((consp (car l))
         (let ((exp (car l)))
           (cond ((and (consp exp) (eq (car exp) 'declare))
                  (append (cdr exp) (collect-types (cdr l))))
                 (t nil))))
        (t nil)))

#+(or akcl openmcl)
(defun convert-type-to-integer-pair (typ)

; Typ is (integer i j), (signed-byte i), or (unsigned-byte i).  We return an
; equivalent type (integer i' j').

  (case (car typ)
    (integer (cdr typ))
    (signed-byte (let ((n (expt 2 (1- (cadr typ)))))
                   (list (- n) (1- n))))
    (unsigned-byte (list 0 (1- (expt 2 (cadr typ)))))
    (t (error
        "Unexpected type for convert-to-integer-type ~s"
        typ))))

#+(or akcl openmcl)
(defconstant *lisp-fns-with-mv-output-type*

; We have used the following form in GCL to compute the list of functions that
; return multiple values.  Presumably this is the correct list for the main
; Lisp package in other Common Lisp implementations as well.

; (let (ans)
;   (do-symbols (sym (find-package "LISP"))
;               (let ((tp (get sym 'compiler::return-type)))
;                 (when (and (consp tp) (eq (car tp) 'values))
;                   (setq ans (cons sym ans)))))
;   ans)

  '(INTERN DECODE-FLOAT READ-LINE MACROEXPAND ROUND GETHASH CEILING
           MACROEXPAND-1 INTEGER-DECODE-FLOAT TRUNCATE FIND-SYMBOL FLOOR))

#+(or akcl openmcl)
(defvar *acl2-output-type-abort* nil)

#+(or akcl openmcl)
(defun max-output-type-for-declare-form (type1 type2)

; We return a supertype of type1 and type2, preferably as small as possible,
; else nil.  We assume that each typei that is not null is (values ...) or is
; some sort of integer type.

  (cond
   ((equal type1 type2)
    type1)
   ((or (eq type1 '*)
        (eq type2 '*))
    '*)
   #+acl2-mv-as-values
   ((not (equal (and (consp type1)
                     (eq (car type1) 'values))
                (and (consp type2)
                     (eq (car type2) 'values))))
    '*)
   ((and (or (eq type1 'integer)
             (and (consp type1)
                  (eq (car type1) 'integer)
                  (or (null (cddr type1))
                      (member '* (cdr type1) :test 'eq))))
         (or (eq type2 'integer)
             (and (consp type2)
                  (eq (car type2) 'integer)
                  (or (null (cddr type2))
                      (member '* (cdr type2) :test 'eq)))))
    'integer)
   ((or (atom type1) (atom type2)) ; so, type is t since neither is *
    t)
   ((cdr (last type1)) ; (not (true-listp type1))
    (error
     "Non-atom, non-true-listp type for max-output-type-for-declare-form: ~s"
     type1))
   ((cdr (last type2)) ; (not (true-listp type2))
    (error
     "Non-atom, non-true-listp type for max-output-type-for-declare-form: ~s"
     type2))
   (t (let ((sym1 (car type1))
            (sym2 (car type2)))
        (cond
         ((eq sym1 sym2)
          (case sym1
            ((signed-byte unsigned-byte)
             (if (< (cadr type1) (cadr type2))
                 type2
               type1))
            (integer
             (list 'integer
                   (min  (cadr type1)  (cadr type2))
                   (max (caddr type1) (caddr type2))))
            #+acl2-mv-as-values
            (values
             (cons 'values (max-output-type-for-declare-form-lst (cdr type1)
                                                                 (cdr type2))))
            (otherwise
             (error
              "Unexpected type for max-output-type-for-declare-form: ~s"
              type1))))
         #+acl2-mv-as-values
         ((or (eq sym1 'values) (eq sym2 'values)) ; but not both
          '*)
         (t (let* ((pair1 (convert-type-to-integer-pair type1))
                   (pair2 (convert-type-to-integer-pair type2))
                   (lower1 (car pair1))
                   (upper1 (cadr pair1))
                   (lower2 (car pair2))
                   (upper2 (cadr pair2)))
              (cond
               ((and (<= lower1 lower2) (>= upper1 upper2))
                type1)
               ((and (>= lower1 lower2) (<= upper1 upper2))
                type2)
               (t
                (list 'integer
                      (min lower1 lower2)
                      (max upper1 upper2)))))))))))

#+(and (or akcl openmcl) acl2-mv-as-values)
(defun max-output-type-for-declare-form-lst (type-list1 type-list2)

; Type-list1 and type-list2 are known to be true lists (null-terminated
; lists).

  (cond ((or (null type-list1) (null type-list2))
         (cond
          ((and (null type-list1) (null type-list2))
           nil)
          ((and *acl2-output-type-abort*
                (or (atom type-list1) (atom type-list2)))
           (cons '*
                 (max-output-type-for-declare-form-lst
                  (cdr type-list1) (cdr type-list2))))
          (t
           (error "Implementation error:~%~
                   max-output-type-for-declare-form-lst called on lists of~%~
                   different length:~%~
                   ~s~%  ~s~%~
                   Please contact the ACL2 implementors."
                  type-list1 type-list2))))
        (t (cons (max-output-type-for-declare-form
                  (car type-list1) (car type-list2))
                 (max-output-type-for-declare-form-lst
                  (cdr type-list1) (cdr type-list2))))))

#+(or akcl openmcl)
(defun output-type-for-declare-form-rec (form)

; We return either nil or else an integer type for form, preferably as small as
; possible.

  (cond
   ((integerp form)
    `(integer ,form ,form))
   ((atom form)
    t)
   ((eq (car form) 'the)
    (let ((typ (cadr form)))
      (cond ((member typ '(integer fixnum) :test 'eq)
             typ)
            ((and (consp typ)
                  (member (car typ)
                          '(integer signed-byte unsigned-byte
                                    #+acl2-mv-as-values
                                    values)
                          :test 'eq))
             typ)
            (t t))))
   ((eq (car form) 'if)
    (cond
     ((eq (cadr form) t) ; as generated for final cond branch in OpenMCL
      (output-type-for-declare-form-rec (caddr form)))
     ((eq (cadr form) nil) ; perhaps not necessary
      (output-type-for-declare-form-rec (cadddr form)))
     (t (let ((type1 (output-type-for-declare-form-rec (caddr form))))
          (if (eq type1 '*) ; optimization
              '*
            (max-output-type-for-declare-form
             type1
             (output-type-for-declare-form-rec (cadddr form))))))))
   #+acl2-mv-as-values
   ((eq (car form) 'values)
    (cond ((null (cddr form)) ; e.g., from (cond (a))
           (output-type-for-declare-form-rec (cadr form)))
          (t
           (cons 'values (output-type-for-declare-form-rec-list (cdr form))))))
   ((member (car form) '(let let*) :test 'eq)
    (cond ((cddr form)
           (output-type-for-declare-form-rec (car (last form))))
          (t t)))
   #+acl2-mv-as-values
   ((eq (car form) 'multiple-value-bind)
    (cond ((cdddr form)
           (output-type-for-declare-form-rec (car (last form))))
          (t t)))
   ((member (car form) '(time progn) :test 'eq)
    (output-type-for-declare-form-rec (car (last form))))
   ((member (car form)
            '(tagbody ; e.g., ld-fn
              throw-raw-ev-fncall ; e.g., from defchoose
              )
            :test 'eq)
    (setq *acl2-output-type-abort* '*))
   (t (multiple-value-bind
       (new-form flg)
       (macroexpand-1 form)
       (cond ((null flg)
              (cond ((atom form) t)
                    ((eq (car form) 'multiple-value-prog1)
                     (and (consp (cdr form))
                          (output-type-for-declare-form-rec (cadr form))))
; Note: We don't expect multiple-value-setq to show up in ACL2.
                    ((and (consp (car form))
                          (eq (caar form) 'lambda))
                     (output-type-for-declare-form-rec (caddr (car form))))
                    ((or (not (symbolp (car form))) ; should always be false
                         (member (car form)
                                 *lisp-fns-with-mv-output-type*
                                 :test 'eq))
                     '*)
                    #-acl2-mv-as-values
                    (t t)
                    #+acl2-mv-as-values
                    (t (let ((x (and (eval '(f-get-global 'current-acl2-world
                                                          *the-live-state*))
                                     (funcall 'get-stobjs-out-for-declare-form
                                              (car form)))))
                         (cond ((consp (cdr x))
                                (cons 'values
                                      (make-list (length x)
                                                 :initial-element
                                                 t)))
                               (t t))))))
             (t (output-type-for-declare-form-rec new-form)))))))
       
#+(and (or akcl openmcl) acl2-mv-as-values)
(defun output-type-for-declare-form-rec-list (forms)
  (cond ((atom forms)
         nil)
        (t (cons (let ((tp (output-type-for-declare-form-rec (car forms))))
                   (if (member tp '(nil *) :test 'eq)
                       t
                     tp))
                 (output-type-for-declare-form-rec-list (cdr forms))))))

#+(or akcl openmcl)
(defun output-type-for-declare-form (fn form)

; We return a list of output types, one per value.  So if #-acl2-mv-as-values,
; then we always return a list of length one.

  #-acl2-mv-as-values
  (declare (ignore fn))
  #-acl2-mv-as-values
  (list 'values (output-type-for-declare-form-rec form))
  #+acl2-mv-as-values
  (let* ((*acl2-output-type-abort* nil) ; protect for call on next line
         (result (output-type-for-declare-form-rec form))
         (stobjs-out (and (eval '(f-get-global 'current-acl2-world
                                               *the-live-state*))
                          (funcall 'get-stobjs-out-for-declare-form fn))))
    (when (and stobjs-out
               (not (eq (and (consp result)
                             (eq (car result) 'values))
                        (consp (cdr stobjs-out))))
               (not *acl2-output-type-abort*))
      (error "Implementation error in ~s:~%~
              stobjs-out and result don't mesh.~%~
              Stobjs-out = ~s~%~
              Result = ~s~%~
              Please contact the ACL2 implementors."
             (list 'output-type-for-declare-form fn '|defun...|)
             stobjs-out result))
    (cond
     ((and (consp result)
           (eq (car result) 'values))
      result)
     (*acl2-output-type-abort*
      '*)
     (t
      (list 'values result)))))

(defun make-defun-declare-form (fn form
                                   &optional
                                   (output-type nil output-type-p))

; See the comment in proclaim-file for why we don't proclaim in more lisps.

  #-(or akcl openmcl) (declare (ignore fn form output-type output-type-p))
  #-(or akcl openmcl) nil

  #+(or akcl openmcl)
  (let* ((output-type
          (if output-type-p
              output-type
            (output-type-for-declare-form fn (car (last form))))))
    (let ((formals (caddr form)))
      (cond
       ((null (intersection formals lambda-list-keywords
                            :test 'eq))
        `(declaim (ftype (function
                          ,(arg-declarations
                            formals
                            (collect-types (cdddr form)))
                          ,output-type)

; WARNING: Do not replace (cadr form) by fn below.  These can differ!  Fn is
; passed to output-type-for-declare-form in order to get its 'stobjs-out, but
; (cadr form) can be the *1* function for fn.  The mistaken placement of fn
; below caused a factor of 4 slowdown in GCL in the first lemma5 in
; books/unicode/utf8-decode.lisp, because the proclaim for function
; utf8-combine4-guard was overwritten by a subsequent weaker proclaimed type
; that was supposed to be generated for the *1* function, but instead was
; generated for utf8-combine4-guard.

                         ,(cadr form))))
       (t nil)))))

#+(or akcl openmcl)
(defun make-defconst-declare-form (form)

; We assume that the form has already been evaluated.

  (let* ((output (macroexpand-till (caddr form) 'the))
         (output-type (cond ((and (consp output)
                                  (eq 'the (car output)))
                             (cadr output))
                            (t nil))))
    (cond
     (output-type
      `(declaim (type ,output-type ,(cadr form))))
     (t (let ((val (symbol-value (cadr form))))
          (if (integerp val)
              `(declaim (type (integer ,val ,val) ,(cadr form)))
            nil))))))

#+(or akcl openmcl)
(defun make-defstobj-declare-form (form)
 (let* ((name (cadr form))
         (args (cddr form))

; The loss of efficiency caused by using funcall and symbol-value below should
; be more than compensated for by the lack of a warning here when building the
; system.

         (template (funcall 'defstobj-template name args))
         (raw-defs (funcall 'defstobj-raw-defs name template

; We do not want to rely on having the world available here, so we pass in nil
; for the final argument of defstobj-raw-defs.  The only effect of using nil
; instead of a world in such a context is that additional checking by
; translate-declaration-to-guard is missing.

                            nil)))
    (cons 'progn
          (mapcar (function
                   (lambda (def) (if (member (symbol-value
                                              '*stobj-inline-declare*)
                                             def
                                             :test (function equal))
                                     nil
                                   (make-defun-declare-form
                                    (car def)
                                    (cons 'defun def)))))
                  raw-defs))))

(defmacro eval-or-print (form stream)
  `(let ((form ,form))
     (when form
       (if ,stream
           (format stream "~s~%" form)
         (eval form)))))

#+(or akcl openmcl)
(defun proclaim-form (form &optional stream)
  (cond ((consp form)
         (case (car form)
               ((in-package) (eval-or-print form stream) nil)
               ((defmacro defvar defparameter) nil)
               ((defconst)
                (eval-or-print (make-defconst-declare-form form) stream)
                nil)
               ((defstobj)
                (eval-or-print (make-defstobj-declare-form form) stream))
               ((eval-when)
                (dolist (x (cddr form))
                        (proclaim-form x stream))
                nil)
               ((progn mutual-recursion)
                (dolist (x (cdr form))
                        (proclaim-form x stream))
                nil)
               ((defun defund)
                (eval-or-print (make-defun-declare-form (cadr form) form)
                               stream)
                nil)
               (defun-one-output
                (eval-or-print (make-defun-declare-form (cadr form)
                                                        form
                                                        '(values t))
                               stream)
                nil)
               (otherwise nil)))
        (t nil)))

(defun proclaim-file (name &optional stream)

; Proclaims the functions in the file name that are either at the top-level, or
; within a progn, mutual-recursion, or eval-when.  IMPORTANT: This function
; assumes that the defconst forms in the given file have already been
; evaluated.  One way to achieve this state of affairs, of course, is to load
; the file first.

; Just before releasing Version_2.5 we decided to consider proclaiming for
; Lisps other than GCL.  However, our tests in Allegro suggested that this may
; not help.  The comment below gives some details.  Perhaps we will proclaim
; for MCL in the future.  At any rate, OpenMCL is supported starting with
; Version_2.8, and we proclaim there since Warren Hunt thought that might be
; useful.

#|
Here is a summary of three comparable user times from certifying all the ACL2
books in June 2000, just before Release 2.5 is complete.  The first column,
labeled "Comp", is the one to be looked at for comparison purposes.  These are
all done on the same Sun workstation, using Allegro 5.0.1.  The meanings of
these numbers are explained below.

                               Comp     Actual   Comments
Recent ACL2 without proclaim:  5:41     5:36:42  no meta
Recent ACL2 *with*  proclaim:  5:54     5:53:58
April ACL2 (before non-std.):  5:48     5:35:58  missing some pipeline and ~40
                                                 sec. user time on powerlists

The "Comp" column estimates how long the run would have taken if all books had
certified, except that no run gets past book batcher-sort in the powerlists/
directory.  (The April run bogs down even slightly earlier.)  The first row is
adjusted by about 4 minutes because the run started with book meta-plus-lessp.
The April run broke on book basic-def from case-studies/pipeline and hence
missed the rest of that directory's books.  The above points account for the
addition of time from "Actual" to the appropriate comparison time in the first
column.
|#

  #-(or akcl openmcl) (declare (ignore stream name))
  #-(or akcl openmcl) nil

  #+(or akcl openmcl)
  (with-open-file
   (file name :direction :input)
   (let ((eof (cons nil nil))
         (*package* *package*))
     (loop
      (let ((form (read file nil eof)))
        (cond ((eq eof form) (return nil))
              (t (proclaim-form form stream)))))
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          ACL2's Implementation of the Backquote Readmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *comma* (make-symbol "COMMA")
  "*comma* is used by the backquote reader.  When the reader
encounters <,foo>, it returns (list *comma* read:<foo>>).")

(defparameter *comma-atsign* (make-symbol "COMMA-ATSIGN")
  "*comma-atsign* is used by the backquote reader.  When the reader
encounters <,@foo>, it returns (list *comma-atsign* read:<foo>).")

(defparameter *backquote-counter* 0
  "READ cannot handle a comma or comma-atsign unless there is a
pending backquote that will eliminate the *comma* or *comma-atsign*.
In the SPECIAL variable *backquote-counter* we keep track of the number of
backquotes that are currently pending.  It is crucial that this variable
be SPECIAL.")

(defun backquote (x)

  "The two functions BACKQUOTE and BACKQUOTE-LST implement backquote
as described on pp. 349-350 of CLTL1 except that (a) use of vector
notation causes an error and (b) the use of ,. is not permitted."

; It must be emphasized that the ACL2 implementation of backquote is
; only one of many implementations that are consistent with the Common
; Lisp specification of backquote.  That spec requires only that
; backquote produce a form that when evaluated will produce a
; designated object.  We impose the requirement that *acl2-readtable*
; be used both when checking files with ACL2 and when later compiling
; or using those files.  This requirement guarantees that we obtain
; the same behavior of backquote across all Common Lisps.  For
; example, it is an ACL2 theorem, across all Common Lisps that

;   (equal (car '`(,a)) 'cons)

; This theorem is definitely not true about the implementation of
; backquote provided by the implementors of each Common Lisp.  In
; fact, the lefthand side of this theorem represents an informal
; misuse of the backquote notation because one is intended to consider
; the effects of evaluating backquoted forms, not the forms
; themselves.  (In some Common Lisps, the lefthand side might even
; evaluate to a symbol in a nonstandard package.)  Nevertheless,
; because we inflict our definition of backquote on the ACL2 user at
; all times, the above equation is a theorem throughout, so no harm is
; done.  On the other hand, if we used the local implementation of
; backquote of each particular Common Lisp, we would get different
; ACL2 theorems in different Common Lisps, which would be bad.

; Unlike most implementors of backquote, we do no serious
; optimization.  To make matters worse, in ACL2 the backquote
; processing functions run interpreted, not compiled.  We feel this
; inattention to efficiency is justified at the moment because in the
; usage we expect, the only serious costs will be small ones, during
; compilation.

  (cond ((and (vectorp x) (not (stringp x)))
         (error "ACL2 does not handle vectors in backquote."))
        ((atom x) (list 'quote x))
        ((eq (car x) *comma*) (cadr x))
        ((eq (car x) *comma-atsign*) (error "`,@ is an error"))
        (t (backquote-lst x))))

(defun backquote-lst (l)
  (cond ((atom l) (list 'quote l))
        ((eq (car l) *comma*)
         (cadr l))
        ((eq (car l) *comma-atsign*)
         (error ". ,@ is illegal."))
        ((and (consp (car l))
              (eq (caar l) *comma*))
         (list 'cons
               (cadr (car l))
               (backquote-lst (cdr l))))
        ((and (consp (car l))
              (eq (caar l) *comma-atsign*))
         (list 'append (cadr (car l)) (backquote-lst (cdr l))))
        (t
         (list 'cons
               (backquote (car l))
               (backquote-lst (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            SUPPORT FOR ACL2 CHARACTER READER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rev1@ (x acc)

; A strange version of linear reverse, which treats the final cdr of x
; as the final element of x.

  (cond
   ((atom x)
    (cons x acc))
   (t (rev1@ (cdr x) (cons (car x) acc)))))

(defun acl2-read-character-string (s acc)

; The reason we're so picky about what we allow as readable character notation
; is the existence of certain restrictions imposed by dpANS.  From the
; documentation for Sharpsign Backslash in dpANS:

;   When the token x is more than one character long, the x must have
;   the syntax of a symbol with no embedded package markers.  In this
;   case, the sharpsign backslash notation parses as the character
;   whose name is (string-upcase x); see *See Character Names::.

; And from the documentation for char-name in dpANS:

;   Returns a string that is the name of the character, or nil if the
;   character has no name.

; However, in akcl for example, (char-name #\\346) evaluates to NIL.  Even if
; it didn't, surely different lisps will define char-name differently.  So,
; we can't allow notation such as #\\346.

  (let ((ch (read-char s)))
    (cond ((member ch *acl2-read-character-terminators*)
           (unread-char ch s)
           (cond
            ((characterp acc)
             acc)
            (t (let ((x (coerce (rev1@ acc nil) 'string)))
                 (cond
                  ((string-equal x "SPACE")
                   #\Space)
                  ((string-equal x "TAB")
                   #\Tab)
                  ((string-equal x "NEWLINE")
                   #\Newline)
                  ((string-equal x "PAGE")
                   #\Page)
                  ((string-equal x "RUBOUT")
                   #\Rubout)
                  #+clisp

; Currently we only allow #\Null in CLISP.  We have to allow it there in some
; fashion because it is written to compiled (.fas) files.  The current approach
; seems to avoid any soundness issue: presumably #\Null is the same in every
; CLISP, and if one tries then to use a book containing #\Null that was
; certified using CLISP, then one will simply get an error.

                  ((string-equal x "NULL")
                   #\Null)
                  (t (funcall
                      (if (fboundp 'interface-er)
                          'interface-er
                        'error)
                      "When the ACL2 reader tries to read #\\x, then ~
                       x must either be a single character followed ~
                       by a character in the list ~x0, ~
                       or else x must be one of Space, Tab, Newline, ~
                       Page, or Rubout (where case is ignored). ~
                       However, ~s1 is none of these."
                      *acl2-read-character-terminators*
                      x)))))))
          (t (acl2-read-character-string s (cons ch acc))))))

(defun acl2-character-reader (s c n)
  (declare (ignore n c))
  (let ((ch (read-char s)))
    (acl2-read-character-string s ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            DUAL PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following function used to be defined in axioms.lisp (with
; #-acl2-loop-only), but we need it here.

(defconstant *main-lisp-package*
  (find-package "COMMON-LISP"))

(defun symbol-package-name (x)
  (cond ((get x *initial-lisp-symbol-mark*))
        ((let ((p (symbol-package x)))

; The following code should be kept in sync with the gcl-only code in the
; vicinity of the calls of rename-package in acl2.lisp, and with the definition
; of *main-lisp-package-name*.  The point is that the low-level package name
; for Lisp symbols is "LISP" in GCL but we want to treat it as "COMMON-LISP" in
; all Lisp implementations.  Thus, the extra test for the *main-lisp-package*
; is probably not necessary except for GCL, but we go ahead for the sake of
; uniformity and robustness.

           (if (eq p *main-lisp-package*)
               (setf (get x *initial-lisp-symbol-mark*)
                     "COMMON-LISP")
             (and p (package-name p)))))

; We use ERROR now because we cannot print symbols without packages
; with ACL2 functions.

        (t (error
            "The symbol ~a, which has no package, was encountered~%~
             by ACL2.  This is an inconsistent state of affairs, one that~%~
             may have arisen by undoing a defpkg but holding onto a symbol~%~
             in the package being flushed, contrary to warnings printed.~%~%"
            x))))

(defvar *global-symbol-key* (make-symbol "*GLOBAL-SYMBOL-KEY*"))

(defvar **1*-symbol-key* (make-symbol "**1*-SYMBOL-KEY*"))

(defun global-symbol (x)
  (or (get x *global-symbol-key*)
      (setf (get x *global-symbol-key*)
            (intern (symbol-name x)
                    (find-package (concatenate 'string
                                               *global-package-prefix*
                                               (symbol-package-name x)))))))

(defun *1*-symbol (x)
  (or (get x **1*-symbol-key*)
      (setf (get x **1*-symbol-key*)
            (intern (symbol-name x)
                    (find-package (concatenate 'string
                                               *1*-package-prefix*
                                               (symbol-package-name x)))))))

(defun *1*-symbol? (x)

; Used in trace.  Returns nil if the *1* package doesn't exist.

  (let ((pack (find-package (concatenate 'string
                                         *1*-package-prefix*
                                         (symbol-package-name x)))))
    (and pack (*1*-symbol x))))

(defmacro defun-*1* (fn &rest args)
  `(defun ,(*1*-symbol fn) ,@args))

(defmacro gv (fn args val)
  (sublis `((funny-fn . ,fn)
            (funny-args . ,args))
          `(let ((gc-on
                  (not (member (f-get-global 'guard-checking-on
                                             *the-live-state*)
                               '(nil :none)
                               :test 'eq))))
             (if (or gc-on
                     (f-get-global 'safe-mode *the-live-state*))
                 (throw-raw-ev-fncall
                  (list 'ev-fncall-guard-er
                        'funny-fn
                        ,(cons 'list 'funny-args)
                        (untranslate* (guard 'funny-fn nil (w *the-live-state*))
                                      t
                                      (w *the-live-state*))

; We assume that the primitives don't take state.

                        nil
                        (not gc-on)))
               ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            ENVIRONMENT SUPPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following is first used in acl2-init.lisp, so we define it here.

(defun getenv$-raw (string)

; The following either returns the value of the given environment variable or
; returns nil (in lisps where we do not yet know how to get that value).

; WARNING: Keep this in sync with the #-acl2-loop-only definition of setenv$.

  #+cmu
  (and (boundp ext::*environment-list*)
       (cdr (assoc-eq (intern string :keyword)
                      ext::*environment-list*)))
  #+(or gcl allegro lispworks openmcl sbcl clisp)
  (let ((fn
         #+gcl       'si::getenv
         #+allegro   'sys::getenv
         #+lispworks 'cl::getenv
         #+openmcl   'ccl::getenv
         #+sbcl      'sb-ext::posix-getenv
         #+clisp     'ext:getenv))
    (and (fboundp fn)
         (funcall fn string))))

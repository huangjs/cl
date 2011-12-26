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

; This file originally written by:  Robert Krug
; email:       rkrug@cs.utexas.edu
; Department of Computer Sciences
; University of Texas at Austin
; Austin, TX 78712-1188 U.S.A.

; We don't intend this file to be compiled.

;                            TRACE stuff

; Allegro's trace facilities are somewhat limited.  However it does have a
; function, advise, which is sufficiently general to allow it to imitate GCL's
; trace facilities as provided within ACL2.  See Franz's documentation for
; information on advise and unadvise.

; We put over into old-trace the macro for trace that comes with Allegro.
; Thus one can type (old-trace foo) and get the effect that (trace
; foo) would have previously provided.

(cond ((null (macro-function 'old-trace))
       (setf (macro-function 'old-trace) (macro-function 'trace))))

(cond ((null (macro-function 'old-untrace))
       (setf (macro-function 'old-untrace) (macro-function 'untrace))))


; The variables *trace-arglist* and *trace-values* will contain the
; cleaned up arglist and values of a traced function.  The alist
; *trace-sublis* allows one to refer to these variables by more
; common names.

(defvar *trace-arglist*)

(defvar *trace-values*)

(defconst *trace-sublis* '((values . *trace-values*)
                           (si::values . *trace-values*)
                           (arglist . *trace-arglist*)
                           (si::arglist . *trace-arglist*)))

; What I am trying to do:
; Trace is called with a list of functions to be traced.  Each element of
; this list can be;
; (Let foo be defined in common lisp but not in ACL2, and
;  bar be defined within ACL2.)
; 1. the name of a non-acl2 function --- foo,
; 2. the name of an acl2 function --- bar,
; 3. a list whose only element is the name of a non-acl2 function --- (foo),
; 4. a list whose only element is the name of an acl2 function --- (bar),
; 5. a list whose car is the name of a non-acl2 function and whose
;    cdr may contain the keywords :entry and :exit, each of which
;    are followed by a function whose value will be printed upon
;    entry and exit respectively of the function being traced ---
;    (foo :entry (list (list 'arg-one (car si::arglist))
;                      (list 'arg-two (nth 1 si::arglist))))),
; 6. a list whose car is the name of an acl2 function and whose
;    cdr may contain the keywords :entry and :exit, each of which
;    are followed by a function whose value will be printed upon
;    entry and exit respectively of the function being traced ---
;    (bar :entry si::arglist
;         :exit (if (eql (nth 1 si::arglist)
;                        (nth 0 values))
;                   'FAILED
;                 (pretty-print-arg (nth 1 values))))
;
; In trace-pre-process we generate a new list as follows, where *1*bar denotes
; (*1*-symbol bar).
; 1. replacing foo with (foo foo),
; 2. replacing bar with (bar bar) & adding (*1*bar bar),
; 3, replacing (foo ...) with (foo foo ...)
; 4. replacing (bar ...) with (bar bar ...) & adding ((*1*bar ...) (bar ...)).
;
; In trace-process we generate some functions for each element
; (<fn-name> <original-name> ...) of the above pre-processed list:
; a. (excl:unadvise <fn-name>)
; b. (excl:advise <fn-name> :before nil nil 
;                 (progn (setq *trace-arglist* (trace-hide-world-and-state
;                                                 si::arglist))
;                        <entry trace-instructions>))
; c. (excl:advise <fn-name> :after nil nil 
;                 (progn (setq *trace-values*
;                              (trace-hide-world-and-state
;                               ,(trace-values <original-name>)))
;                        (setq *trace-arglist* (trace-hide-world-and-state
;                                               si::arglist))
;                        <exit trace-instructions>))
;
; Unless explicitly overridden by the :entry or :exit keywords,
; <entry trace-instructions> and <exit trace-instructions> print the
; arguments and returned values respectively.  A minor amount of
; cleaning up is done, such as printing |*STATE*| instead of the
; entire state if it is one of the arguements or values.

(defun trace-pre-process (lst)
  (let ((new-lst nil))
    (dolist (x lst new-lst)
      (let ((sym (cond
                  ((symbolp x) x)
                  ((and (consp x) (symbolp (car x)))
                   (car x))
                  (t (interface-er "Not a symbol or a cons of a symbol: ~s0" x)))))
        (if (function-symbolp sym (w *the-live-state*))

            ;; We have an acl2 function.

            (cond ((symbolp x)
                   (push (list (*1*-symbol x) x) new-lst)
                   (push (list x x) new-lst))
                  (t
                   (push (list* (*1*-symbol (car x)) (car x) (cdr x))
                         new-lst)
                   (push (list* (car x) (car x) (cdr x)) new-lst)))

          ;; We do not have an acl2 symbol.

          (if (fboundp sym)
              (if (symbolp x)
                  (push (list x) new-lst)
                (push x new-lst))
            (interface-er "~s0 is not a bound function symbol." sym)))))))

; We construct the (excl:advise <fn-name> :before ...) form that performs the
; tracing on entry.

(defun trace-entry (name l)
  (cond ((null l)
         `(excl:advise ,name :before nil nil
                       (progn (setq *trace-arglist* (trace-hide-world-and-state
                                                     si::arglist))
                              (custom-trace-ppr
                               :in
                               (cons ',name *trace-arglist*)))))
        ((eq (car l) :entry)
         `(excl:advise ,name :before nil nil
                       (progn (setq *trace-arglist* (trace-hide-world-and-state
                                                     si::arglist))
                              (custom-trace-ppr
                               :in
                               (cons ',name
                                     ,(sublis *trace-sublis* 
                                              (cadr l)))))))
        (t
         (trace-entry name (cdr l)))))


; These next three functions were blindly copied from akcl-acl2-trace.lisp

(defun trace-multiplicity (name)
  (cond ((and (f-boundp-global 'current-acl2-world *the-live-state*)
              (stobjs-out name
                          (f-get-global 'current-acl2-world *the-live-state*)))
         (length
          (stobjs-out name
                      (f-get-global 'current-acl2-world *the-live-state*))))
        (t 1)))
  
(defun trace-formals (name)
  (cond ((and (f-boundp-global 'current-acl2-world *the-live-state*)
              (getprop name 'formals nil 'current-acl2-world
                       (f-get-global 'current-acl2-world *the-live-state*))))))

(defun trace-values (name)
  (list 'cons
        '(car values)
        (let ((mul (trace-multiplicity name)))
          (cond ((eql mul 1) nil)
                (t (list 'acl2::mv-refs (1- mul)))))))

; We construct the (excl:advise <fn-name> :after ...) form that performs the
; tracing on entry.

(defun trace-exit (name original-name l)
  (cond ((null l)
         `(excl:advise ,name :after nil nil
                       (progn (setq *trace-values*
                                    (trace-hide-world-and-state
                                     ,(trace-values original-name)))
                              (setq *trace-arglist* (trace-hide-world-and-state
                                                     si::arglist))
                              (custom-trace-ppr
                               :out
                               (cons ',name *trace-values*)))))
        ((eq (car l) :exit)
         `(excl:advise ,name :after nil nil
                       (progn (setq *trace-values*
                                    (trace-hide-world-and-state
                                     ,(trace-values original-name)))
                              (setq *trace-arglist* (trace-hide-world-and-state
                                                     si::arglist))
                              (custom-trace-ppr
                               :out
                               (cons ',name
                                     ,(sublis *trace-sublis*
                                              (cadr l)))))))
        (t
         (trace-exit name original-name (cdr l)))))


(defun traced-fns-lst (lst)
  (list 'QUOTE (mapcar #'car lst)))

; We perform a little error checking, and gather together all the (excl:advise
; ...) functions.

(defun trace-process (lst)
  (let ((new-lst (list (traced-fns-lst lst))))  ;; for the ret. val.
    (dolist (x lst new-lst)
      (cond ((member :cond (cddr x))
             (interface-er "The use of :cond is not supported in ~
                            Allegro."))
            ((member :break (cddr x))
             (interface-er "The use of :break is not supported in ~
                            Allegro.  However, you can use either ~
                            (~s0 :entry (break)) or (~s0 :exit (break)). ~
                            See any Lisp documentation for more on ~
                            break and its options." (car x)))
            (t
             (push (trace-exit (car x) (cadr x) (cddr x)) new-lst)
             (push (trace-entry (car x) (cddr x)) new-lst)
             (push `(excl:unadvise ,(car x)) new-lst))))))
            
(excl:without-package-locks
 (defmacro trace (&rest fns)
   (cons 'progn
         (trace-process (trace-pre-process fns)))))

(excl:without-package-locks
 (defmacro untrace (&rest fns)
   (if (null fns)
       '(excl:unadvise)
     (cons
      'progn
      (let ((ans nil))
        (dolist (fn fns ans)
                (push `(excl:unadvise ,fn) ans)
                (push `(excl:unadvise ,(*1*-symbol fn)) ans)))))))

; ACL2 Version 2.9 -- A Computational Logic for Applicative Common Lisp
; Copyright (C) 2004  University of Texas at Austin

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

; This file is allegro-acl2-trace, with modifications.

; OpenMCL's trace facilities are somewhat limited.  However it does have a
; function, advise, which is sufficiently general to allow it to imitate GCL's
; trace facilities as provided within ACL2.  This function seems to be poorly
; documented, but see the file ccl/lib/encapsulate.lisp in the OpenMCL sources.

; We put over into old-trace the macro for trace that comes with OpenMCL.
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
			   (ccl::values . *trace-values*)
			   ;;(ccl:values . *trace-values*)
			   (arglist . *trace-arglist*)
			   (ccl::arglist . *trace-arglist*)
			   (ccl:arglist . *trace-arglist*)
                           ))

(defun trace-pre-process (lst)
  (let ((new-lst nil))
    (dolist (x lst new-lst)
      (let ((sym (cond ((symbolp x) x)
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

; We construct the (ccl:advise <fn-name> ... :when :before) form that performs the
; tracing on entry.

(defun trace-entry (name l)
  (cond ((null l)
	 `(ccl:advise ,name
		       (progn (setq *trace-arglist* (trace-hide-world-and-state ccl::arglist))
			      (custom-trace-ppr :in
                                                 (cons ',name *trace-arglist*)))
                       :when :before))
	((eq (car l) :entry)
	 `(ccl:advise ,name
		       (progn (setq *trace-arglist* (trace-hide-world-and-state ccl::arglist))
			      (custom-trace-ppr :in
                                                 (cons ',name
                                                       ,(sublis *trace-sublis* 
                                                                (cadr l)))))
                       :when :before))
	(t
	 (trace-entry name (cdr l)))))

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

#-acl2-mv-as-values
(defun trace-values (name)
  (list 'cons
        '(car values)
        (let ((mul (trace-multiplicity name)))
          (cond ((eql mul 1) nil)
                (t (list 'acl2::mv-refs (1- mul)))))))

#+acl2-mv-as-values
(defun trace-values (name)
  (declare (ignore name))
  'values)

; We construct the (ccl:advise <fn-name> ... :when :after) form that performs the
; tracing on exit.

(defun trace-exit (name original-name l)
  (cond ((null l)
	 `(ccl:advise ,name
		       (progn (setq *trace-values*
                                    (trace-hide-world-and-state
                                     ,(trace-values original-name)))
			      (setq *trace-arglist* (trace-hide-world-and-state ccl::arglist))
			      (custom-trace-ppr :out
                                                 (cons ',name *trace-values*)))
                       :when :after))
	((eq (car l) :exit)
	 `(ccl:advise ,name
		       (progn (setq *trace-values*
                                    (trace-hide-world-and-state
                                     ,(trace-values original-name)))
			      (setq *trace-arglist* (trace-hide-world-and-state ccl::arglist))
			      (custom-trace-ppr :out
                                                 (cons ',name
                                                       ,(sublis *trace-sublis*
                                                                (cadr l)))))
                       :when :after))
	(t
	 (trace-exit name original-name (cdr l)))))

(defun traced-fns-lst (lst)
  (list 'QUOTE (mapcar #'car lst)))

; We perform a little error checking, and gather together all the (ccl:advise
; ...) functions.

(defun trace-process (lst)
  (let ((new-lst (list (traced-fns-lst lst))))  ;; for the ret. val.
    (dolist (x lst new-lst)
      (cond ((member :cond (cddr x))
	     (interface-er "The use of :cond is not supported in ~
                            OpenMCL."))
	    ((member :break (cddr x))
	     (interface-er "The use of :break is not supported in ~
                            OpenMCL.  However, you can use either ~
                            (~s0 :entry (break)) or (~s0 :exit (break)). ~
                            See any Lisp documentation for more on ~
                            break and its options." (car x)))
	    (t
	     (push (trace-exit (car x) (cadr x) (cddr x)) new-lst)
	     (push (trace-entry (car x) (cddr x)) new-lst)
	     (push `(ccl:unadvise ,(car x)) new-lst))))))
	
(let ((temp ccl::*warn-if-redefine-kernel*))
  (setf ccl::*warn-if-redefine-kernel* nil)
  (defmacro trace (&rest fns)
    (cons 'progn
          (trace-process (trace-pre-process fns))))
  (setf ccl::*warn-if-redefine-kernel* temp))

(let ((temp ccl::*warn-if-redefine-kernel*))
  (setf ccl::*warn-if-redefine-kernel* nil)
  (defmacro untrace (&rest fns)
    (if (null fns)
        '(ccl:unadvise t)
      (cons 'progn
            (let ((ans nil))
              (dolist (fn fns ans)
                (push `(ccl:unadvise ,fn) ans)
                (push `(ccl:unadvise ,(*1*-symbol fn)) ans))))))
  (setf ccl::*warn-if-redefine-kernel* temp))

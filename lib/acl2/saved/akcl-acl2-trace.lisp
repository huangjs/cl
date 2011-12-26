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

; We don't intend this file to be compiled.

;                            TRACE stuff

(defvar *traced-fn*)

; We put over into old-trace the macro for trace that comes with ACKL.
; Thus one can type (old-trace foo) and get the effect that (trace
; foo) would have previously provided.

(cond ((null (macro-function 'old-trace))
       (setf (macro-function 'old-trace) (macro-function 'trace))))

(cond ((null (macro-function 'old-untrace))
       (setf (macro-function 'old-untrace) (macro-function 'untrace))))

(defmacro trace (&rest fns)
  (let (acl2-fns all-fns)
    (sloop::sloop for fn in fns
                  do (if (function-symbolp
                          (cond
                           ((symbolp fn) fn)
                           ((and (consp fn) (symbolp (car fn)))
                            (car fn))
                           (t (error "Not a symbol or a cons of a symbol: ~s" fn)))
                          (w *the-live-state*))
                         (push fn acl2-fns)
                       (push fn all-fns)))
    (cons
     'old-trace
     (progn

; Turn every element of acl2-fns into (list* fn fn ...).

       (setq acl2-fns
             (sloop::sloop for x in acl2-fns
                           collect
                           (cond ((symbolp x) (list x x))
                                 (t (list* (car x) (car x) (cdr x))))))

; Trace the *1* functions too.  Then every element of acl2-fns will have the
; form (list* fn original-fn ...).

       (dolist (x acl2-fns)
         (push (cons (*1*-symbol (car x))
                     (cdr x))
               acl2-fns))
       (dolist (fn acl2-fns)
         (push (progn (cond ((member :break (cdr fn))
                             (interface-er "Use of :break is not permitted in ~
                                            TRACE.  Consider :entry (progn ~
                                            (break) arglist) instead.")))
                      (cons (car fn)
                            (trace-fix-entry
                             (car fn)
                             (trace-fix-exit (car fn)
                                             (cadr fn)
                                             (cddr fn)))))
               all-fns))
       all-fns))))

(defmacro untrace (&rest fns)
  (cons
   'old-untrace
   (let ((ans fns))
     (dolist (fn fns)
       (push (*1*-symbol fn) ans))
     ans)))

(defun trace-ppr-gcl (x)
  (trace-ppr x)
  '>)

(defun trace-fix-entry (name l)
  (cond ((endp l)
         (list :entry
               `(trace-ppr-gcl (cons ',name (trace-hide-world-and-state
                                             si::arglist)))))
        ((eq (car l) :entry)
         (list* :entry
                `(trace-ppr-gcl (cons ',name
                                      (progn (setq si::arglist
                                                   (trace-hide-world-and-state
                                                    si::arglist))
                                             (let ((arglist si::arglist))
                                               ,(cadr l)))))
                (cddr l)))
        (t (list* (car l) (cadr l) (trace-fix-entry name (cddr l))))))

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

(defun trace-fix-exit (name original-name l)
  (cond ((endp l)
         (list :exit
               `(trace-ppr-gcl (cons ',name
                                     (trace-hide-world-and-state
                                      ,(trace-values original-name))))))
        ((eq (car l) :exit)
         (list* :exit
                `(trace-ppr-gcl (cons ',name
                                      (let ((values (trace-hide-world-and-state
                                                     ,(trace-values
                                                       original-name)))
                                            (arglist si::arglist))
                                        ,(cadr l))))
                (cddr l)))
        (t (list* (car l) (cadr l) (trace-fix-exit name original-name (cddr l))))))

; unitsc.lsp               Gordon S. Novak Jr.              ; 18 Apr 03

; Copyright (c) 2003 Gordon S. Novak Jr. and The University of Texas at Austin.

(defmacro glispconstantflg    (x) `(get ,x 'glispconstantflg))
(defmacro glispconstantval    (x) `(get ,x 'glispconstantval))

; 28 Apr 94
; Cube root
; returns a negative real root for a negative argument.
(defun cbrt (x)
  (and (numberp x) (if (>= x 0) (expt x 1/3) (- (expt (- x) 1/3)))))

; modified version for stand-alone use with units
; 27 Mar 89; 06 Jun 90; 20 May 93; 03 Jan 95; 18 Apr 03
(defun glerror (fn msgstr &rest args)
  (format t "error detected by ~A~%" fn)
  (apply #'format (cons t (cons msgstr args)))
  (terpri) )

; modified version for stand-alone use with units
; 15-Feb-89; 05 Apr 90; 12 Sep 91; 18 Sep 91; 19 Sep 91; 17 Jan 92; 03 Nov 92
; 10 Nov 95; 26 Jul 96; 18 Apr 03
; Get the value of a compile-time constant 
(defun glconstval (x)
  (cond ((or (null x)
	     (eq x t)
	     (numberp x)
	     (characterp x)
	     (stringp x))
	  x)
	((and (symbolp x) (constantp x)) (eval x))
	((quotep x) (cadr x))
	((and (symbolp x)
	      (glispconstantflg x))
	  (glispconstantval x))
	(t (error "NOMSG"))))

; 18 Apr 03
(defun glunitexpansion (u)
  (let ((flat (glunitexpand u)))
    (list '/ (cons '* (car flat)) (cons '* (cadr flat))) ))

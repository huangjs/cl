;
;  This file is part of GRT, a Common Lisp raytracing system
;  Copyright (C) 2002 Nikodemus Siivola
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
;  USA
;

(in-package grt)

(defun read-prompt (prompt &rest values)
  (apply #'format `(t ,prompt ,@values))
  (let ((str (string-trim " " (read-line))))
    (if (string= "" str)
	nil
      str)))

(defmacro let-values (bindings &body body)
  "LET-like wrapper for MULTIPLE-VALUE-BIND."
  (let ((let-form nil))
    (dolist (binding bindings)
      (destructuring-bind (vals form) binding
	(if let-form
	    (setf let-form `(multiple-value-bind ,vals ,form ,let-form))
	  (setf let-form `(multiple-value-bind ,vals ,form ,@body)))))
    let-form))

(defun n-to-2 (function targets)
  "Used in building better macroes: transforms a call of N args to N-1
   consecutive calls of 2 args."
  (if (null (cdr targets))
      (car targets)
      (list function (car targets) (n-to-2 function (cdr targets)))))

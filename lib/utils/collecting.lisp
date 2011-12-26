;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - collecting.lisp
;; Description	     - Collecting lists forwards
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - 1989
;; Last Modified On  - Mon Jun  3 17:32:59 2002
;; Last Modified By  - Tim Bradshaw (tfb at lostwithiel)
;; Update Count	     - 12
;; Status	     - Unknown
;; 
;; $Id: collecting.lisp,v 1.6 2002/06/03 16:33:26 tfb Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Collecting lists forwards
;;; This is an old macro cleaned up a bit

;;; These macros hardly seem worth copyrighting, but are copyright
;;; 1989-2000 by me, Tim Bradshaw, and may be used for any purpose
;;; whatsoever by anyone. There is no warranty whatsoever. I would
;;; appreciate acknowledgement if you use this in anger, and I would
;;; also very much appreciate any feedback or bug fixes.


(defpackage :hjs.util.collecting
  (:use #:CL)
  (:export #:collecting
	   #:with-collectors))

(in-package :hjs.util.collecting)

(defmacro collecting (&body forms)
  ;; Collect some random stuff into a list by keeping a tail-pointer
  ;; to it, return the collected list.  No real point in using
  ;; gensyms, although one probably should on principle.
  "Collect things into a list forwards.  Within the body of this macro
   The form `(COLLECT THING)' will collect THING into the list returned by 
   COLLECTING.  Uses a tail pointer -> efficient."
  (let (($resnam$ (gensym)) ($tail$ (gensym)) ($thing$ (gensym)))
    `(let
	 (,$resnam$ ,$tail$)
       (macrolet
	   ((collect
		(thing)
	      ;; Collect returns the thing it's collecting
	      `(let ((,',$thing$ ,thing))
		 (if ,',$resnam$
		     (setf (cdr ,',$tail$)
			   (setf ,',$tail$ (list ,',$thing$)))
		     (setf ,',$resnam$
			   (setf ,',$tail$ (list ,',$thing$))))
		 ,',$thing$)))
	 ,@forms)
       ,$resnam$)))

(defmacro with-collectors ((&rest collectors) &body forms)
  ;; multiple-collector version of COLLECTING.
  "Collect some things into lists forwards.  
The names in COLLECTORS are defined as local macros, which each collect into a 
separate list.  Returns as many values as there are collectors."
  (let ((cvns (mapcar #'(lambda (c)
			  (make-symbol (concatenate 'string
						    (symbol-name c) "-VAR")))
		      collectors))
	(ctns (mapcar #'(lambda (c)
			  (make-symbol (concatenate 'string
						    (symbol-name c) "-TAIL")))
		      collectors)))
    `(let (,@cvns ,@ctns)
       (macrolet ,(mapcar #'(lambda (cn cvn ctn)
			      `(,cn (v)
				    (let ((vn (make-symbol "V")))
				      `(let ((,vn ,v))
					 (if ,',cvn
					     (setf (cdr ,',ctn)
						   (setf ,',ctn (list ,vn)))
					     (setf ,',cvn 
						   (setf ,',ctn (list ,vn))))
					 ,vn))))
			  collectors cvns ctns)
	 ,@forms
	 (values ,@cvns)))))

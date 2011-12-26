;;; -*- syntax: common-lisp; base: 10; mode: lisp -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             panning.lsp
;;;
;;; Version:          1.0
;;;
;;; Purpose:          
;;;
;;;                   Get the constant power panning scalers for left and right
;;;                   channels given a degree from 0 (left) to 90 (right).
;;;
;;;                   Avoids the "hole in the middle" problem that occurs when
;;;                   trying to centre a signal in the middle of two speakers
;;;                   by "incorrectly" placing exactly half the full signal in
;;;                   both channels.
;;;
;;;                   The formulas are: 
;;;
;;;                      Amp Left  = (sqrt-2 / 2) * (cos(theta) + sin(theta)) 
;;;                      Amp Right = (sqrt-2 / 2) * (cos(theta) - sin(theta))
;;;
;;;                   where theta is the desired angle between left and right
;;;                   speakers expressed between +45 (left) and -45 (right).
;;;
;;;                   Note however that our degree argument to get-cpp-scalers
;;;                   is between 0 (left) and 90 (right) and that the function
;;;                   will return the scalers for left and right channels (in
;;;                   the order) as a 2-element list. E.g.
;;;
;;;                   (get-cpp-scalers 0)  => (1.0 0.0)
;;;                   (get-cpp-scalers 45) => (0.70710677 0.70710677)
;;;                   (get-cpp-scalers 90) => (0.0 1.0)
;;;
;;;                   In the middle then, when theta is 0 and our degree 45,
;;;                   the scalers for left and right channels will be 0.707
;;;                   both, not 0.5.
;;;
;;;                   See The Computer Music Tutorial, Curtis Roads, pp 459-461
;;;                   for more details. 
;;;
;;; Author:           Michael Edwards - michael@ccrma.stanford.edu
;;;
;;; Creation date:    January 25th 2001
;;;
;;; $$ Last modified: 20:23:50 Fri Jan 26 2001 W. Europe Standard Time
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cpp-scalers (degree)
  (let* ((magic (/ (sqrt 2) 2))
         ;; although we (in clm) specify the degree from 0 - 90, for the sake
         ;; of the calculation it's from -45 to 45 with 0, not 45, being the
         ;; centre. 
         (radians (degrees-to-radians (- 45.0 degree)))
         (cos (cos radians))
         (sin (sin radians)))
    (list (* magic (+ cos sin))
          (* magic (- cos sin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun degrees-to-radians (degrees)
  (* degrees (/ (* 2 pi) 360)))

;;; BIL sez: this is called degrees->radians in mus.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF or original panning.lsp


;;; BIL adds this as a special version of make-locsig

(clm::def-optkey-fun make-cpp-locsig ((degree 0.0) (distance 1.0) (reverb 0.0) (channels nil))
  ;; this could be extended to 4 chans...
  (let* ((dist (/ 1.0 (max distance 1.0)))
	 (sdist (/ 1.0 (sqrt (max distance 1.0))))
	 (out-chans (or channels (and *output* (clm::IO-chans *output*)) *clm-channels* 1))
	 (outn-arr (make-double-float-array out-chans))
	 (revn-arr (if *reverb* (make-double-float-array (clm::IO-chans *reverb*))))
	 (rscale (* sdist reverb)))
    (if *reverb* (setf (aref revn-arr 0) (double-float rscale)))
    (if (= out-chans 1)
	(setf (aref outn-arr 0) (double-float dist))
      (if (= out-chans 2)
	  (let ((vals (get-cpp-scalers degree)))
	    (setf (aref outn-arr 0) (double-float (* dist (car vals))))
	    (setf (aref outn-arr 1) (double-float (* dist (cadr vals)))))
	(warn "make-cpp-locsig can't handle ~A chans" channels)))
    (make-instance 'locsig :outn outn-arr :revn revn-arr)))

(defmacro cpp-locsig (gen i inval)
  `(locsig ,gen ,i ,inval))

;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;

(in-package :cmn)

#-excl (defun ratiop (n) (and (not (integerp n)) (typep n 'ratio)))
#+(and excl (not cltl2)) (import '(excl:ratiop))

(defun divide (a b) 
  ;; we don't want user-level divides dropping the poor user into the debugger!
  (if (zerop b) 
      (progn 
	(warn "attempt to divide by 0") 
	a)				;SAIL!
    (/ a b)))


;; flush lisp rationals which server can't handle and prettify print-out
;; ~,3F is needed because gcl prints out endless bogus digits, thereby confusing postscript
;; not-rational should only be used with ~A (sigh -- this is gcl's fault)

(defun not-rational (x) (if (integerp x) x (format nil "~,3F" (float x))))

(defun list-p (lst) (and lst (listp lst))) ;goddam listp is T if passed nil

;;; maxtrix multiplication done by hand (no need for full multiply) 
;;; (third column is assumed to be 0 0 1 since we're in flatland)
;;; rather than optimize using rotate scale concat etc, I'll just use concat.

(defun rotate-matrix (matrix angle)	;degrees
  (flet ((cosd (n) (cos (* n (/ pi 180))))
	 (sind (n) (sin (* n (/ pi 180)))))
    (let ((cs (cosd angle))
	  (sn (sind angle)))
      (if (null matrix)
	  (list cs sn (- sn) cs 0 0)
	(let ((a (first matrix))
	      (b (second matrix))
	      (c (third matrix))
	      (d (fourth matrix))
	      (e (fifth matrix))
	      (f (sixth matrix)))
	  (list (+ (* a cs) (* c sn))
		(+ (* b cs) (* d sn))
		(- (* c cs) (* a sn))
		(- (* d cs) (* b sn))
		e f))))))

(defun scale-matrix (matrix x-scale y-scale)
  (if (not matrix)
      (list x-scale 0 0 y-scale 0 0)
    (list (* x-scale (first matrix))
	  (* x-scale (second matrix))
	  (* y-scale (third matrix))
	  (* y-scale (fourth matrix))
	  (fifth matrix)
	  (sixth matrix))))

(defun mirror-matrix (matrix)
  (if matrix
      (list (- (first matrix)) (- (second matrix)) (third matrix) (fourth matrix) (fifth matrix) (sixth matrix))
    (list -1 0 0 1 0 0)))

(defun flip-matrix (matrix)
  (if matrix
      (list (first matrix) (second matrix) (- (third matrix)) (- (fourth matrix)) (fifth matrix) (sixth matrix))
    (list 1 0 0 -1 0 0)))

(defun truncated-matrix-multiply (m1 m2)
  (let ((a1 (first m1))  (a2 (first m2))
	(b1 (second m1)) (b2 (second m2))
	(c1 (third m1))  (c2 (third m2))
	(d1 (fourth m1)) (d2 (fourth m2))
	(e1 (fifth m1))  (e2 (fifth m2))
	(f1 (sixth m1))  (f2 (sixth m2)))
    (list (+ (* a1 a2) (* b1 c2))
	  (+ (* a1 b2) (* b1 d2))
	  (+ (* c1 a2) (* d1 c2))
	  (+ (* c1 b2) (* d1 d2))
	  (+ (* e1 a2) (* f1 c2) e2)
	  (+ (* e1 b2) (* f1 d2) f2))))


(defun transform-box (mat tv bb)
  
  ;;Transforming Axis-Aligned Bounding Boxes
  ;;by Jim Arvo
  ;;from "Graphics Gems", Academic Press, 1990
  ;;
  ;;Transforms a 3D axis-aligned box via a 3x3 matrix and a translation
  ;;vector and returns an axis-aligned box enclosing the result.
  ;;
  ;;reduced here to 2-D and reversed sense of rotation angle
  ;;(original was clockwise, but PostScript is counterclockwise) 
  
  (let* ((amin (make-array 2))
	 (amax (make-array 2))
	 (bmin (make-array 2))
	 (bmax (make-array 2))
	 (a 0.0) (b 0.0))
    
    (setf (aref amin 0) (float (first bb)))
    (setf (aref amin 1) (float (second bb)))
    
    (setf (aref amax 0) (float (third bb)))
    (setf (aref amax 1) (float (fourth bb)))
    
    (setf (aref bmin 0) (float (first tv)))
    (setf (aref bmin 1) (float (second tv)))
    
    (setf (aref bmax 0) (float (first tv)))
    (setf (aref bmax 1) (float (second tv)))
    
    (loop for i from 0 to 1 do
      (loop for j from 0 to 1 do
	(setf a (* (aref amin j) (nth (+ (* j 2) i) mat)))
	(setf b (* (aref amax j) (nth (+ (* j 2) i) mat)))
	(if (< a b)
	    (progn
	      (incf (aref bmin i) a)
	      (incf (aref bmax i) b))
	  (progn
	    (incf (aref bmin i) b)
	    (incf (aref bmax i) a)))))
    (list (aref bmin 0) (aref bmin 1)
	  (aref bmax 0) (aref bmax 1))))


(defun creation-date ()
  (flet ((month-name (month) (nth (- month 1) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
	 (day-name (day) (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
    (multiple-value-bind (second minute hour date month year day daylight-saving-p time-zone)
	(get-decoded-time)
      (declare (ignore second daylight-saving-p time-zone))
      (format nil "~A ~D-~A-~D at ~D:~2,'0D"
	      (day-name day) date (month-name month) (- year 2000) hour minute))))

(defvar smallest-note .015625)		;256-th note (1/64)

(defun ratify-1 (ux)
  (if (zerop ux) (list 0 1)
    (let ((tt 1) 
	  (err smallest-note)
	  (a1 0) 
	  (b2 0) 
	  (a2 1) 
	  (b1 1) 
	  (a 0)  
	  (b 0)
	  (x (/ 1.0 ux)))
      (loop while t do
	(setf a (+ (* a1 tt) a2)) 
	(setf b (+ (* tt b1) b2))
	(if (and (/= b 0) (or (> b 100) (<= (abs (- ux (/ a b))) err))) (return-from ratify-1 (list a b)))
	(if (< b -100) (return-from ratify-1 (list (- a) (- b))))
	(setf x (/ 1 (- x tt))) 
	(setf tt (floor x)) 
	(setf a2 a1) 
	(setf b2 b1) 
	(setf a1 a) 
	(setf b1 b)))))

(defun ratify (num)			;rational returns gigantic useless factors
  (if (floatp num)
      (if (<= num 16.0)
	  (ratify-1 num)
	(multiple-value-bind (int frac) (floor num)
	  (if (= frac 0.0)
	      (list int 1)
	    (let ((vals (ratify-1 frac)))
	      (list (+ (* int (second vals)) (first vals)) (second vals))))))
    (if (ratiop num)
	(list (numerator num) (denominator num))
      (list num 1))))

(defun fratify (num)
  (if (floatp num)
      (apply #'/ (ratify num))
    num))

(defun iratify (num) (coerce (apply #'/ (ratify num)) 'rational))

;;; in Postscript, if the string to be printed contains "(" or ")" the
;;;   only safe way to deal with them is to prepend the escape character "\"
(defun ps-letters (letters)
  (let ((trouble (or (find #\( letters)
		     (find #\) letters))))
    (if trouble
	(let* ((baddies (+ (count #\( letters) (count #\) letters)))
	       (inlen (length letters))
	       (i -1)
	       (strlen (+ inlen baddies))
	       (newstr (make-string strlen)))
	  (loop for k from 0 below inlen do
	    (let ((kc (elt letters k)))
	      (if (or (char-equal #\( kc)
		      (char-equal #\) kc))
		  (setf (elt newstr (incf i)) #\\))
	      (setf (elt newstr (incf i)) kc)))
	  newstr)
      letters)))

(defun decimal-to-octal (n &optional (nn 0) (pow 1)) 
  (if (< n 8) 
      (+ (* n pow) nn) 
    (multiple-value-bind 
	(int frac) 
	(floor n 8) 
      (decimal-to-octal int (+ nn (* frac pow)) (* pow 10)))))





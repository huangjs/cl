(in-package :cl-user)

(defpackage :hjs.math.number-theory
  (:use #:common-lisp #:iterate)
  (:export :primep
	   :prime-numbers<=
	   :prime-numbers-of-amount
	   :prime->index
	   :index->prime
	   :factor-integer
	   :integers
	   :stream-sum
	   :fibgen
	   :fibs
	   :posint->digits
	   :digits->posint
	   :make-digits
	   :sum-of-numbers
	   :sum-of-square
	   :sum-of-cube
	   ))

(in-package :hjs.math.number-theory)


(declaim (type (vector (integer 0)) *prime-numbers*))


(defparameter *prime-numbers*
  (make-array 1
	      :element-type '(integer 0)
	      :adjustable t
	      :fill-pointer 1
	      :initial-contents '(2)))


;;; helper function
(defun %last-prime ()
  (aref *prime-numbers* (1- (length *prime-numbers*))))

(defun %primep (num)
  (declare (type integer num))
  (if (< num 2)
      nil
      (iter (for i in-vector *prime-numbers*)
	    (with bound = (isqrt num))
	    (when (> i bound)
	      (return t))
	    (when (zerop (mod num i))
	      (return nil))
	    (finally (return i)))))

(defun %prime-numbers<= (max)
  (iter (for i in-vector *prime-numbers*)
	(while (<= i max))
	(collect i)))

(defun %fill-primes-from (start end)
  (iter (for i from start to end)
	(when (%primep i)
	  (vector-push-extend i *prime-numbers*)))
  'done)

(defun %fill-primes-of-amount (amount)
  (iter (for i from (1+ (%last-prime)))
	(with count = 0)
	(while (< count amount))
	(when (%primep i)
	  (vector-push-extend i *prime-numbers*)
	  (incf count)))
  'done)


;;; ridiculously fast prime number constructor
(defun %rebuild-prime-from-sieve< (max)
  (declare (type fixnum max)
	   (optimize (speed 3) (debug 1) (safety 0)))
  (let ((result (make-array max :element-type 'bit :initial-element 1))
	(sqrt-of-max (1+ (isqrt max)))
	(count (if (> max 2)
		   (- max 2)
		   0)))
    (declare (type fixnum sqrt-of-max count))
    (setf (aref result 0) 0)
    (setf (aref result 1) 0)
    (dotimes (i sqrt-of-max)
      (when (= (aref result i) 1)
	(loop for j of-type fixnum = (* 2 i) then (+ j i)
	   until (> j (- max 1))
	   do (when (= (aref result j) 1)
		(setf (aref result j) 0)
		(decf count)))))
    ;; (values result count)
    ;; collect primes
    (multiple-value-bind (primes length)
	(iter (for i index-of-vector result)
	      (with count = 0)
	      (when (= (aref result i) 1)
		(collect i into xxx)
		(incf count))
	      (finally (return (values xxx count))))
      (setf *prime-numbers* (make-array length :element-type '(integer 0) :adjustable t
					:initial-contents primes)))))

(defun %rebuild-prime-from-sieve-of-amount (n)
  (if (<= n 3)
      (%rebuild-prime-from-sieve< 100)
      (%rebuild-prime-from-sieve< (ceiling (* n (+ (log n) (log (log n))))))))


;;; prime sequence/predicates
(defun primep (num)
  (let ((T/F/i (%primep num)))
    (case T/F/i
      (t (return-from primep t))
      ((nil) (return-from primep nil))
      (otherwise			; prime number not enough
       (iter (for i from (1+ T/F/i) to (1+ (isqrt num)))
	     (if (%primep i)
		 (progn
		   (vector-push-extend i *prime-numbers*)
		   (when (zerop (mod num i))
		     (return-from primep nil))))
	     (finally (return-from primep t)))))))

(defun prime-numbers<= (max)
  (let ((last (%last-prime)))
    (when (< last max)
      (%rebuild-prime-from-sieve< (1+ max)))
    (%prime-numbers<= max)))


(defun prime-numbers-of-amount (amount)
  (let ((length (length *prime-numbers*)))
    (when (< length amount)
      (%rebuild-prime-from-sieve-of-amount amount))
    (hjs.data.sequence:take amount *prime-numbers*)))

(defun index->prime (n)
  (let ((length (length *prime-numbers*)))
    (when (< length n)
      (%rebuild-prime-from-sieve-of-amount n))
    (aref *prime-numbers* (1- n))))

(defun prime->index (prime)
  ;; note: index start form 1
  (when (> prime (%last-prime))
    (%rebuild-prime-from-sieve< (1+ prime)))
  (1+ (hjs.data.sequence:binary-search prime *prime-numbers*)))


;;; factor integer, trial method
(defun %factor-integer (num)
  (let ((bound (isqrt num)))
    (labels ((try-factor ()
	       (iter (for i in-vector *prime-numbers*)
		     (while (<= i bound))
		     (when (zerop (mod num i))
		       (return (cons i (%factor-integer (/ num i)))))
		     (finally (return num)))))
      (let ((partial (try-factor)))
	(cond ((consp partial)
	       partial)
	      ((< (%last-prime) bound)
	       (%fill-primes-from (1+ (%last-prime)) bound)
	       (try-factor))
	      (t
	       (list num)))))))

(defun factor-integer (num &key (pack t))
  (if pack
      (hjs.data.sequence:pack (%factor-integer num))
      (%factor-integer num)))

;;; numbers
(defun integers (start &optional end)
  (if (or (null end) (<= start end))
      (ll:cons start (integers (1+ start) end))
      nil))

(defun stream-sum (ll)
  (apply #'+ (ll:enumerate-all ll)))


(defun fibgen (a b)
  (ll:cons a (fibgen b (+ a b))))

(defun fibs ()
  (fibgen 1 1))


;;; digits/posint->digits/digits->posint (posint = positive integer)
(defun make-digits (amount)
  (make-array amount :element-type '(integer 0 9)))

(defun posint->digits (num &key into)
  (let* ((length (ceiling (log num 10)))
	 (result (if into into (make-digits length)))
	 (start-index (- (length result) length))
	 (end-index (1- (length result))))
    (when (> start-index 0)
      (iter (for i from 0 below start-index)
	    (setf (aref result i) 0)))
    (iter (for i from end-index downto start-index)
	  (for (values quo rem) initially (floor num 10) then (floor quo 10))
	  (setf (aref result i) rem))
    result))

(defun digits->posint (digits)
  (iter (for i in-sequence digits)
	(for result initially 0 then (+ (* result 10) i))
	(finally (return result))))


;;; sum-of-numbers / sum-of-square / sum-of-cube
(defun sum-of-numbers (start end &optional (step 1))
  (let ((n (floor #I"(end-start)/step + 1")))
    (* (/ (+ start end) 2) n)))

(defun sum-of-square (start end)
  (flet ((f (n)
	   #I"n*(n+1)*(2*n+1)/6"))
    (if (= start 1)
	(f end)
	(- (f end) (f (1- start))))))

(defun sum-of-cube (start end)
  (flet ((f (n)
	   #I"n*n*(n+1)*(n+1)/4"))
    (if (= start 1)
	(f end)
	(- (f end) (f (1- start))))))


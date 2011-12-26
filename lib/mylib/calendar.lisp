(in-package :cl-user)

(defpackage :hjs.util.time
  (:use :cl :hjs.meta.macro :split-sequence :date)
  (:export #:translate-japanese-year
	   #:parse-kanji-date
	   #:year
	   #:month
	   #:day
	   #:hour
	   #:minute
	   #:sec
	   #:time-string
	   #:date-string
	   #:weekday))

(in-package :hjs.util.time)

(defnewconstant +timezone-tokyo+ -9)
(defnewconstant +timezone-beijing+ -8)

(defun year (&optional (time (get-universal-time)))
  (write-to-string (nth-value 5 (decode-universal-time time))))

(defun month (&optional (time (get-universal-time)))
  (write-to-string (nth-value 4 (decode-universal-time time))))

(defun day (&optional (time (get-universal-time)))
  (write-to-string (nth-value 3 (decode-universal-time time))))

(defun hour (&optional (time (get-universal-time)))
  (write-to-string (nth-value 2 (decode-universal-time time))))

(defun minute (&optional (time (get-universal-time)))
  (write-to-string (nth-value 1 (decode-universal-time time))))

(defun sec (&optional (time (get-universal-time)))
  (write-to-string (nth-value 0 (decode-universal-time time))))

(defun time-string (&key (time (get-universal-time)) (timezone +timezone-tokyo+))
  (with-date time timezone
    (format nil "~a:~a:~a" hour minute second)))

(defun date-string (&key (time (get-universal-time)) (timezone +timezone-tokyo+))
  (with-date time timezone
    (format nil "~a/~a/~a" year month day-of-month)))

(defun weekday (&key (time (get-universal-time)) (timezone +timezone-tokyo+))
  "Return weekday start from Monday as 0"
  (with-date time timezone
    day-of-week))


;;; FIXME: it's a hack
(defun translate-japanese-year (year)
  "Translate a japanese year (number) to standard year."
  (cond ((<= year 0)
	 (error "year must be a positive integer."))
	((<= year 25)
	 (+ 1988 year))
	((<= year 64)
	 (+ 1925 year))
	(t
	 (error "year must be less than or equal to 64."))))


;;; fixme: not complete, just a hack...
(defun parse-kanji-date (datestring)
  (destructuring-bind (month rest)
      (split-sequence #\月 datestring)
    (let ((date (subseq rest 0 (1- (length rest)))))
      (concatenate 'string
		   (year)
		   "/"
		   month
		   "/"
		   date))))

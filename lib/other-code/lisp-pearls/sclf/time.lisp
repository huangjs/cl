;;;  time.lisp --- time primitives

;;;  Copyright (C) 2006, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: time.lisp, Time-stamp: <2007-03-09 20:53:12 wcp> $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

(in-package :sclf)

(defun year (epoch &optional time-zone)
  "Return the year of EPOCH."
  (sixth (multiple-value-list (decode-universal-time epoch time-zone))))

(defun month (epoch &optional time-zone)
  "Return the month of EPOCH."
  (fifth (multiple-value-list (decode-universal-time epoch time-zone))))

(defun day (epoch &optional time-zone)
  "Return the day of EPOCH."
  (fourth (multiple-value-list (decode-universal-time epoch time-zone))))

(defun week-day (epoch &optional time-zone)
  "Return the day of the week of EPOCH."
  (seventh (multiple-value-list (decode-universal-time epoch time-zone))))

(defun leap-year-p (year)
  "Return true if YEAR is a leap year."
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
	   (zerop (mod year 400)))))

(defun last-day-of-month (month year)
  "Return the last day of the month as integer."
  (be last (elt #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))
    (if (and (= last 28)
	     (leap-year-p year))
	(1+ last)
	last)))

(defun add-months (months epoch &optional time-zone)
  "Add MONTHS to EPOCH, which is a universal time.  MONTHS can be
negative."
  (multiple-value-bind (ss mm hh day month year) (decode-universal-time epoch time-zone)
    (multiple-value-bind (y m) (floor (+ month months -1) 12)
      (let ((new-month (1+ m))
	    (new-year (+ year y)))
	(encode-universal-time ss mm hh
			       (min day (last-day-of-month new-month (year epoch)))
			       new-month
			       new-year
			       time-zone)))))

(defun add-days (days epoch)
  "Add DAYS to EPOCH, which is an universal time.  DAYS can be
negative."
  (+ (* 60 60 24 days) epoch))

;; The following two functions are based on Thomas Russ <tar@isi.edu>
;; code which didn't carry any copyright notice, so I assume it was in
;; the public domain.

(defun iso-time-string (time &key time-zone with-timezone-p)
  "Return an ISO 8601 string representing TIME.  The time zone is
included if WITH-TIMEZONE-P is true."
  (flet ((format-timezone (zone)
	   (if (zerop zone)
               "Z"
               (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                 ;; Sign of time zone is reversed in ISO 8601 relative
                 ;; to Common Lisp convention!
                 (format nil "~:[+~;-~]~2,'0D:~2,'0D"
                         (> zone 0) h (round m))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
	(decode-universal-time time time-zone)
      (declare (ignore dow dst))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
              year month day hour minute second
              with-timezone-p (format-timezone zone)))))

(defun parse-iso-time-string (time-string)
  "Parse an ISO 8601 formated string and return the universal
time."
  (flet ((parse-delimited-string (string delimiter n)
           ;; Parses a delimited string and returns a list of
           ;; n integers found in that string.
           (let ((answer (make-list n :initial-element 0)))
             (loop
		for i upfrom 0
		for start = 0 then (1+ end)
		for end = (position delimiter string :start (1+ start))
		do (setf (nth i answer)
			 (parse-integer (subseq string start end)))
		when (null end) return t)
             (values-list answer)))
         (parse-fixed-field-string (string field-sizes)
           ;; Parses a string with fixed length fields and returns
           ;; a list of integers found in that string.
           (let ((answer (make-list (length field-sizes) :initial-element 0)))
             (loop
		with len = (length string)
		for start = 0 then (+ start field-size)
		for field-size in field-sizes
		for i upfrom 0
		while (< start len)
		do (setf (nth i answer)
			 (parse-integer (subseq string start (+ start field-size)))))
             (values-list answer))))
    (flet ((parse-iso8601-date (date-string)
             (let ((hyphen-pos (position #\- date-string)))
               (if hyphen-pos
		   (parse-delimited-string date-string #\- 3)
		   (parse-fixed-field-string date-string '(4 2 2)))))
           (parse-iso8601-timeonly (time-string)
             (let* ((colon-pos (position #\: time-string))
                    (zone-pos (or (position #\- time-string)
                                  (position #\+ time-string)))
                    (timeonly-string (subseq time-string 0 zone-pos))
                    (zone-string (when zone-pos (subseq time-string (1+ zone-pos))))
                    (time-zone nil))
               (when zone-pos
                 (multiple-value-bind (zone-h zone-m)
		     (parse-delimited-string zone-string #\: 2)
                   (setq time-zone (+ zone-h (/ zone-m 60)))
                   (when (char= (char time-string zone-pos) #\-)
                     (setq time-zone (- time-zone)))))
               (multiple-value-bind (hh mm ss)
		   (if colon-pos
		       (parse-delimited-string timeonly-string #\: 3)
		       (parse-fixed-field-string timeonly-string '(2 2 2)))
		 (values hh mm ss time-zone)))))
      (let ((time-separator (position #\T time-string)))
	(multiple-value-bind (year month date)
	    (parse-iso8601-date
	     (subseq time-string 0 time-separator))
	  (if time-separator
	      (multiple-value-bind (hh mm ss zone)
		  (parse-iso8601-timeonly
		   (subseq time-string (1+ time-separator)))
		(if zone
		    ;; Sign of time zone is reversed in ISO 8601
		    ;; relative to Common Lisp convention!
		    (encode-universal-time ss mm hh date month year (- zone))
		    (encode-universal-time ss mm hh date month year)))
	      (encode-universal-time 0 0 0 date month year)))))))

(defun time-string (time &optional time-zone)
  "Return a string representing TIME in the form:
  Tue Jan 25 12:55:40 2005"
  (multiple-value-bind (ss mm hh day month year week-day)
      (decode-universal-time time time-zone)
    (format nil "~A ~A ~A ~D:~2,'0D:~2,'0D ~A"
	    (subseq (week-day->string week-day) 0 3)
	    (subseq (month->string month) 0 3)
	    day
	    hh mm ss
	    year)))

(defun beginning-of-month (month year &optional time-zone)
  (encode-universal-time 0 0 0 1 month year time-zone))

(defun end-of-month (month year &optional time-zone)
  (1- (add-months 1 (encode-universal-time 0 0 0 1 month year time-zone))))

(defun beginning-of-first-week (year &optional time-zone)
  "Return the epoch of the first week of YEAR.  As the first week
of the year needs to have Thursday in this YEAR, the returned
time can actually fall in the previous year."
  (let* ((Jan-1st (encode-universal-time 0 0 0 1 1 year time-zone))
	 (start (- 4 (week-day (add-days 4 Jan-1st)))))
    (add-days start Jan-1st)))

(defun beginning-of-week (week year &optional time-zone)
  "Return the epoch of the beginning of WEEK of YEAR."
  (add-days (* (1- week) 7) (beginning-of-first-week year time-zone)))

(defun end-of-week (week year &optional time-zone)
  "Return the epoch of the beginning of WEEK of YEAR."
  (1- (beginning-of-week (1+ week) year time-zone)))

(defun end-of-last-week (year &optional time-zone)
  "Return the epoch of the last week of YEAR.  As the last week
of the year needs to have Thursday in this YEAR, the returned
time can fall in the next year."
  (1- (beginning-of-first-week (1+ year) time-zone)))

(defun seconds-from-beginning-of-the-year (time &optional time-zone)
  (- time (encode-universal-time 0 0 0 1 1 (year time) time-zone)))

(defun day-of-the-year (time &optional time-zone)
  "Return the day within the year of TIME starting from 1 up to
365 (or 366)."
  (1+ (truncate (seconds-from-beginning-of-the-year time time-zone)
		(* 60 60 24))))

(defun week (time &optional time-zone)
  "Return the number of the week and the year TIME referes to.
Week is an integer from 1 to 52.  Due to the way the first week
of the year is calculated a day in one year could actually be in
the last week of the previous or next year."
  (let* ((year (year time))
	 (start (beginning-of-first-week year time-zone))
	 (days-from-start (truncate (- time start) (* 60 60 24)))
	 (weeks (truncate days-from-start 7))
	 (week-number (mod weeks 52)))
    (values (1+ week-number)
	    (cond ((< weeks 0)
		   (1- year))
		  ((> weeks 51)
		   (1+ year))
		  (t year)))))

(defun week-day->string (day &optional sunday-first)
  "Return the weekday string corresponding to DAY number."
  (elt (if sunday-first
	   #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
	   #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
       day))

(defconstant +month-names+  #("January" "February" "March" "April" "May" "June" "July"
			      "August" "September" "October" "November" "December"))

(defun month->string (month)
  "Return the month string corresponding to MONTH number."
  (elt +month-names+ (1- month)))

(defun month-string->number (month)
  (1+ (position month +month-names+ :test #'string-equal)))

(defun print-time-span (span &optional stream)
  "Print in English the time SPAN expressed in seconds."
  (let* ((minute 60)
	 (hour (* minute 60))
	 (day (* hour 24))
	 (seconds span))
    (macrolet ((split (divisor)
		 `(when (>= seconds ,divisor)
		    (prog1 (truncate seconds ,divisor)
		      (setf seconds (mod seconds ,divisor))))))
      (let* ((days (split day))
	     (hours (split hour))
	     (minutes (split minute)))
	(format stream "~{~A~^ ~}" (remove nil
					   (list
					    (when days
					      (format nil "~D day~:P" days))
					    (when hours
					      (format nil "~D hour~:P" hours))
					    (when minutes
					      (format nil "~D minute~:P" minutes))
					    (when (or (> seconds 0)
						      (= span 0))
					      (format nil "~D second~:P" seconds)))))))))

(defun next-week-day (epoch week-day &optional time-zone)
  "Return the universal time of the next WEEK-DAY starting from epoch."
  (add-days (mod (- week-day (week-day epoch time-zone)) 7)
	    epoch))

(defun next-monday (epoch &optional time-zone)
  "Return the universal time of the next Monday starting from
EPOCH."
  (next-week-day epoch 0 time-zone))

(defun full-weeks-in-span (start end &optional time-zone)
  "Return the number of full weeks in time span START to END.  A
full week starts on Monday and ends on Sunday."
  (be first-monday (next-monday start time-zone)
    (truncate (- end first-monday) (* 7 24 60 60))))

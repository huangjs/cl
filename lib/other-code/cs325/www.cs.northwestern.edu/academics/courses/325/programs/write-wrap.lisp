;;;-*- Mode: Lisp; Package: WRITE-WRAP -*-

;;; Updates:
;;; 1/3/03 made DEFPACKAGE compatible with Allegro Modern [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defpackage #:write-wrap
  (:use #:common-lisp)
  (:export #:write-wrap)
  )

(in-package #:write-wrap)

;;; (WRITE-WRAP stream string width &optional indent first-indent)
;;;   writes string to stream, split into width-size lengths, breaking
;;;   at returns and spaces in the string, if possible, indenting every
;;;   line indent spaces (default = 0), except the first line which is
;;;   indented first-indent spaces (default = indent).
;;;
;;; Note: to generate a string simply use with-output-to-string
;;;   (WITH-OUTPUT-TO-STRING (s) (WRITE-WRAP s ...))
;;;
;;; Had to turn off *PRINT-PRETTY* because Franz turns it on
;;; by default and if you turn it off globally, it breaks the IDE
;;; in 6.0!

(defun write-wrap (stream strng width
                          &optional indent (first-indent indent))
  (let ((*print-pretty* nil))
    (do* ((end (length strng))
          (indent-string (when (and indent (> indent 0))
                           (make-string indent
                                        :initial-element #\space)))
          (first-indent-string (when (and first-indent (> first-indent 0))
                                 (make-string first-indent
                                              :initial-element #\space)))
          (start 0 (1+ next))
          (next (break-pos strng start end width)
                (break-pos strng start end width))
          (margin first-indent-string indent-string))
         ((null next))
      (when margin (write-string margin stream))
      (write-string strng stream :start start :end next)
      (terpri stream))))


;;; (whitespace-p char) is true if ch is whitespace.

(defun whitespace-p (ch)
 (member ch '(#\linefeed #\newline #\return #\space #\tab)))

;;; (break-pos string start end width) returns the position to break string
;;;   at, guaranteed to be no more than width characters.  If there's a`
;;;   return, its position is used, else the last space before the width
;;;   cutoff, else width.  If the end comes before width, then end is 
;;;   returned.

(defun break-pos (strng start end width)
 (unless (or (null start) (>= start end))
   (let ((limit (min (+ start width) end)))
     (or (position #\newline strng :start start :end limit)
	 (and (= end limit) end)
	 (position #\space strng :start start :end limit :from-end t)
	 limit))))		;;insert warning here, if desired


#|
;;; (non-whitespace-pos string &optional start) returns the position of
;;;   the first non-whitespace character in string, after start, if any.

;;; Not used now but was used before to set and update START in WRITE-WRAP
;;; to skip spaces. The current WRITE-WRAP keeps user spacing, except
;;; when replacing a space with a line break.

(defun non-whitespace-pos (strng &optional (start 0))
  (position-if-not #'whitespace-p strng :start start))

|#

(provide "write-wrap")

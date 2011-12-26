;;; Common Lisp extensions for XlispStat 3.39
;;; ----------------------------------------------------------------------
;;; - File: extendstat.lsp
;;; - Author: Chris Riesbeck
;;; - Date: October 21, 1994

;;; Last update:
;;; 10/21/97 added :KEY to REDUCE [CKR]


;;; This file  defines a few Common Lisp functions and macros that
;;; are not in XlispStat 3.50 that are useful in some CS C25 code.
;;;
;;;  Users of ``real'' Common Lisp should not load this file.

(in-package :xlisp)

(export '(clear-input substitute nsubstitute))



;;; CLEAR-INPUT
;;; ----------------------------------------------------------------------

;;; Not really (because LISTEN is not defined), but will at least flush
;;; a line with just spaces and a carriage return

(defun clear-input (&optional stream)
  (when (member (peek-char t stream) '(#\C-M #\space #\tab))
    (read-line stream)))


;;; SUBSTITUTE, NSUBSTITUTE
;;; ----------------------------------------------------------------------

(defun substitute (new old seq 
                       &key (start 0) (end (length seq))
                            (test #'eql) (key #'identity))
  (nsubstitute new old (copy-seq seq)
               :start start :end end :test test :key key))

(defun nsubstitute (new old seq
                       &key (start 0) (end (length seq))
                            (test #'eql) (key #'identity))
  (do ((i start (1+ i)))
      ((>= i end) seq)
    (when (funcall test old (funcall key (elt seq i)))
      (setf (elt seq i) new))))


;;; Redefine REDUCE
;;; ----------------------------------------------------------------------
;;;
;;; Add :KEY argument to REDUCE
;;;
;;; Needed by style-critic.
;;;
;;; (3.50 also doesn't take :FROM-END, :START, or :END but we don't
;;;  need these.)

(unless (fboundp 'old-reduce)
  (setf (symbol-function 'old-reduce)
        (symbol-function 'reduce))

  (defun reduce (fn lst &key (key nil key-p) (initial-value nil init-p))
    (let ((new-lst (if key-p (mapcar key lst) lst)))
      (if init-p
          (old-reduce fn new-lst :initial-value initial-value)
          (old-reduce fn new-lst))))
  )
           


(provide "extendstat")

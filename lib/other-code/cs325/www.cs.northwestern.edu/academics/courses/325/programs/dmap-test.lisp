;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple test file for dmap-lite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defines a small memory. Clears any previous memory!!!
;;;
;;; Load this file then say
;;;
;;;  > (dmap-test:test-dmap)
;;;  ...
;;;
;;; This will parse (milton friedman says interest rates are rising).
;;;
;;; Calling test-dmap a second time should re-parse the
;;; sentence, but not create any new memory structures.
;;;
;;;
;;; Change log
;;; ----------
;;; 06/07/2006: redid test code to use lisp-unit and with-monitors [CKR]
;;; 06/06/2006: Updated packaging code and other bits to standard CL2 [CKR]
;;; 11/16/98 Added :ADD argument to call to find-instances [CKR]


;;; PACKAGES
;;; --------

(defpackage #:dmap-test
  (:use #:common-lisp #:lisp-unit #:frames #:mops #:dmap)
  (:export #:test-dmap)
  )

(in-package #:dmap-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default test sentence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *friedman-sentence*
    '(milton friedman says interest rates are rising))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clear memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clear-memory)
(remove-all-cseqs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmop m-human (m-root))
(defmop m-economist (m-human))
(defmop m-variable (m-root))
(defmop m-change (m-root))
(defmop m-event (m-root))
(defmop m-change-event (m-event) 
  :variable m-variable :change m-change)
(defmop m-communication-event (m-event)
  :actor m-human :info m-event)

(definstance m-milton-friedman (m-economist))
(definstance m-interest-rates (m-variable))
(definstance m-increase (m-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define concept sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defphrase m-root something)
(defphrase m-human someone)
(defphrase m-economist some economist)

(defphrase m-milton-friedman milton friedman)
(defphrase m-interest-rates interest rates)
(defphrase m-increase rising)
(defphrase m-change-event (:variable) are (:change))
(defphrase m-communication-event (:actor) says (:info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define test functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; demo-dmap runs dmap on the test sentences and displays
;;; the concepts recognized.
(defun demo-dmap (&optional (sent *friedman-sentence*))
  (reset-cseqs)
  (with-monitors (m-root)
    (format t "Parsing ~S~%" sent)
    (parse sent :package 'dmap-test)))

;;; dmap-recognize runs dmap on a sentence and returns
;;; a list (with possible duplicates) of the concepts
;;; recognized, suitable for automated testing.
(defun dmap-recognize (sent)
  (let ((l nil))
    (reset-cseqs)
    (with-monitors ((m-root 
                     :collect #'(lambda (item start end) 
                                  (push item l))))
      (parse sent :package 'dmap-test)
      l)))

(defun concepts-equal (l1 l2)
  (set-equal l1 l2 :test #'concept-equal))

(defun concept-equal (c1 c2)
  (or (eql c1 c2)
      (instance-of c1 c2)
      (instance-of c2 c1)))

(defun instance-of (c1 c2)
  (and (instance-p c1)
       (abstp c2 c1)))

(define-test friedman-test
  (assert-equality #'concepts-equal
                   '(M-INCREASE M-INTEREST-RATES M-MILTON-FRIEDMAN
                     M-CHANGE-EVENT M-COMMUNICATION-EVENT)
                   (dmap-recognize *friedman-sentence*))
  )
  

(provide "dmap-test")


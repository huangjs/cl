;;; question.lisp

(eval-when (load compile eval)
  (unless (find-package :ask-question)
    (make-package :ask-question
                  :use (list (or (find-package :common-lisp)
                                 (find-package :lisp))))))

(in-package :ask-question)

(require "tables")
(use-package :tables)

(export '(ask-question define-question define-response))

;------------------------------------------------
;********** questions and responses *************
;------------------------------------------------

(deftable question-for)

(deftable response-for)

(defmacro define-question (&rest L)
  `(add-question ',(car L) ',L))

(defmacro define-response (&rest L)
  `(add-response ',(car L) ',L))

(defun add-question (qname question)
  (setf (question-for qname) question)
  qname)

(defun add-response (rname response)
  (setf (response-for rname) response)
  rname)
 
;-----------------------------------------------
;*************** ask-question ******************
;-----------------------------------------------

(defun ask-question (qname)
  (let ((text (question-text qname)))
    (when (null text)
      (error "Undefined question name ~S" qname))
    (display-template text)
    (display-responses (question-response qname))))

(defun display-template (text)
  (terpri)
  (dolist (token text)
    (display-token token)))

(defun display-token (token)
  (if (keywordp token)
    (format t "<~A> " token)
    (format t "~(~A~) " token)))

(defun display-responses (rnames)
  (mapc #'display-response rnames))

(defun display-response (rname)
  (let ((text (response-text rname)))
    (if (null text)
      (warn "Undefined response name ~S -- ignoring" rname)
      (display-template text))))

(defun question-text (qname)
  (getf (rest (question-for qname)) :text))

(defun question-response (qname)
  (getf (rest (question-for qname)) :default-responses))

(defun response-text (rname)
  (getf (rest (response-for rname)) :text))


(provide "ask-question")

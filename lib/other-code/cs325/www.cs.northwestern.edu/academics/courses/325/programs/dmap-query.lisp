(require "frames")
(require "show-frame")
(require "mops")
(require "dmap")

(use-package :frames)
(use-package :mops)
(use-package :dmap)

;;; (add-monitor concept [function])

(defun parse-keys (sent)
  (let ((question-list nil))
    (reset-cseqs)
    (add-monitor 'm-question 
                 #'(lambda (concept start end)
                     (declare (ignore start end))
                     (pushnew concept question-list)))
    (format t "Parsing ~S~%" sent)
    (parse sent)
    question-list))

(defun collect-question-answers (question-mops)
  (loop for mop in question-mops
        append (get-answers mop)))

(defun get-answers (question-mop)
  (find-instances 'm-question-answer
                  (list :question question-mop)))


(defun display-answers (answer-mops)
  (dolist (answer-mop answer-mops)
    (format t "~&Try ~S -- it says ~S~%" 
            (<- answer-mop :source)
            (<- answer-mop :answer))))


(provide "dmap-query")

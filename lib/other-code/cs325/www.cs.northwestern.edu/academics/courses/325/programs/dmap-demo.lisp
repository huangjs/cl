;;; Simple demo of DMAP with MOP browser.
;;;
;;; Load this file then call
;;;
;;; > (TEST-DMAP)
;;;
;;; A list of concepts recognized should be printed.
;;; Browse them with (BROWSE 'concept).
;;;

;;; MODULES
;;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "dmap-test")
  (require "mop-browser")
  )

;;; PACKAGES
;;; --------

(in-package "CL-USER")

(use-package "MOP-BROWSER")

(defun demo-dmap (&optional (sent *test-sentence*))
  (reset-cseqs)
  (let ((concepts nil))
    (add-monitor 'm-root
                 #'(lambda (concept start end)
                     (declare (ignore start end))
                     (push concept concepts)))
    (format t "Parsing ~S~%" sent)
    (parse sent)
    (browse (list "Parse" concepts))))

(provide "dmap-demo")
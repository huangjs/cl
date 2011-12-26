;;; A very simple function call counting utility
;;;
;;; USAGE
;;;
;;;  (monitor-function function-name)
;;;     Start counting calls to function-name.
;;;  (monitor-count function-name)
;;;     Return current number of calls made to function-name.
;;;  (clear-monitor-count function-name)
;;;     Reset count on function-name to 0.
;;;  (unmonitor-function function-name)
;;;     Restore function-name to unmonitored form.
;;;  (unmonitor-all)
;;;     Restore all monitored functions to unmonitored form.
;;;
;;; Also useful:
;;;
;;;  (monitor-functions function-name-list)
;;;     Monitor all the functions in the list.
;;;  (monitor-file file-name &key :package)
;;;     Monitor all the functions DEFUN'ed in the file. If :package
;;;     is given, the file is read with that package.
;;;
;;;     Ex. (monitor-file "my-code.lsp")
;;;         (monitor-file "my-code.lsp" :my-package)
;;;
;;;     Note: Load the file first to define the functions, before
;;;           calling monitor-file.
;;;
;;; Known bugs:
;;;
;;;   You can't monitor built-in Lisp functions.
;;;
;;;   If you monitor a function, then redefine it:
;;;     - you can't monitor the new definition, because monitor 
;;;       thinks the function is still monitored. 
;;;     - f you unmonitor it, it'll bring back the old definition.



(require "tables")
(use-package :tables)

(require "map-file")


(deftable monitor-count)
(deftable monitor-source)

(defun monitor-file (file &key (package (find-package :common-lisp-user)))
  (let ((*package* (find-package package)))
    (map-file 'monitor-defun file)))

(defun monitor-defun (form)
  (and (consp form)
       (eql (first form) 'defun)
       (monitor-function (second form))))

(defun monitor-functions (fn-names)
  (mapc #'monitor-function fn-names))

(defun monitor-function (fn-name)
  (cond ((not (fboundp fn-name))
         (error "~S is not defined" fn-name))
    ((monitored-p fn-name)
     (warn "~S already monitored" fn-name)
     nil)
    (t (add-monitor fn-name))))

(defun unmonitor-function (fn-name)
  (if (monitored-p fn-name)
    (remove-monitor fn-name)
    (warn "~S not monitored" fn-name)))

(defun unmonitor-all ()
  (let ((l nil))
    (map-table #'(lambda (fn-name source)
                   (setf (symbol-function fn-name)
                         source)
                   (push fn-name l))
               (monitor-source))
    (clear-table (monitor-source))
    l))

(defun clear-monitor-counts ()
  (map-table #'(lambda (fn-name count)
                 (declare (ignore count))
                 (clear-monitor-count fn-name))
             (monitor-count)))

(defun clear-monitor-count (fn-name)
  (setf (monitor-count fn-name) 0))

(defun monitor-counts ()
  (let ((counts nil))
    (map-table #'(lambda (fn-name count)
                   (push (cons fn-name count)
                         counts))
               (monitor-count))
    counts))

;;; Internal code from here on

(defun add-monitor (fn-name)
  (let ((source (symbol-function fn-name)))
    (setf (monitor-source fn-name) source)
    (setf (symbol-function fn-name)
          (make-monitor fn-name source))
    (clear-monitor-count fn-name)
    fn-name))

(defun make-monitor (fn-name source)
  #'(lambda (&rest args)
      (incf (monitor-count fn-name))
      (apply source args)))

(defun monitored-p (fn-name)
  (not (null (monitor-source fn-name))))


(defun remove-monitor (fn-name)
  (setf (symbol-function fn-name)
        (monitor-source fn-name))
  (remove-key fn-name (monitor-source)))

(provide "monitor")

(in-package :cl-user)

(defpackage :hjs.util.file
  (:use :cl)							
  (:export #:head-of-file))

(in-package :hjs.util.file)


(defun head-of-file (file max-chars &key (external-format :utf-8))
  (with-open-file (in file :external-format external-format)
    (let* ((length (min (file-length in) max-chars))
           (text (make-array length :element-type 'character :fill-pointer t)))
      (setf (fill-pointer text) (read-sequence text in))
      text)))

(defun file-size (pathname)
  (with-open-file (stream pathname)
    (file-length stream)))



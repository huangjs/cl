;; Copyright (c) 2008 Accelerated Data Works, Ryan Davis

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;load up both backends
(require 'adw-charting-vecto)
(require 'adw-charting-google)
(require 'cl-fad)


(defpackage #:make-docs
  (:use #:cl #:adw-charting))

(in-package #:make-docs)


;; run all the snippets
(defun load-examples ()
  (dolist (file (cl-fad:list-directory
		   (merge-pathnames
		    "examples/"
		    (asdf:component-pathname
		     (asdf:find-system :adw-charting)))))
    (when (string-equal "lisp"
			(pathname-type file))
      (ignore-errors (load file))
      (format T "Loaded ~a~%" (pathname-name file)))))

#|
(load-examples)
;;move *.png to
(merge-pathnames
 "doc/"
 (asdf:component-pathname
  (asdf:find-system :adw-charting)))
(quit)
|#
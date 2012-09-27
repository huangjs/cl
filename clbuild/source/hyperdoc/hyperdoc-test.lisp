;; Copyright (c) 2003 Nikodemus Siivola

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :hyperdoc-test
  (:use :cl :rt :hyperdoc))

;;;; TEST PACKAGE

(defpackage :foo
  (:use :cl)
  (:shadow #:cons 
	   #:car)
  (:export  #:cons
	    #:car
	    #:ensure-class))

(in-package :foo)

(defvar *hyperdoc-base-uri* "http://www.example.com/foo/")

(defun hyperdoc-lookup (symbol type)
  (declare (ignore type))
  (case symbol
    (cons "cons.html")
    (ensure-class "ensure-class.html")))

;;;; OTHER TEST PACKAGE

(defpackage :bar
  (:use :cl)
  (:export #:foo))

(in-package :bar)

(defvar *hyperdoc-base-uri* "file://bar/")

(defun hyperdoc-lookup (symbol doc-type)
  (when (member doc-type '(variable function))
    (concatenate 'string (string-downcase (symbol-name doc-type)) 
		 "/" (string-downcase (symbol-name symbol)))))


;;;; ACTUAL TESTS

(in-package :hyperdoc-test)

(deftest cl.1
    (lookup 'cons) 
  "http://www.lispworks.com/reference/HyperSpec/Body/a_cons.htm")

(deftest cl.2
    (lookup 'foo:cons) 
  "http://www.example.com/foo/cons.html")

(deftest cl.3
    (prog2
	(setf (base-uri 'foo) "http://newbase.com/")
	(lookup 'foo:cons) 
      (setf (base-uri 'foo) "http://www.example.com/foo/"))
  "http://newbase.com/cons.html")

(deftest cl.4
    (lookup 'foo:car)
  nil)

(deftest cl.5
    (lookup 'car) 
  "http://www.lispworks.com/reference/HyperSpec/Body/f_car_c.htm")

(deftest mop.1
    (lookup 'ensure-class) 
  "http://www.alu.org/mop/dictionary.html#ensure-class")

(deftest mop.2
    (lookup 'foo:ensure-class)
  "http://www.example.com/foo/ensure-class.html")

(deftest all-types.1
    (lookup 'bar:foo)
  (("file://bar/variable/foo" . "VARIABLE")
   ("file://bar/function/foo" . "FUNCTION")))

;; STOLEN FROM lisppaste
;; http://common-lisp.net/project/lisppaste/
;; Copyright Brian Mastenbrook

;; Copyright (c) 2003 Brian Mastenbrook

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
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :abbrev)

(defun could-be-wrap (term char-set)
  (loop for char in char-set
        if (and (> (length term) 1)
                (char= (elt term 0) char)
                (char= (elt term (1- (length term))) char))
        return char))

(defun abbrev (term &key wrap)
  (if (> (length term) 0)
      (if (char= (elt term 0) #\:)
          (abbrev (subseq term 1))
          (let ((char (could-be-wrap term '(#\* #\+))))
            (if char
                (abbrev (subseq term 1 (1- (length term))) :wrap char)
                (let ((split (split-sequence #\- term)))
                  (if (and (> (length split) 1)
                           (every #'(lambda (e) (> (length e) 0)) split))
                      (let ((abbrev (format nil "~{~C~^-~}"
                                            (mapcar #'(lambda (e)
                                                        (elt e 0)) split))))
                        (when wrap
                          (setf abbrev (format nil "~C~A~C"
                                               wrap abbrev wrap))
                          (setf term (format nil "~C~A~C"
                                             wrap term wrap)))
                        abbrev))))))))

;;;;
;;;; misc-unit-tests.lisp
;;;;
;;;; Copyright (C) 2006, Jack D. Unrue
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 
;;;;     1. Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;; 
;;;;     2. Redistributions in binary form must reproduce the above copyright
;;;;        notice, this list of conditions and the following disclaimer in the
;;;;        documentation and/or other materials provided with the distribution.
;;;; 
;;;;     3. Neither the names of the authors nor the names of its contributors
;;;;        may be used to endorse or promote products derived from this software
;;;;        without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS "AS IS" AND ANY
;;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DIS-
;;;; CLAIMED.  IN NO EVENT SHALL THE AUTHORS AND CONTRIBUTORS BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

(in-package :graphic-forms.uitoolkit.tests)

(define-test primary-display-test
  (let ((display (gfw:obtain-primary-display)))
    (assert-true display)
    (assert-true (gfw:primary-p display))
    (let ((size (gfw:size display)))
      (assert-true (> (gfs:size-width size) 0))
      (assert-true (> (gfs:size-height size) 0)))
    (let ((size (gfw:client-size display)))
      (assert-true (> (gfs:size-width size)) 0)
      (assert-true (> (gfs:size-height size)) 0))
    (assert-true (> (length (gfw:text display)) 0))))

(define-test indexed-sort-list-test
  (let* ((orig1   '("zzz" "mmm" "aaa"))
         (result1 (gfs::indexed-sort orig1 #'string< #'identity))
         (orig2   '((zzz 10) (mmm 5) (aaa 1)))
         (result2 (gfs::indexed-sort orig2 #'string< #'first)))
    (assert-true (string= "aaa" (first result1)))
    (assert-true (string= "mmm" (second result1)))
    (assert-true (string= "zzz" (third result1)))
    (assert-true (eql     'aaa  (first (first result2))))
    (assert-true (=       1     (second (first result2))))
    (assert-true (eql     'mmm  (first (second result2))))
    (assert-true (=       5     (second (second result2))))
    (assert-true (eql     'zzz  (first (third result2))))
    (assert-true (=       10    (second (third result2))))))

(defun validate-array-elements (arr1 arr2)
  (assert-true (string= "aaa" (elt arr1 0)))
  (assert-true (string= "mmm" (elt arr1 1)))
  (assert-true (string= "zzz" (elt arr1 2)))
  (assert-true (eql     'aaa  (first  (elt arr2 0))))
  (assert-true (=       1     (second (elt arr2 0))))
  (assert-true (eql     'mmm  (first  (elt arr2 1))))
  (assert-true (=       5     (second (elt arr2 1))))
  (assert-true (eql     'zzz  (first  (elt arr2 2))))
  (assert-true (=       10    (second (elt arr2 2)))))

(define-test indexed-sort-non-adjustable-array-test
  (let* ((orig1   (make-array 3 :initial-contents '("zzz" "mmm" "aaa")))
         (result1 (gfs::indexed-sort orig1 #'string< #'identity))
         (orig2   (make-array 3 :initial-contents '((zzz 10) (mmm 5) (aaa 1))))
         (result2 (gfs::indexed-sort orig2 #'string< #'first)))
    (assert-false (array-has-fill-pointer-p result1))
    (assert-false (array-has-fill-pointer-p result2))
    (assert-false (adjustable-array-p result1))
    (assert-false (adjustable-array-p result2))
    (assert-equal 3 (first (array-dimensions result1)))
    (assert-equal 3 (first (array-dimensions result2)))
    (assert-equal 3 (length result1))
    (assert-equal 3 (length result2))
    (validate-array-elements result1 result2)))

(define-test indexed-sort-adjustable-array-test
  (let ((orig1   (make-array 3 :adjustable t :fill-pointer 0))
        (orig2   (make-array 3 :adjustable t :fill-pointer 0)))
    (loop for item in '("zzz" "mmm" "aaa") do (vector-push item orig1))
    (loop for item in '((zzz 10) (mmm 5) (aaa 1)) do (vector-push item orig2))
    (let ((result1 (gfs::indexed-sort orig1 #'string< #'identity))
          (result2 (gfs::indexed-sort orig2 #'string< #'first)))
      (assert-true  (array-has-fill-pointer-p result1))
      (assert-true  (array-has-fill-pointer-p result2))
      (assert-true  (adjustable-array-p result1))
      (assert-true (adjustable-array-p result2))
      (assert-equal 3 (first (array-dimensions result1)))
      (assert-equal 3 (first (array-dimensions result2)))
      (assert-equal 3 (length result1))
      (assert-equal 3 (length result2))
      (validate-array-elements result1 result2))))

(define-test remove-element-list-test
  (let ((orig '(a b c))
        (remainder nil))
    (multiple-value-bind (tmp victim) (gfs::remove-element orig 1 nil)
      (setf remainder tmp)
      (assert-equal 2 (length tmp))
      (assert-eql 'a (first tmp))
      (assert-eql 'c (second tmp))
      (assert-eql 'b victim))
    (multiple-value-bind (tmp victim) (gfs::remove-element remainder 1 nil)
      (setf remainder tmp)
      (assert-equal 1 (length tmp))
      (assert-eql 'a (first tmp))
      (assert-eql 'c victim))
    (multiple-value-bind (tmp victim) (gfs::remove-element remainder 0 nil)
      (assert-false tmp)
      (assert-eql 'a victim))))

(define-test remove-elements-list-test
  (let ((orig '(a b c d e f))
        (remainder nil))
    (multiple-value-bind (tmp victims)
        (gfs::remove-elements orig (gfs:make-span :start 2 :end 4) nil)
      (setf remainder tmp)
      (assert-equal 3 (length victims))
      (assert-eql 'c (first victims))
      (assert-eql 'd (second victims))
      (assert-eql 'e (third victims))
      (assert-equal 3 (length tmp))
      (assert-eql 'a (first tmp))
      (assert-eql 'b (second tmp))
      (assert-eql 'f (third tmp)))
    (multiple-value-bind (tmp victims)
        (gfs::remove-elements remainder (gfs:make-span :start 0 :end 1) nil)
      (setf remainder tmp)
      (assert-equal 2 (length victims))
      (assert-eql 'a (first victims))
      (assert-eql 'b (second victims))
      (assert-equal 1 (length tmp))
      (assert-eql 'f (first tmp)))
    (multiple-value-bind (tmp victims)
        (gfs::remove-elements remainder (gfs:make-span :start 0 :end 0) nil)
      (assert-false tmp)
      (assert-equal 1 (length victims))
      (assert-eql 'f (first victims)))))

(define-test remove-element-non-adjustable-array-test
  (let ((orig (make-array 3 :initial-contents '(a b c)))
        (tmp nil))
    (setf tmp (gfs::remove-element orig 1 (lambda () (make-array 2))))
    (assert-false (array-has-fill-pointer-p tmp))
    (assert-false (adjustable-array-p tmp))
    (assert-equal 2 (length tmp))
    (assert-eql 'a (elt tmp 0))
    (assert-eql 'c (elt tmp 1))
    (setf tmp (gfs::remove-element tmp 1 (lambda () (make-array 1))))
    (assert-equal 1 (length tmp))
    (assert-eql 'a (elt tmp 0))
    (assert-false (gfs::remove-element tmp 0 (lambda () (make-array 0))))))

(defun reaam-test-make-array ()
  (make-array 10 :fill-pointer 0 :adjustable t))

(define-test remove-elements-adjustable-array-test
  (let ((orig (reaam-test-make-array))
        (tmp nil))
    (loop for item in '(a b c d e f) do (vector-push-extend item orig))
    (setf tmp (gfs::remove-elements orig
                                    (gfs:make-span :start 2 :end 4)
                                    #'reaam-test-make-array))
    (assert-true (array-has-fill-pointer-p tmp))
    (assert-true (adjustable-array-p tmp))
    (assert-equal 3 (length tmp))
    (assert-eql 'a (elt tmp 0))
    (assert-eql 'b (elt tmp 1))
    (assert-eql 'f (elt tmp 2))
    (setf tmp (gfs::remove-elements tmp
                                    (gfs:make-span :start 0 :end 1)
                                    #'reaam-test-make-array))
    (assert-equal 1 (length tmp))
    (assert-eql 'f (elt tmp 0))
    (assert-false (gfs::remove-elements tmp
                                        (gfs:make-span :start 0 :end 0)
                                        #'reaam-test-make-array))))

(define-test clamp-size-test
  (let ((min-size (gfs:make-size :width 10 :height 10))
        (max-size (gfs:make-size :width 100 :height 100))
        (test-sizes (loop for width in  '(5  10 50 100 150)
                          for height in '(10 5 100 50 150)
                          collect (gfs:make-size :width width :height height)))
        (expected-sizes-1 (loop for width in  '(10 10 50 100 100)
                                for height in '(10 10 100 50 100)
                                collect (gfs:make-size :width width :height height)))
        (expected-sizes-2 (loop for width in  '(5 10 50 100 100)
                                for height in '(10 5 100 50 100)
                                collect (gfs:make-size :width width :height height)))
        (expected-sizes-3 (loop for width in  '(10 10 50 100 150)
                                for height in '(10 10 100 50 150)
                                collect (gfs:make-size :width width :height height))))
    (loop for min-size-1 in (list min-size nil min-size nil)
          for max-size-1 in (list max-size max-size nil nil)
          for exp-list in (list expected-sizes-1 expected-sizes-2 expected-sizes-3 test-sizes)
          do (loop for test-size in test-sizes
                   for exp-size in exp-list
                   do (let ((clamped-size (gfs::clamp-size test-size min-size-1 max-size-1)))
                        (assert-true (gfs:equal-size-p exp-size clamped-size) exp-size test-size))))))

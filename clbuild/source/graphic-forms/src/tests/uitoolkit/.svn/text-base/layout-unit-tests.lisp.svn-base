;;;;
;;;; layout-unit-tests.lisp
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

(define-test layout-attributes-test
  (let ((widget1 (make-instance 'mock-widget :handle (cffi:make-pointer 1234)))
        (widget2 (make-instance 'mock-widget :handle (cffi:make-pointer 5678))))
    (let ((data1 (list widget1 (list 'a 1 'b 2)))
          (data2 (list widget2 (list 'a 10 'c 30)))
          (layout (make-instance 'gfw:layout-manager)))
      (setf (slot-value layout 'gfw::data) (list data1 data2))
      (assert-equal 1 (gfw:layout-attribute layout widget1 'a))
      (assert-equal 2 (gfw:layout-attribute layout widget1 'b))
      (let ((tmp (gfw::obtain-children-with-attribute layout 'b)))
        (assert-equal 1 (length tmp))
        (assert-true (cffi:pointer-eq (gfs:handle (car (first tmp))) (gfs:handle widget1))))
      (assert-equal 10 (gfw:layout-attribute layout widget2 'a))
      (assert-equal 30 (gfw:layout-attribute layout widget2 'c))
      (let ((tmp (gfw::obtain-children-with-attribute layout 'c)))
        (assert-equal 1 (length tmp))
        (assert-true (cffi:pointer-eq (gfs:handle (car (first tmp))) (gfs:handle widget2))))
      (assert-true (null (gfw::obtain-children-with-attribute layout 'd)))
      (setf (gfw:layout-attribute layout widget1 'b) 66
            (gfw:layout-attribute layout widget2 'd) 100)
      (assert-equal 1 (gfw:layout-attribute layout widget1 'a))
      (assert-equal 66 (gfw:layout-attribute layout widget1 'b))
      (assert-equal 10 (gfw:layout-attribute layout widget2 'a))
      (assert-equal 30 (gfw:layout-attribute layout widget2 'c))
      (assert-equal 100 (gfw:layout-attribute layout widget2 'd)))))

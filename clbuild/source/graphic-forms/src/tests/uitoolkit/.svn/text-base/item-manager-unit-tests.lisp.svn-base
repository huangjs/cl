;;;;
;;;; item-manager-unit-tests.lisp
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

(defvar *test-hwnd* (cffi:make-pointer 1))

(defun validate-item (expected actual &optional expected-id (expected-hwnd *default-hwnd*))
  (assert-true (typep actual 'mock-item))
  (if expected-id
    (assert-equal expected-id (gfw:item-id actual))
    (assert-false (zerop (gfw::item-id actual))))
  (if expected-hwnd
    (assert-equality #'cffi:pointer-eq expected-hwnd (gfs:handle actual))
    (assert-equality #'eql expected-hwnd (gfs:handle actual)))
  (assert-equality #'equal expected (gfw:data-of actual)))

(defun validate-item-array (expected array &optional (expected-hwnd *default-hwnd*))
  (assert-true (vectorp array))
  (assert-true (array-has-fill-pointer-p array))
  (assert-true (adjustable-array-p array))
  (assert-equal (length expected) (length array))
  (dotimes (i (length array))
    (validate-item (elt expected i) (elt array i) nil expected-hwnd)))

(define-test copy-item-sequence-test
  (let ((values '(a b c)))
    (validate-item-array values (gfw::copy-item-sequence *test-hwnd* values 'mock-item) *test-hwnd*)
    (let ((tmp (loop for datum in values
                               collect (make-instance 'mock-item :data datum
                                                                 :handle *test-hwnd*))))
      (validate-item-array values (gfw::copy-item-sequence *test-hwnd* tmp 'mock-item) *test-hwnd*))
    (let ((tmp (make-array 3 :initial-contents (loop for datum in values
                                                      collect datum))))
      (validate-item-array values (gfw::copy-item-sequence *test-hwnd* tmp 'mock-item) *test-hwnd*))
    (let ((tmp (make-array 3 :initial-contents (loop for datum in values
                                                      collect (make-instance 'mock-item
                                                                             :data datum
                                                                             :handle *test-hwnd*)))))
      (validate-item-array values (gfw::copy-item-sequence *test-hwnd* tmp 'mock-item) *test-hwnd*))))

(define-test item-manager-positions-test
  (let* ((values '(a b c))
         (mgr (make-instance 'mock-item-manager :items values))
         (items (slot-value mgr 'gfw::items)))
    (assert-equal 0 (gfw:item-index mgr (elt items 0)))
    (assert-equal 1 (gfw:item-index mgr (elt items 1)))
    (assert-equal 2 (gfw:item-index mgr (elt items 2)))))

(define-test item-manager-modifications-test
  (let ((values1 '(a b c))
        (values2 '(1 2 3))
        (disp (make-instance 'gfw:event-dispatcher)))
    (let ((mgr1 (make-instance 'mock-item-manager :items values1))
          (mgr2 (make-instance 'mock-item-manager :items values2 :handle *test-hwnd*))
          (mgr3 (make-instance 'mock-item-manager)))

      (gfw::put-widget (gfw::thread-context) mgr3)
      (unwind-protect
          (progn

            ;; sanity check initial states
            ;;
            (validate-item-array values1 (slot-value mgr1 'gfw::items))
            (validate-item-array values2 (slot-value mgr2 'gfw::items) *test-hwnd*)
            (assert-true (zerop (length (slot-value mgr3 'gfw::items))))

            ;; append a new item to each and sanity check again
            ;;
            (gfw:append-item mgr1 'd disp)
            (validate-item-array (append values1 '(d)) (slot-value mgr1 'gfw::items))
            (gfw:append-item mgr2 4 disp)
            (validate-item-array (append values2 '(4)) (slot-value mgr2 'gfw::items) *test-hwnd*)
            (gfw:append-item mgr3 t disp)
            (validate-item-array (list t) (slot-value mgr3 'gfw::items))

            ;; delete all from mgr1
            ;;
            (let ((tmp (gfw:items-of mgr1)))
              (assert-equal 4 (length tmp))
              (gfw:delete-all mgr1)
              (assert-true (zerop (length (gfw:items-of mgr1))))
              (loop for actual in tmp
                    for expected in (append values1 '(d))
                    do (validate-item expected actual nil nil)))

            ;; delete an item from mgr2 (using delete-item)
            ;;
            (let ((tmp (gfw:items-of mgr2)))
              (gfw:delete-item mgr2 0)
              (validate-item 1 (first tmp) nil nil)
              (assert-equal 3 (length (gfw:items-of mgr2)))
              (loop for actual in (gfw:items-of mgr2)
                    for expected in (mapcar (lambda (x) (1+ x)) (subseq values2 0 3))
                    do (validate-item expected actual nil *test-hwnd*)))

            ;; delete last item from mgr3 (using dispose)
            ;;
            (let ((tmp (gfw:items-of mgr3)))
              (gfs:dispose (first tmp))
              (assert-true (zerop (length (gfw:items-of mgr3))))
              (validate-item t (first tmp) nil nil))

            ;; copy items from mgr2 to mgr1
            ;;
            (setf (gfw:items-of mgr1) (gfw:items-of mgr2))
            (assert-equal 3 (length (gfw:items-of mgr1)))
            (loop for actual in (gfw:items-of mgr1)
                  for expected in (subseq (append values2 '(4)) 1 4)
                  do (validate-item expected actual nil *default-hwnd*)))

        (gfw::delete-widget (gfw::thread-context) *default-hwnd*)))))

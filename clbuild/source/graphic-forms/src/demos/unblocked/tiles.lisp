;;;;
;;;; tiles.lisp
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

(defstruct tile (kind 0) (location (gfs:make-point)))

(defvar *unblocked-random-state* nil)

(defun init-tiles (width height kinds)
  (if (null *unblocked-random-state*)
    (setf *unblocked-random-state* (make-random-state)))
  (let ((tiles (make-array width :initial-element nil))
        (*random-state* *unblocked-random-state*))
    (dotimes (i width)
      (let ((column (make-array height :initial-element 0)))
        (setf (aref tiles i) column)
        (dotimes (j height)
          (setf (aref column j) (random (1+ kinds))))))
    tiles))

(defun size-tiles (tiles)
  (gfs:make-size :width (length tiles) :height (length (aref tiles 0))))

(defun map-tiles (func tiles)
  (let ((size (size-tiles tiles)))
    (dotimes (j (gfs:size-height size))
      (dotimes (i (gfs:size-width size))
        (let ((kind (aref (aref tiles i) j)))
          (funcall func (gfs:make-point :x i :y j) kind))))))

(defun print-tiles (tiles)
  (let ((size (size-tiles tiles)))
    (dotimes (j (gfs:size-height size))
      (dotimes (i (gfs:size-width size))
        (let ((kind (aref (aref tiles i) j)))
          (if (< kind 0)
            (print "  ")
            (format t "~d " kind))))
      (format t "~%"))))

(defun eql-point (pnt1 pnt2)
  (and (= (gfs:point-x pnt1) (gfs:point-x pnt2))
       (= (gfs:point-y pnt1) (gfs:point-y pnt2))))

(defun obtain-tile (tiles pnt)
  (if (null pnt)
    (return-from obtain-tile 0))
  (let ((column (aref tiles (gfs:point-x pnt))))
    (aref column (gfs:point-y pnt))))

(defun set-tile (tiles pnt kind)
  (let ((column (aref tiles (gfs:point-x pnt))))
    (setf (aref column (gfs:point-y pnt)) kind)))

(defun neighbor-point (tiles orig-pnt delta-x delta-y)
  (let ((size (size-tiles tiles))
        (new-x (+ (gfs:point-x orig-pnt) delta-x))
        (new-y (+ (gfs:point-y orig-pnt) delta-y)))
    (unless (or (< new-x 0)
                (< new-y 0)
                (>= new-x (gfs:size-width size))
                (>= new-y (gfs:size-height size)))
      (return-from neighbor-point (gfs:make-point :x new-x :y new-y)))
    nil))

(defun neighbor-points (tiles orig-pnt)
  (loop for pnt in (list (neighbor-point tiles orig-pnt 0 -1)
                         (neighbor-point tiles orig-pnt 0 1)
                         (neighbor-point tiles orig-pnt -1 0)
                         (neighbor-point tiles orig-pnt 1 0))
        when pnt
        collect pnt))

(defun shape-tiles (tiles tile-pnt results)
  (when (null (gethash tile-pnt results))
    (let ((kind (obtain-tile tiles tile-pnt)))
      (setf (gethash tile-pnt results) kind)
      (loop for pnt2 in (neighbor-points tiles tile-pnt)
            when (= kind (obtain-tile tiles pnt2))
            do (shape-tiles tiles pnt2 results)))))

(defun shape-tile-points (shape)
  (let ((shape-pnts nil))
    (maphash (lambda (pnt kind)
               (declare (ignore kind))
               (push pnt shape-pnts))
             shape)
    shape-pnts))

(defun shape-size (shape)
  (hash-table-count shape))

(defun shape-kind (shape)
  (if (null shape)
    (return-from shape-kind 0))
  (let ((kind nil))
    (maphash (lambda (pnt k)
               (declare (ignore pnt))
               (if (null kind)
                 (setf kind k)))
             shape)
    kind)) 

(defun collapse-column (column-tiles)
  (let ((new-column (make-array (length column-tiles) :initial-element 0))
        (new-index 0)
        (count (length column-tiles)))
    (dotimes (i count)
      (let ((kind (aref column-tiles i)))
        (unless (zerop kind)
          (setf (aref new-column new-index) kind)
          (incf new-index))))
    new-column))

(defun collapse-tiles (tiles)
  (let ((size (size-tiles tiles)))
    (dotimes (i (gfs:size-width size))
      (setf (aref tiles i) (collapse-column (aref tiles i)))))
  tiles)

(defun clone-tiles (orig-tiles)
  (let* ((width (gfs:size-width (size-tiles orig-tiles)))
         (new-tiles (make-array width :initial-element nil)))
    (dotimes (i width)
      (setf (aref new-tiles i) (copy-seq (aref orig-tiles i))))
    new-tiles))

(defun find-shape (tiles accept-p)
  (if (null *unblocked-random-state*)
    (setf *unblocked-random-state* (make-random-state)))
  (let ((*random-state* *unblocked-random-state*)
        (candidate-shapes nil))
    (dotimes (col-index (length tiles))
      (let ((column-tiles (aref tiles col-index)))
        (dotimes (tile-index (length column-tiles))
          (let ((shape (make-hash-table :test #'equalp)))
            (shape-tiles tiles (gfs:make-point :x col-index :y tile-index) shape)
            (if (funcall accept-p shape)
              (push shape candidate-shapes))))))
    (unless candidate-shapes
      (return-from find-shape nil))
    (elt candidate-shapes (random (length candidate-shapes)))))

#|
(defun find-shape (tiles accept-p)
  (if (null *unblocked-random-state*)
    (setf *unblocked-random-state* (make-random-state)))
  (let ((*random-state* *unblocked-random-state*)
        (shape nil))
    (loop for col-index = (random (length tiles))
          for column-tiles = (aref tiles col-index)
          for tile-index = (random (length column-tiles))
          for tmp-shape = (make-hash-table :test #'equalp)
          until shape
          do (progn
               (shape-tiles tiles (gfs:make-point :x col-index :y tile-index) tmp-shape)
               (if (and (> (shape-size tmp-shape) 1) (funcall accept-p tmp-shape))
                 (setf shape tmp-shape))))
    shape))
|#
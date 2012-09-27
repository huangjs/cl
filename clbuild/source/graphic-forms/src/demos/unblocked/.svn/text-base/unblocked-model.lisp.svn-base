;;;;
;;;; unblocked-model.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-tile-kinds+   6)
  (defconstant +horz-tile-count+ 17)
  (defconstant +vert-tile-count+ 12)
  (defconstant +max-levels+      21))

(defvar *points-needed-table* (loop for level from 1 to +max-levels+
                                    collect (* 250 level level)))

(defun lookup-level-reached (score)
  (loop for entry in *points-needed-table*
        for level from 1
        until (> entry score)
        finally (return level)))

(defun compute-new-game-tiles ()
  (collapse-tiles (init-tiles +horz-tile-count+ +vert-tile-count+ (1- +max-tile-kinds+))))

(defun accept-shape-p (shape)
  (let ((size (shape-size shape))
        (kind (shape-kind shape)))
    (and (> size 1) (/= kind 0) (/= kind +max-tile-kinds+))))

(defclass unblocked-game-model ()
  ((score
    :accessor score-of
    :initform 0)
   (shape-data
    :accessor shape-data-of
    :initform nil)
   (original-tiles
    :accessor original-tiles-of
    :initform nil)
   (active-tiles
    :accessor active-tiles-of
    :initform nil)))

(defvar *game* (make-instance 'unblocked-game-model))

(defun model-new ()
  (let ((tiles (compute-new-game-tiles)))
    (setf (score-of *game*)          0
          (original-tiles-of *game*) tiles
          (active-tiles-of *game*)   tiles)))

(defun model-rollback ()
  (setf (score-of *game*)        0
        (active-tiles-of *game*) (original-tiles-of *game*)))

(defun model-tiles ()
  (active-tiles-of *game*))

(defun update-model-score (shape-data)
  (incf (score-of *game*) (* 5 (length shape-data))))

(defun update-model-tiles (shape-data)
  (setf (active-tiles-of *game*)
        (if shape-data
          (progn
            (loop with tmp = (clone-tiles (active-tiles-of *game*))
                  for pnt in shape-data do (set-tile tmp pnt 0)
                  finally (return (collapse-tiles tmp))))
          (original-tiles-of *game*))))

(defun regenerate-model-tiles ()
  (setf (active-tiles-of *game*) (compute-new-game-tiles)))

(defun model-level ()
  (lookup-level-reached (score-of *game*)))

(defun game-points-needed ()
  (- (nth (1- (model-level)) *points-needed-table*) (score-of *game*)))

(defun model-score ()
  (score-of *game*))

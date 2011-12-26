;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           edge-table.lisp
;;;;  Purpose:        Edge table routines for reversi
;;;;  Programer:      Kevin M. Rosenberg based on code by Peter Norvig
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: edge-table.lisp 10866 2006-01-15 18:32:28Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg 
;;;; and Copyright (c) 1998-2002 Peter Norvig
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************


(in-package #:reversi)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *edge-and-x-lists*
    '((22 11 12 13 14 15 16 17 18 27)
      (72 81 82 83 84 85 86 87 88 77)
      (22 11 21 31 41 51 61 71 81 72)
      (27 18 28 38 48 58 68 78 88 77))
    "The four edges (with their X-squares)."))

(defparameter *top-edge* (first *edge-and-x-lists*))

(defvar *edge-table* nil
  "Array of values to player-to-move for edge positions.")

;;(declaim (type (simple-array fixnum #.(expt 3 10)) *edge-table*))

(defun make-edge-table ()
  (setq *edge-table* (make-array (expt 3 10) :element-type 'fixnum
				 :adjustable nil :fill-pointer nil))
  (init-edge-table)
  *edge-table*)

(deftype edge-table () '(simple-array fixnum (*)))


(defun map-edge-n-pieces (fn player board n squares index)
  "Call fn on all edges with n pieces."
  ;; Index counts 1 for player; 2 for opponent
  (declare (fixnum n index)
	   (type player player)
	   (type square index)
	   (type (simple-array fixnum (100)) board)
	   (list squares)
	   (optimize (speed 3) (space 0) (safety 0)))
  (cond
    ((< (length squares) n) nil)
    ((null squares) (funcall fn board index))
    (t (let ((index3 (* 3 index))
             (sq (first squares)))
	 (declare (fixnum index3 sq))
         (map-edge-n-pieces fn player board n (rest squares) index3)
         (when (and (plusp n) (= (bref board sq) empty))
           (setf (bref board sq) player)
           (map-edge-n-pieces fn player board (- n 1) (rest squares)
                              (+ 1 index3))
           (setf (bref board sq) (opponent player))
           (map-edge-n-pieces fn player board (- n 1) (rest squares)
                              (+ 2 index3))
           (setf (bref board sq) empty))))))



(defun possible-edge-moves-value (player board index)
  "Consider all possible edge moves. 
  Combine their values into a single number."
  (declare (type board board)
	   (type player player)
	   (type square index))
  (combine-edge-moves
   (cons
      (list 1.0 (aref *edge-table* index)) ;; no move
      (loop for sq in *top-edge*             ;; possible moves
            when (= (bref board sq) empty)
            collect (possible-edge-move player board sq)))
    player))


(defun edge-index (player board squares)
  "The index counts 1 for player; 2 for opponent,
  on each square---summed as a base 3 number."
  (declare (type board board)
	   (type player player)
	   (type cons squares)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((index 0))
    (declare (fixnum index))
    (dolist (sq squares)
      (declare (type square sq))
      (setq index 
	(the fixnum 
	  (+ 
	   (the fixnum (* index 3))
	   (the fixnum (cond ((= (bref board sq) empty) 0)
			     ((= (bref board sq) player) 1)
			     (t 2)))))))
    index))

(defun possible-edge-move (player board sq)
  "Return a (prob val) pair for a possible edge move."
  (declare (type board board)
	   (type player player)
	   (type square sq))
  (let ((new-board (replace-board (svref *ply-boards* player) board)))
    (make-move sq player new-board)
    (list (edge-move-probability player board sq)
          (- (aref *edge-table*
		    (edge-index (opponent player)
                               new-board *top-edge*))))))

(defun combine-edge-moves (possibilities player)
  "Combine the best moves."
  (declare (type player player)
	   (list possibilities)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((prob 1.0)
        (val 0.0)
        (fn (if (= player black) #'> #'<)))
    (declare (short-float prob val))
    (loop for pair in (sort possibilities fn :key #'second)
          while (>= prob 0.0)
	do (incf val (* prob (first pair) (second pair)))
	   (decf prob (* prob (first pair))))
    (round val)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((corner/xsqs '((11 . 22) (18 . 27) (81. 72) (88 . 77))))
    (defun corner-p (sq) (assoc sq corner/xsqs))
    (defun x-square-p (sq) (rassoc sq corner/xsqs))
    (defun x-square-for (corner) (cdr (assoc corner corner/xsqs)))
    (defun corner-for (xsq) (car (rassoc xsq corner/xsqs)))))

(defun edge-move-probability (player board square)
  "What's the probability that player can move to this square?"
  (declare (type board board)
	   (type player player)
	   (type square square)
	   (optimize (speed 3) (safety 0) (space 0)))
  (cond
    ((x-square-p square) .5) ;; X-squares
    ((legal-p square player board) 1.0) ;; immediate capture
    ((corner-p square) ;; move to corner depends on X-square
     (let ((x-sq (x-square-for square)))
       (declare (type square x-sq))
       (cond
         ((= (bref board x-sq) empty) .1)
         ((= (bref board x-sq) player) 0.001)
         (t .9))))
    (t (/ (aref
            '#2A((.1  .4 .7)
                 (.05 .3  *)
                 (.01  *  *))
            (count-edge-neighbors player board square)
            (count-edge-neighbors (opponent player) board square))
          (if (legal-p square (opponent player) board) 2 1)))))

(defun count-edge-neighbors (player board square)
  "Count the neighbors of this square occupied by player."
  (declare (type board board)
	   (type player player)
	   (type square square))
  (count-if #'(lambda (inc)
		(declare (type square inc))
                (= (bref board (+ square inc)) player))
            '(+1 -1)))

(defparameter *static-edge-table*
  '#2A(;stab  semi    un 
       (   *    0 -2000) ; X
       ( 700    *     *) ; corner
       (1200  200   -25) ; C
       (1000  200    75) ; A
       (1000  200    50) ; B
       (1000  200    50) ; B
       (1000  200    75) ; A
       (1200  200   -25) ; C
       ( 700    *     *) ; corner
       (   *    0 -2000) ; X
       ))
(declaim (type (simple-array t (* *)) *static-edge-table*))

(defun static-edge-stability (player board)
  "Compute this edge's static stability"
  (declare (type board board)
	   (type player player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (loop for sq in *top-edge*
      for i from 0
      sum (the fixnum 
	    (cond
	     ((= (bref board sq) empty) 0)
	     ((= (bref board sq) player)
	      (aref *static-edge-table* i
		    (piece-stability board sq)))
	     (t (- (aref *static-edge-table* i
			 (piece-stability board sq))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((stable 0) (semi-stable 1) (unstable 2))
    (declare (type fixnum stable semi-stable unstable))
    
    (defun piece-stability (board sq)
      (declare (type board board)
	       (fixnum sq)
	       (optimize (speed 3) (safety 0) (space 0)))
      (cond
	((corner-p sq) stable)
	((x-square-p sq)
	 (if (eql (bref board (corner-for sq)) empty)
	     unstable semi-stable))
	(t (let* ((player (bref board sq))
		  (opp (opponent player))
		  (p1 (find player board :test-not #'eql
			    :start sq :end 19))
		  (p2 (find player board :test-not #'eql
			    :start 11 :end sq
			    :from-end t)))
	     (declare (fixnum player opp))
	     (cond
	       ;; unstable pieces can be captured immediately
	       ;; by playing in the empty square
	       ((or (and (eql p1 empty) (eql p2 opp))
		    (and (eql p2 empty) (eql p1 opp)))
		unstable)
	       ;; Semi-stable pieces might be captured
	       ((and (eql p1 opp) (eql p2 opp)
		     (find empty board :start 11 :end 19))
		semi-stable)
	       ((and (eql p1 empty) (eql p2 empty))
		semi-stable)
	       ;; Stable pieces can never be captured
	       (t stable))))))))


(defun init-edge-table ()
  "Initialize *edge-table*, starting from the empty board."
  ;; Initialize the static values
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (loop for n-pieces from 0 to 10 do 
        (map-edge-n-pieces
	 #'(lambda (board index)
	     (declare (type board board)
		      (fixnum index))
              (setf (aref *edge-table* index)
                    (the fixnum (static-edge-stability black board))))
	 black (initial-board) n-pieces *top-edge* 0))
  ;; Now iterate five times trying to improve:
  (dotimes (i 5) 
    (declare (fixnum i))
    ;; Do the indexes with most pieces first
    (loop for n-pieces fixnum from 9 downto 1 do 
          (map-edge-n-pieces
            #'(lambda (board index)
		(declare (type board board)
			 (fixnum index))
                (setf (aref *edge-table* index)
                      (the fixnum (possible-edge-moves-value black board index))))
            black (initial-board) n-pieces *top-edge* 0))))


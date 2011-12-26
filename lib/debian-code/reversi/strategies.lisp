;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: reversi -*-
;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           strategies.lisp
;;;;  Purpose:        Strategy routines for reversi
;;;;  Programer:      Kevin Rosenberg based on code by Peter Norvig
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: strategies.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg 
;;;; and Copyright (c) 1998-2002 Peter Norvig
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(in-package #:reversi)

(defun random-strategy (player board)
  "Make any legal move."
  (declare (type player player)
	   (type board board))
  (random-nth (legal-moves player board)))

(defun maximize-difference (player board)
  "A strategy that maximizes the difference in pieces."
  (declare (type player player)
	   (type board board))
  (funcall (maximizer #'count-difference) player board))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,
  apply EVAL-FN to each resulting board, and choose 
  the move for which EVAL-FN returns the best score.
  FN takes two arguments: the player-to-move and board"
  #'(lambda (player board)
      (declare (type player player)
	       (type board board))
      (let* ((moves (legal-moves player board))
             (scores (mapcar #'(lambda (move)
				 (funcall
				  eval-fn
				  player
				  (make-move move player
					     (copy-board board))))
                             moves))
             (best (apply #'max scores)))
	(declare (fixnum best))
        (elt moves (position best scores)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *weights*
      (make-array 100 :element-type 'fixnum 
		  :fill-pointer nil :adjustable nil
		  :initial-contents
		  '(0   0   0  0  0  0  0   0  0 0
		    0 120 -20 20  5  5 20 -20 120 0
		    0 -20 -40 -5 -5 -5 -5 -40 -20 0
		    0  20  -5 15  3  3 15  -5  20 0
		    0   5  -5  3  3  3  3  -5   5 0
		    0   5  -5  3  3  3  3  -5   5 0
		    0  20  -5 15  3  3 15  -5  20 0
		    0 -20 -40 -5 -5 -5 -5 -40 -20 0
		    0 120 -20 20  5  5 20 -20 120 0
		    0   0   0  0  0  0  0   0   0 0)))
  (declaim (type (simple-array fixnum (100)) *weights*))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq all-squares 
    (sort (loop for i from 11 to 88 
	      when (<= 1 (mod i 10) 8) collect i)
	  #'> :key #'(lambda (sq) (elt *weights* sq)))))


(defun weighted-squares (player board)
  "Sum of the weights of player's squares minus opponent's."
  (declare (type player player)
	   (type board board))
  (let ((opp (opponent player)))
    (loop for i in all-squares
          when (= (bref board i) player) 
          sum (aref *weights* i)
          when (= (bref board i) opp)
          sum (- (aref *weights* i)))))

(defconstant winning-value (- most-positive-fixnum 70))
(defconstant losing-value  (+ most-negative-fixnum 70))

(defun final-value (player board)
  "Is this a win, loss, or draw for player?"
  (declare (type player player)
	   (type board board))
  (case (signum (count-difference player board))
    (-1 losing-value)
    ( 0 0)
    (+1 winning-value)))

(defun final-value-weighted (player board)
  "Is this a win, loss, or draw for player?"
  (declare (type player player)
	   (type board board))
  (let ((diff (count-difference player board)))
    (case (signum diff)
      (-1 (+ losing-value diff))
      ( 0 0)
      (+1 (+ winning-value diff)))))

(defun minimax (player board ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  (declare (type player player)
	   (type board board)
	   (fixnum ply)
	   (optimize (speed 3) (space 0) (safety 0)))
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (minimax (opponent player) board
                            (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move nil)
                  (best-val nil))
              (dolist (move moves)
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (minimax
                                 (opponent player) board2
                                 (- ply 1) eval-fn))))
                  (when (or (null best-val)
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move))))))

(defun minimax-searcher (ply eval-fn)
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (declare (type player player)
	       (type board board))
      (multiple-value-bind (value move)
          (minimax player board ply eval-fn) 
        (declare (ignore value))
        move)))

(defun alpha-beta (player board achievable cutoff ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  (declare (type player player)
	   (type board board)
	   (fixnum achievable cutoff ply)
	   (optimize (speed 3) (safety 0) (space 0)))
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta (opponent player) board
                               (- cutoff) (- achievable)
                               (- ply 1) eval-fn))
                (final-value player board))
	  (let ((best-move (first moves)))
	    (declare (type move best-move))
	    (loop for move in moves do
		  (let* ((board2 (make-move move player
					    (copy-board board)))
			 (val (- (alpha-beta
                                 (opponent player) board2
                                 (- cutoff) (- achievable)
                                 (- ply 1) eval-fn))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))
                until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  (declare (fixnum depth))
  #'(lambda (player board)
      (declare (type board board)
	       (type player player))
      (multiple-value-bind (value move)
          (alpha-beta player board losing-value winning-value
                      depth eval-fn) 
        (declare (ignore value))
        move)))

(defun modified-weighted-squares (player board)
  "Like WEIGHTED-SQUARES, but don't take off for moving
  near an occupied corner."
  (declare (type player player)
	   (type board board)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((w (weighted-squares player board)))
    (declare (fixnum w))
    (dolist (corner '(11 18 81 88))
      (declare (type square corner))
      (when (not (= (bref board corner) empty))
        (dolist (c (neighbors corner))
	  (declare (type square c))
          (when (not (= (bref board c) empty))
            (incf w (* (- 5 (aref *weights* c))
                       (if (= (bref board c) player)
                           +1 -1)))))))
    w))

(eval-when (:compile-toplevel :load-toplevel :execute)
(let ((neighbor-table (make-array 100 :initial-element nil)))
  ;; Initialize the neighbor table
  (dolist (square all-squares)
    (declare (type square square))
    (dolist (dir +all-directions+)
      (declare (type dir dir))
      (if (valid-p (+ square dir))
          (push (+ square dir)
                (aref neighbor-table square)))))

  (defun neighbors (square)
    "Return a list of all squares adjacent to a square."
    (aref neighbor-table square))))


(defun mobility-simple (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))



(defstruct (node) 
  (square(missing-argument) :type square)
  (board (missing-argument) :type board)
  (value (missing-argument) :type integer))

(defun alpha-beta-searcher2 (depth eval-fn)
  "Return a strategy that does A-B search with sorted moves."
  #'(lambda (player board)
      (declare (type player player)
	       (type board board))
      (multiple-value-bind (value node)
          (alpha-beta2
            player (make-node :board board
                              :value (funcall eval-fn player board))
            losing-value winning-value depth eval-fn)
        (declare (ignore value))
        (node-square node))))

(defun alpha-beta2 (player node achievable cutoff ply eval-fn)
  "A-B search, sorting moves by eval-fn"
  ;; Returns two values: achievable-value and move-to-make
  (declare (fixnum ply)
	   (optimize (speed 3) (space 0) (safety 0)))
  (if (= ply 0)
      (values (node-value node) node)
      (let* ((board (node-board node))
             (nodes (legal-nodes player board eval-fn)))
        (if (null nodes)
            (if (any-legal-move? (opponent player) board)
                (values (- (alpha-beta2 (opponent player)
                                        (negate-value node)
                                        (- cutoff) (- achievable)
                                        (- ply 1) eval-fn))
                        nil)
                (values (final-value player board) nil))
	  (let ((best-node (first nodes)))
              (loop for move in nodes
                    for val = (- (alpha-beta2
                                   (opponent player)
                                   (negate-value move)
                                   (- cutoff) (- achievable)
                                   (- ply 1) eval-fn))
                    do (when (> val achievable)
                         (setf achievable val)
                         (setf best-node move))
                    until (>= achievable cutoff))
              (values achievable best-node))))))

(defun negate-value (node)
  "Set the value of a node to its negative."
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (setf (node-value node) (- (node-value node)))
  node)

(defun legal-nodes (player board eval-fn)
  "Return a list of legal moves, each one packed into a node."
  (let ((moves (legal-moves player board)))
    (sort (map-into
            moves
            #'(lambda (move)
                (let ((new-board (make-move move player
                                            (copy-board board))))
                  (make-node
                    :square move :board new-board
                    :value (funcall eval-fn player new-board))))
            moves)
          #'> :key #'node-value)))

(defun alpha-beta3 (player board achievable cutoff ply eval-fn
                    killer)
  (declare (type board board)
	   (type player player)
	   (type fixnum achievable cutoff ply)
	   (optimize (speed 3) (space 0) (safety 0)))
  "A-B search, putting killer move first."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (put-first killer (legal-moves player board))))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta3 (opponent player) board
                                (- cutoff) (- achievable)
                                (- ply 1) eval-fn nil))
                (final-value player board))
            (let ((best-move (first moves))
                  (new-board (svref *ply-boards* ply))
                  (killer2 nil)
                  (killer2-val winning-value))
	      (declare (type move best-move)
		       (type board new-board)
		       (type fixnum killer2-val))
              (loop for move in moves
		  do (multiple-value-bind (val reply)
		       (alpha-beta3
			(opponent player)
			(make-move move player
				   (replace-board new-board board))
			(- cutoff) (- achievable)
			(- ply 1) eval-fn killer2)
		       (setf val (- val))
		       (when (> val achievable)
			 (setq achievable val)
			 (setq best-move move))
		       (when (and reply (< val killer2-val))
			 (setq killer2 reply)
			 (setq killer2-val val)))
		  until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta3w (player board achievable cutoff ply eval-fn
                    killer)
  (declare (type board board)
	   (type player player)
	   (type fixnum achievable cutoff ply)
	   (type (or null move) killer)
	   (optimize (speed 3) (safety 0) (space 0)))
  "A-B search, putting killer move first."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (put-first killer (legal-moves player board))))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta3 (opponent player) board
                                (- cutoff) (- achievable)
                                (- ply 1) eval-fn nil))
                (final-value-weighted player board))
            (let ((best-move (first moves))
                  (new-board (svref *ply-boards* ply))
                  (killer2 nil)
                  (killer2-val winning-value))
	      (declare (type move best-move)
		       (type board new-board)
		       (type fixnum killer2-val))
              (loop for move in moves
		  do (multiple-value-bind (val reply)
		       (alpha-beta3
			(opponent player)
			(make-move move player
				   (replace-board new-board board))
			(- cutoff) (- achievable)
			(- ply 1) eval-fn killer2)
		       (setf val (- val))
		       (when (> val achievable)
			 (setq achievable val)
			 (setq best-move move))
		       (when (and reply (< val killer2-val))
			 (setq killer2 reply)
			 (setq killer2-val val)))
		  until (>= achievable cutoff))
              (values achievable best-move))))))


(defun alpha-beta-searcher3 (depth eval-fn)
  "Return a strategy that does A-B search with killer moves."
  #'(lambda (player board)
      (declare (type board board)
	       (type player player))
      (multiple-value-bind (value move)
          (alpha-beta3 player board losing-value winning-value
                       depth eval-fn nil)
        (declare (ignore value))
        move)))

(defun alpha-beta-searcher3w (depth eval-fn)
  "Return a strategy that does A-B search with killer moves."
  #'(lambda (player board)
      (nth-value 1
		 (alpha-beta3w player board losing-value winning-value
			       depth eval-fn nil))))

(defun put-first (killer moves)
  "Move the killer move to the front of moves,
  if the killer move is in fact a legal move."
  (if (member killer moves)
      (cons killer (delete killer moves))
      moves))

(defun mobility (player board)
  "Current Mobility is the number of legal moves.
  Potential mobility is the number of blank squares
  adjacent to an opponent that are not legal moves.
  Returns current and potential mobility for player."
  (declare (type board board)
	   (type player player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((opp (opponent player))
        (current 0)    ; player's current mobility
        (potential 0))			; player's potential mobility
    (declare (type player opp)
	     (type fixnum current potential))
    (dolist (square all-squares)
      (declare (type square square))
      (when (= (bref board square) empty)
        (cond ((legal-p square player board)
               (incf current))
	      ((some-neighbors board opp (neighbors square))
	       (incf potential))
	      )))
    (values current (the fixnum (+ current potential)))))


(defun some-neighbors (board opp neighbors)
  (declare (type board board)
	   (type player opp)
	   (type cons neighbors)
	   (optimize (speed 3) (safety 0) (space 0)))
  (block search
    (dolist (sq neighbors)
      (declare (type square sq))
      (when (= (bref board sq) opp)
	(return-from search t)))
    (return-from search nil)))

(defun edge-stability (player board)
  "Total edge evaluation for player to move on board."
  (declare (type board board)
	   (type player player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (loop for edge-list of-type (simple-array fixnum (*)) in *edge-and-x-lists*
      sum (aref *edge-table* (edge-index player board edge-list))))

(defun iago-eval (player board)
  "Combine edge-stability, current mobility and
  potential mobility to arrive at an evaluation."
  ;; The three factors are multiplied by coefficients
  ;; that vary by move number:
  (declare (type board board)
	   (type player player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((c-edg  (+ 312000 (* 6240 *move-number*)))
        (c-cur (if (< *move-number* 25)
		   (+ 50000 (* 2000 *move-number*))
		 (+ 75000 (* 1000 *move-number*))))
        (c-pot 20000))
    (declare (fixnum c-edg c-cur c-pot))
    (multiple-value-bind (p-cur p-pot)
        (mobility player board)
      (multiple-value-bind (o-cur o-pot)
          (mobility (opponent player) board)
        ;; Combine the three factors into one sum:
        (+ (round (* c-edg (edge-stability player board))
		  32000)
	   (round (* c-cur (- p-cur o-cur))
		  (+ p-cur o-cur 2))
	   (round (* c-pot (- p-pot o-pot))
		  (+ p-pot o-pot 2)))))))


;; Strategy Functions

(defun iago (depth)
  "Use an approximation of Iago's evaluation function."
  (declare (fixnum depth))
  (alpha-beta-searcher3 depth #'iago-eval))

;; Maximizer (1-ply)
(defun mx-df ()
  (maximizer #'count-difference))

(defun mx-wt ()
  (maximizer #'weighted-squares))

(defun mx-md-wt ()
  (maximizer #'modified-weighted-squares))

;; Minimax-searcher

(defun mm-df (ply)
  (minimax-searcher ply #'count-difference))

(defun mm-wt (ply)
  (minimax-searcher ply #'weighted-squares))

(defun mm-md-wt (ply)
  (minimax-searcher ply #'modified-weighted-squares))

;; Alpha-beta3 searcher
(defun ab3-df (ply)
  (alpha-beta-searcher3 ply #'count-difference))

(defun ab3-wt (ply)
  (alpha-beta-searcher3 ply #'weighted-squares))

(defun ab3-md-wt (ply)
  (alpha-beta-searcher3 ply #'modified-weighted-squares))


(defun ab3w-df (ply)
  (declare (fixnum ply))
  (alpha-beta-searcher3w ply #'count-difference))

(defun ab3w-wt (ply)
  (declare (fixnum ply))
  (alpha-beta-searcher3w ply #'weighted-squares))

(defun ab3w-md-wt (ply)
  (declare (fixnum ply))
  (alpha-beta-searcher3w ply #'modified-weighted-squares))


(defun rr (ply n-pairs)
  (round-robin 
   (list #'random-strategy (ab3-df ply) (ab3-wt ply) (ab3-md-wt ply) (iago 3)) 
   n-pairs 
   10
   '(random ab3-df ab3-wt ab3-md-wt iago)))

  
(defun text-reversi ()
  "Sets up a text game between player and computer"
  )

					  
      

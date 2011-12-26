;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: reversi -*-
;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           base.lisp
;;;;  Purpose:        Basic functions for reversi
;;;;  Programer:      Kevin Rosenberg based on code by Peter Norvig
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: base.lisp 10501 2005-04-30 10:10:29Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2002 by Kevin M. Rosenberg 
;;;; and Copyright (c) 1998-2002 Peter Norvig
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(in-package #:reversi)

(defparameter +all-directions+ '(-11 -10 -9 -1 1 9 10 11))
(defconstant +default-max-minutes+ 30)

(defconstant empty 0 "An empty square")
(defconstant black 1 "A black piece")
(defconstant white 2 "A white piece")
(defconstant outer 3 "Marks squares outside the 8x8 board")
;;(declaim (type (unsigned-byte 8) empty black white outer))
(declaim (type fixnum empty black white outer))

#|
(deftype piece () '(unsigned-byte 8))
(deftype player () '(unsigned-byte 8))
(deftype move () '(unsigned-byte 8))
(deftype square () '(unsigned-byte 8))
|#

(deftype piece () 'fixnum)
(deftype player () 'fixnum)
(deftype move () 'fixnum)
(deftype square () 'fixnum)
(deftype dir () 'fixnum)
(deftype board () '(simple-array fixnum (100)))
;;(deftype board () '(simple-array (unsigned-byte 8) (100)))
(deftype clock () '(simple-array integer (3)))

(defun make-moves ()
  (make-array 60 :element-type 'cons :fill-pointer 0
	      :adjustable nil))
(deftype moves () '(array cons (60)))


(defclass reversi-game ()
  ((bl-strategy :initarg :bl-strategy
		:documentation "Strategy function for black"
		:reader bl-strategy)
   (wh-strategy :initarg :wh-strategy
		:documentation "Strategy function for white"
		:reader wh-strategy)
   (board :type board :initarg :board
	  :documentation "The board configuration"
	  :reader board)
   (move-number :type fixnum :initarg :move-number 
		:documentation "The number of the move to be played"
		:accessor move-number)
   (player :type player :initarg :player
		:documentation "ID of next player to move"
		:accessor player)
   (moves :type moves :initarg :moves
	  :documentation "An array of moves played in the game"
	  :accessor moves)
   (print? :type boolean :initarg :print?
	   :documentation "Whether to print progress of this game"
	   :reader print?)
   (record-game? :type boolean :initarg :record-game?
	   :documentation "Whether to record moves and clcck of this game"
	   :reader record-game?)
   (final-result :type (or null fixnum) :initarg :final-result
		 :documentation "Final count, is NIL while game in play"
		 :accessor final-result)
   (max-minutes :type fixnum :initarg :max-minutes
		:documentation "Maximum minites for each player"
		:reader max-minutes)
   (clock :type clock :initarg :clock :initform nil
	  :documentation "An array of time-units left"
	  :accessor clock))
  (:default-initargs 
      :bl-strategy nil
    :wh-strategy nil
    :board (initial-board)
    :move-number 1
    :player black
    :moves (make-moves)
    :print? nil
    :record-game? nil
    :final-result nil
    :max-minutes +default-max-minutes+
    :clock (make-clock +default-max-minutes+)))


(defun name-of (piece) (schar ".@O?" piece))
(defun title-of (piece)
  (declare (fixnum piece))
  (nth (the fixnum (1- piece)) '("Black" "White")) )
       
(defmacro opponent (player) 
  `(if (= ,player black) white black))

(defmacro bref (board square)
  `(the piece (aref (the board ,board) (the square ,square))))

(defparameter all-squares
    (loop for i fixnum from 11 to 88
	  when (<= 1 (the fixnum (mod i 10)) 8)
	  collect i)
  "A list of all squares")

(defun initial-board ()
  "Return a board, empty except for four pieces in the middle."
  ;; Boards are 100-element vectors, with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER.  Initially
  ;; the 4 center squares are taken, the others empty.
  (let ((board (make-array 100 :element-type 'fixnum
                           :initial-element outer
			   :adjustable nil :fill-pointer nil)))
    (declare (type board board))
    (dolist (square all-squares)
      (declare (fixnum square))
      (setf (bref board square) empty))
    (setf (bref board 44) white   (bref board 45) black
          (bref board 54) black   (bref board 55) white)
    board))

(defun copy-board (board)
  (copy-seq board))

(defgeneric make-clock (clock))
(defmethod make-clock ((clock array))
  (make-array (+ 1 (max black white))
	      :element-type 'integer
	      :initial-contents clock
	      :adjustable nil
	      :fill-pointer nil))

(defmethod make-clock ((minutes integer))
  (make-array (+ 1 (max black white))
	      :element-type 'integer
	      :initial-element 
	      (* minutes 60 
		 internal-time-units-per-second)
	      :adjustable nil
	      :fill-pointer nil))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (declare (type board board)
	   (type fixnum player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (the fixnum (- (the fixnum (count player board))
		 (the fixnum (count (opponent player) board)))))

(defun valid-p (move)
  (declare (type move move)
	   (optimize (speed 3) (safety 0) (space 0)))
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and (typep move 'move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

#+ignore
(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (declare (type board board)
	   (type move move)
	   (type player player))
  (and (= (the piece (bref board move)) empty)
       (some #'(lambda (dir) (declare (type dir dir)) (would-flip? move player board dir))
             +all-directions+)))

#+ignore
(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (declare (type board board)
	   (type move move)
	   (type player player)
	   (optimize speed (safety 0) (space 0)))
  (if (= (bref board move) empty)
      (block search
	(let ((i 0))
	  (declare (fixnum i))
	  (tagbody t
	    (when (>= i 8) (return-from search nil))
	    (when (would-flip? move player board (aref +all-directions+ i))
	      (return-from search t))
	    (incf i)
	    (go t))))
    nil))

(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (declare (type board board)
	   (type move move)
	   (type player player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (if (= (the piece (bref board move)) empty)
      (block search
	(dolist (dir +all-directions+)
	  (declare (type dir dir))
	  (when (would-flip? move player board dir)
	    (return-from search t)))
	(return-from search nil))
    nil))

(defstruct (state (:constructor make-state-struct))
  move player board clock)

(defun make-state (move player clock board)
  (make-state-struct :move move :player player :clock (make-clock clock) :board (copy-board board)))

(defun make-game-move (game move player)
  (when (record-game? game)
    (vector-push (make-state move player (clock game) (board game))
		 (moves game)))
  (make-move move player (board game))
  (incf (move-number game)))

(defun reset-game (game &optional (move-number 1))
  (if (record-game? game)
      (when (< move-number (move-number game))
	(let ((old-state (aref (moves game) (1- move-number))))
	  (if old-state
	      (progn
		(setf (player game) (state-player old-state))
		(replace-board (board game) (state-board old-state))
		(replace (clock game) (state-clock old-state))
		(setf (fill-pointer (moves game)) (1- move-number))
		(setf (move-number game) move-number))
	    (warn "Couldn't find old state"))))
  (warn "Tried to reset game, but game is not being recorded")))
  
(defun make-move (move player board)
  "Update board to reflect move by player"
  ;; First make the move, then make any flips
  (declare (type board board)
	   (type move move)
	   (type player)
	   (optimize (speed 3) (safety 0) (space 0)))
  (setf (bref board move) player)
  (dolist (dir +all-directions+)
    (declare (type dir dir))
    (make-flips move player board dir))
  board)

(defun make-flips (move player board dir)
  "Make any flips in the given direction."
  (declare (type board board)
	   (type move move)
	   (type player player)
	   (type dir dir)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (loop for c from (+ move dir) by dir until (= c (the fixnum bracketer))
            do (setf (bref board c) player)))))

(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?
  If so, return the square number of the bracketing piece."
  ;; A flip occurs if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, bracketed by 
  ;; one of player's pieces
  (declare (type board board)
	   (type move move)
	   (type player player)
	   (type dir dir)
	   (optimize (speed 3) (safety 0) (space 0)))
  (let ((c (+ move dir)))
    (declare (type square c))
    (and (= (the piece (bref board c)) (the player (opponent player)))
         (find-bracketing-piece (the fixnum (+ c dir)) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (declare (type board board)
	   (type square square)
	   (type player player)
	   (type dir dir)
	   (optimize (speed 3) (safety 0))
)
  (cond ((= (bref board square) player) square)
        ((= (bref board square) (the player (opponent player)))
         (find-bracketing-piece (the square (+ square dir)) player board dir))
        (t nil)))

(defun next-to-play (board previous-player &optional (print nil))
  "Compute the player to move next, or NIL if nobody can move."
  (declare (type board board)
	   (type player previous-player)
	   (type boolean print))
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
          ((any-legal-move? previous-player board) 
           (when print
             (format t "~&~c has no moves and must pass."
                     (name-of opp)))
           previous-player)
          (t nil))))

(defun any-legal-move? (player board)
  "Does player have any legal moves in this position?"
  (declare (type player player)
	   (type board board))
  (some #'(lambda (move) (declare (type move move)) (legal-p move player board))
        all-squares))


(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  ;;*** fix, segre, 3/30/93.  Was remove-if, which can share with all-squares.
  (declare (type player player)
	   (type board board))
  (loop for move in all-squares
      when (legal-p move player board) collect move))

(defun replace-board (to from)
  (replace to from))

(defvar *ply-boards*
  (apply #'vector (loop repeat 40 collect (initial-board))))


(defvar *move-number* 1 "The number of the move to be played")
(declaim (type fixnum *move-number*))

(defun make-game (bl-strategy wh-strategy 
		  &key 
		  (print t) 
		  (minutes +default-max-minutes+)
		  (record-game nil))
  (let ((game
	 (make-instance 'reversi-game :bl-strategy bl-strategy
			:wh-strategy wh-strategy
			:print? print
			:record-game? record-game
			:max-minutes minutes)))
    (setf (clock game) (make-clock minutes))
    game))

(defun play-game (game)
  (catch 'game-over
    (until (null (player game))
	   (setq *move-number* (move-number game))
	   (get-move game
		     (if (= (player game) black) 
			 (bl-strategy game)
		       (wh-strategy game))
		     (player game)
		     (board game) (print? game) (clock game))
	   (setf (player game) 
	     (next-to-play (board game) (player game) (print? game)))
	   (incf (move-number game))))
  (when (print? game)
    (format t "~&The game is over.  Final result:")
    (print-board (board game) (clock game)))
  (count-difference black (board game)))


(defun reversi (bl-strategy wh-strategy 
                &optional (print t) (minutes +default-max-minutes+))
  (play-game (make-game bl-strategy wh-strategy :print print
			:record-game nil :minutes minutes)))

(defvar *clock* (make-clock +default-max-minutes+) "A copy of the game clock")
(defvar *board* (initial-board) "A copy of the game board")

(defun get-move (game strategy player board print clock)
  "Call the player's strategy function to get a move.
  Keep calling until a legal move is made."
  ;; Note we don't pass the strategy function the REAL board.
  ;; If we did, it could cheat by changing the pieces on the board.
  (when print (print-board board clock))
  (replace *clock* clock)
  (let* ((t0 (get-internal-real-time))
         (move (funcall strategy player (replace-board *board* board)))
         (t1 (get-internal-real-time)))
    (decf (elt clock player) (- t1 t0))
    (cond
      ((< (elt clock player) 0)
       (format t "~&~c has no time left and forfeits."
               (name-of player))
       (throw 'game-over (if (eql player black) -64 64)))
      ((eq move 'resign)
       (throw 'game-over (if (eql player black) -64 64)))
      ((and (valid-p move) (legal-p move player board))
       (when print
         (format t "~&~c moves to ~a." 
                 (name-of player) (88->h8 move)))
       (make-game-move game move player))
      (t (warn "Illegal move: ~a" (88->h8 move))
         (get-move game strategy player board print clock)))))


(defun random-reversi-series (strategy1 strategy2 
                              n-pairs &optional (n-random 10))
  "Play a series of 2*n games, starting from a random position."
  (reversi-series
    (switch-strategies #'random-strategy n-random strategy1)
    (switch-strategies #'random-strategy n-random strategy2)
    n-pairs))

(defun switch-strategies (strategy1 m strategy2)
  "Make a new strategy that plays strategy1 for m moves,
  then plays according to strategy2."
  #'(lambda (player board)
      (funcall (if (<= *move-number* m) strategy1 strategy2)
               player board)))

(defun reversi-series (strategy1 strategy2 n-pairs)
  "Play a series of 2*n-pairs games, swapping sides."
  (let ((scores
          (loop repeat n-pairs
             for random-state = (make-random-state)
             collect (reversi strategy1 strategy2 nil)
             do (setf *random-state* random-state)
             collect (- (reversi strategy2 strategy1 nil)))))
    ;; Return the number of wins (1/2 for a tie),
    ;; the total of the point differences, and the
    ;; scores themselves, all from strategy1's point of view.
    (values (+ (count-if #'plusp scores)
               (/ (count-if #'zerop scores) 2))
            (apply #'+ scores)
            scores)))

(defun round-robin (strategies n-pairs &optional
                    (n-random 10) (names strategies))
  "Play a tournament among the strategies.
  N-PAIRS = games each strategy plays as each color against
  each opponent.  So with N strategies, a total of
  N*(N-1)*N-PAIRS games are played."
  (let* ((N (length strategies))
         (totals (make-array N :initial-element 0))
         (scores (make-array (list N N)
                             :initial-element 0)))
    ;; Play the games
    (dotimes (i N)
      (loop for j from (+ i 1) to (- N 1) do 
          (let* ((wins (random-reversi-series
                         (elt strategies i)
                         (elt strategies j)
                         n-pairs n-random))
                 (losses (- (* 2 n-pairs) wins)))
            (incf (aref scores i j) wins)
            (incf (aref scores j i) losses)
            (incf (aref totals i) wins)
            (incf (aref totals j) losses))))
    ;; Print the results
    (dotimes (i N)
      (format t "~&~a~20T ~4f: " (elt names i) (elt totals i))
      (dotimes (j N)
        (format t "~4f " (if (eql i j) '---
			   (aref scores i j)))))))


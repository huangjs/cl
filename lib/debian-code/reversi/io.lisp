;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: reversi -*-
;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           io.lisp
;;;;  Purpose:        Basic Input-Output for reversi
;;;;  Programer:      Kevin Rosenberg based on code by Peter Norvig
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: io.lisp 7061 2003-09-07 06:34:45Z kevin $
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
(let ((square-names 
        (cross-product #'concat-symbol
                       '(? A B C D E F G H ?)
                       '(? 1 2 3 4 5 6 7 8 ?))))
  (declare (type list square-names))

  (defun h8->88 (str)
    "Convert from alphanumeric to numeric square notation."
    (or (position (string str) square-names :test #'string-equal)
        str))

  (defun 88->h8 (num)
    "Convert from numeric to alphanumeric square notation."
    (if (valid-p num)
        (nth num square-names)
      num)))

(defun moves-to-string (moves)
  (let (move-list)
    (dotimes (i (length moves))
      (push (format nil "~2d: ~a ~a~%"
		    (1+ i)
		    (title-of (nth 1 (elt moves i)))
		    (symbol-name (88->h8 (nth 0 (elt moves i)))))
	    move-list))
    (setq move-list (nreverse move-list))
    (list-to-delimited-string move-list #\space))))

(defun human (player board)
  "A human player for the game of Reversi"
  (format t "~&~c to move ~a: " (name-of player)
          (mapcar #'88->h8 (legal-moves player board)))
  (h8->88 (read)))


(defun print-board (&optional (board *board*) clock)
  "Print a board, along with some statistics."
  ;; First print the header and the current score
  (format t "~2&    A B C D E F G H   [~c=~2a ~c=~2a (~@d)]"
          (name-of black) (count black board)
          (name-of white) (count white board)
          (count-difference black board))
  ;; Print the board itself
  (loop for row from 1 to 8 do
        (format t "~&  ~d " row)
        (loop for col from 1 to 8
              for piece = (bref board (+ col (* 10 row)))
              do (format t "~c " (name-of piece))))
  ;; Finally print the time remaining for each player
  (when clock
    (format t "  [~c=~a ~c=~a]~2&"
            (name-of black) (time-string (elt clock black))
            (name-of white) (time-string (elt clock white)))))


(defun time-string (time)
  "Return a string representing this internal time in min:secs."
  (multiple-value-bind (min sec)
      (floor (round time internal-time-units-per-second) 60)
    (format nil "~2d:~2,'0d" min sec)))


       
    

;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: reversi -*-
;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           io-clim.lisp
;;;;  Purpose:        CLIM GUI for reversi
;;;;  Programer:      Kevin M. Rosenberg
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: io-clim.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg 
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(in-package #:reversi)

#+mcclim (shadowing-import 'clim-internals::stream-set-cursor-position)

(defparameter cell-inner-width 40)
(defparameter cell-inner-height 40)
(defparameter half-cell-inner-width 20)
(defparameter half-cell-inner-height 20)
(defparameter line-thickness 2)
(defparameter piece-radius 16)
(defparameter cell-width (+ line-thickness cell-inner-width))
(defparameter cell-height (+ line-thickness cell-inner-height))
(defparameter label-height 42)
(defparameter label-width 42)

(defparameter board-width (+ 30 (* 8 cell-width)))
(defparameter board-height (+ 30 (* 8 cell-height)))

(defparameter status-width 300)


(defstruct (gui-player (:constructor make-gui-player-struct))
  id name searcher eval ply strategy start-time
  searcher-id eval-id)

(defun make-gui-player (&key id name strategy searcher-id eval-id (ply 0))
  (let ((p (make-gui-player-struct :id id :ply ply
				   :name name :strategy strategy
				   :searcher-id searcher-id :eval-id eval-id))
	(search-func
	 (cond
	  ((eq searcher-id :human)
	   #'human)
	  ((eq searcher-id :minimax)
	   #'minimax-searcher)
	  ((eq searcher-id :alpha-beta)
	   #'alpha-beta-searcher)
	  ((eq searcher-id :alpha-beta2)
	   #'alpha-beta-searcher2)
	  ((eq searcher-id :alpha-beta3)
	   #'alpha-beta-searcher3)
	  ((eq searcher-id :random)
	   #'random-strategy)))
	(eval-func
	 (cond
	  ((eq eval-id :difference)
	   #'count-difference)
	  ((eq eval-id :weighted)
	   #'weighted-squares)
	  ((eq eval-id :modified-weighted)
	   #'modified-weighted-squares)
	  ((eq eval-id :iago)
	   #'iago-eval))))
    (unless strategy
      (cond
       ((eq search-func #'human)
	)
       ((eq search-func #'random-strategy)
	(setf (gui-player-strategy p) search-func))
       (t
	(setf (gui-player-strategy p)
	  (funcall search-func ply eval-func)))))
    p))


(defun gui-player-human? (gp)
  (eql (gui-player-searcher-id gp) :human))

(defun current-gui-player (frame)
    (if frame
	(aif (reversi-game frame)
	     (cond
	       ((null (player it))
		nil)
	       ((= (player it) black)
		(black-player frame))
	       ((= (player it) white)
		(white-player frame))
	       (t
		nil))
	     nil)
      nil))

(defun current-gui-player-human? (frame)
  #+ignore
  (aif (current-gui-player frame)
       (gui-player-human? it)
       nil)
  (gui-player-human? (current-gui-player frame))
  )

(define-application-frame reversi ()
  ((game :initform nil
	 :accessor reversi-game)
   (minutes :initform 30
	    :accessor minutes)
   (black-player :initform nil
		 :accessor black-player)
   (white-player :initform  nil
		 :accessor white-player)
   (debug-messages :initform nil
		   :accessor debug-messages)
   (msgbar-string :initform nil
	     :accessor msgbar-string)
   (human-time-start :initform nil
		     :accessor reversi-human-time-start))
  (:panes
    (board :application
	     :display-function 'draw-board
	     :text-style '(:sans-serif :bold :very-large)
;;	     :incremental-redisplay t
	     :text-cursor nil
	     :background +green+
	     :borders nil
	     :scroll-bars nil
	     :width (+ label-width board-width)
	     :height (+ label-height  board-height)
	     :min-width board-width
	     :min-height board-height
	     :max-width +fill+
	     :max-height +fill+
	     )
    (status :application
	     :display-function 'draw-status
	     :text-style '(:sans-serif :bold :large)
	     :incremental-redisplay t
	     :text-cursor nil
	     :background +white+
	     :scroll-bars nil
	     :width status-width
	     :max-width +fill+
	     :max-height +fill+
	     :height :compute)
    (history :application
	     :display-function 'draw-history
	     :text-style '(:fix :roman :normal)
	     :incremental-redisplay t
	     :text-cursor nil
	     :background +white+
	     :width 220 
	     :height :compute
	     :min-width 100
	     :initial-cursor-visibility :on
	     :scroll-bars :vertical
	     :max-width +fill+
	     :max-height +fill+
             :end-of-page-action :scroll
	     :end-of-line-action :scroll)
    (debug-window :application
	     :display-function 'draw-debug-window
	     :text-style '(:serif :roman :normal)
	     :incremental-redisplay t
	     :text-cursor nil
	     :background +white+
	     :width :compute 
	     :height :compute
	     :scroll-bars :vertical
	     :max-width +fill+
	     :max-height +fill+
	     :end-of-page-action :scroll
	     :end-of-line-action :scroll
	     )
    (msgbar :application
	     :display-function 'draw-msgbar
	     :text-style '(:sans-serif :roman :normal)
	     :incremental-redisplay t
	     :text-cursor nil
	     :background (make-rgb-color 0.75 0.75 0.75)
	     :foreground +red+
	     :scroll-bars nil
	     :width :compute
	     :height 25
	     :max-width +fill+
	     :max-height +fill+
	     :end-of-page-action :scroll
	     :end-of-line-action :scroll))
  (:pointer-documentation nil)
  (:command-table (reversi
		   :inherit-from (user-command-table
				  reversi-game-table
				  reversi-help-table)
		     :menu (("Game"
			     :menu reversi-game-table
			     :keystroke #\G  
			     :documentation "Game commands")
			    ("Help"
			     :menu reversi-help-table
			     :keystroke #\H
			     :documentation "Help Commands"))))
  (:menu-bar t)
  (:layouts
   (default 
       (horizontally   () 
	   (vertically   () 
	     (horizontally ()
	       board status)
	     msgbar
	     debug-window)
	   history)
       ))
  )

 ;;(:spacing 3) 

(defmethod frame-standard-input ((reversi reversi))
  (get-frame-pane reversi 'debug-window))

(defmethod frame-standard-output ((reversi reversi))
  (get-frame-pane reversi 'debug-window))

(defmethod run-frame-top-level :before ((reversi reversi) &key)
  (initialize-reversi reversi))


(defmethod read-frame-command ((reversi reversi) &key (stream *standard-input*))
  (let ((abort-chars #+Genera '(#\Abort #\End)
		     #-Genera nil))
    (let ((command (read-command-using-keystrokes
		     (frame-command-table reversi) abort-chars
		     :stream stream)))
      (if (characterp command)
	  (frame-exit reversi)
	command))))

(define-presentation-type reversi-cell ()
 :inherit-from '(integer 11 88))

#-lispworks
(define-presentation-method highlight-presentation ((type reversi-cell) 
						    record stream state)
  state
  (multiple-value-bind (xoff yoff)
      (clim::convert-from-relative-to-absolute-coordinates 
       stream (output-record-parent record))
    (with-bounding-rectangle* (left top right bottom) record
      (draw-rectangle* stream
		       (+ left xoff) (+ top yoff)
		       (+ right xoff) (+ bottom yoff)
		       :ink +flipping-ink+))))

(define-reversi-command com-select-cell ((move 'reversi-cell))  
  (with-application-frame (frame)
    (with-slots (game) frame
      (let ((gui-player (current-gui-player frame)))
	(when (and game gui-player (gui-player-human? gui-player))
	  (if (not (legal-p move (gui-player-id gui-player) (board game)))
	      (set-msgbar frame
			  (format nil "Illegal move: ~a"
				  (symbol-name (88->h8 move))))
	    (progn
	      (decf (elt (clock game) (player game)) 
		    (- (get-internal-real-time) (gui-player-start-time gui-player)))
	      (make-move-gui game move (gui-player-id gui-player))
	      (setf (player game) (next-to-play (board game) (player game)))
	      (get-move-gui frame))))))))
	       

(define-presentation-to-command-translator select-cell
    (reversi-cell com-select-cell reversi 
     :documentation "Select cell"
     :tester ((object frame window) (cell-selectable-p object frame window)))
    (object)
    (list object))

(defun cell-selectable-p (object frame window)
  (when (and (eq (get-frame-pane frame 'board) window)
	     (reversi-game frame))
    (let ((game (reversi-game frame)))
      (if (legal-p object (player game) (board game))
	  t
	nil))))



(defun new-game-gui (frame)
  (setf (reversi-game frame) 
    (make-game 
     (gui-player-strategy (black-player frame))
     (gui-player-strategy (white-player frame))
     :record-game t
     :print nil
     :minutes (minutes frame)))
  (set-msgbar frame "New Game")
  (get-move-gui frame))


	  
(defmethod initialize-reversi ((reversi reversi))
  (setf (black-player reversi) 
    (make-gui-player :id black :searcher-id :human)
    )
  (setf (white-player reversi)
    (make-gui-player :id white 
		     :searcher-id :alpha-beta3 
		     :eval-id :iago
		     :ply 5)))


(defun square-number (row column)
  (declare (fixnum row column))
  (+ (* 10 (1+ row))
     (1+ column)))

(defmethod draw-status ((reversi reversi) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((game (reversi-game reversi)))
    (when game
      (if (null (player game))
	  (progn
	    (setf (final-result game) (count-difference black (board game)))
	    (format stream "Game Over~2%"))
	(format stream "Move Number ~d~2%" (move-number game)))
      (format stream "Pieces~%  ~a ~2d~%  ~a ~2d~%  Difference ~2d~2&"
	      (title-of black) (count black (board game))
	      (title-of white) (count white (board game))
	      (count-difference black (board game)))
      (when (clock game)
	(format stream "Time Remaining~%  ~a ~a~%  ~a ~a~2%"
		(title-of black) (time-string (elt (clock game) black))
		(title-of white) (time-string (elt (clock game) white))))
      (let ((gui-player (current-gui-player reversi)))
	(when (and gui-player (gui-player-human? gui-player))
	  (let ((legal-moves
		 (loop for move in (legal-moves (gui-player-id gui-player)
						(board game))
		     collect (symbol-name (88->h8 move)))))
	    (if legal-moves
		(format stream "Valid Moves~%~A" 
			(list-to-delimited-string legal-moves #\space)))))
	(when (null (player game))
	  (cond
	    ((zerop (final-result game))
	     (format stream "It's a draw!"))
	    ((plusp (final-result game))
	      (format stream "Black wins by ~d!" (final-result game)))
	    (t
	     (format stream "White wins by ~d!" (- 0 (final-result game))))))))))



(defmethod add-debug ((reversi reversi) msg)
  (setf (debug-messages reversi) (append (debug-messages reversi) (list msg))))

(defmethod set-msgbar ((reversi reversi) msg)
  (setf (msgbar-string reversi) msg))

(defmethod draw-debug-window ((reversi reversi) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (filling-output (stream)
    (dolist (msg (debug-messages reversi))
      (princ msg stream)
      (terpri stream))))

(defmethod draw-msgbar ((reversi reversi) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (when (msgbar-string reversi)
    (princ (msgbar-string reversi) stream)))


(defmethod draw-history ((reversi reversi) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((game (reversi-game reversi)))
    (when (and game (> (move-number game) 1))
      (formatting-item-list (stream :move-cursor t :row-wise nil :n-columns 1)
	(dotimes (i (1- (move-number game)))
	    (let ((state (aref (moves game) i)))
	      (when state
		(let ((str (format nil "~2d: ~5a ~2a"
				   (1+ i) (title-of (state-player state)) 
				   (88->h8 (state-move state)))))
		  (updating-output (stream :unique-id i :cache-value str)
		    (with-end-of-page-action (stream :scroll)
		      (formatting-cell (stream :align-x :right :align-y :top)
			(format stream str)
			(terpri stream))))))))))))

#+ignore
(defmethod draw-history ((reversi reversi) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((game (reversi-game reversi)))
    (when (and game (> (move-number game) 1))
      (formatting-item-list (stream :move-cursor t :row-wise nil :n-columns 2)
	(dotimes (i (1- (move-number game)))
	    (let ((state (aref (moves game) i)))
	      (when state
		(let ((str (format nil "~2d: ~5a ~2a"
				   (1+ i) (title-of (state-player state)) 
				   (88->h8 (state-move state)))))
		  (updating-output (stream :unique-id i :cache-value str)
		    (with-end-of-page-action (stream :scroll)
		      (formatting-cell (stream :align-x :right :align-y :top)
			(format stream str)
			(terpri stream))))))))))))


#|
      (let ((viewport (window-viewport stream)))
	(multiple-value-bind (x y) (stream-cursor-position stream)
	  (add-debug reversi (format nil "~d ~d: ~s" x y viewport))
	  (if (> y (bounding-rectangle-bottom viewport))
	      (decf y (bounding-rectangle-bottom viewport)))
	  (window-set-viewport-position stream 0 0))))))
  |#    
      
		


(defvar *reversi-frame* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *force*
  #+(and os-threads microsoft-32)
  t
  #-(and os-threads microsoft-32)
  nil))

(defun clim-reversi ()
  (unless (or *force* (null *reversi-frame*))
    (setq *reversi-frame* (make-application-frame 'reversi)))
  (setq *reversi-frame* (run-frame 'reversi *reversi-frame*)))


(defun run-frame (frame-name frame)
  (flet ((do-it ()
	   (when (or *force* (null frame))
	     (setq frame (make-application-frame frame-name)))
	   (run-frame-top-level frame)))
    #+allegro
    (mp:process-run-function (write-to-string frame-name) #'do-it)
    #-allegro
    (do-it))
  frame)


(define-command-table reversi-game-table
    :menu (("New" :command com-reversi-new)
	   ("Backup" :command (com-reversi-backup))
	   ("Exit" :command (com-reversi-exit))))

(define-command-table reversi-help-table)


(define-command (com-reversi-new :name "New Game"
				 :command-table reversi-game-table
				 :keystroke (:n :control)
				 :menu ("New Game" 
					:after :start
					:documentation "New Game"))
    ()
  (with-application-frame (frame)
    (new-game-gui frame)))

(define-command (com-reversi-recommend :name "Recommend Move"
				       :command-table reversi-game-table
				       :keystroke (:r :control)
				       :menu ("Recommend Move" 
					      :after "New Game"
					      :documentation "Recommend Move"))
    ()
  (with-application-frame (frame)
    (let ((game (reversi-game frame))
	  (player (current-gui-player frame)))
      (when (and game player)
	(when (gui-player-human? player)
	  (let* ((port (find-port))
		 (pointer (port-pointer port)))
	    (when pointer
	      (setf (pointer-cursor pointer) :busy))
	  (set-msgbar frame "Thinking...")
	  (let ((move (funcall (iago 8) (gui-player-id player)
			       (board game))))
	    (when pointer
	      (setf (pointer-cursor pointer) :default))
	    (when move
	      (set-msgbar frame
			  (format nil "Recommend move to ~a"
				  (symbol-name (88->h8 move))))))))))))

(define-command (com-reversi-backup :name "Backup Move"
				    :command-table reversi-game-table
				    :keystroke (:b :control)
				    :menu ("Backup Move" 
					   :after "Recommend Move"
					   :documentation "Backup Move"))
    ()
  (with-application-frame (frame)
    (let ((game (reversi-game frame)))
      (when (and game (> (move-number game) 2))
	(reset-game game (- (move-number game) 2))))))


(define-command (com-reversi-exit :name "Exit"
				  :command-table reversi-game-table
				  :keystroke (:q :control)
				  :menu ("Exit" 
					 :after "Backup Move"
					 :documentation "Quit application"))
    ()
  (clim:frame-exit clim:*application-frame*))


(define-command (com-reversi-options :name "Game Options"
				 :command-table reversi-game-table
				 :menu ("Game Options" :documentation "Game Options"))
    ()
  (with-application-frame (frame)
    (game-dialog frame)))



;(define-command-table reversi-game
;  :inherit-from (reversi-game-table)
;  :inherit-menu t)

;(define-command-table reversi-help)
;    :inherit-from (reversi-help-commands)
;    :inherit-menu t)

(define-command (com-about :command-table reversi-help-table
			   :menu
			   ("About Reversi"
			    :after :start
			    :documentation "About Reversi"))
    ()
  t)
;;  (acl-clim::pop-up-about-climap-dialog *application-frame*))



(defun make-move-gui (game move player)
    (make-game-move game move player))
  
(defun get-move-gui (frame)
  (let ((gui-player (current-gui-player frame)))
    (when gui-player
      (if (gui-player-human? gui-player)
	  (setf (gui-player-start-time gui-player) (get-internal-real-time))
	(computer-move gui-player frame)))))

(defun computer-move (gui-player frame)
  (let* ((game (reversi-game frame))
	 (port (find-port))
	 (pointer (port-pointer port)))
    (setq pointer nil) ;; pointer causes crash in CLIM. ? port value wrong
    (when pointer
      (setf (pointer-cursor pointer) :busy))
    (set-msgbar frame "Thinking...")
    (while (eq gui-player (current-gui-player frame))
	   (setf (gui-player-start-time gui-player) 
	     (get-internal-real-time))
	   (let ((move (funcall (gui-player-strategy gui-player)
				(player game) 
				(replace-board *board* (board game)))))
	     (when (and move (legal-p move (player game) (board game)))
	       (decf (elt (clock game) (player game)) 
		     (- (get-internal-real-time) 
			(gui-player-start-time gui-player)))
	       (make-move-gui game move (player game))
	       (setf (player game) 
		 (next-to-play (board game) (player game))))))
    (set-msgbar frame nil)
    (when pointer
      (setf (pointer-cursor pointer) :default)))
  (setq gui-player (current-gui-player frame))

  (if (and gui-player (not (gui-player-human? gui-player)))
    (redisplay-frame-pane frame (get-frame-pane frame 'board)))
  (get-move-gui frame))

 


(defun game-dialog (frame)
  (let* ((stream (get-frame-pane frame 'debug-window))
	 ;;	 (white-strategy-id (white-strategy-id frame)
	 ;;	 (black-strategy-id (black-strategy-id frame))
	 (wh (white-player frame))
	 (bl (black-player frame))
	 (white-searcher (gui-player-searcher-id wh))
	 (white-evaluator (gui-player-eval-id wh))
	 (white-ply (gui-player-ply wh))
	 (black-searcher (gui-player-searcher-id bl))
	 (black-evaluator (gui-player-eval-id bl))
	 (black-ply (gui-player-ply bl))
	 (minutes (minutes frame)))
    
    (accepting-values (stream :own-window t
			      :label "Reversi Parameters")
      (setq minutes
	(accept 'integer 
		:stream stream
		:prompt "Maximum minutes" :default minutes))
      (terpri stream)
      (format stream "White Player~%")
      (setq white-searcher
	(accept '(member :human :random :minimax :alpha-beta3) 
		:stream stream
		:prompt "White Player Search" :default white-searcher))
      (terpri stream)
      (setq white-evaluator
	(accept '(member :difference :weighted :modified-weighted :iago) 
		:stream stream
		:prompt "White Player Evaluator" :default white-evaluator))
      (terpri stream)
      (setq white-ply 
	(accept 'integer 
		:stream stream
		:prompt "White Ply" :default white-ply))
      (terpri stream)
      (terpri stream)
      (format stream "Black Player~%")
      (terpri stream)
      (setq black-searcher
	(accept '(member :human :random :minimax :alpha-beta3) 
		:stream stream
		:prompt "Black Player Search" :default black-searcher))
      (terpri stream)
      (setq black-evaluator
	(accept '(member :difference :weighted :modified-weighted :iago) 
		:stream stream
		:prompt "Black Player Evaluator" :default black-evaluator))
      (terpri stream)
            (setq black-ply 
	      (accept 'integer 
		      :stream stream
		      :prompt "Black Ply" :default black-ply))
      (terpri stream)
      )
    (setf (minutes frame) minutes)
    (setf (white-player frame) (make-gui-player :id white 
					 :searcher-id white-searcher
					 :eval-id white-evaluator
					 :ply white-ply))
    (setf (black-player frame) (make-gui-player :id black 
					 :searcher-id black-searcher
					 :eval-id black-evaluator
					 :ply black-ply))
    ))


(defmethod draw-board ((reversi reversi) stream &key max-width max-height)
  "This should produce a checkerboard pattern."
  (declare (ignore max-width max-height))
  (let ((game (reversi-game reversi)))
    (dotimes (i 8)
      (draw-text stream 
		 (elt "abcdefgh" i)
		 (make-point
		  (+ label-width (* cell-width i)
		     half-cell-inner-width)
		  0)
		 :align-x :center :align-y :top))
    (dotimes (i 8)
      (draw-text stream 
		 (format nil "~d" (1+ i))
		 (make-point
		  0
		  (+ label-height (* cell-height i)
		       half-cell-inner-height))
		 :align-x :left :align-y :center))
    (stream-set-cursor-position stream label-width label-height)
    (surrounding-output-with-border (stream)
      (formatting-table (stream :y-spacing 0 :x-spacing 0)
	(dotimes (row 8)
	  (formatting-row (stream)
	    (dotimes (column 8)
	      (let* ((cell-id (square-number row column))
		     (value 
		      (if game
			  (bref (board game) cell-id)
			empty)))
		(updating-output (stream :unique-id cell-id 
					 :cache-value value)
		  (formatting-cell (stream :align-x :right :align-y :top)
		    (with-output-as-presentation (stream cell-id 'reversi-cell)
		      (draw-rectangle* stream 0 0 cell-width cell-height :filled t :ink +green+)
		      (draw-rectangle* stream 0 0 cell-width cell-height :filled nil)
		      (cond
		       ((= value black)
			(draw-circle* 
			 stream 
			 half-cell-inner-width 
			 half-cell-inner-height 
			 piece-radius :filled t :ink +black+))
		       ((= value white)
			(draw-circle* 
			 stream 
			 half-cell-inner-width 
			 half-cell-inner-height 
			 piece-radius :filled t :ink +white+))))))))))))))




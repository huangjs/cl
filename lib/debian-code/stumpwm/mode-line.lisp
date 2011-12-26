;; Copyright (C) 2006 Shawn Betts
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; a modeline that can enabled per screen and per frame (todo).
;;
;; Code:

;; to figure out this memory-fault error
(declaim (optimize (debug 3)))

(in-package "STUMPWM")

(defstruct mode-line
  window
  format
  position
  gc)

(defvar *mode-line-screen-position* :top
  "Where should the mode line be displayed? :top or :bottom.")

(defvar *mode-line-frame-position* :top
  "Where should the mode line be displayed? :top or :bottom.")

(defvar *mode-line-border-width* 1
  "How thick shall the mode line border be?")

(defvar *mode-line-pad-x* 5
  "How much padding should be between the modeline text and the sides")

(defvar *mode-line-pad-y* 1
  "How much padding should be between the modeline text and the top/bottom")

(defvar *mode-line-background-color* "Gray20"
  "mode line background color")

(defvar *mode-line-foreground-color* "Gray50"
  "mode line foreground color")

(defvar *mode-line-border-color* "Gray30"
  "mode line border color")

(defvar *screen-mode-line-format* "%w"
  "A template for displaying mode line for each screen. Turn it
on with the function TOGGLE-MODE-LINE or the mode-line command.

It is a list where each element may be a string, a symbol, or a list.

For a symbol its value is used.

For a list of the form (:eval FORM) FORM is evaluated and the
result is used as a mode line element.

A string is printed verbatim in the mode line except for
%-constructs:

%w -- print the window list
")

(defvar *screen-mode-line-formatters* '((#\w fmt-window-list)
					(#\g fmt-group-list))
  "An alist containing format character format function pairs for
formatting screen mode-lines. functions are passed the screen's
current group.")

(defvar *current-mode-line-formatters* nil
  "used in formatting modeline strings.")

(defvar *current-mode-line-formatter-args* nil
  "used in formatting modeline strings.")

(defvar *mode-line-timeout* 60
  "The amount of time between modeline updates.")

(defvar *mode-line-timer* nil
  "The timer that updates the modeline")

(defun make-mode-line-window (parent screen)
  "Create a window suitable for a modeline."
  (xlib:create-window
   :parent parent
   :x 0 :y 0 :width 1 :height 1
   :background (get-color-pixel screen *mode-line-background-color*)
   :border (get-color-pixel screen *mode-line-border-color*)
   :border-width *mode-line-border-width*
   ;; You can click the modeline
   :event-mask (xlib:make-event-mask :button-press :exposure)
   ;; these windows are not controlled by the window manager
   :override-redirect :on))

(defgeneric resize-mode-line-for (mode-line obj)
  (:documentation "resize the modeline for a screen, frame, ...other?"))

(defmethod resize-mode-line-for (ml (obj screen))
  (setf (xlib:drawable-width (mode-line-window ml)) (- (screen-width obj) (* 2 (xlib:drawable-border-width (mode-line-window ml))))
	(xlib:drawable-height (mode-line-window ml)) (+ (font-height (xlib:gcontext-font (mode-line-gc ml))) (* *mode-line-pad-y* 2))
	(xlib:drawable-x (mode-line-window ml)) 0
	(xlib:drawable-y (mode-line-window ml)) (if (eq (mode-line-position ml) :top)
						    0 
						    (screen-height obj))))

(defgeneric mode-line-format-elt (elt))

(defmethod mode-line-format-elt ((elt string))
  (apply 'format-expand *current-mode-line-formatters* elt 
	 *current-mode-line-formatter-args*))

(defmethod mode-line-format-elt ((elt symbol))
  (if (boundp elt)
      (let ((val (symbol-value elt)))
	;; ignore T and nil, like emacs.
	(unless (or (eq val t)
		    (eq val nil))
	  (mode-line-format-elt val)))
      (symbol-name elt)))

(defmethod mode-line-format-elt ((elt null))
  "")

(defmethod mode-line-format-elt ((elt list))
  (etypecase (first elt)
    ((or string list)
     (apply 'concatenate 'string
	    (mapcar 'mode-line-format-elt elt)))
    (symbol
     (mode-line-format-elt
      (case (first elt)
	;; FIXME: silently failing is probably not the best idea.
	(:eval (ignore-errors (eval (second elt))))
	(t (and (boundp (first elt))
		(symbol-value (first elt))
		(second elt))))))))

(defun mode-line-format-string (ml)
  (mode-line-format-elt (mode-line-format ml)))

(defun make-mode-line-gc (window screen)
  (xlib:create-gcontext :drawable window
			:font (screen-font screen)
			:foreground (get-color-pixel screen *mode-line-foreground-color*)
			:background (get-color-pixel screen *mode-line-background-color*)))

(defun make-screen-mode-line (screen format)
  (let ((w (make-mode-line-window (screen-root screen) screen)))
    (xlib:map-window w)
    (make-mode-line :window w
		    :format format
		    :position *mode-line-screen-position*
		    :gc (make-mode-line-gc w screen))))

;; This is a regular defun in an attempt to get rid of the
;; MEMORY-FAULT return-from error.

;; (defgeneric redraw-mode-line-for (ml thing)
;;   (:documentation "redraw the modeline for a screen, frame, ...other?"))

;; (defmethod redraw-mode-line-for (ml (obj screen))
(defun redraw-mode-line-for (ml screen)
  (let* ((*current-mode-line-formatters* *screen-mode-line-formatters*)
	 (*current-mode-line-formatter-args* (list (screen-current-group screen)))
	 (string (mode-line-format-string ml)))
    (xlib:draw-image-glyphs (mode-line-window ml) (mode-line-gc ml)
			    *mode-line-pad-x*
			    (+ (xlib:font-ascent (xlib:gcontext-font (mode-line-gc ml)))
			       *mode-line-pad-y*)
			    string
			    :translate #'translate-id
			    :size 16)
    ;; Just clear what we need to. This reduces flicker.
    (xlib:clear-area (mode-line-window ml)
		     :x (+ *mode-line-pad-x*
			   (xlib:text-width (xlib:gcontext-font (mode-line-gc ml)) string
					    :translate #'translate-id)))))

(defun update-screen-mode-lines ()
  (dolist (i *screen-list*)
    (when (screen-mode-line i)
      (redraw-mode-line-for (screen-mode-line i) i))))

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer* (run-with-timer *mode-line-timeout*
                                          *mode-line-timeout*
                                          'update-screen-mode-lines)))

(defun maybe-cancel-mode-line-timer ()
  (unless (find-if 'screen-mode-line *screen-list*)
    (when (timer-p *mode-line-timer*)
      (cancel-timer *mode-line-timer*)
      (setf *mode-line-timer* nil))))

(defun toggle-mode-line (screen &optional (format '*screen-mode-line-format*))
  (check-type screen screen)
  (check-type format (or symbol list string))
  (if (screen-mode-line screen)
      (progn
	(dolist (group (screen-groups screen))
	  (when (eq (mode-line-position (screen-mode-line screen)) :top)
	    (offset-frames group 0 (- (true-height (mode-line-window (screen-mode-line screen))))))
	  (expand-tree (tile-group-frame-tree group) (true-height (mode-line-window (screen-mode-line screen))) :bottom)
	  (sync-all-frame-windows group))
	(xlib:destroy-window (mode-line-window (screen-mode-line screen)))
	(xlib:free-gcontext (mode-line-gc (screen-mode-line screen)))
	(setf (screen-mode-line screen) nil)
        (maybe-cancel-mode-line-timer))
      (progn
	(setf (screen-mode-line screen) (make-screen-mode-line screen format))
	(resize-mode-line-for (screen-mode-line screen) screen)
	(redraw-mode-line-for (screen-mode-line screen) screen)
	;; move the frames
	(dformat 3 "modeline: ~s~%" (screen-mode-line screen))
	(dolist (group (screen-groups screen))
	  (when (eq (mode-line-position (screen-mode-line screen)) :top)
	    (offset-frames group 0 (true-height (mode-line-window (screen-mode-line screen)))))
	  (expand-tree (tile-group-frame-tree group) (- (true-height (mode-line-window (screen-mode-line screen)))) :bottom)
	  (sync-all-frame-windows group))
        ;; setup the timer
        (turn-on-mode-line-timer))))

(defun screen-mode-line-mode (screen arg &optional (format '*screen-mode-line-format*))
  "Turn on the mode line for SCREEN if and only if ARG is non-nil."
  ;; only do something if the state they want and the current state
  ;; differ.
  (check-type screen screen)
  (check-type format (or symbol list string))
  (unless (eq (and arg t)
	      (and (screen-mode-line screen) t))
    (toggle-mode-line screen format)))
      
(define-stumpwm-command "mode-line" ()
  (toggle-mode-line (current-screen)))

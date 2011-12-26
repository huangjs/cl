;; This file is part of GRT, a Common Lisp raytracing system
;; Copyright (C) 2002 Nikodemus Siivola
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA  02111-1307  USA

(in-package grt)

(declaim (type sdl:uint32 +rmask+ +gmask+ +bmask+ +amask+))
;; Set masks
(let ((big-e (= (sdl:byte-order) sdl:+big-endian+)))
  (defparameter +rmask+ (if big-e #xff000000 #x000000ff))
  (defparameter +gmask+ (if big-e #x00ff0000 #x0000ff00))
  (defparameter +bmask+ (if big-e #x0000ff00 #x00ff0000))
  (defparameter +amask+ (if big-e #x000000ff #xff000000)))
(defparameter +bpp+  32)

(defstruct window
  video
  buffer)

(declaim
 (inline draw-pixel)
 (ftype (function
	 (window (unsigned-byte 32) (unsigned-byte 32)
		 (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8))
	 (values))
	draw-pixel))
(defun draw-pixel (window x y r g b)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sdl:draw-pixel (window-buffer window) x y r g b)
  (values))

(defun open-window (width height)
  "Opens an X window for drawing."
  (sdl:init (logior sdl:+init-video+))
  (let ((surface (sdl:set-video-mode width height +bpp+
				     (logior sdl:+resizable+
					     sdl:+hwsurface+))))
    (when (sgum:null-pointer-p surface)
      (error "Unable to set video mode"))
    (sdl:wm-set-caption "GRT Image" nil)
    (make-window :video surface
		 :buffer (sdl:create-rgb-surface sdl:+swsurface+
						 width height +bpp+ 
						 +rmask+ +gmask+
						 +bmask+ +amask+))))

(defun event-loop (update-fn)
  (declare (type function update-fn))
  (sdl:event-loop
   ;; This allows us to exit when 'q' is typed and behave
   ;; ``normally'' the rest of the time.
   (:key-down (scan-code key mod unicode)
	      (cond ((= key (char-code #\q))
		     (return))))
   (:quit () (return))
   (:idle ()
	  (funcall update-fn))))

(defun wait-for-close (window)
  (event-loop (lambda ())))

(declaim (inline refresh-window))
(defun refresh-window (window)
  "Wrapper function for sdl:blit-surface"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sdl:blit-surface (window-buffer window) sgum:+null-pointer+
		    (window-video window) sgum:+null-pointer+)
  (sdl:update-rect (window-video window) 0 0 0 0)
  (values))

(defun close-window (window)
  (declare (ignorable window))
  (sdl:quit))


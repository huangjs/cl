;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-

(in-package :cmn)

;;; to port to a new graphics environment, we need at a minimum lineto, moveto, and a polygon filler.
;;; next step involves text, arc, font/color/line-width settings.
;;; then curveto (Bezier curves), dashed lines, various filling routines, etc.
;;;
;;; Postscript's fill routine is substantially smarter than either X's or Quickdraw's.
;;; In some cases, we can get the latter to work by using the "even-odd" rule, rather than
;;; the default "winding" rule, modulo screwing around with curve direction and so on.
;;; A number of music symbols are complex enough that nothing works completely -- I'll
;;; probably have to make two versions, one broken into separate pieces for X and Quickdraw.


;;; -------------------------------- G-COMMENT --------------------------------

(defun g-comment (score x)
  ;; place a comment in the graphics output stream 
  ;; this is purely a debugging convenience, so it can be ignored
  (format (output score) "% ~A~%" x))


;;; -------------------------------- G-PUSH and G-POP (matrix handlers) --------------------------------

(defun g-push (score matrix)
  ;; matrix is the transformation matrix from the current coordinate system to the new one
  (format (output score) " gsave [ ~{~A ~}] concat~%" (map 'list #'not-rational matrix)))

(defun g-pop (score)
  (format (output score) " grestore~%"))


;;; -------------------------------- G-SET-LINE-WIDTH --------------------------------

(defun g-set-line-width (score xs)
  ;; set line-width for subsequent drawing (can be 0 = thinnest possible line)
  (format (output score) " ~A setlinewidth~%" (not-rational xs)))


;;; -------------------------------- G-SET-PATTERN --------------------------------
;;;
;;; if type is not nil or :outlined, we funcall the type field with the score as its argument.

(defun g-set-pattern (score type)
  (if (and type (not (eq type :outlined))) 
      (funcall type score)))



;;; -------------------------------- G-SET-COLOR --------------------------------

(defun g-set-color (score rgb)
  (format (output score) " ~{~,3F ~} setrgbcolor" rgb))


;;; -------------------------------- G-SET-FONT --------------------------------

(defun g-set-font (score font size)
  ;; establish font as new default
  (format (output score) " /~A findfont ~D scalefont setfont~%" font (round size)))


;;; -------------------------------- G-MOVETO and G-RMOVETO
;;;
;;; Our underlying model here is postscript's notion of a current path which can later be filled and whatnot.

(defun g-moveto (score xs ys)
  ;; move (without drawing) the current point to (xs ys)
  (format (output score) " ~,2F ~,2F moveto~%" xs ys))

(defun g-rmoveto (score dxs dys)
  ;; move the current point by (dxs dys)
  (format (output score) " ~,2F ~,2F rmoveto~%" dxs dys))


;;; -------------------------------- LINETO and RLINETO

(defun g-lineto (score xs ys)
  ;; draw a line (hopefully in the current line-width) from the current point to (xs ys)
  ;; the current point in cmn is [(x score),(y score)]
  (format (output score) " ~,2F ~,2F lineto~%" xs ys))

(defun g-rlineto (score dxs dys)
  ;; draw a line from the current point by (dxs dys)
  (format (output score) " ~,2F ~,2F rlineto~%" dxs dys))


(defun g-dashed-line (score x y pattern rl)
  ;; can be horizontal or vertical (dashed bar line for example)
  (format (output score) " ~,2F ~,2F ~A [~{~A ~}] 0 setdash stroke~% [] 0 setdash~%" 
	  x y 
	  (if rl "rlineto" "lineto")
	  (map 'list #'not-rational pattern)))



;;; -------------------------------- BEZIER CURVES --------------------------------

(defun g-curveto (score x0 y0 x1 y1 x2 y2)
  ;; draw Bezier curve
  ;; Postscript uses a cubic Bezier curve, as opposed to the quadratic Quickdraw font-manager version
  (format (output score) " ~,2F ~,2F ~,2F ~,2F ~,2F ~,2F curveto~%" x0 y0 x1 y1 x2 y2))



;;; -------------------------------- ARCS --------------------------------
;;;
;;; CMN only calls for an arc when it is assuming no current path is in progress
;;; (that is, from PostScript's point of view, there is always a preceding newpath).

(defun g-arc (score x y r ang1 ang2 &optional fill)
  ;; arc of a circle centered at (x y) radius r in current line-width or (if fill) filled, partial arc if ang1 ang2
  ;; ang1=0 and ang2=360 is a full circle (very common special case)
  ;; in Postscript angle increases counterclockwise, 0=>x axis (3 o'clock), ang2 is from 0, not from ang1
  (format (output score) " ~,3F ~,3F ~,3F ~D ~D newpath arc~A~%" x y r ang1 ang2 (if fill " fill" " stroke")))


;;; -------------------------------- DRAW and FILL --------------------------------
;;;
;;; This is where all the actual work takes place

(defun g-draw (score &optional width)
  ;; if some object is awaiting a draw decision, send it out
  (if (and (numberp width) (/= width 0))
      (format (output score) " ~D setlinewidth stroke 0 setlinewidth~%" width)
    (format (output score) " stroke~%")))

(defun g-fill (score cmd)
  ;; here we need to grab all the points as a possibly self-intersecting polygon
  (format (output score) " ~A~%" cmd))


;;; -------------------------------- TEXT --------------------------------

(defun g-text (score txt &optional outlined)
  ;; in PS, assume font is set up
  ;; in others, make sure current font is active
  (format (output score) " (~A) ~A~%" (ps-letters txt) (if outlined "false charpath stroke" "show")))


(defun g-mustext (score txt dx dy)
  (when (output score)
    (moveto score dx dy)
    (g-set-font score (music-font score) (size score))
    (format (output score) " (\\~O) show~%" txt)))


  
;;; -------------------------------- OPEN and CLOSE --------------------------------

(defun g-header (score title user-name)
  (format (output score) "%!PS-Adobe-2.0 EPSF-2.0~%") ;this is what our Next's seem to have -- Adobe recommends 3.0
  ;; exact caps matter here --if you had %!Ps-Adobe, WriteNow would throw up its hands and refuse to paste it in.
  (if title (format (output score) "%%Title: ~A~%" title))
  (if user-name (format (output score) "%%Creator: ~A~%" user-name))
  (format (output score) "%%CreationDate: ~A~%" (creation-date))
  (if (bounded score)
      (progn
	(setf (box-x0 score) (first (bounded score)))
	(setf (box-y0 score) (second (bounded score)))
	(setf (box-x1 score) (third (bounded score)))
	(setf (box-y1 score) (fourth (bounded score)))
	(format (output score) "%%BoundingBox:~{ ~D~}~%" (map 'list #'ceiling (bounds score))))
    (format (output score) "%%BoundingBox:(atend)~%"))
  (format (output score) "%%EndComments~%")
  (if (prologue score) (loop for head in (reverse (prologue score)) do (funcall head score)))
  (format (output score) "%%EndProlog~%")
  (format (output score) "%%Page: 1 1~%")
  (if (page-color score) (format (output score) "clippath ~{~,3F ~} setrgbcolor fill " (page-color score)))
  (format (output score) "~{~,3F ~} setrgbcolor" (or (color score) '(0 0 0))))

(defun g-footer (score)
  (format (output score) " showpage~%%%Trailer~%"))

(defun g-bbox (score)
  (format (output score) "%%BoundingBox:~{ ~D~}~%" (map 'list #'(lambda (n) (round (1+ (* (scr-size score) n)))) (bounds score))))

(defun g-send (score cmd)
  (format (output score) " ~A~%" cmd))

(defun g-page-number (score page eject)
  (when eject (format (output score) " showpage~%"))
  (format (output score) "%%Page: ~D ~D~%" page page))



#+(and excl cltl2) (eval-when (compile load eval) (excl:advise excl:exit :before cmn-exit-advice nil (x-close)))
    
#+cmu (defun bye () (x-close) (extensions::quit)) ;who can remember which lisp uses which!?!?!?
#+cmu (defun exit () (x-close) (extensions::quit))
#+sbcl (defun bye () (sb-ext:quit))
#+sbcl (defun exit () (sb-ext:quit))
#+(and clisp (not ansi-cl) (not have-quit)) (defun quit () (lisp:bye))
#+(and clisp ansi-cl (not have-quit)) (defun quit () (bye))
#+(and clisp (not ansi-cl) (not have-quit)) (defun exit () (lisp:bye))
#+(and clisp ansi-cl (not have-quit)) (defun exit () (bye))
#+(and clisp have-quit) (defun quit () (ext::quit))
#+(and clisp have-quit) (defun exit () (ext::quit))
#+excl (defun bye () (excl:exit))
#+excl (defun quit () (excl:exit))
#+openmcl (defun bye () (ccl::quit))
#+openmcl (defun exit () (ccl::quit))
;;; nothing is more annoying than a program that will not exit!

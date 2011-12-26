
;;;;  Program to print out fonts 
;;;;  Warning!  This takes a long time!

;;;  These constants locate the printout on an 8.5 x 11 page.
;;;  All scaling is in points.
;;;  A 16 x 16 grid is used to show 256 chars.

(defconstant top 630)    ; top of grid
(defconstant delta 30)   ; distance between lines
(defconstant bottom (- top (* delta 16)))
(defconstant left 66)    ; left side of grid
(defconstant right (+ left (* delta 16)))

; Functions in this library shown at the end of this example
(library "lib.pl")

(defvar ch " ")    ; A scratch 1 character string 

(defun show-font (name fnt) 0  ; name is a symbol
    ;; start by drawing the header
    (font 30 times)          ; selects 30 point times-roman
    ;; Converting id's to strings requires a buffer.
    ;; The blank string is used as the buffer.
    (setf name (cvs name "                                    "))
    (center (concat-str "Sample of Font " name) 720)
    (setfont (scalefont fnt 18))
    (center name 700)
    (font 18 times)
    (center "Samples in 18 point type" 680)
    ;; Now, the grid
    (newpath)
    (dotimes (i 17)
              (moveto left (+ bottom (* i delta)))
              (lineto right (+ bottom (* i delta)))
              (moveto (+ left (* i delta)) bottom)
              (lineto (+ left (* i delta)) top))
    (setlinewidth 1)
    (setlinecap 1)
    (stroke)

    ;; Iterate through each of the 16 rows and columns
    (dotimes (r 16)
       (dotimes (c 16)
         (let ((charcode (+ (* r 16) c))) ; char to print
           (setf (elt ch 0) charcode)
           (gpreserve   ; in library.  Adds gsave and grestore
            ;; move to box for char in grid
            (translate (+ (* delta c) left)
                       (- top (* delta (1+ r))))
            (font 5 times)  ; for lettering charcode
            (moveto 3 25)
            (show (cvs charcode "    "))  ; integer to string
            (when (/= (stringwidth ch) 0) ; in case no character
               (moveto 10 10)
               (setfont (scalefont fnt 18))
               (show ch)
               (moveto 0 10)  ; centering ticks
               (lineto 4 10)
               (moveto 10 0)
               (lineto 10 4)
               (setlinewidth 0.3)
               (stroke))))))
    (showpage))

;;; Here is the top level of this program.  Iterate through every font
;;; in the font dictionary and print it.

;(dodict (n f (fontdirectory))
;     (show-font n f))

(show-font 'symbol (findfont 'symbol))


;;; This is a title page for the manual

;;;  This function sets the font to Times-Roman
(defun times (size) 0
   (setfont (scalefont (findfont 'Times-Roman) size)))  ; Choose a font

;;;  A simple function to place a string at some given point
(defun showat (str x y) 0
   (moveto x y)
   (show str))

;;; Main program.
(times 150)                     ; select 100 point type
(setlinejoin 1)                 ; rounded line joins
(dolist (width '(12 8 4))       ; Slowly narrow the stroke width
          (moveto 150 450)      ; where to place title
          (charpath "PLisp" nil); Outline of characters
          (setlinewidth width)  ; Set the width
          (setgray (* (- width 4) 0.1))  ; Slowly get blacker
          (stroke))             ; stroke it

(times 40)                      ; now for the easy part
(showat "A " 130 350)           ; make Lisp and PostScript line up
(let ((savex (currentpoint)))   ; save point at start of "Lisp"
   (show "Lisp to")    
   (showat "PostScript Compiler" savex 310)) ; line it up

(times 30)
(showat "John C. Peterson" 250 190)
(showat "University of Arizona" 250 160)

(showpage)  ; Needed to actually print the page


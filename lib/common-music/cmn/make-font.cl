;;; make a standard type 3 postscript font from the music glyphs in cmn-glyphs.lisp
;;;
;;; to use this, fire up an empty lisp (no cmn), comment out the in-package statement
;;; at the beginning of cmn-glyphs.lisp, load cmn-glyphs.lisp, load this file,
;;; call (make-font "hiho") or whatever.

(defun lineto (s a b) (format s "    ~,3F ~,3F lineto~%" a b))
(defun rlineto (s a b) (format s "    ~,3F ~,3F rlineto~%" a b))
(defun moveto (s a b) (format s "    ~,3F ~,3F moveto~%" a b))
(defun rmoveto (s a b) (format s "    ~,3F ~,3F rmoveto~%" a b))
(defun curveto (s a b c d e f) (format s "    ~,3F ~,3F ~,3F ~,3F ~,3F ~,3F curveto~%" a b c d e f))
(defun fill-in (s &key even-odd) (if even-odd (format s "    eofill~%") (format s "    fill~%")))
(defun circle (s x y r &optional (ang1 0) (ang2 360) fill) 
  (format s "    ~,3F ~,3F ~D ~D ~D newpath arc~A~%" x y r ang1 ang2 (if fill " fill" " stroke")))
(defun comment (s x) (declare (ignore s x)))
(defun draw (s &optional width) (declare (ignore width)) (format s "    stroke~%"))
(defun output-type (s) (declare (ignore s)))
(defun matrix-front (s &rest args) (declare (ignore args)) (format s "    gsave [1 0 0 1 .2 0] concat~%"))
(defun matrix-back (s &rest args) (declare (ignore args)) (format s "    grestore~%"))
(defun scr-size (n) (declare (ignore n)) 1.0)
(defun line-width (s) (declare (ignore s)) 0.0)
(defun setf-line-width (s val) (format s "    ~,3F setlinewidth~%" val))
(defsetf line-width setf-line-width)
(defun music-font (score) nil)

(defvar charmap nil) 
(defvar fbbox nil)

(defun get-bbox ()
  (let ((x0 0.0)
	(y0 0.0)
	(x1 0.0)
	(y1 0.0))
  (loop for bbox in (list
    treble-clef-bounds percussion-clef-bounds c-clef-bounds bass-clef-bounds turn-bounds mordent-bounds double-mordent-bounds
    trill-section-bounds arpeggio-bounds tr-bounds accent-bounds breath-mark-bounds caesura-bounds fermata-bounds upside-down-fermata-bounds
    repeat-sign-bounds upper-bracket-bounds lower-bracket-bounds segno-bounds coda-bounds pedal-off-bounds ped-bounds left-paren-bounds
    right-paren-bounds wedge-bounds zero-bounds one-bounds two-bounds three-bounds four-bounds five-bounds six-bounds seven-bounds
    eight-bounds nine-bounds common-time-bounds cut-time-bounds plus-bounds sharp-bounds flat-bounds double-sharp-bounds
    natural-bounds double-flat-bounds f-bounds p-bounds m-bounds z-bounds s-bounds r-bounds double-whole-note-bounds
    whole-note-bounds half-note-bounds quarter-note-bounds diamond-bounds rhythmX-bounds triangle-bounds square-bounds
    up8th-bounds extend-flag-up-bounds down8th-bounds extend-flag-down-bounds whole-rest-bounds half-rest-bounds
    quarter-rest-bounds rest8-bounds rest16-bounds rest32-bounds rest64-bounds rest128-bounds measure-rest-bounds double-whole-rest-bounds) do
	(setf x0 (min x0 (first bbox)))
	(setf y0 (min y0 (second bbox)))
	(setf x1 (max x1 (third bbox)))
	(setf y1 (max y1 (fourth bbox))))
  (list x0 y0 x1 y1)))


(defun add-to-font (s name fun bbox pos)
  (push (list name pos) charmap)
  (format s "/~A { 0 0 ~,3F ~,3F ~,3F ~,3F setcachedevice~%" 
	  name 
	  (first bbox) (second bbox) (third bbox) (fourth bbox))
  (funcall fun s)
  (format s "} def~%~%"))

(defun make-font (name &optional (smap t)) ;(sonata font map) other choice is musictex (this chooses the position of glyphs in the font)
  (setf charmap nil)
  (setf fbbox (get-bbox))
  (with-open-file (fil name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format fil 
"%%PS-Adobe-2.0
%%Title: ~A 
%%FontName: ~A
%%DocumentData: Clean7Bit
%%EndComments
/~A 11 dict begin
/PaintType 0 def
/FontType 3 def
/BuildChar{ exch begin
	    Encoding exch get
	    CharStrings exch get
	    exec end 
	   } def
/FontMatrix [1 0 0 1 0 0 ] def
/FontBBox [~,3F ~,3F ~,3F ~,3F] def
/CharStrings 75 dict begin
/.notdef {} def
"
    name name name
    (first fbbox) (second fbbox) (third fbbox) (fourth fbbox))
    
    (add-to-font fil "treble" #'draw-treble-clef treble-clef-bounds (if smap #o46 #o117))
    (add-to-font fil "percussion" #'draw-percussion-clef percussion-clef-bounds (if smap #o57 #o115))
    (add-to-font fil "c-clef" #'draw-c-clef c-clef-bounds (if smap #o102 #o113))
    (add-to-font fil "bass" #'draw-bass-clef bass-clef-bounds (if smap #o77 #o111))
    (add-to-font fil "turn" #'draw-turn turn-bounds (if smap #o124 #o103))
    (add-to-font fil "mordent" #'draw-mordent mordent-bounds (if smap #o155 #o167))
    (add-to-font fil "doublemordent" #'draw-double-mordent double-mordent-bounds (if smap #o265 #o130))
    (add-to-font fil "trill" #'draw-trill-section trill-section-bounds (if smap #o176 #o127))
    (add-to-font fil "arpeggio" #'draw-arpeggio arpeggio-bounds (if smap #o147 #o343))
    (add-to-font fil "tr" #'draw-tr tr-bounds (if smap #o331 #o201))
    (add-to-font fil "accent" #'draw-accent accent-bounds (if smap #o76 #o314))
    (add-to-font fil "breath" #'draw-breath-mark breath-mark-bounds (if smap #o54 #o40))
    (add-to-font fil "caesura" #'draw-caesura caesura-bounds (if smap #o42 #o200))
    (add-to-font fil "fermata" #'draw-fermata fermata-bounds (if smap #o125 #o120))
    (add-to-font fil "upsidedownfermata" #'draw-upside-down-fermata upside-down-fermata-bounds (if smap #o165 #o121))
    (add-to-font fil "repeatmeasure" #'draw-repeat-sign repeat-sign-bounds (if smap #o324 #o116))
    (add-to-font fil "upperbracket" #'draw-upper-bracket upper-bracket-bounds (if smap #o302 #o202))
    (add-to-font fil "lowerbracket" #'draw-lower-bracket lower-bracket-bounds (if smap #o114 #o203))
    (add-to-font fil "segno" #'draw-segno segno-bounds (if smap #o45 #o126))
    (add-to-font fil "coda" #'draw-coda coda-bounds (if smap #o336 #o125))
    (add-to-font fil "pedaloff" #'draw-pedal-off pedal-off-bounds (if smap #o52 #o150))
    (add-to-font fil "ped" #'draw-ped ped-bounds (if smap #o241 #o43))
    (add-to-font fil "leftparen" #'draw-left-paren left-paren-bounds (if smap #o50 #o244))
    (add-to-font fil "rightparen" #'draw-right-paren right-paren-bounds (if smap #o51 #o245))
    (add-to-font fil "wedge" #'draw-wedge wedge-bounds (if smap #o340 #o204))
    (add-to-font fil "zero" #'draw-zero zero-bounds (if smap #o60 #o60))
    (add-to-font fil "one" #'draw-one one-bounds (if smap #o61 #o61))
    (add-to-font fil "two" #'draw-two two-bounds (if smap #o62 #o62))
    (add-to-font fil "three" #'draw-three three-bounds (if smap #o63 #o63))
    (add-to-font fil "four" #'draw-four four-bounds (if smap #o64 #o64))
    (add-to-font fil "five" #'draw-five five-bounds (if smap #o65 #o65))
    (add-to-font fil "six" #'draw-six six-bounds (if smap #o66 #o66))
    (add-to-font fil "seven" #'draw-seven seven-bounds (if smap #o67 #o67))
    (add-to-font fil "eight" #'draw-eight eight-bounds (if smap #o70 #o70))
    (add-to-font fil "nine" #'draw-nine nine-bounds (if smap #o71 #o71))
    (add-to-font fil "commontime" #'draw-common-time common-time-bounds (if smap #o143 #o123))
    (add-to-font fil "cuttime" #'draw-cut-time cut-time-bounds (if smap #o103 #o122))
    (add-to-font fil "plus" #'draw-plus plus-bounds (if smap #o53 #o341))
    (add-to-font fil "sharp" #'draw-sharp sharp-bounds (if smap #o43 #o313))
    (add-to-font fil "flat" #'draw-flat flat-bounds (if smap #o142 #o310))
    (add-to-font fil "doublesharp" #'draw-double-sharp double-sharp-bounds (if smap #o334 #o315))
    (add-to-font fil "natural" #'draw-natural natural-bounds (if smap #o156 #o316))
    (add-to-font fil "doubleflat" #'draw-double-flat double-flat-bounds (if smap #o272 #o312))
    (add-to-font fil "f" #'draw-f f-bounds (if smap #o146 #o205))
    (add-to-font fil "p" #'draw-p p-bounds (if smap #o160 #o206))
    (add-to-font fil "m" #'draw-m m-bounds (if smap #o275 #o207))
    (add-to-font fil "z" #'draw-z z-bounds (if smap #o172 #o210))
    (add-to-font fil "s" #'draw-s s-bounds (if smap #o163 #o211))
    (add-to-font fil "r" #'draw-r r-bounds (if smap #o363 #o212))
    (add-to-font fil "doublewholenote" #'draw-double-whole-note double-whole-note-bounds (if smap #o127 #o320))
    (add-to-font fil "wholenote" #'draw-whole-note whole-note-bounds (if smap #o167 #o252))
    (add-to-font fil "halfnote" #'draw-half-note half-note-bounds (if smap #o372 #o251))
    (add-to-font fil "quarternote" #'draw-quarter-note quarter-note-bounds (if smap #o317 #o250))
    (add-to-font fil "diamond" #'draw-diamond diamond-bounds (if smap #o341 #o46))
    (add-to-font fil "rhythmX" #'draw-rhythmX rhythmX-bounds (if smap #o300 #o157))
    (add-to-font fil "triangle" #'draw-triangle triangle-bounds (if smap #o261 #o213))
    (add-to-font fil "square" #'draw-square square-bounds (if smap #o255 #o214))
    (add-to-font fil "ethflagup" #'draw-8th-flag-up up8th-bounds (if smap #o152 #o50))
    (add-to-font fil "extendflagup" #'draw-extend-flag-up extend-flag-up-bounds (if smap #o373 #o215))
    (add-to-font fil "ethflagdown" #'draw-8th-flag-down down8th-bounds (if smap #o112 #o55))
    (add-to-font fil "extendflagdown" #'draw-extend-flag-down extend-flag-down-bounds (if smap #o360 #o216))
    (add-to-font fil "wholerest" #'draw-whole-rest whole-rest-bounds (if smap #o267 #o254))
    (add-to-font fil "halfrest" #'draw-half-rest half-rest-bounds (if smap #o356 #o253))
    (add-to-font fil "quarterrest" #'draw-quarter-rest quarter-rest-bounds (if smap #o316 #o76))
    (add-to-font fil "ethrest" #'draw-8th-rest rest8-bounds (if smap #o344 #o77))
    (add-to-font fil "sthrest" #'draw-16th-rest rest16-bounds (if smap #o305 #o100))
    (add-to-font fil "tndrest" #'draw-32nd-rest rest32-bounds (if smap #o250 #o101))
    (add-to-font fil "sxthrest" #'draw-64th-rest rest64-bounds (if smap #o364 #o102))
    (add-to-font fil "othrest" #'draw-128th-rest rest128-bounds (if smap #o345 #o103))
    (add-to-font fil "measurerest" #'draw-measure-rest measure-rest-bounds (if smap #o335 #o73))
    (add-to-font fil "doublemeasurerest" #'draw-double-whole-rest double-whole-rest-bounds (if smap #o343 #o217))

    (format fil "~%currentdict end readonly def~%~%/Encoding 256 array~%")
    (loop for i from 0 to 255 do
      (let ((curglf (find i charmap :key #'second :test #'=)))
	(if curglf
	    (format fil "dup ~D /~A put~%" i (first curglf))
	  (format fil "dup ~D /.notdef put~%" i))))
    (format fil "~%readonly def~%currentdict end definefont pop~%~%")))


;;; once generated, you can test-run the font with something like:
;;; /hiho findfont 40 scalefont setfont 100 100 moveto (\46) show

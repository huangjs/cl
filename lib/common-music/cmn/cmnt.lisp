;;; these are the examples in cmn.wn

#|
(cmn staff treble c4 q)
(cmn (size 24) staff treble c4 w double-bar)

(cmn (size 40)
     (system brace 
	     (staff treble (meter 6 8) 
		    (c4 e. tenuto) (d4 s) (ef4 e sf tremolo) 
		    (c4 e) (d4 s) (en4 s) (fs4 e (fingering 3))) 
	     (staff treble (meter 3 4) 
		    (c5 e. marcato) (d5 s bartok-pizzicato) (ef5 e) 
		    (c5 e staccato tenuto) (d5 s down-bow) (en5 s) (fs5 e)))
     (system bracket
	     (staff bar bass (meter 6 16) 
		    (c4 e. wedge) (d4 s staccato) (ef4 e left-hand-pizzicato) 
		    (c4 e tenuto accent rfz) (d4 s mordent) (en4 s pp) (fs4 e fermata))))
|#
(cmn (size 24)
     system brace 
	     staff treble (meter 6 8) 
		    c4 e. tenuto d4 s ef4 e sf tremolo
		    c4 e d4 s en4 s fs4 e (fingering 3) 
	     staff treble (meter 3 4) 
		    c5 e. marcato d5 s bartok-pizzicato ef5 e 
		    c5 e staccato tenuto d5 s down-bow en5 s fs5 e
     system bracket
	     staff bar bass (meter 6 16) 
		    c4 e. wedge d4 s staccato ef4 e left-hand-pizzicato 
		    c4 e tenuto accent rfz d4 s mordent en4 s pp fs4 e fermata)


#|

(cmn (size 24) (beam-spacing .4) (beam-width .275) staff treble (c4 s) (d4 s))
(cmn (size 24) (curvy-flags nil) staff treble (a4 s))
(cmn (size 24) treble (meter 6 8 (meter-size 2)))

(cmn (staff treble (c4 w (marcato (dx .5)))))
(cmn staff (treble mirrored (dx .5)) (treble (rotate 180) (dy .75) (scale -2 .5)))
(cmn (size 24) (header-margin .25) (footer-margin .25) staff 
  (treble mirrored (dx .5)) (treble (rotate 180) (dy .75) (scale -2 .5)) 
  (bass (gray-scale .5) (dx .75)) (bass (outlined .02) (dx .75)) 
  (quarter-rest (rotate 45) (dx .5) (scale 1 2)) (eighth-rest (rotate 90) (dy -.4) (scale 2 1)))

(cmn (size 24) (system bracket (staff) (staff brace) (staff)))
(cmn (staff (staff-name "E-sharp Bazooka") (staff-lines 2) (e4 w fff)) 
     (staff (staff-name "c-flat Clavicle") (staff-lines 0) treble d4 q c5 h.) 
     (staff (staff-name "Violin") (staff-size .5) sub-bass g2 h c2 h))

(cmn (size 24) bar c4 q begin-and-end-repeat-bar c4 q interior-double-bar c4 q 
	dashed-bar c4 q begin-repeat-bar c4 q end-repeat-bar c4 q double-bar)
(cmn (size 24) staff french-violin c4 q treble c4 q soprano c4 q mezzo-soprano c4 q alto c4 q tenor c4 q baritone c4 q bass c4 q sub-bass c4 q)
(cmn staff treble c4 q cs4 q cf4 q cn4 q (c4 q (sign double-sharp)) (c4 q (sign double-flat)))
(cmn staff tenor b-major c4 q bar (cancel b-major) ef-major c4 q)
(cmn (size 24) (header-margin .125) (footer-margin .125) staff tenor b-major c4 q bar (cancel b-major) ef-major c4 q)
(cmn staff treble (meter 6 8) c4 h. bar (meter '5+7 8) c4 w bar cut-time c4 w (meter 2 2 in-parentheses)
     c4 w (meter 9 8) (meter '4+5 8 in-parentheses) c4 w)

 (cmn (size 24) staff treble 
     (g4 q (grace-note fs4)) 
     (g4 q (grace-note (slurred nil) (stem-direction :down) b4 (note-head :diamond) staccato tenuto wedge fermata))
            ;; omit the slur and use a diamond head on the grace note and get very confused about articulation
     (g4 q (grace-note e4 fs4)) 
     (g4 q (grace-note gf4 ef4)) 
     (g4 q (grace-note (slashed nil) b4 a4))
            ;; omit the slash 
     (b4 q (grace-note d4 e4 fs4 g4 a4)) 
     (g4 q (grace-note ef5 e bf4 e ef4 e)))
            ;; just one beam on the grace notes

(cmn (free-expansion-factor 2.5) staff treble whole-rest half-rest quarter-rest
    eighth-rest sixteenth-rest thirty-second-rest sixty-fourth-rest one-twenty-eighth-rest)

(cmn staff treble (c5 e (onset .5)) (g4 e. (onset 1.25)))

(cmn (size 24) staff treble c5 q mordent c5 q turn c5 q short-trill c5 q trilled-turn c5 q trill 
                  c5 q (mordent (ornament-sign small-flat)) 
                 (c5 q (trill (sign-position :up) (ornament-sign small-natural))) 
                 (cs5 q (trill (ornament-sign small-sharp) (sign-position :in-parentheses))) 
                 (c5 q (trill (wavy-line t))) (c5 q (trill (ornament-sign flat))))
(cmn staff treble (chord (notes c4 e4 g4) h (arpeggio arrow-down)))

(cmn staff treble (e4 q (fingering 3)) (chord (notes c5 g4) q (fingering 7 2)))
(cmn staff treble (meter 2 4) (c4 q fermata) (quarter-rest g.p.) (c4 q breath-mark) (c4 q upside-down-fermata) double-bar)
(cmn staff treble (c4 q begin-slur) (d4 q) e4 q end-slur)
(cmn staff treble (c4 q (setf slur-tag (begin-slur))) (d4 (end-slur slur-tag)))

(cmn staff treble 
     (c4 h (setf lower-text (text- "hi")) 
	   (setf upper-text (text- "sil" (y0 2.0))))
     (d4 q (-text- "ver" upper-text (y0 2.0))) 
     (d4 q (-text- "ho" lower-text)) 
     (e4 q (-text "!" upper-text (y0 2.0))) 
     (e4 h (-text "away!" lower-text)))
(cmn staff treble (c4 h (onset 0) stem-down) (c5 e (onset 0) stem-up) (b4 e (onset .5) stem-up) (a4 q (onset 1.0) stem-up))

(cmn staff treble (c4 q (note-head :diamond) (auxiliary-note c6 no-stem in-parentheses)))

(cmn (output-file "2.eps") staff treble c5 s)
(cmn (size 24) staff treble (c4 q (graphics (file "2.eps") (scale .6 .6) (dx 2.0) (dy 1.0) (rotate 90))))
(cmn (size 24) staff treble (c4 q (text "middle-c" (dx 1.0) (dy 2.0) (rotate 180)  (font-size 12))))

(cmn (size 24) staff treble (c4 h (text- "hi")) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!")))
(cmn (text-connecting-pattern '(5 10)) (size 24) staff treble (c4 h (text- "hi")) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!")))
(cmn staff treble (c4 h (glissando-to a4)) quarter-rest (c4 h begin-glissando) (g4 h end-glissando))
(cmn (free-expansion-factor 2.5) staff treble (c4 h (begin-glissando (text "we're number 1"  (font-scaler .3)))) (c5 h end-glissando))
(cmn (free-expansion-factor 2.0) staff treble (c4 h (crescendo (duration 2.0))) (c4 h (begin-crescendo (onset-offset 1.0))) (g4 h end-crescendo))

(cmn staff treble (c4 q (note-head :diamond) (with-cmn (scale .5 .5) (dx -1.0) (dy 1.5) staff treble c6 q no-stem)))


(cmn staff treble (c4 q begin-tie) (c4 q end-tie) (c4 q (begin-tie (tie-direction :up) (tie-curvature .5))) (c4 q end-tie))
(cmn staff treble (chord (notes c4 e4 g4 c5) q begin-tie) (chord (notes c4 e4 g4 c5) q end-tie))
(cmn staff treble (chord (notes (c4 (setf tie-c4 (begin-tie))) e4 g4 c5) q) (chord (notes (c4 (end-tie tie-c4)) e4 g4 c5) q))

(cmn staff treble (meter 2 4) f4 e. f4 q. f4 h.)
(cmn (size 24) staff treble (meter 2 4) f4 e. f4 q. f4 h.)

(cmn (size 24) staff treble (meter 2 4) (chord (notes f4 af4 df5) e.) (chord (notes f4 af4 df5) q.) (chord (notes f4 af4 df5) h.))


(cmn (size 24) staff treble 
     g4 q begin-slur e4 q fs4 q end-slur
     g4 q begin-slur gf4 q ef4 q end-slur
     g4 q begin-slur ef4 q c4 q end-slur
     g4 q begin-slur b4 q a4 q end-slur
     b4 q begin-slur e5 q d5 q cs5 q end-slur
     b4 q begin-slur d4 q e4 q fs4 q g4 q a4 q end-slur
     g4 q begin-slur ef5 q bf4 q ef4 q end-slur)

 (cmn (size 24) staff treble 
     g4 q begin-slur ef4 q c4 q end-slur
     g4 q begin-slur b4 q a4 q end-slur
     b4 q begin-slur e5 q d5 q cs5 q end-slur
     a4 q begin-slur e4 q f4 q g4 q a4 q end-slur
     g4 q begin-slur ef5 q bf4 q ef4 q end-slur)


(cmn (size 40) (free-expansion-factor 2.0) staff treble 
  (chord (notes f5 ef5 df5 a4) q begin-tie) (chord (notes f5 e5 d5 a4) q end-tie) 
  (chord (notes f4 gf4 af4 d5) q begin-tie) (chord (notes f4 g4 a4 d5) q end-tie))

(cmn (size 40) (free-expansion-factor 1.5) staff treble (meter 2 4) 
     (chord (notes f5 af4 df5) (rq 5/4)) 
     (chord (notes f5 ef5 df5 a4) (rq 3/2)) 
     (chord (notes f4 gf4 af4 d5) (rq 5/4)))
(cmn (size 24) (free-expansion-factor 1.5) staff treble (meter 2 4) 
     (chord (notes f5 af4 df5) (rq 5/4)) 
     (chord (notes f5 ef5 df5 a4) (rq 3/2)) 
     (chord (notes f4 gf4 af4 d5) (rq 5/4) stem-down))

(cmn staff treble (c4 q tremolo) (c4 w paranoid-tremolo) (c4 e begin-tremolo) (d4 e end-tremolo) (c5 h begin-measured-tremolo) (d5 h end-measured-tremolo))
(cmn staff treble (c4 q tremolo) (c4 w paranoid-tremolo) (c4 q (tremolo (tremolo-slashes 1))) (c5 h begin-measured-tremolo) (d5 h end-measured-tremolo) (c4 e begin-tremolo) (d4 e end-tremolo))

(cmn (size 24) staff treble (c4 q tremolo) (c4 w paranoid-tremolo) (c5 q (tremolo (tremolo-slashes 1))) (c4 e begin-tremolo) (d4 e end-tremolo))

(cmn (size 24) staff treble (meter 2 4) (c4 q) (c4 q) (bar begin-first-ending) (c4 q) (c4 q) (end-repeat-bar end-first-ending begin-second-ending) (c4 q) (c4 q) (bar end-second-ending) c4 q c4 q double-bar)
(cmn staff treble c4 te c4 te c4 te (c4 (rq 1/5)) (c4 (rq 1/5)) (c4 (rq 1/5)) (c4 (rq 1/5)) (c4 (rq 1/5)) (c5 (rq 2/3)) (c5 (rq 1/3)))

(cmn (size 24) staff treble c4 w (bar segno) (c4 w) (bar coda) (c4 w) (bar (change-beat q. e)) (c4 w))
(cmn staff bass c5 e d5 e f5 e g5 e c5 q g4 q c4 q)

(cmn staff treble (meter 2 4) c4 q c4 q bar repeat-measure bar
  c4 q g4 q bar (repeat-measure 440) c4 q c4 q double-bar)


(defun display-smiley-face (mark note score &optional size)
  (let* ((y-off (+ (y0 mark) (dy mark) (staff-y0 note) 
		   (* (max 11 (+ 4 (head-line note))) *staff-line-separation*)))
	 (r (/ 10 40))
	 (x-off (+ (x0 note) -.05 (dx mark) (center note) (x0 mark))))
    (setf (line-width score) .025)
    (circle score x-off y-off r)
    (circle score x-off y-off (* r .6) 190 350)
    (circle score (- x-off (* .33 r)) (+ y-off (* .25 r)) (* r .1))
    (fill-in score)
    (circle score (+ x-off (* .33 r)) (+ y-off (* .25 r)) (* r .1))
    (fill-in score)
    (setf (line-width score) 0)))

(defvar smiley-face (make-instance 'write-protected-sundry :name :smiley-face :mark #'display-smiley-face))
(defun smiley-face (&rest objects) (apply #'mark #'display-smiley-face :smiley-face objects))
(cmn staff treble c4 q smiley-face)
(cmn (free-expansion-factor 2.0) staff treble (c4 h begin-measured-tremolo) (c5 h end-measured-tremolo))
(cmn (size 24) staff treble c4 q begin-two-octaves-up d4 q e4 q f4 q end-two-octaves-up)

(cmn (size 24) staff treble c4 e d4 e (c4 e (begin-beam)) (d4 s) (e4 s (end-beam)) (c4 e (setf beam (beam-))) (d4 e (-beam- beam)) (e4 e (-beam beam)))

;;; these are various test cases

(cmn (staff (staff-size .5) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .6) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .7) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .8) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .9) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size 1.0) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size 1.25) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e))

(cmn (staff (staff-size .5) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .6) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .7) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .8) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .9) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size 1.0) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size 1.25) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e))


(cmn (size 24) 
     (staff treble 
	    (chord (notes c4 a4 f5) e)
	    (chord (notes d4 f4 a4 c5) e)
	    (chord (notes f5 b4 g4) q)
	    (chord (notes g4 b4 d5) q)
	    (chord (notes f4 a4 e5) h)
	    (chord (notes g4 f4 c5 b4 d5) w)
	    (chord (notes e4 g4 f4 a4 c5 b4 d5) s stem-down)
	    (chord (notes e5 g5 f5 a4 c5 b4 d5) s)
	    (chord (notes c4 d4 f4 g4) e)
	    (chord (notes g4 b4 c5 e5) q))

   (staff alto
	    (chord (notes fn4 gn3) e)
	    (chord (notes ef4 ff3) e)
	    (chord (notes fs4 ds4) e)
	    (chord (notes as3 cs4) e)
	    (chord (notes ff4 df4 bf3) w)
	    (chord (notes gs3 bn3 ef4) w)
	    (chord (notes ds4 as3 ds3) w))

  (staff treble
	    (chord (notes fn4 af4 cn5 ef5) q)
	    (chord (notes an4 ef5 gn5) q)
	    (chord (notes en5 cn5 an4 fn4 dn4) q)
	    (chord (notes cn4 ef4 gn4 bf4 ds5 fn5) h.)
	    (chord (notes dn6 an5 ff5 gn4 ef4 cn4) w))

  (staff bass
	    (chord (notes a3 gf3 bn2) q)
	    (chord (notes cf3 df3 gf3) q)
	    (chord (notes gf2 bn2 cn3 fn3) q)
	    (chord (notes cf3 ff3 gf3 cf4) q)))


(cmn (setf s1 (staff treble (meter 6 8) a4 e a4 e a4 e a4 q.)) (staff (tied-to s1) (d4 q. stem-down) (d4 q. stem-down)))

 (cmn (size 24) (footer-margin .25) (prolog #'(lambda (score) (make-ISO-encoded-version score "Times-Roman" "ISO-Times-Roman"))) staff treble (c4 q (glyphs (dy 1.5) #\s 374 337 (font-name "ISO-Times-Roman") (dx -.25) (font-scaler .5) (gray-scale .25))))

(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 7) (dy0 -.5) (dy .5)))) (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi)))

 (cmn (size 40) (header-margin 0) (footer-margin .25) staff treble 
  (chord (notes c4 e4 g4) h (arpeggio arrow-down)) 
  (chord (notes c4 e4 g4) h arrow-up) 
  (chord (notes d4 g4 e5) h no-arpeggio))

(cmn staff treble a4 q. (grace-note f4 g4) g4 e f4 q g5 q. g5 e (grace-note a5) begin-slur f5 e g5 e end-slur)
|#

(cmn (size 24) (free-expansion-factor 2.50) (header-margin .25) (footer-margin .5) staff treble 
  (c4 q (crescendo (duration 2.0))) (d4 q) (c4 q (begin-diminuendo (end-dynamic f) (width .75))) (d4 q) (e4 q end-diminuendo)
  (c4 q (begin-crescendo ppp f)) d4 q e4 q f4 q (g4 q (end-crescendo)))

(cmn (transform '(0 1 -1 0 200 200)) (staff (x1 7.5) treble c4 e d4 e))


;;; to get this to fit into the WriteNow page, manually edit the 
;;; %%BoundingBox to be 1 10 187 280

(cmn (size 16) (header-margin .1) (footer-margin .65) 
  (system (staff (x1 3)
      (make-sundry :name :phantom-staff
		   :mark #'(lambda (mark note score &optional justifying)
			     (setf (gray-scale score) .5)
			     (loop for i from 0 below 5 and y from 16.25 by .250 do
			       (moveto score (/ 35.82 16) y)
			       (lineto score 3.5 y))
			     (moveto score (/ 35.82 16) 16.25)
			     (lineto score (/ 35.82 16) 18.25)
			     (draw score)
			     (matrix-front score (translate-matrix score mark (/ 32 16) (/ 260 16)))
			     (draw-lower-bracket score)
			     (matrix-back score)
			     (moveto score (/ 32 16) (/ 260 16))
			     (lineto score (/ 34 16) (/ 260 16))
			     (lineto score (/ 34 16) 18.25)
			     (lineto score (/ 32 16) 18.25)
			     (lineto score (/ 32 16) (/ 260 16))
			     (fill-in score)
			     (setf (gray-scale score) 0)))

  (text  "}"  (font-scaler 3.0)                          (x0 2.75) (y0 9.75))
  (text  "system-separation"  (font-scaler .5)           (x0 4)    (y0 10.35))
  (text  "}"  (font-scaler 1.75)                         (x0 3)    (y0 6.75))
  (text  "staff-separation"  (font-scaler .5)            (x0 4)    (y0 7.1))
  (text  "}"  (font-scaler 3.5)                          (x0 2.625)    (y0 13.625))
  (text  "line-separation"  (font-scaler .5)             (x0 4.25)    (y0 14.375))
  (text  "}"  (font-scaler 1.25)                         (x0 3.25)    (y0 3.125))
  (text  "dy = 1.0 (one staff height)"  (font-scaler .5) (x0 4) (y0 3.25))
   ))
  (system bracket (staff (x1 1.0)) (staff brace (x1 1.0)) (staff (x1 1.0))))

(cmn (size 60) (system (staff (x1 3))) (system bracket (staff (x1 1.0)) (staff brace (x1 1.0)) (staff (x1 1.0))))


;;; this is the Opus 1 example on page 9 -- once cmn has created it,
;;; you have to manually edit the BoundingBox to y1=275 for WriteNow's benefit.

(cmn (size 12) (footer-margin 2.5) (left-margin 1)
     (title (text "Opus 1"  (font-scaler 1.0) (dy .5)))
   (staff
      (make-sundry :name :box
		   :mark #'(lambda (mark note score &optional justifying)
			     (moveto score 1 1) ;outline of page
			     (lineto score 17 1)
			     (lineto score 17 22)
			     (lineto score 1 22)
			     (lineto score 1 1)
			     (draw score)
			     
			     (setf (gray-scale score) .25) ;shadow of page
			     (moveto score (/ 205 12) (/ 263 12))
			     (lineto score (/ 205 12) (/ 11 12))
			     (lineto score (/ 12 12) (/ 11 12))
			     (draw score)
			     (setf (gray-scale score) .5)
			     (moveto score (/ 206 12) (/ 262 12))
			     (lineto score (/ 206 12) (/ 10 12))
			     (lineto score (/ 13 12) (/ 10 12))
			     (draw score)

			     (moveto score 2 3) ;inner dashed line
			     (lineto score 16 3)
			     (lineto score 16 20)
			     (lineto score 2 20)
			     (lineto score 2 3 :pattern '(5 7))
			     (draw score)
			     (setf (gray-scale score) 0)

			     (moveto score 1 22.25) ;top width
			     (lineto score 1 22.75)
			     (draw score)
			     (setf (gray-scale score) .5)
			     (moveto score 1.25 22.5)
			     (lineto score 6.5 22.5 :pattern '(3 5))
			     (draw score)
			     (setf (gray-scale score) 0)

			     (display
			      (text "page-width (8.5)"
				     (font-scaler .5) (x0 7) (y0 22.3))
			      nil score)
			     (setf (gray-scale score) .5)
			     (moveto score 11 22.5)
			     (lineto score 16.75 22.5 :pattern '(3 5))
			     (draw score)
			     (setf (gray-scale score) 0)
			     (moveto score 17 22.25)
			     (lineto score 17 22.75)
			     (draw score)

			     (moveto score .25 1)
			     (lineto score .75 1)
			     (draw score)
			     (setf (gray-scale score) .5)
			     (moveto score .5 1.25)
			     (lineto score .5 8.75 :pattern '(3 5))
			     (draw score)
			     (setf (gray-scale score) 0)
			     (display
			      (text "page-height (11.0)" (rotate 90)
				     (font-scaler .5) (x0 .7) (y0 9.5))
			      nil score)
			     (moveto score .25 22)
			     (lineto score .75 22)
			     (draw score)
			     (setf (gray-scale score) .5)
			     (moveto score .5 21.75)
			     (lineto score .5 14 :pattern '(3 5))
			     (draw score)
			     (setf (gray-scale score) 0)

			     (display 
			      (text "}" (font-name "Courier") (font-scaler 2) (x0 2) (y0 1.5))
			      nil score)
			     (display 
			      (text "footer-margin (1.0)" 
				     (font-scaler .5) (x0 3.125) (y0 1.875))
			      nil score)

			     (display 
			      (text "}" (font-name "Courier") (font-scaler 2) (x0 2) (y0 20.5))
			      nil score)
			     (display 
			      (text "header-margin (1.0)" 
				     (font-scaler .5) (x0 3.125) (y0 20.875))
			      nil score)

			     (display 
			      (text "}" (rotate 90) (font-name "Courier") (font-scaler 1) (x0 1.75) (y0 3.125))
			      nil score)
			     (display 
			      (text "left-margin (0.5)" (rotate 90)
				     (font-scaler .5) (x0 1.65) (y0 3.75))
			      nil score)
			     
			     (display 
			      (text "}" (rotate -90) (font-name "Courier") (font-scaler 1) (x0 16.25) (y0 19.875))
			      nil score)
			     (display 
			      (text "right-margin (0.5)" (rotate -90)
				     (font-scaler .5) (x0 16.4) (y0 19.25))
			      nil score)
			     
			     (display
			      (text "Menlo Park 1992" 
				     (font-scaler .3) (x0 10) (y0 14.5))
			      nil score)

			     (display 
			      (text "(cmn staff bar treble a4 e begin-slur"
				    (font-name "Courier") (font-scaler .5) (x0 3.5) (y0 12))
			      nil score)
			     (display
			      (text "b4 e c5 e d5 e e5 q end-slur bar)"
				    (font-name "Courier") (font-scaler .5) (x0 4.5) (y0 11.5))
			      nil score)
			     (display
			      (text ";;; see "
				    (font-name "Courier-Oblique") (font-scaler .5) (x0 3.5) (y0 10.5))
			      nil score)
			     (display 
			      (text "cmnt.lisp" 
				    (font-name "Helvetica-Bold") (font-scaler .5) (x0 5.9) (y0 10.5))
			      nil score)
			     (display 
			      (text " for the full cmn code" 
				    (font-name "Courier-Oblique") (font-scaler .5) (x0 8.125) (y0 10.5))
			      nil score)
			     (display 
			      (text ";;; to produce this silliness."
				    (font-name "Courier-Oblique") (font-scaler .5) (x0 3.5) (y0 10))
			      nil score)
			     ))

      bar treble a4 e begin-slur b4 e c5 e d5 e e5 q end-slur bar))

(cmn (size 24) (header-margin .1) (footer-margin .4) staff tenor b-major c4 q bar (cancel b-major) ef-major c4 q bar bass foo-major c4 q)
(cmn (size 24) (header-margin .1) (footer-margin .25) staff treble (c4 q (note-head :diamond) (auxiliary-note c6 no-stem in-parentheses)))

(cmn (size 24) (footer-margin .5) (header-margin 0) (left-margin 5) 
  staff 
    (text "(cmn staff treble"  (font-name "Courier") (font-size 10) (x0 1.5) (y0 3)) 
    (text "(c4 q (note-head :diamond)"  (font-name "Courier") (font-size 10) (x0 2) (y0 2.5)) 
    (text "(with-cmn (scale .75 .75) (dx -1.0) (dy 1.0)"  (font-name "Courier") (font-size 10) (x0 2.5) (y0 2)) 
    (text "staff treble c6 q no-stem)))"  (font-name "Courier") (font-size 10) (x0 3) (y0 1.5))
    treble (c4 q (note-head :diamond) (with-cmn (scale .75 .75) (dx -1.0) (dy 1.0) staff treble c6 q no-stem)))
;; edit boundingbox manually to 1 15 671 110 for WriteNow


;;; Schubert example is in beams.cmn


    (cmn (size 24) (footer-margin .25) (header-margin .25) staff treble g4 q gs4 q gf4 q gn4 q 
     (g4 q double-sharp) (g4 q double-flat) (g4 q (natural-sharp)) (g4 q (natural-flat)))

  (cmn (size 24) (free-expansion-factor 1.075) (header-margin .25) (footer-margin .25) 
    staff treble (meter 6 8) c4 h. bar (meter '5+7 8) c4 w
    bar cut-time c4 w bar (meter 2 2 in-parentheses (dx .1)) c4 h
    bar (meter 9 8) (meter '4+5 8 (dx .1) in-parentheses) c4 w bar 
   (meter 3 2 note-head-denominator) c4 h bar (meter 3 4 suppressed-denominator (meter-size 2)) c4 q.)



;;; percussion example
 (cmn (size 60) staff (staff-lines 1) (start-line 2) percussion (meter 4 4) 
    (b4 w (cymbal (dy 1.1) (dx .2)) wood-stick) (b4 w (gong (dy 1.1) (dx .2)) bass-drum)
    (b4 w (suspended-cymbal (dy 1.1) (dx .2)) metal-stick) (b4 w (hi-hat (dy 1.1) (dx .2)) triangle-stick) 
    (b4 w (tambourine (dy 1.1) (dx .2)) soft-stick) (b4 w (maracas (dy 1.1) (dx .2)) hard-stick) 
    (b4 w (cow-bells (dy 1.1) (dx .2)) wire-brush)  (b4 w (triangle (dy 1.1) (dx .2)) rubber-stick))

;;; pedal example
 (cmn (free-expansion-factor 3) (staff treble (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (g4 q -u.c.ped) (e4 q -pedal -sost.ped)))

;;; accent example

(cmn staff treble (c4 q (sul-tasto- (dy .5)) fingernail) 
  (d4 q (double-tongue (dy .125))) (e4 q doink martellato) (f4 q heavy-accent) 
  (g4 q rip (triple-tongue (dy .125)) (-sul-tasto)) (a4 q nail-pizzicato) 
  (b4 q (no-accent (dy -.25))) (c5 q Hauptstimme sprechstimme) (d5 q NebenStimme circled-stem))

;;; cmn full example
 (cmn (size 60) (free-expansion-factor 1.5) (system-separation 0) (footer-margin .1) (header-margin 0) 
   (system (staff treble (c4 w (sul-tasto- (dy .5)) fingernail) 
     (d4 w (double-tongue (dy .125))) (e4 w doink martellato) (f4 w heavy-accent) 
     (g4 w rip (triple-tongue (dy .125)) (-sul-tasto)) (a4 w nail-pizzicato smear) 
     (b4 w no-accent) (c5 h Hauptstimme (sprechstimme)) (d5 h NebenStimme circled-stem))) 
   (system (staff (staff-lines 1) (start-line 2) percussion 
    (b4 w (cymbal (dy 1.1) (dx .2)) wood-stick) (b4 w (gong (dy 1.1) (dx .2)) (bass-drum (dy -.05)))
    (b4 w (suspended-cymbal (dy 1.1) (dx .2)) metal-stick) (b4 w (hi-hat (dy 1.1) (dx .2)) (triangle-stick (dy .1)))
    (b4 w (tambourine (dy 1.1) (dx .2)) soft-stick) (b4 w (maracas (dy 1.1) (dx .2)) hard-stick) 
    (b4 w (cow-bells (dy 1.1) (dx .2)) wire-brush)  (b4 w (triangle (dy 1.1) (dx .2)) wood-stick))))

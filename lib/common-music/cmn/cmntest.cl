(in-package :cmn)

;;; tests for cmn

(setf *cmn-output-type* :postscript)

(defvar all-done nil)

(defun cmntest (n)
  (when (not all-done)
    (print n) (force-output)
    (eval n)))

(when (probe-file "fux.cmn") (cmntest '(load "fux.cmn")))
(when (probe-file "moz.cmn") (cmntest '(load "moz.cmn")))
(when (probe-file "gus.cmn") (cmntest '(load "gus.cmn")))
(when (probe-file "carl.cmn") (cmntest '(load "carl.cmn")))
(when (probe-file "joh.cmn") (cmntest '(load "joh.cmn")))
(when (probe-file "franz.cmn") (cmntest '(load "franz.cmn")))
(when (probe-file "fred.cmn") (cmntest '(load "fred.cmn")))
(when (probe-file "bucky.cmn") (cmntest '(load "bucky.cmn")))
(when (probe-file "mir.cmn") (cmntest '(load "mir.cmn")))
(when (probe-file "henry.cmn") (cmntest '(load "henry.cmn")))
(when (probe-file "duke.cmn") (cmntest '(load "duke.cmn")))
(when (probe-file "jimmy.cmn") (cmntest '(load "jimmy.cmn")))
(when (probe-file "joe.cmn") (cmntest '(load "joe.cmn")))
(when (probe-file "ccarh93.cmn") (cmntest '(load "ccarh93.cmn")))

(cmntest '(cmn (section (redundant-accidentals nil) (staff treble (meter 4 4) cs4 q cs4 q cs4 q cs4 q)) 
     (section (staff treble cs4 q cs4 q cs4 q cs4 q)) ))
(cmntest '(cmn (size 100) (staff (staff-lines 0) (treble (gray-scale .75)) 
     (c4 w (text "hi" (gray-scale .5)) (begin-tie (gray-scale .5)) (marcato (dy .125) (gray-scale .5) (dx .75))) 
     (c4 w end-tie) (bass (outlined .01)) g2 w)))
(cmntest '(cmn staff treble c4 w begin-and-end-repeat-bar c4 w begin-and-end-repeat-bar-without-thin-lines 
     c4 w begin-and-end-repeat-bar-with-one-thick-line c4 w end-repeat-bar))
(cmntest '(cmn (system (brace (brace-staves 3)) (staff (staff-size .7) treble c4 q) (staff treble c4 q) (staff bass c3 q))))
(cmntest '(cmn staff (treble (scale 0 0)) (ef-major (scale 0 0)) (meter 3 4) c4 q c4 h c4 h c4 q unmetered c4 w c4 w c4 w))
(cmntest '(cmn staff treble (meter 3 4 (meter-size 2) suppressed-denominator) c4 q))
(cmntest '(cmn staff treble (meter 3 4 note-head-denominator) c4 q))
(cmntest '(cmn (staff (mm 60 (gray-scale .5) (scale 2 1)) treble (c4 q (fermata (gray-scale .5) (scale 2 1))))))
(cmntest '(cmn staff treble (meter 3 4) (c4 q breath-mark) c4 h p c4 h c4 q unmetered (c4 w (breath-mark (scale 0 0))) (c4 w (p (scale 0 0))) c4 w))
(cmntest '(cmn (dynamics-size .5) (staff treble (mf (dy 3.0) (gray-scale .5) (scale 3 1)) (c4 q (ppp (gray-scale .5) (scale 2 1))))))
(cmntest '(cmn staff treble (g4 q (natural-sharp (dx -.1)))))
(cmntest '(cmn staff treble (c4 q (p in-parentheses)) (c4 q (pp in-parentheses)) (c4 q (sp in-parentheses)) 
  (c4 q (spp in-parentheses)) (c4 q (f in-parentheses)) (c4 q (ff in-parentheses)) 
  (c4 q (fp in-parentheses)) (c4 q (rfz in-parentheses)) (c4 q (sff in-parentheses)))) 

(cmntest '(cmn staff treble 
  a3 q staccato d4 h staccato b4 w staccato e5 q staccato d6 q staccato as3 q staccato df4 q staccato fs5 w staccato
  a3 q accent d4 h accent  b4 w accent e5 q accent  d6 q accent as3 q accent df4 q accent fs5 w accent
  a3 q little-swell d4 h little-swell  b4 w little-swell e5 q little-swell  d6 q little-swell 
    as3 q little-swell df4 q little-swell fs5 w little-swell
  a3 q wedge d4 h wedge  b4 w wedge e5 q wedge  d6 q wedge as3 q wedge df4 q wedge fs5 w wedge
  a3 q tenuto d4 h tenuto  b4 w tenuto e5 q tenuto  d6 q tenuto as3 q tenuto df4 q tenuto fs5 w tenuto
  a3 q marcato d4 h marcato  b4 w marcato e5 q marcato  d6 q marcato as3 q marcato df4 q marcato fs5 w marcato
  a3 q down-bow d4 h down-bow  b4 w down-bow e5 q down-bow  d6 q down-bow as3 q down-bow df4 q down-bow fs5 w down-bow
  a3 q up-bow d4 h up-bow  b4 w up-bow e5 q up-bow  d6 q up-bow as3 q up-bow df4 q up-bow fs5 w up-bow
  a3 q detache d4 h detache  b4 w detache e5 q detache  d6 q detache as3 q detache df4 q detache fs5 w detache
  a3 q martele d4 h martele  b4 w martele e5 q martele  d6 q martele as3 q martele df4 q martele fs5 w martele
  a3 q thumb d4 h thumb  b4 w thumb e5 q thumb  d6 q thumb as3 q thumb df4 q thumb fs5 w thumb
  a3 q natural-harmonic d4 h natural-harmonic  b4 w natural-harmonic e5 q natural-harmonic  d6 q natural-harmonic 
    as3 q natural-harmonic df4 q natural-harmonic fs5 w natural-harmonic
  a3 q bartok-pizzicato d4 h bartok-pizzicato  b4 w bartok-pizzicato e5 q bartok-pizzicato  d6 q bartok-pizzicato 
    as3 q bartok-pizzicato df4 q bartok-pizzicato fs5 w bartok-pizzicato
  a3 q stopped-note d4 h stopped-note  b4 w stopped-note e5 q stopped-note  d6 q stopped-note 
    as3 q stopped-note df4 q stopped-note fs5 w stopped-note
  a3 q open-note d4 h open-note  b4 w open-note e5 q open-note  d6 q open-note as3 q open-note df4 q open-note fs5 w open-note
  a3 q left-hand-pizzicato d4 h left-hand-pizzicato  b4 w left-hand-pizzicato e5 q left-hand-pizzicato  d6 q left-hand-pizzicato 
    as3 q left-hand-pizzicato df4 q left-hand-pizzicato fs5 w left-hand-pizzicato
  ))
(cmntest '(cmn (staff treble 
       (note gf5 w. (trill (wavy-line t) (ornament-sign small-natural) (sign-position :in-parentheses) (other-note f5)))) 
     (staff bass c3 q c3 q c3 q c3 q c3 q c3 q )))
(cmntest '(cmn staff treble (c4 q begin-octave-up) c4 q line-break c4 q c4 q line-break c4 q (c4 q end-octave-up)))
(cmntest '(cmn treble c4 (rq 5/8) c4 (rq 1/8) c4 (rq 2/8) c4 (rq 5/8) c4 (rq 3/8) c4 (rq 1/8) c4 (rq 6/8) c4 (rq 1/8) c4 (rq 7/8) c4 (rq 1/8)))
(cmntest '(cmn treble c4 (rq 2/5) c4 (rq 3/5) c4 (rq 1/5) c4 (rq 1/5) c4 (rq 2/5) c4 (rq 1/5) c4 (rq 4/5) c4 (rq 1/5)))
(cmntest '(cmn treble c4 (rq 1/9) c4 (rq 7/9) c4 (rq 1/9) c4 (rq 2/9) c4 (rq 5/9) c4 (rq 2/9))) ;looks weird, but is consistent
(cmntest '(cmn treble c4 (rq 2/11) c4 (rq 3/11) c4 (rq 4/11) c4 (rq 2/11) c4 (rq 6/11) c4 (rq 1/11) c4 (rq 4/11))) ;ditto
(cmntest '(cmn treble c4 (rq 3/13) c4 (rq 3/13) c4 (rq 3/13) c4 (rq 4/13) c4 (rq 5/13) c4 (rq 4/13) c4 (rq 4/13)))
(cmntest '(cmn treble c4 (rq 2/7) c4 (rq 3/7) c4 (rq 2/7) c4 (rq 4/7) c4 (rq 3/7) c4 (rq 1/7) c4 (rq 1/7) c4 (rq 5/7) c4 (rq 6/7) c4 (rq 1/7)))

;;; broken: (loses dot)
(cmntest '(cmn treble c4 (rq 1/6) c4 (rq 2/6) c4 (rq 3/6)))

;;; broken: loses dot and no "9" over beam (it's confused by 3/9 reduced to 1/3 by lisp)
(cmntest '(cmn treble c4 (rq 2/9) c4 (rq 3/9) c4 (rq 4/9)))
(cmntest '(cmn staff treble (c5 w (begin-tremolo (dy .5))) (bf4 w end-tremolo) (c5 w (begin-tremolo (dx .25))) (bf4 w end-tremolo)
      (c5 w begin-tremolo) (bf4 w end-tremolo) (c5 w begin-tremolo) line-break (bf4 w end-tremolo) 
      (c5 w begin-tremolo) (bf4 w end-tremolo) (c5 h begin-tremolo) (bf4 h end-tremolo)
      (c4 q (tremolo (dy .5))) (chord (notes c4 g4) q (tremolo (dx .5)))))
(cmntest '(cmn (size 100) cs4 q bf3 q ef4 q fs5 q af5 q bf5 q))
;;; note-head size tests
(cmntest '(cmn (note-head-size .75) staff treble f5 w fs5 h c5 w cs5 h f4 w fs4 h c4 w cs4 h))
(cmntest '(cmn (note-head-size 1.5) staff treble f5 w fs5 h c5 w cs5 h f4 w fs4 h c4 w cs4 h))
(cmntest '(cmn (note-head-size 1.5) staff treble f5 e fs5 e c5 e cs5 e f4 e fs4 e c4 e cs4 e))
(cmntest '(cmn (note-head-size 1.5) staff treble c6 e c6 e c6 q a3 q a3 e a3 e))
(cmntest '(cmn (note-head-size 1.5) staff treble (chord (notes c5 g5 c6) h) (chord (notes a3 d4) h) 
  (chord (notes c5 c6) e) (chord (notes c5 c6) e) (chord (notes a3 e4) e) (chord (notes a3 e4) e)))
(cmntest '(cmn (note-head-size 1.5) (grace-note-size 1.0) staff treble (g4 h (appoggiatura (notes bn4 cs5))) 
     (bn4 e stem-up) (cs5 e stem-up) g4 h))
(cmntest '(cmn (size 200) (note-head-size 1.3) staff treble (c5 q stem-up) (b4 q (sign flat (dx -.1)) stem-down (onset 0))))
(cmntest '(cmn staff treble (c4 q (sharp invisible))))
(cmntest '(cmn (size 100) staff treble (chord q (notes ds4 gf4)) (chord q (notes df4 gf4)) (chord q (notes dn4 gf4)))) 
(cmntest '(cmn 
 (staff bar treble
	(chord (notes dn4 cn5 en5 an5 ds6 fs6) q no-stem) ;1
	(chord (notes dn4 bf4 en5 an5 ef6 gn6) q no-stem) ;2
	(chord (notes dn4 bf4 en5 fs5 an5 cn7) q no-stem) ;3
	(chord (notes dn4 gs4 as4 en5 fs5 ds6) q no-stem) ;4
	(chord (notes dn4 bf4 en5 cn6 an6) q no-stem) ;5
	(chord (notes dn4 ds5 en5 cs6 gs6) q no-stem) ;6
	(chord (notes an4 ef5 gn5 cs6 as6) q no-stem) ;7
	(chord (notes gs4 an4 fs5 gn5 cs6 en6) q no-stem) ;8
	(chord (notes gs4 fs5 gn5 cs6 fn6) q no-stem) ;9
	(chord (notes gs4 fs5 gn5 cs6 bn6) q no-stem) ;10
	(chord (notes ds4 gs4 fs5 gn5 dn6) q no-stem) ;11
	(chord (notes ds4 gs4 gn5 an5 bn5 cs7) q no-stem)))) ;12
(cmntest '(cmn (staff treble (chord (notes f4 g4) q.) (chord (notes f4 g4 a4) q.) 
	    (chord (notes e4 f4 g4 a4) q.) (chord (notes e4 f4 g4) q.) 
	    (chord (notes e4 g4 a4) q.) (chord (notes f4 a4 b4) q.) 
	    (chord (notes e4 f4) q.) (chord (notes f4 g4 a4 b4) q.) 
	    (chord (notes e4 f4 a4) q.) (chord (notes f4 g4 b4) q.))
     (staff treble (chord (notes f5 g5) q.) (chord (notes f5 g5 a5) q.) 
	    (chord (notes e5 f5 g5 a5) q.) (chord (notes e5 f5 g5) q.) 
	    (chord (notes e5 g5 a5) q.) (chord (notes f5 a5 b5) q.) 
	    (chord (notes e5 f5) q.) (chord (notes f5 g5 a5 b5) q.) 
	    (chord (notes e5 f5 a5) q.) (chord (notes f5 g5 b5) q.))))

(cmn treble (meter 4 4) (c4 q) (bar (dashed t) (within-measure t)) (c4 q)(c4 q)(c4 q) (c4 q) (c4 q)(c4 q)(c4 q))
(cmn treble (meter 4 4) (c4 q) (c4 e) (bar (dashed t) (within-measure t)) (c4 e)(c4 q)(c4 q) (c4 q) (c4 q)(c4 q)(c4 q))

;;; note-head-size tests
(cmntest '(cmn (note-head-size 1.5) staff treble (chord (notes a3 b3) q) (chord (notes a3 b3) e) (chord (notes a3 b3) e) ))
(cmntest '(cmn (note-head-size 1.5) staff treble (chord (notes c5 g5 c6) h) (chord (notes c5 d5) e) (chord (notes c5 d5) e) (chord (notes c5 d5) q) ))
(cmntest '(cmn (note-head-size 1.5) staff treble 
  (chord (notes cs5 gf5 cn6) h) (chord (notes cs5 ds5) e) 
  (chord (notes cf5 df5) e) (chord (notes cn5 ds5) q)))
(cmntest '(cmn staff treble (meter 4 4) (chord (notes a4 e5) (duration 7))))
(cmntest '(cmn (staff treble (chord (notes d4 e4 a4) q) (chord (notes a4 g5 a5) q)) 
     (staff bass (chord (notes e3 a3 b3) q) (chord (notes f2 g2 c3) q))))
(cmntest '(cmn (size 100) 
     (staff treble 
	    (setf midc0 (c4 s (stem-direction :up))) 
	    (setf midc1 (c4 s (stem-direction :down)))
	    (setf midc2 (chord (notes c4 g4) s (stem-direction :up)))
	    (setf midc3 (chord (notes c4 g4) s (stem-direction :down))))
  (staff bass 
	 (chord (notes c3 g3) s (tied-to midc0)) 
	 (chord (notes c3 g3) s (tied-to midc1)) 
	 (c3 s (tied-to midc2)) 
	 (c3 s (tied-to midc3)) 
	 )))
(cmntest '(cmn (size 100) 
     (staff treble 
	    (setf midc0 (c4 q (stem-direction :up))) 
	    (setf midc1 (c4 q (stem-direction :down)))
	    (setf midc2 (chord (notes c4 gf4) q (stem-direction :up)))
	    (setf midc3 (chord (notes c4 gf4) q (stem-direction :down))))
  (staff bass 
	 (chord (notes c3 gf3) q (tied-to midc0)) 
	 (chord (notes c3 gf3) q (tied-to midc1)) 
	 (c3 q (tied-to midc2)) 
	 (c3 q (tied-to midc3)) 
	 )))
(cmntest '(cmn (size 100) 
     (staff treble 
	    (setf midc0 (c4 q (stem-direction :up))) 
	    (setf midc1 (c4 q (stem-direction :down)))
	    (setf midc2 (chord (notes d4 ef4 en5) q (stem-direction :up)))
	    (setf midc3 (chord (notes d4 ef4 en5) q (stem-direction :down))))
  (staff bass 
	 (chord (notes d3 ef3 bf3) q (tied-to midc0)) 
	 (chord (notes d3 ef3 bf3) q (tied-to midc1)) 
	 (c3 q (tied-to midc2)) 
	 (c3 q (tied-to midc3)) 
	 )))
(cmntest '(cmn (staff treble (setf mc (c4 q stem-up))) (staff bass c3 (tied-to mc)) (staff bass c3 (tied-to mc)) (staff bass c3 (tied-to mc))))
(cmntest '(cmn (staff treble (setf mc (c4 e stem-up))) (staff bass c3 e (tied-to mc)) (staff bass c3 e (tied-to mc)) (staff bass c3 e (tied-to mc))))
(cmntest '(cmn (staff treble (setf mc (c4 e stem-up)) (setf md (c4 e stem-up))) 
     (staff bass c3 e (tied-to mc) (d3 e (note-head :none) (tied-to md)))
     (staff bass cs3 e (tied-to mc) ef3 e (tied-to md)) 
     (staff bass c3 e (tied-to mc) (chord (notes c3 gf3) e (tied-to md)))))
(cmntest '(cmn (size 100)
     (staff treble (chord (notes c4 g4) s) (setf midc3 (chord (notes d4 ef4 en5) s (stem-direction :down))))
     (staff bass sixteenth-rest (c3 s (tied-to midc3)))))
(cmntest '(cmn (size 100) (staff treble sixteenth-rest sixteenth-rest
     (setf midc3 (chord (notes d4 ef4 an5) s (stem-direction :down))) sixteenth-rest sixteenth-rest)
     (staff bass sixteenth-rest sixteenth-rest (cs3 s (tied-to midc3)) sixteenth-rest sixteenth-rest)))
(cmntest '(cmn (size 100) 
     (staff bar treble (setf arp1 (d4 s stem-up (note-head :x))) (setf arp2 (d4 s stem-down (note-head :x)))) 
     (staff bar bass (dn3 s (tied-to arp1)  (note-head :triangle)) (dn3 s (tied-to arp2)  (note-head :triangle)))))
(cmntest '(cmn (automatic-ties nil) staff treble dotted-half-rest dotted-quarter-rest dotted-eighth-rest dotted-sixteenth-rest))
(cmntest '(cmn staff treble (c4 (rq 2/5)) (c4 (rq 2/5)) (rest (rq 2/5)) (c4 (rq 2/5)) (c4 (rq 2/5))))
(cmntest '(cmn (size 40) (free-expansion-factor 1.0) staff treble 
     (g4 q (grace-note e4 e4)) 
     (g4 q (grace-note stem-down e4 e4))
     (g4 q (grace-note a4 e4))
     (g4 q (grace-note stem-down a4 e4))
     (g4 q (grace-note e4 a4))
     (g4 q (grace-note stem-down e4 a4))
     (line-break)
     (g4 q (grace-note e4 e e4 e)) 
     (g4 q (grace-note stem-down e4 e e4 e))
     (g4 q (grace-note a4 e e4 e))
     (g4 q (grace-note stem-down a4 e e4 e))
     (g4 q (grace-note e4 e a4 e))
     (g4 q (grace-note stem-down e4 e a4 e))
     (line-break)
     (g4 q (grace-note e4 f4 e4 f4)) 
     (g4 q (grace-note stem-down e4 f4 e4 b4))
     (g4 q (grace-note a4 f4 e4 c4))
     (g4 q (grace-note stem-down a4 f4 e4 c4))
     (g4 q (grace-note e4 f4 a4 b4))
     (line-break)
     (g4 q (grace-note stem-down e4 f4 a4 b4))
     (g4 q (grace-note (chord e4 b4))) 
     (g4 q (grace-note (chord ef4 bf4)))
     (g4 q (grace-note (chord ef4 af4 bf4 en5)))
     (g4 q (grace-note a4 (chord c4 c5)))
     (g4 q (grace-note (chord d4 a4) (chord c4 g4) (chord b3 fs4) (chord bf4 af4 gf4) (chord bf4 af4 ef4) (chord a3 e4)))
     (line-break)
     (chord (notes g4 d5) q (grace-note b4))
     (chord (notes gs4 df5) q (grace-note e4 f4 g4 a4))
     (g4 q (grace-note stem-down (chord e4 b4))) 
     (g4 q (grace-note stem-down (chord ef4 bf4)))))

;;; see grace.cmn for many 2 note cases
(cmntest '(cmn (size 40) staff treble 
     (g4 q (grace-note fs4)) 
     (g4 q (grace-note gf4)) 
     (g4 q (grace-note ef4)) 
     (g4 q (grace-note b4)) 
     (b4 q (grace-note e5)) 
     (b4 q (grace-note d4)) 
     (g4 q (grace-note ef5))))

(cmntest '(cmn (size 40) staff treble 
     (g4 q (grace-note e4 fs4)) 
     (g4 q (grace-note gf4 ef4)) 
     (g4 q (grace-note ef4 c4)) 
     (g4 q (grace-note b4 a4)) 
     (b4 q (grace-note e5 d5 cs5)) 
     (b4 q (grace-note d4 e4 fs4 g4 a4)) 
     (g4 q (grace-note ef5 bf4 ef4))))

(cmntest '(cmn (size 60) staff treble a4 q. (grace-note f4 g4) g4 e f4 q g5 q. g5 e (grace-note a5) begin-slur f5 e g5 e end-slur))

(cmntest '(cmn (size 60) staff treble (c6 q (appoggiatura b5 a5)) (c6 q (appoggiatura bf5 as5)) (c5 q (appoggiatura b4 d5))))
(cmntest '(cmn staff treble (c4 q (grace-note (notes d4 (chord fs4 bf4))))))
(cmntest '(cmn staff treble (c4 q (grace-note (notes d4 a4)))))
(cmntest '(cmn treble (c4 (rq 1/6) (setf hi (beat-subdivision- (subdivision 3)))) (c4 (rq 2/6) (-beat-subdivision- hi)) (c4 (rq 3/6) (-beat-subdivision hi))))
(cmntest '(cmn staff treble (meter 2 4) (c4 te) (g4 te) (a4 te) (e5 te) (f5 te) (g5 te) 
     (c4 tq) (g4 te) (c4 te) (g4 tq) (e5 tq) (d5 te) (d5 te) (e5 tq)
     (c4 te) (c5 tq) (c5 tq) (c4 te)))
(cmntest '(cmn staff treble (meter 2 4) (c4 (rq 1/6)) (d4 (rq 1/6)) (e4 (rq 1/6)) (f4 (rq 1/6)) (g4 (rq 1/6)) (a4 (rq 1/6))))
(cmntest '(cmn staff treble (meter 4 4) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5))))
(cmntest '(cmn staff treble (meter 4 4) (c4 th) (c4 th) (c4 th)))
(cmntest '(cmn staff treble d4 ts f4 ts a4 ts b4 s d5 s   
                  d4 ts f4 ts a4 ts b4 s a4 s
		  d4 s d4 s d4 ts d4 ts d4 ts
		  d4 s f4 s a4 ts b4 ts d5 ts
		  d4 s d4 ts d4 ts d4 ts d4 s))
(cmntest '(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 3) (dy .5)))) 
     (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi))))
(cmntest '(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 3) (bracket-type :down-up) (dy .5)))) 
     (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi))))
(cmntest '(cmn staff treble (c4 te no-beam) (rest (rq 1/3) no-beam) (e4 te no-beam)))
(cmntest '(cmn staff treble c4 ts c4 ts c4 ts d4 e e4 e f4 e g4 e g4 ts g4 ts g4 ts))
(cmntest '(cmn staff treble (meter 4 4) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 2/5)) (c4 (rq 2/5))))
(cmntest '(cmn staff treble c4 ts d4 ts e4 ts f4 q e4 ts d4 ts c4 ts))
(cmntest '(cmn (automatic-beams nil) staff treble c4 ts c4 ts c4 te c4 ts c4 ts c4 ts c4 ts c4 ts c4 ts c4 te)) 
(cmntest '(cmn staff treble (g4 (rq 2/3)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) 
   (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 2/3))))
;;; now for a somewhat silly distinction...
(cmntest '(cmn staff treble c4 ts c4 ts c4 ts c4 e c4 ts c4 ts c4 ts c4 te.))
(cmntest '(cmn staff treble (c4 q (text- "hi")) c4 q c4 q c4 q c4 q c4 q (line-mark) c4 q c4 q c4 q (c4 q (-text "ha"))))
(cmntest '(cmn staff treble (c4 q (text- "hi")) c4 q c4 q c4 q c4 q c4 q (line-mark) (c4 q (-text- "ho")) c4 q c4 q (c4 q (-text "ha"))))
(cmntest '(cmn (size 24) (line-separation 4.0) (automatic-line-breaks nil) (automatic-bars nil)     
  (system bracket (staff bar d4 h b3 q bf3 q bar quarter-rest (e4 q begin-slur
            (setf txt1 (text- "accel. al doppio movimento" (y #'(lambda (mark note score &optional justifying) (+ (staff-y0 note) 1.5)))
               (connecting-pattern '(7 20)) (font-name "Times-BoldItalic")(font-scaler .5))) (begin-crescendo mp))
      (e4 h (grace-note f4)) (ds4 q) (dn4 q) (bar) (e4 h) (b3 q) (bf3 h) (g3 q end-crescendo end-slur)
      (bar) (e4 q p begin-slur) (e4 q (grace-note f4)) (ds4 q) (dn4 q) (e4 h) (bar) (LINE-MARK)
      (b3 q) (bf3 q) (e4 q) (g3 q) (fs3 q end-slur) (e4 q begin-tie begin-slur) bar 
      (e4 q end-tie) (ds4 q) (dn4 q) (e4 q) (b3 q) (bf3 q) (e4 q ) (g3 q)
      (fs3 q end-slur (mm 120 q (dx -1.5) unjustified in-parentheses) (-text txt1 "")) (bar )	(LINE-MARK)))))
(cmntest '(cmn staff treble (c4 q (text- "hi" (dy 1.0))) d4 q line-break c4 q (c4 q (-text "ho"))))
(cmntest '(cmn staff treble (c4 q (text- "hi" (dy 1.0))) d4 q line-break c4 q (c4 q (-text "ho" (dy 1.0)))))
(cmntest '(cmn staff treble (c4 h (text- "hi")) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!"))))

(cmntest '(cmn staff treble 
     (c4 h (setf aa1 (text- "hi")) 
	 (setf aa2 (text- "yow" (y0 2.0))))
     (d4 q (-text- "za" aa2 (y0 2.0))) 
     (d4 q (-text- "ho" aa1)) 
     (e4 q (-text "!" (y0 2.0))) 
     (e4 h (-text "away!"))))

(cmntest '(cmn staff treble (c4 h (text- "hi")) (d4 e) (e4 h (-text "ho"))))
(cmntest '(cmn staff treble c4 q (bass begin-first-ending) c4 q (bar end-first-ending)))
(cmntest '(cmn staff treble (meter 2 4) (c4 q) (c4 q) (bar begin-first-ending) (c4 q) (c4 q) 
     (end-repeat-bar end-first-ending begin-second-ending) (c4 q) (c4 q end-second-ending) 
     bar c4 q (c4 q begin-first-ending) bar (c4 q) (c4 q) 
     (begin-and-end-repeat-bar end-first-ending begin-second-ending) c4 q (quarter-rest end-second-ending) 
     bar c4 q c4 q (bass begin-first-ending) bar (c4 q) (c4 q) (bar end-first-ending)))
(cmntest '(cmn (size 60) staff treble (meter 2 4) (c4 q) (c4 q) (bar begin-first-ending) (c4 q) (c4 q) 
     (end-repeat-bar end-first-ending begin-second-ending) (c4 q) line-mark (c4 q end-second-ending) 
     bar c4 q (c4 q begin-first-ending) bar (c4 q) (c4 q) (begin-and-end-repeat-bar end-first-ending begin-second-ending) 
     c4 q (quarter-rest end-second-ending) bar c4 q c4 q (bass (dx -.25) begin-first-ending) bar (c4 q) (c4 q) (bar end-first-ending)))
(cmntest '(cmn (size 200) (footer-margin 3.0) (staff (staff-lines 0) 
  (c4 w (begin-tie (tie-curvature .3) (dashed '(.15 .05)) (gray-scale .4)) (marcato (dy .125) (gray-scale .5) (dx .625))) (c4 w end-tie))))
(cmntest '(cmn staff treble (chord (notes c4 d4 e4) (begin-tie (tie-direction :down)) q) (chord (notes c4 d4 e4) end-tie q)))
(cmntest '(cmn staff treble 
     (chord (notes c5 a4) q stem-down (begin-tie)) (chord (notes c5 a4) q stem-up (end-tie)) 
     (chord (notes c5 a4) q stem-up (begin-tie)) (chord (notes c5 a4) q stem-down (end-tie)) 
     (c4 q stem-down (begin-tie)) (c4 q stem-up (end-tie)) 
     (c4 q stem-up (begin-tie)) (c4 q stem-down (end-tie)) 
     (chord (notes cs5 a4) q stem-down (begin-tie)) (chord (notes c5 a4) q stem-up (end-tie)) 
     (chord (notes c5 af4) q stem-up (begin-tie)) (chord (notes c5 a4) q stem-down (end-tie)) 
     (cs4 q stem-down (begin-tie)) (c4 q stem-up (end-tie)) 
     (cf4 q stem-up (begin-tie)) (c4 q stem-down (end-tie)) 
     (chord (notes c5 g4 a4) q stem-down (begin-tie)) (chord (notes c5 g4 a4) q stem-up (end-tie)) 
     (chord (notes c5 b4 a4) q stem-up (begin-tie)) (chord (notes c5 a4 b4) q stem-down (end-tie))
     line-mark
     (chord (notes c5 a4 e4) q stem-down (begin-tie)) (chord (notes c5 a4 e4) q stem-down (end-tie)) 
     (chord (notes c5 a4 d4) q stem-up (begin-tie)) (chord (notes c5 a4 d4) q stem-up (end-tie)) 
     (chord (notes cs5 a4) q stem-down (begin-tie)) (chord (notes c5 a4) q stem-down (end-tie)) 
     (chord (notes c5 af4) q stem-up (begin-tie)) (chord (notes c5 a4) q stem-up (end-tie)) 
     (chord (notes c5 g4 a4) q stem-down (begin-tie)) (chord (notes c5 g4 a4) q stem-down (end-tie)) 
     (chord (notes c5 b4 a4) q stem-up (begin-tie)) (chord (notes c5 a4 b4) q stem-up (end-tie))))

(cmntest '(cmn staff treble (meter 2 4) 
     (engorge (loop for i from 0 to 10 
	       collect (chord (notes c4 g4) e.)
	       collect (chord (notes c5 g5) e.)
	       collect (chord (notes bf4 c5) e.)
	       collect (chord (notes c4 d4) e.)
	       collect (chord (notes cs4 ds4 gs4) e.)
	       collect (chord (notes ef6 df6 bf5) e.)))))

(cmntest '(cmn (size 24) (free-expansion-factor 2) staff treble 
     (c4 q begin-slur) (e4 q end-slur) (c4 q begin-slur) (c5 q end-slur) (c5 q begin-slur) 
     (a4 q end-slur) (c5 q begin-slur) (c4 q end-slur) (a4 q begin-slur) (c5 q end-slur)
     (e6 q begin-slur) (c5 q end-slur) (e3 q begin-slur) (a4 q end-slur)))
(cmntest '(cmn (size 24) (free-expansion-factor 2) staff treble 
     (c4 q begin-slur) (d4 q) (e4 q end-slur) 
     (c4 q begin-slur) (c4 q) (c4 q end-slur) 
     (c4 q begin-slur) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (b4 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (c4 q end-slur) 
     (a4 q begin-slur) (b4 q) (c5 q end-slur)
     (c5 q begin-slur) (c5 q) (c5 q end-slur)
     (g4 q begin-slur) (c4 q) (e4 q end-slur) 
     (g4 q begin-slur) (e4 q) (c4 q end-slur) 
     (c5 q begin-slur) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (c4 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (c5 q end-slur) 
     (a4 q begin-slur) (b4 q) (c4 q end-slur)
     (e3 q begin-slur) (a4 q) (g3 q end-slur)
     (e6 q (begin-slur (slur-direction :down))) (b4 q) (g6 q end-slur)
     ))
(cmntest '(cmn (size 24) (free-expansion-factor 2) staff treble 
     (c4 q begin-slur) (d4 q) (f4 q) (e4 q end-slur) 
     (c4 q begin-slur) (c4 q) (c4 q) (c4 q end-slur) 
     (c4 q begin-slur) (a4 q) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (b4 q) (g4 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (e4 q) (c4 q end-slur) 
     (a4 q begin-slur) (b4 q) (b4 q) (c5 q end-slur)
     (c5 q begin-slur) (c5 q) (c5 q) (c5 q end-slur)
     (g4 q begin-slur) (c4 q) (c5 q) (e4 q end-slur) 
     (g4 q begin-slur) (e4 q) (d4 q) (c4 q end-slur) 
     (c5 q begin-slur) (a4 q) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (c4 q) (c5 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (c4 q) (c5 q end-slur) 
     (a4 q begin-slur) (b4 q) (b4 q) (c4 q end-slur)
     (e3 q begin-slur) (a4 q) (a4 q) (g3 q end-slur)
     (a4 q begin-slur) g4 q f4 q (a4 q end-slur)
     (a4 q begin-slur) f4 q g4 q (a4 q end-slur)
     (c5 q begin-slur) d5 q e5 q (a4 q end-slur)
     (c5 q begin-slur) a5 q g5 q f5 q e5 q d5 q (c5 q end-slur)
     (e6 q (begin-slur (slur-direction :down))) (b4 q) (b4 q) (g6 q end-slur)
     ))
(cmntest '(cmn (size 24) staff treble 
     g4 q begin-slur e4 q fs4 q end-slur
     g4 q begin-slur gf4 q ef4 q end-slur
     g4 q begin-slur ef4 q c4 q end-slur
     g4 q begin-slur (b4 q stem-up) a4 q end-slur
     b4 q begin-slur e5 q d5 q cs5 q end-slur
     b4 q begin-slur d4 q e4 q fs4 q g4 q a4 q end-slur
     g4 q begin-slur ef5 q (bf4 q stem-up) ef4 q end-slur))
(cmntest '(cmn staff treble c4 e c4 e c4 e c4 e (c4 e begin-beam) line-mark c4 e (c4 e end-beam)))
(cmntest '(cmn (size 32) staff treble sixteenth-rest c4 e c4 s sixteenth-rest c5 e c5 s c6 s 
     sixteenth-rest sixteenth-rest c4 s c4 s sixteenth-rest sixteenth-rest c6 s
     (sixteenth-rest begin-beam) c4 e (sixteenth-rest end-beam) (sixteenth-rest begin-beam) c5 e (sixteenth-rest end-beam)
     (thirty-second-rest begin-beam) c4 e. (thirty-second-rest end-beam)
     c6 s c6 s sixteenth-rest c6 s  c5 s sixteenth-rest c5 s c5 s  c4 s c4 s sixteenth-rest c4 s  
     g3 s g3 s sixteenth-rest g3 s  c6 s c5 s sixteenth-rest c4 s  c4 s c5 s sixteenth-rest c6 s
     sixteenth-rest c4 s c4 s c4 s c4 s sixteenth-rest c4 s c4 s 
     c4 s c4 s sixteenth-rest c4 s c4 s c4 s c4 s sixteenth-rest
     c4 e. c4 s c4 e.. c4 (rq 1/8) c4 s c4 s c4 e c4 s c4 e c4 s c4 s c4 e. c4 (rq 1/8) c4 e..
     c4 e c5 e c5 e c4 e c4 s e4 s g4 s c5 s c5 s a4 s f4 s c4 s
     c4 s. (c4 (rq 1/8)) (c4 (rq 1/8)) c4 s. c4 s.. (d4 (rq 1/16)) (d4 (rq 1/16)) c4 s..))

(cmntest '(cmn (staff treble (chord (notes c4 c5) e (setf ib0 (beam-))) (eighth-rest)) 
     (staff bass (eighth-rest) (chord (notes g3 g2) e (-beam ib0)))))

(cmntest '(cmn (staff treble (df5 e (onset 0) (setf ib0 (beam-))) (eighth-rest) (eighth-rest) (c5 e (onset 1.5) (-beam- ib0))) 
     (staff bass (eighth-rest) (c3 e (onset .5) (-beam- ib0)) (e3 e (onset 1.0) (-beam ib0)) (eighth-rest))))

(cmntest '(cmn (staff treble (df5 s (onset 0) (setf ib0 (beam-))) (sixteenth-rest) (eighth-rest) (sixteenth-rest) (c5 s (onset 1.25) (-beam- ib0))) 
     (staff bass (sixteenth-rest) (c3 s (onset .25) (-beam- ib0)) (d3 e (onset .5) (-beam- ib0)) 
	    (e3 s (onset 1.0) (-beam ib0)) (sixteenth-rest))))

(cmntest '(cmn (staff-separation 0) 
  (staff (staff-lines 1) (start-line 2) percussion 
    (b4 s (setf hi (beam- ))) dotted-eighth-rest) 
  (staff (staff-lines 1) (start-line 2) percussion 
    sixteenth-rest (b4 s (-beam- hi)) sixteenth-rest (b4 s (-beam- hi)))
  (staff (staff-lines 1) (start-line 2) percussion 
    eighth-rest (b4 s (-beam hi)) sixteenth-rest)))

(cmntest '(cmn (automatic-rests nil) (staff-separation 0)
     (staff (staff-lines 1) (start-line 2) percussion 
	    (sixteenth-rest (setf hi (beam-))) (eighth-rest invisible) (setf mc (b4 s)))
     (staff (staff-lines 1) (start-line 2) percussion 
	    (b4 s. (onset .25) (-beam- hi)) (b4 s (-beam- hi) (tied-to mc) (onset .75)))
     (staff (staff-lines 1) (start-line 2) percussion 
	    (b4 (rq 1/8) (onset .625) (-beam hi)) (sixteenth-rest (scale 0 0)) bar)))

(cmntest '(cmn staff treble (chord e (notes g4 g5)) (chord e (notes a4 as4)) (chord e (notes c4 e4)) (chord e (notes d5 ds5))))

(cmntest '(cmn (size 32) staff treble 
     (meter 2 4) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 5 16) c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s 
     (meter 2 4) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 3 8) c4 e c4 e c4 e               c4 e c4 e c4 e 
     (meter 7 8) c4 e c4 e c4 e c4 e c4 e c4 e c4 e     c4 e c4 e c4 e c4 e c4 e c4 e c4 e 
     (meter 7 16) c4 s c4 s c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s c4 s c4 s 
     (meter 1 4) c4 e c4 e      c4 e c4 e 
     (meter 13 8) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e))

(cmntest '(cmn (size 24) staff treble 
     (meter 2 4 (beaming 2)) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 5 16 (beaming (list .75 .5))) c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s 
     (meter 2 4 (beaming (list .5 1.5))) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 3 8 (beaming (list 1.0 .5))) c4 e c4 e c4 e               c4 e c4 e c4 e 
     (meter 7 8 (beaming (list 1.5 2.0))) c4 e c4 e c4 e c4 e c4 e c4 e c4 e     c4 e c4 e c4 e c4 e c4 e c4 e c4 e 
     (meter 7 16 (beaming (list .75 1.0))) c4 s c4 s c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s c4 s c4 s 
     (meter 1 4 (beaming .5)) c4 e c4 e      c4 e c4 e 
     (meter 13 8 (beaming (list 4.0 2.5))) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e      
     (meter 19 8 (beaming (list .5 5.0 1.5 2.5))) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e))

(cmntest '(cmn (staff treble (chord e (notes g4 g5)) (chord e (notes g4 a4 c5 e4)) (chord e (notes c4 e4)) (chord e (notes d5 d5))) 
     (staff bass c3 e c3 e c3 e c3 e)))
(cmntest '(cmn (size 60) staff treble (chord e (notes g4 g5)) (chord e (notes gs4 a4 d5)) (chord e (notes g4 g5)) (chord e (notes fs4 cs5 d5)) ))
(cmntest '(cmn staff treble (chord s (notes d5 ds5 fs4)) (chord s (notes fs5 ds5 a4))))
(cmntest '(cmn staff treble (chord s (notes fs4 cs5 c5)) (chord s (notes cs5 f5 d5)) (chord s (notes ds5 fs4 c5)) (chord s (notes ds5 d5 cs5))
      (chord s (notes fs4 f5 c5)) (chord s (notes c5 cs5 f5)) (chord s (notes d5 ds5 fs4)) (chord s (notes fs5 ds5 a4))))

(cmntest '(cmn staff treble (meter 3 4) (c4 s begin-beam) c4 e. c4 s c4 e. c4 s (c4 e. end-beam)))
(cmntest '(cmn staff treble (c4 e. begin-beam) c4 s c4 e.. (c4 (rq 1/8) end-beam) 
                  (c5 e. begin-beam) c5 s c5 e.. (c5 (rq 1/8) end-beam) 
                  (c4 (rq 1/8) begin-beam) c4 e.. c4 e. (c4 s end-beam) 
		  (c5 (rq 1/8) begin-beam) c5 e.. c5 s (c5 e. end-beam) ))
(cmntest '(cmn staff treble (c4 e. begin-beam) c4 s c4 e c4 s c4 (rq 1/8) (c4 (rq 1/8) end-beam) 
                  (c5 e. begin-beam) c5 s c5 e c5 s c5 (rq 1/8) (c5 (rq 1/8) end-beam) 
                  (c4 (rq 1/8) begin-beam) c4 (rq 1/8) c4 e c4 s c4 s c4 e (c4 s end-beam) 
		  (c5 (rq 1/8) begin-beam) c5 e c5 s. c5 s c5 s. (c5 s. end-beam) ))
(cmntest '(cmn staff treble (meter 4 4) c4 e c4 q sixteenth-rest c4 s c4 s c4 e c4 q c4 e))

(cmntest '(cmn (section (staff treble c4 q) (staff) (staff bass d3 q)) 
     (section (staff treble e4 q) (staff brace g4 q) (staff bass e3 q))))
(setf v1 (staff treble (meter 3 4) c4 q c4 h))
(setf v2 (staff treble (meter 3 4) d4 h d4 q))
(cmntest '(cmn (staff-engorge (list v1 v2))))

(setf s1 (system (staff treble (meter 3 4) c4 q c4 h) (staff bass (meter 3 4) c3 h c3 q)))
(setf s2 (system (staff treble (meter 3 4) d4 q d4 h) (staff bass (meter 3 4) d3 h d3 q)))
(cmntest '(cmn (system-engorge (list s1 s2))))

(setf sc1 (score staff treble c4 h c4 q))
(setf sc2 (score (initial-onset 3) staff treble (d4 q (onset 3)) (d4 h (onset 4))))
(cmntest '(cmn (score-engorge (list sc1 sc2))))

(cmntest '(cmn staff bar treble measure-rest bass bar measure-rest bar))
(cmntest '(cmn staff treble (meter 2 4) (c4 q (beat 2)) (cs4 e (beat 1)) (d4 e (beat 2.5))))
(cmntest '(cmn (staff treble (meter 1 4) (c4 (rq 1/5)) (c4 (rq 3/5)) (c4 (rq 1/5)))))
(cmntest '(cmn (staff treble (meter 1 4) (c4 (rq 1/5) (setf hi (beat-subdivision- (subdivision 5) )))
        (c4 (rq 3/5) (-beat-subdivision- hi)) (c4 (rq 1/5) (-beat-subdivision hi)))))
(cmntest '(cmn (staff treble (meter 2 4) (c4 (rq 1/7) (setf hi (beat-subdivision- (subdivision 7) )))
        (c4 (rq 5/7) (-beat-subdivision- hi)) (c4 (rq 1/7) (-beat-subdivision hi)))))
(cmntest '(cmn (staff treble (meter 2 4) (c4 (rq 1/7) (setf hi (beat-subdivision- (subdivision 7) )))
        (c4 (rq 4/7) (-beat-subdivision- hi)) (c4 (rq 1/7) (-beat-subdivision- hi)) (c4 (rq 1/7) (-beat-subdivision hi)))))
(cmntest '(cmn (staff treble (meter 1 4) (c4 tq) (c4 te))))
(cmntest '(cmn (redundant-accidentals nil) staff bar treble (key d-minor) (meter 1 4) bs4 bar bf4 bar bn4 bar b4 double-bar))
(cmntest '(cmn (redundant-accidentals nil) staff bar treble (key d-minor) (meter 1 4) b4 s bf4 s b4 s b4 s))
(cmntest '(cmn (automatic-naturals t) (implicit-accidental-style :old-syle) staff treble (meter 2 4) cs4 q c4 q c4 q cs4 q))
(def-key foo-major bf4 ef5 fs5)
(cmntest '(cmn (automatic-naturals t) (implicit-accidental-duration 1) 
     staff treble (key foo-major) c4 q cs4 q f4 q cs4 q (chord (notes c4 g4 gs5) q) e4 q e4 q g4 q))
(cmntest '(cmn (automatic-naturals t) staff treble ds4 q begin-tie d4 q end-tie))
(cmntest '(cmn (automatic-naturals t) staff treble (chord (notes ds4 gf4) q begin-tie) (chord (notes d4 g4) q end-tie)))
(cmntest '(cmn (redundant-accidentals nil) staff treble (meter 4 4) cs4 q cs4 q cs4 q cs4 q cs4 q cs4 q cs4 q cs4 q ))
(cmntest '(cmn (redundant-accidentals nil) (staff alto (meter 3 4) fs4 s d4 s cs4 s as3 s gs3 s e3 s ds3 s cs3 s cs4 q bar)))
(cmntest '(cmn (redundant-accidentals nil) staff bar treble (chord e (notes b5 a4)) (chord s (notes ds6 a4)) (chord s (notes b5 a4))
      (chord q (notes c6 a4)) (chord s (notes cs6 a4)) (chord s (notes ds6 a4)) (chord e (notes dn6 a4)) (chord e (notes d6 a4)) bar))
(cmntest '(cmn (section (redundant-accidentals nil) (staff treble (meter 4 4) cs4 q cs4 q cs4 q cs4 q)) 
     (section (staff treble cs4 q cs4 q cs4 q cs4 q)) ))

;; need to insert final bars if barred and section end on a bar and doesn't have one
(cmntest '(cmn (section (curvy-flags nil) 
	      (system (staff treble (meter 2 4) c4 e g4 q c4 e) (staff bass (meter 2 4) c3 h)) (staff alto (meter 2 4) c4 h))
     (section (curvy-flags t) 
	      (system (staff treble c4 e g4 q c4 e) (staff bass c3 h)) (staff alto c4 h))))
(cmntest '(cmn (staff (staff-size 1.25) treble (meter 2 4) c4 e c4 e c4 e c4 e bar whole-rest bar c4 e c4 e c4 e c4 e) 
        (staff (staff-size .50) bar bass (meter 2 4) whole-rest bar c4 e c4 e c4 e c4 e bar whole-rest bar)))

(cmntest '(cmn (size 60)
      (staff (staff-size .5) treble (engorge (loop for i from 0 to 18 collect (c4 q))))
      (setf hi (staff treble (engorge (loop for i from 0 to 18 collect (d4 q)))) )
      (staff (tied-to hi) (treble invisible) g4 q)
      (staff (staff-size .75) (brace (brace-staves 3)) bass (engorge (loop for i from 0 to 18 collect (e4 q))))))

  (cmntest '(cmn 
   (size 12) (automatic-measure-numbers 1)
   (layout :old-style)
   (engorge 
    (loop for i from 0 to 3 
     collect (system 
  	    bracket 
	    (engorge 
	     (loop for j from 0 to 3 
	      collect (staff 
		       bar 
		       (if (= j 0) treble
			 (if (= j 1) alto
			   (if (= j 2) tenor bass)))
		       (meter 2 4) 
		       (engorge 
			(loop for k from 0 to 256 
			 collect 
			  (if (< k 128)
			      (if (< k (* 16 (+ 1 (* 4 (if (= i 2) 1 (if (= i 3) 0 i))) j))) (c4 q) quarter-rest)
			    (if (< (- 256 k) (* 16 (+ 1 (* 4 (if (= i 2) 1 (if (= i 3) 0 i))) j))) (c4 q) quarter-rest)))) 
		       full-double-bar))))))))

  (cmntest '(cmn (page-width 4) staff treble e-major c4 q c4 q c4 q bar (cancel e-major) line-mark c4 q c4 q c4 q bar))
  (cmntest '(cmn (always-show-staff-names nil) (page-width 4)
       staff (staff-name "hi there") treble c4 q c4 q c4 q bass bar line-mark c4 q c4 q c4 q line-mark c4 q c4 q c4 q))

  (cmntest '(cmn (staff (layout :new-style) (meter 2 4 invisible) (treble unjustified (dx -.75) (onset 8)) (c4 q (onset 8)))
       (staff bass (meter 2 4) (engorge (loop for i from 0 to 15 collect (e3 q))))))
  (cmntest '(cmn treble c4 q (rqq '(1 (1 1 1)) c4 staccato c4 c4) g4 q)) 
  (cmntest '(cmn treble (rqq '(1 (1 (2 (1 1 1 1 1)))) c4 staccato c4 c4  c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1  (1 (1 (1 2)) 1)) c4 staccato c4 c4 c4) 
              (rqq '(1 ((1 (2 1)) 1 1)) c4 staccato c4 c4 c4) 
	      (rqq '(1 (1 1 (1 (2 1)))) c4 staccato c4 c4 c4)))
  (cmntest '(cmn treble c4 q (rqq '(1 (1 1 1 2)) c4 staccato c4 c4 c4) g4 q)) 
  (cmntest '(cmn treble (rqq '(1  (1 (1 (1 2)) 1/2 1/2)) c4 staccato c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1  (1 (1 (1 2)) 3/4 1/4)) c4 staccato c4 c4 c4 c4))) 
  (cmntest '(cmn treble (rqq '(1 (2 3 2 3 2)) c4 c4 c4 c4 c4))) ;testing dots
  (cmntest '(cmn treble (rqq '(1 (2 3 2 3 3)) c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 (4 3 2 3 4)) c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(2 (4 3 2 3 4)) c4 c4 c4 c4 c4))) ;testing auto-tie over-ride
  (cmntest '(cmn treble (rqq '(3 (4 3 2 3 4)) c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 (1 1 1)) c4 c4 c4) (rqq '(2 (1 1 1 1 1)) c4 c4 c4 c4 c4) (rqq '(1 (1 (2 (1 2 2)))) c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 (1 2)) (1 ((3 (1 2 4)) 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 (1 (1 (1 1 1)) 7/8 1/8 (2 (1 1 1 (2 (1 1 1)))))) g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4)))
  (cmntest '(cmn treble (rqq '(1 ((1 (1 1 1)) (1 (1 1 1)) (1 (1 1 1)))) g4 g4 g4 g4 g4 g4 g4 g4 g4 ))) ;test beam groupings
  (cmntest '(cmn treble (rqq '(2 ((1 (1 1 1)) (1 (1 2)) (1 (1 1 1)))) g4 fs4 sixteenth-rest (chord c4 ef4) eighth-rest af4 bf4 gf4)))
  (cmntest '(cmn treble (rqq '(2 ((1 (1 1 1 2)) (1 (1 2)) (1 (1 2 4)))) g4 fs4 sixteenth-rest en4 (chord c4 ef4) eighth-rest af4 bf4 gf4)))  
  (cmntest '(cmn treble (rqq '(1 ((1 (1 2)) (1 ((3 (1 1 1)) 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 (1 2)) (1 ((3 (1 2 3)) 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(5 (1 (1 (1 2)) 1 1)) c4 c4 c4 c4 c4))) ;test flagged note in quarter
  (cmntest '(cmn treble (rqq '(1 (2 5)) c4 c4 c4))) ;test auto-tie
  (cmntest '(cmn (size 18)                         ;test overall spacing
    (staff treble (rqq '(2 ((1 (1 1 1)) (1 (1 2)) (1 (1 1 1)))) g4 fs4 sixteenth-rest (chord c4 ef4) eighth-rest af4 bf4 gf4))
    (staff treble (rqq '(1 (1 1 1)) c4 c4 c4) (rqq '(1 (1 (2 (1 2 2)))) c4 c4 c4 c4)) 
    (staff treble c4 e c4 e c4 e c4 e c4 q)))
  (cmntest '(cmn treble (rqq '(1 ((1 (1 2)) (1 (3 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 (2 5)) (1 (1 6)) (1 (6 1)))) c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(2 ((1 (2 5)) (1 (1 6 3)) (1 (6 1)))) c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 (2 5)) (1 (1 9)) (1 (9 1)))) c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 ((1 ((1 (1 1 1)) 1 1)) 1 1)) 1 1)) c4 c4 c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 (2 5)) (1 (1 9)) (1 (9 5)))) c4 c4 c4 c4 c4 c4 c4)))


  ;; rqq bugs:
  ;; 2 of triplet should be a half note
  (cmntest '(cmn treble (rqq '(4 (1 (2 (1 2)) 1 1)) c4 c4 c4 c4 c4)))

  ;; first flag misplaced vertically(!)
  (cmntest '(cmn treble (rqq '(1 (2 9)) c4 c4 c4))) ;test auto-tie

  ;; a mess -- and '(3 ...) is worse
  (cmntest '(cmn treble (rqq '(1 ((1 ((1 ((1 (1 2)) 1 2)) 2 2)) 2 2 1)) c4 c4 c4 c4 c4 c4 c4 c4 c4)))
  (cmntest '(cmn treble (rqq '(1 ((1 ((1 ((1 (1 1 1)) 1 2)) 2 2)) 2 2 1)) c4 c4 c4 c4 c4 c4 c4 c4 c4 c4)))

  
  ;; related bugs:
  ;; no dot on 3/6
  (cmntest '(cmn treble c4 (rq 1/6) c4 (rq 2/6) c4 (rq 3/6)))

  ;; all screwed up (the 3/9 becomes 1/3 and everything goes to hell)
  (cmntest '(cmn treble c4 (rq 2/9) c4 (rq 3/9) c4 (rq 4/9)))
(cmntest '(cmn staff treble (c4 e (begin-wedge-beam 4 1)) (c4 e) (c4 e) (c4 e (end-wedge-beam))))
(cmntest '(cmn staff treble (c5 e (begin-wedge-beam 4 1)) (c5 e) (c5 e) (c5 e (end-wedge-beam))))
(cmntest '(cmn staff treble (c4 e (begin-wedge-beam 1 4)) (c4 e) (c4 e) (c4 e (end-wedge-beam))))
(cmntest '(cmn staff treble (c4 e (begin-wedge-beam 1 4)) (c4 e) (d4 e) (e4 e) (f4 e) (d4 e) (e4 e) (g4 e (end-wedge-beam))))
(cmntest '(cmn (staff treble 
	    (c4 e (let ((wbl (wedge-beam- 4 1 (dy -.5)))) (setf wb (first (data wbl))) wbl)) (c4 e (-wedge-beam- wb)) quarter-rest)
     (staff treble quarter-rest (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam wb)))))
(cmntest '(cmn (size 40) 
     (staff treble 
	    (c4 e (let ((wbl (wedge-beam- 3 1 (dy -.5)))) (setf wb (first (data wbl))) wbl)) (c4 e (-wedge-beam- wb)) quarter-rest)
     (staff (dy -.5) treble quarter-rest (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam- wb)) 
	    (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam wb)) )))
(cmntest '(cmn (size 40) 
     (staff treble eighth-rest
	    (c4 e (let ((wbl (wedge-beam- 3 1 (dy -.5)))) (setf wb (first (data wbl))) wbl)) (c4 e (-wedge-beam- wb)) quarter-rest)
     (staff (dy -.5) treble (c4 e (-wedge-beam- wb)) quarter-rest (c4 e (-wedge-beam- wb)) 
	    (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam wb)) )))

;;; the nominal note duration for every note under the wedge beam should be the duration that corresponds to the
;;; minimum number of beams -- i.e. if we go from 2 to 6 beams, every note should have a duration of s (sixteenth=2 beams).
;;; (I subsequently added a check for this).

(cmntest '(cmn staff treble (c4 s (begin-wedge-beam 2 4)) (c4 s) (c4 s) (c4 s (end-wedge-beam))))

(cmntest '(cmn (size 24) (AUTOMATIC-LINE-BREAKS nil) 
  (staff treble       
    (note fs5 s (begin-wedge-beam 2 4))(note fn5 s)(note e5 s)(note fs5 s) 
    (note fn5 s) (note e5 s) (note fs5 s) (note fn5 s)
    (note e5 s) (note fs5 s) (note fn5 s) (note e5 s)
    (note fs5 s) (note fn5 s) (note e5 s) (note fs5 s)
    (note fn5 s) (note e5 s) (note fs5 s)(note fn5 s)
    (note e5 s) (note fs5 s) (note fn5 s) (note e5 s (end-wedge-beam))
    (bar)(line-mark))))
 (cmntest '(cmn (free-expansion-factor 3) staff treble (c4 q pedal-) (d4 q -pedal-) (e4 q -pedal)))
 (cmntest '(cmn (free-expansion-factor 3) staff treble (c4 q pedal-) (d4 q -pedal-) (e4 q (-pedal (dx 2.0)))))
 (cmntest '(cmn (free-expansion-factor 3) (staff treble (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (g4 q -u.c.ped) (e4 q -pedal -sost.ped))))
 (cmntest '(cmn staff treble c4 q sost.ped))
 (cmntest '(cmn (free-expansion-factor 3) (staff (dy 2) treble 
    (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (line-break (dy -2)) (g4 q -u.c.ped) (e4 q -pedal -sost.ped))))
 (cmntest '(cmn (free-expansion-factor 3) (staff (dy 2) treble 
    (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (g4 q -pedal) (a4 q pedal-) 
    (line-break (dy -2)) (g4 q -u.c.ped) (e4 q -pedal -sost.ped))))
 (cmntest '(cmn (free-expansion-factor 3) staff treble (c4 q pedal-) (d4 q -pedal-) line-break 
    (e4 q -pedal-) (d4 q -pedal-) (c4 q -pedal-) line-break  (e4 q) (d4 q -pedal-) (c4 q -pedal)))
 (cmntest '(cmn (size 60) staff (staff-lines 1) (start-line 2) percussion (meter 4 4) 
    (b4 w (cymbal (dy 1.1) (dx .2))) (b4 w (gong (dy 1.1) (dx .2))) 
    (b4 w (suspended-cymbal (dy 1.1) (dx .2))) (b4 w (hi-hat (dy 1.1) (dx .2))) 
    (b4 w (tambourine (dy 1.1) (dx .2))) (b4 w (maracas (dy 1.1) (dx .2))) 
    (b4 w (cow-bells (dy 1.1) (dx .2)))  (b4 w (triangle (dy 1.1) (dx .2))) ))

 (cmntest '(cmn (size 60) staff (staff-lines 1) (start-line 2) percussion (meter 4 4) 
    (b4 w (cymbal (dy 1.1) (dx .2)) wood-stick) (b4 w (gong (dy 1.1) (dx .2)) bass-drum)
    (b4 w (suspended-cymbal (dy 1.1) (dx .2)) metal-stick) (b4 w (hi-hat (dy 1.1) (dx .2)) triangle-stick) 
    (b4 w (tambourine (dy 1.1) (dx .2)) soft-stick) (b4 w (maracas (dy 1.1) (dx .2)) hard-stick) 
    (b4 w (cow-bells (dy 1.1) (dx .2)) wire-brush)  (b4 w (triangle (dy 1.1) (dx .2)) rubber-stick)))
(cmntest '(cmn staff treble c4 w (harp-setting :sharp :flat :natural :flat :sharp :natural :flat (dy -1.5))))
(cmntest '(cmn staff treble c4 q bar two-measure-rest bar three-measure-rest bar 
   four-measure-rest bar five-measure-rest bar six-measure-rest bar seven-measure-rest bar eight-measure-rest))
#-cmu19d  (cmntest '(cmn (transpose (staff treble g4 q gs4 q gf4 q gn4 q 
		       (g4 q double-sharp) (g4 q double-flat) (g4 q (natural-sharp)) (g4 q (natural-flat))) 
		:to-key df-major)))
#-cmu19d  (cmntest '(cmn (staff treble c4 q) (transpose (staff bass c3 q) :from-key bf-major)))
#-cmu19d  (cmntest '(cmn (transpose (score (staff treble c4 q) (system (staff treble c4 q) (staff bass c3 q))) :from-key bf-major)))
#-cmu19d  (cmntest '(cmn (transpose (staff treble df5 q ef5 q) :from-key df-major :octave -1)))
#-cmu19d  (cmntest '(cmn (transpose (staff treble df5 q ef5 q) :from-key df-major :to-key no-key :octave -1)))
#|
  (cmntest '(cmn (size 100) 
    (transpose 
      (staff treble 
        (c4 q (note-head :diamond) 
          (with-cmn (scale .75 .75) (dx -1.0) (dy 1.5) 
            (transpose (staff treble c4 q no-stem) :to-key d-major)))) 
      :to-key d-major)))
|#
#-cmu19d(cmntest '(cmn (transpose (staff treble bf3 q c4 q ef4 q f4 q) :from-key bf-major :to-key c-major)))

(cmntest '(pmn staff treble c4 q e4 h bf4 q bar))
(cmntest '(pmn staff treble (chord (notes c4 e4 g4) q) e4 h bf4 q bar))
(cmntest '(pmn (size 16) (free-expansion-factor 1.5) (staff treble c4 q c4 e c4 q c4 s c4 s c4 q bar) (staff bass c4 e c4 e c4 q c4 q c4 e bar)))
(cmntest '(cmn staff treble c4 q))
(cmntest '(cmn (size 24) staff treble c4 w double-bar))

(cmntest '(cmn (size 40)
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
		    (c4 e tenuto accent rfz) (d4 s mordent) (en4 s pp) (fs4 e fermata)))))
(cmntest '(cmn (size 24)
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
		    c4 e tenuto accent rfz d4 s mordent en4 s pp fs4 e fermata))



(cmntest '(cmn (size 24) (beam-spacing .4) (beam-width .275) staff treble (c4 s) (d4 s)))
(cmntest '(cmn (size 24) (curvy-flags nil) staff treble (a4 s)))
(cmntest '(cmn (size 24) treble (meter 6 8 (meter-size 2))))

(cmntest '(cmn (staff treble (c4 w (marcato (dx .5))))))
(cmntest '(cmn staff (treble mirrored (dx .5)) (treble (rotate 180) (dy .75) (scale -2 .5))))

(cmntest '(cmn (size 24) (header-margin .25) (footer-margin .25) staff 
  (treble mirrored (dx .5)) (treble (rotate 180) (dy .75) (scale -2 .5)) 
  (bass (gray-scale .5) (dx .75)) (bass (outlined .02) (dx .75)) 
  (quarter-rest (rotate 45) (dx .5) (scale 1 2)) (eighth-rest (rotate 90) (dy -.4) (scale 2 1))))

(cmntest '(cmn (size 24) (system bracket (staff) (staff brace) (staff))))
(cmntest '(cmn (staff (staff-name "E-sharp Bazooka") (staff-lines 2) (e4 w fff)) 
     (staff (staff-name "c-flat Clavicle") (staff-lines 0) treble d4 q c5 h.) 
     (staff (staff-name "Violin") (staff-size .5) sub-bass g2 h c2 h)))

(cmntest '(cmn (size 24) bar c4 q begin-and-end-repeat-bar c4 q interior-double-bar c4 q 
	dashed-bar c4 q begin-repeat-bar c4 q end-repeat-bar c4 q double-bar))
(cmntest '(cmn (size 24) staff french-violin c4 q treble c4 q soprano c4 q mezzo-soprano c4 q alto c4 q tenor c4 q baritone c4 q bass c4 q sub-bass c4 q))
(cmntest '(cmn staff treble c4 q cs4 q cf4 q cn4 q (c4 q (sign double-sharp)) (c4 q (sign double-flat))))
(cmntest '(cmn staff tenor b-major c4 q bar (cancel b-major) ef-major c4 q))
(cmntest '(cmn (size 24) (header-margin .125) (footer-margin .125) staff tenor b-major c4 q bar (cancel b-major) ef-major c4 q))
(cmntest '(cmn staff treble (meter 6 8) c4 h. bar (meter '5+7 8) c4 w bar cut-time c4 w (meter 2 2 in-parentheses)
     c4 w (meter 9 8) (meter '4+5 8 in-parentheses) c4 w))

 (cmntest '(cmn (size 24) staff treble 
     (g4 q (grace-note fs4)) 
     (g4 q (grace-note (slurred nil) (stem-direction :down) b4 (note-head :diamond) staccato tenuto wedge fermata))
            ;; omit the slur and use a diamond head on the grace note and get very confused about articulation
     (g4 q (grace-note e4 fs4)) 
     (g4 q (grace-note gf4 ef4)) 
     (g4 q (grace-note (slashed nil) b4 a4))
            ;; omit the slash 
     (b4 q (grace-note d4 e4 fs4 g4 a4)) 
     (g4 q (grace-note ef5 e bf4 e ef4 e))))
            ;; just one beam on the grace notes

(cmntest '(cmn (free-expansion-factor 2.5) staff treble whole-rest half-rest quarter-rest
    eighth-rest sixteenth-rest thirty-second-rest sixty-fourth-rest one-twenty-eighth-rest))

(cmntest '(cmn staff treble (c5 e (onset .5)) (g4 e. (onset 1.25))))

(cmntest '(cmn (size 24) staff treble c5 q mordent c5 q turn c5 q short-trill c5 q trilled-turn c5 q trill 
                  c5 q (mordent (ornament-sign small-flat)) 
                 (c5 q (trill (sign-position :up) (ornament-sign small-natural))) 
                 (cs5 q (trill (ornament-sign small-sharp) (sign-position :in-parentheses))) 
                 (c5 q (trill (wavy-line t))) (c5 q (trill (ornament-sign flat)))))
(cmntest '(cmn staff treble (chord (notes c4 e4 g4) h (arpeggio arrow-down))))

(cmntest '(cmn staff treble (e4 q (fingering 3)) (chord (notes c5 g4) q (fingering 7 2))))
(cmntest '(cmn staff treble (meter 2 4) (c4 q fermata) (quarter-rest g.p.) (c4 q breath-mark) (c4 q upside-down-fermata) double-bar))
(cmntest '(cmn staff treble (c4 q begin-slur) (d4 q) e4 q end-slur))
(cmntest '(cmn staff treble (c4 q (setf slur-tag (begin-slur))) (d4 (end-slur slur-tag))))

(cmntest '(cmn staff treble 
     (c4 h (setf lower-text (text- "hi")) 
	   (setf upper-text (text- "sil" (y0 2.0))))
     (d4 q (-text- "ver" upper-text (y0 2.0))) 
     (d4 q (-text- "ho" lower-text)) 
     (e4 q (-text "!" upper-text (y0 2.0))) 
     (e4 h (-text "away!" lower-text))))
(cmntest '(cmn staff treble (c4 h (onset 0) stem-down) (c5 e (onset 0) stem-up) (b4 e (onset .5) stem-up) (a4 q (onset 1.0) stem-up)))

(cmntest '(cmn staff treble (c4 q (note-head :diamond) (auxiliary-note c6 no-stem in-parentheses))))

(cmntest '(cmn (output-file "2.eps") staff treble c5 s))
(cmntest '(cmn (size 24) staff treble (c4 q (graphics (file "2.eps") (scale .6 .6) (dx 2.0) (dy 1.0) (rotate 90)))))
(cmntest '(cmn (size 24) staff treble (c4 q (text "middle-c" (dx 1.0) (dy 2.0) (rotate 180)  (font-size 12)))))

(cmntest '(cmn (size 24) staff treble (c4 h (text- "hi")) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!"))))
(cmntest '(cmn (text-connecting-pattern '(5 10)) (size 24) staff treble (c4 h (text- "hi")) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!"))))
(cmntest '(cmn staff treble (c4 h (glissando-to a4)) quarter-rest (c4 h begin-glissando) (g4 h end-glissando)))

(cmntest '(cmn (free-expansion-factor 2.5) staff treble (c4 h (begin-glissando (text "we're number 1"  (font-scaler .3)))) (c5 h end-glissando)))

(cmntest '(cmn (free-expansion-factor 2.0) staff treble (c4 h (crescendo (duration 2.0))) (c4 h (begin-crescendo (onset-offset 1.0))) (g4 h end-crescendo)))

(cmntest '(cmn staff treble (c4 q (note-head :diamond) (with-cmn (scale .5 .5) (dx -1.0) (dy 1.5) staff treble c6 q no-stem))))


(cmntest '(cmn staff treble (c4 q begin-tie) (c4 q end-tie) (c4 q (begin-tie (tie-direction :up) (tie-curvature .5))) (c4 q end-tie)))
(cmntest '(cmn staff treble (chord (notes c4 e4 g4 c5) q begin-tie) (chord (notes c4 e4 g4 c5) q end-tie)))
(cmntest '(cmn staff treble (chord (notes (c4 (setf tie-c4 (begin-tie))) e4 g4 c5) q) (chord (notes (c4 (end-tie tie-c4)) e4 g4 c5) q)))

(cmntest '(cmn staff treble (meter 2 4) f4 e. f4 q. f4 h.))
(cmntest '(cmn (size 24) staff treble (meter 2 4) f4 e. f4 q. f4 h.))

(cmntest '(cmn (size 24) staff treble (meter 2 4) (chord (notes f4 af4 df5) e.) (chord (notes f4 af4 df5) q.) (chord (notes f4 af4 df5) h.)))


(cmntest '(cmn (size 24) staff treble 
     g4 q begin-slur e4 q fs4 q end-slur
     g4 q begin-slur gf4 q ef4 q end-slur
     g4 q begin-slur ef4 q c4 q end-slur
     g4 q begin-slur b4 q a4 q end-slur
     b4 q begin-slur e5 q d5 q cs5 q end-slur
     b4 q begin-slur d4 q e4 q fs4 q g4 q a4 q end-slur
     g4 q begin-slur ef5 q bf4 q ef4 q end-slur))

 (cmntest '(cmn (size 24) staff treble 
     g4 q begin-slur ef4 q c4 q end-slur
     g4 q begin-slur b4 q a4 q end-slur
     b4 q begin-slur e5 q d5 q cs5 q end-slur
     a4 q begin-slur e4 q f4 q g4 q a4 q end-slur
     g4 q begin-slur ef5 q bf4 q ef4 q end-slur))


(cmntest '(cmn (size 40) (free-expansion-factor 2.0) staff treble 
  (chord (notes f5 ef5 df5 a4) q begin-tie) (chord (notes f5 e5 d5 a4) q end-tie) 
  (chord (notes f4 gf4 af4 d5) q begin-tie) (chord (notes f4 g4 a4 d5) q end-tie)))

(cmntest '(cmn (size 40) (free-expansion-factor 1.5) staff treble (meter 2 4) 
     (chord (notes f5 af4 df5) (rq 5/4)) 
     (chord (notes f5 ef5 df5 a4) (rq 3/2)) 
     (chord (notes f4 gf4 af4 d5) (rq 5/4))))
(cmntest '(cmn (size 24) (free-expansion-factor 1.5) staff treble (meter 2 4) 
     (chord (notes f5 af4 df5) (rq 5/4)) 
     (chord (notes f5 ef5 df5 a4) (rq 3/2)) 
     (chord (notes f4 gf4 af4 d5) (rq 5/4) stem-down)))

(cmntest '(cmn staff treble (c4 q tremolo) (c4 w paranoid-tremolo) (c4 e begin-tremolo) (d4 e end-tremolo) (c5 h begin-measured-tremolo) (d5 h end-measured-tremolo)))

(cmntest '(cmn staff treble (c4 q tremolo) (c4 w paranoid-tremolo) (c4 q (tremolo (tremolo-slashes 1))) (c5 h begin-measured-tremolo) (d5 h end-measured-tremolo) (c4 e begin-tremolo) (d4 e end-tremolo)))

(cmntest '(cmn (size 24) staff treble (c4 q tremolo) (c4 w paranoid-tremolo) (c5 q (tremolo (tremolo-slashes 1))) (c4 e begin-tremolo) (d4 e end-tremolo)))


(cmntest '(cmn (size 24) staff treble (meter 2 4) (c4 q) (c4 q) (bar begin-first-ending) (c4 q) (c4 q) (end-repeat-bar end-first-ending begin-second-ending) (c4 q) (c4 q) (bar end-second-ending) c4 q c4 q double-bar))
(cmntest '(cmn staff treble c4 te c4 te c4 te (c4 (rq 1/5)) (c4 (rq 1/5)) (c4 (rq 1/5)) (c4 (rq 1/5)) (c4 (rq 1/5)) (c5 (rq 2/3)) (c5 (rq 1/3))))

(cmntest '(cmn (size 24) staff treble c4 w (bar segno) (c4 w) (bar coda) (c4 w) (bar (change-beat q. e)) (c4 w)))
(cmntest '(cmn staff bass c5 e d5 e f5 e g5 e c5 q g4 q c4 q))

(cmntest '(cmn staff treble (meter 2 4) c4 q c4 q bar repeat-measure bar
  c4 q g4 q bar (repeat-measure 440) c4 q c4 q double-bar))


(defun display-smiley-face (mark note score &optional size)
  (let* ((y-off (+ (y0 mark) (dy mark) (staff-y0 note) 
		   (* (max 11 (+ 4 (head-line note))) *staff-line-separation*)))
	 (r (/ 10 40))
	 (x-off (+ (x0 note) -.05 (dx mark) (center note) (x0 mark))))
    (setf (line-width score) .025)
    (circle score x-off y-off r)
    (draw score)
    (circle score x-off y-off (* r .6) 190 350)
    (draw score)
    (circle score (- x-off (* .33 r)) (+ y-off (* .25 r)) (* r .1))
    (fill-in score)
    (circle score (+ x-off (* .33 r)) (+ y-off (* .25 r)) (* r .1))
    (fill-in score)
    (setf (line-width score) 0)))

(defvar smiley-face (make-instance 'write-protected-sundry :name :smiley-face :mark #'display-smiley-face))
(defun smiley-face (&rest objects) (apply #'mark #'display-smiley-face :smiley-face objects))
(cmntest '(cmn staff treble c4 q smiley-face))
(cmntest '(cmn (free-expansion-factor 2.0) staff treble (c4 h begin-measured-tremolo) (c5 h end-measured-tremolo)))
(cmntest '(cmn (size 24) staff treble c4 q begin-two-octaves-up d4 q e4 q f4 q end-two-octaves-up))
  
(cmntest '(cmn (size 24) staff treble c4 e d4 e (c4 e (begin-beam)) (d4 s) (e4 s (end-beam)) (c4 e (setf beam (beam-))) (d4 e (-beam- beam)) (e4 e (-beam beam))))

;;; these are various test cases

(cmntest '(cmn (staff (staff-size .5) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .6) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .7) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .8) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size .9) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size 1.0) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)
     (staff (staff-size 1.25) treble c4 e d4 e e4 e f4 e cs4 e ds4 e ef4 e ff4 e g4 e a4 e)))

(cmntest '(cmn (staff (staff-size .5) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .6) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .7) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .8) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size .9) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size 1.0) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)
     (staff (staff-size 1.25) treble c5 e d5 e e5 e f5 e cs5 e ds5 e ef5 e ff5 e g5 e a5 e)))


(cmntest '(cmn (size 24) 
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
	    (chord (notes cf3 ff3 gf3 cf4) q))))


(cmntest '(cmn (setf s1 (staff treble (meter 6 8) a4 e a4 e a4 e a4 q.)) (staff (tied-to s1) (d4 q. stem-down) (d4 q. stem-down))))

(cmntest '(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 7) (dy0 -.5) (dy .5)))) (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi))))

 (cmntest '(cmn (size 40) (header-margin 0) (footer-margin .25) staff treble 
  (chord (notes c4 e4 g4) h (arpeggio arrow-down)) 
  (chord (notes c4 e4 g4) h arrow-up) 
  (chord (notes d4 g4 e5) h no-arpeggio)))

(cmntest '(cmn staff treble a4 q. (grace-note f4 g4) g4 e f4 q g5 q. g5 e (grace-note a5) begin-slur f5 e g5 e end-slur))

(cmntest '(cmn (size 24) (free-expansion-factor 2.50) (header-margin .25) (footer-margin .5) staff treble 
  (c4 q (crescendo (duration 2.0))) (d4 q) (c4 q (begin-diminuendo (end-dynamic f) (width .75))) (d4 q) (e4 q end-diminuendo)
  (c4 q (begin-crescendo ppp f)) d4 q e4 q f4 q (g4 q (end-crescendo))))

(cmntest '(cmn (transform '(0 1 -1 0 200 200)) (staff (x1 7.5) treble c4 e d4 e)))


;;; to get this to fit into the WriteNow page, manually edit the 
;;; %%BoundingBox to be 1 10 187 280
(cmntest '(cmn (size 16) (header-margin .1) (footer-margin .65) 
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
  (system bracket (staff (x1 1.0)) (staff brace (x1 1.0)) (staff (x1 1.0)))))

(cmntest '(cmn (size 50) (system (staff (x1 3))) (system bracket (staff (x1 1.0)) (staff brace (x1 1.0)) (staff (x1 1.0)))))


;;; this is the Opus 1 example on page 9 -- once cmn has created it,
;;; you have to manually edit the BoundingBox to y1=275 for WriteNow's benefit.

(cmntest '(cmn (size 12) (footer-margin 2.5) (left-margin 1)
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

      bar treble a4 e begin-slur b4 e c5 e d5 e e5 q end-slur bar)))

(def-key foo-major bf4 ef5 fs5)

(cmntest '(cmn (size 24) (header-margin .1) (footer-margin .4) staff tenor b-major c4 q bar (cancel b-major) ef-major c4 q bar bass foo-major c4 q))

(cmntest '(cmn (size 24) (header-margin .1) (footer-margin .25) staff treble (c4 q (note-head :diamond) (auxiliary-note c6 no-stem in-parentheses))))

(cmntest '(cmn (size 24) (footer-margin .5) (header-margin 0) (left-margin 5) 
  staff 
    (text "(cmn staff treble" (font-name "Courier") (font-size 10) (x0 1.5) (y0 3)) 
    (text "(c4 q (note-head :diamond)" (font-name "Courier") (font-size 10) (x0 2) (y0 2.5)) 
    (text "(with-cmn (scale .75 .75) (dx -1.0) (dy 1.0)" (font-name "Courier") (font-size 10) (x0 2.5) (y0 2)) 
    (text "staff treble c6 q no-stem)))" (font-name "Courier") (font-size 10) (x0 3) (y0 1.5))
    treble (c4 q (note-head :diamond) (with-cmn (scale .75 .75) (dx -1.0) (dy 1.0) staff treble c6 q no-stem))))
;; edit boundingbox manually to 1 15 671 110 for WriteNow


;;; Schubert example is in beams.cmn


    (cmntest '(cmn (size 24) (footer-margin .25) (header-margin .25) staff treble g4 q gs4 q gf4 q gn4 q 
     (g4 q double-sharp) (g4 q double-flat) (g4 q (natural-sharp)) (g4 q (natural-flat))))

  (cmntest '(cmn (size 24) (free-expansion-factor 1.075) (header-margin .25) (footer-margin .25) 
    staff treble (meter 6 8) c4 h. bar (meter '5+7 8) c4 w
    bar cut-time c4 w bar (meter 2 2 in-parentheses (dx .1)) c4 h
    bar (meter 9 8) (meter '4+5 8 (dx .1) in-parentheses) c4 w bar 
   (meter 3 2 note-head-denominator) c4 h bar (meter 3 4 suppressed-denominator (meter-size 2)) c4 q.))



;;; percussion example
 (cmntest '(cmn (size 60) staff (staff-lines 1) (start-line 2) percussion (meter 4 4) 
    (b4 w (cymbal (dy 1.1) (dx .2)) wood-stick) (b4 w (gong (dy 1.1) (dx .2)) bass-drum)
    (b4 w (suspended-cymbal (dy 1.1) (dx .2)) metal-stick) (b4 w (hi-hat (dy 1.1) (dx .2)) triangle-stick) 
    (b4 w (tambourine (dy 1.1) (dx .2)) soft-stick) (b4 w (maracas (dy 1.1) (dx .2)) hard-stick) 
    (b4 w (cow-bells (dy 1.1) (dx .2)) wire-brush)  (b4 w (triangle (dy 1.1) (dx .2)) wood-stick)))

;;; pedal example
 (cmntest '(cmn (free-expansion-factor 3) (staff treble (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (g4 q -u.c.ped) (e4 q -pedal -sost.ped))))

;;; accent example

(cmntest '(cmn staff treble (c4 q (sul-tasto- (dy .5)) fingernail) 
  (d4 q (double-tongue (dy .125))) (e4 q doink martellato) (f4 q heavy-accent) 
  (g4 q rip (triple-tongue (dy .125)) (-sul-tasto)) (a4 q nail-pizzicato) 
  (b4 q (no-accent (dy -.25))) (c5 q Hauptstimme sprechstimme) (d5 q NebenStimme circled-stem)))

;;; cmn full example
 (cmntest '(cmn (size 24) (free-expansion-factor 1.5) (system-separation 0) (footer-margin .1) (header-margin 0) 
   (system (staff treble (c4 w (sul-tasto- (dy .5)) fingernail) 
     (d4 w (double-tongue (dy .125))) (e4 w doink martellato) (f4 w heavy-accent) 
     (g4 w rip (triple-tongue (dy .125)) (-sul-tasto)) (a4 w nail-pizzicato smear) 
     (b4 w no-accent) (c5 h Hauptstimme (sprechstimme)) (d5 h NebenStimme circled-stem))) 
   (system (staff (staff-lines 1) (start-line 2) percussion 
    (b4 w (cymbal (dy 1.1) (dx .2)) wood-stick) (b4 w (gong (dy 1.1) (dx .2)) (bass-drum (dy -.05)))
    (b4 w (suspended-cymbal (dy 1.1) (dx .2)) metal-stick) (b4 w (hi-hat (dy 1.1) (dx .2)) (triangle-stick (dy .1)))
    (b4 w (tambourine (dy 1.1) (dx .2)) soft-stick) (b4 w (maracas (dy 1.1) (dx .2)) hard-stick) 
    (b4 w (cow-bells (dy 1.1) (dx .2)) wire-brush)  (b4 w (triangle (dy 1.1) (dx .2)) wood-stick)))))


(cmntest '(cmn (size 40)
     (grace-note-size .75)

  (staff treble 

     (g4 q (grace-note a3))
     (g4 q (grace-note c4)) 
     (g4 q (grace-note ef4)) 
     (g4 q (grace-note fs4)) 
     (g4 q (grace-note gf4)) 
     (g4 q (grace-note b4)) 
     (g4 q (grace-note e5)) 
     (g4 q (grace-note g5))
     )
     
  (staff treble

     (g4 q (grace-note a3 stem-down))
     (g4 q (grace-note c4 stem-down)) 
     (g4 q (grace-note ef4 stem-down)) 
     (g4 q (grace-note fs4 stem-down)) 
     (g4 q (grace-note gf4 stem-down)) 
     (g4 q (grace-note b4 stem-down)) 
     (g4 q (grace-note e5 stem-down)) 
     (g4 q (grace-note g5 stem-down))
     )
     
  (staff treble 

     (g3 q (grace-note e3)) 
     (g3 q (grace-note f3))
     (g3 q (grace-note gf3))
     (g3 q (grace-note a3)) 
     (g3 q (grace-note ef4)) 
     (g3 q (grace-note fs4)) 
     (g3 q (grace-note gf4)) 
     (g3 q (grace-note b4)) 
     )

  (staff (dy -1.0) treble 

     (g5 q (grace-note ef4)) 
     (g5 q (grace-note gf4)) 
     (g5 q (grace-note b4)) 
     (g5 q (grace-note e5)) 
     (g5 q (grace-note fs5)) 
     (g5 q (grace-note a5))
     (g5 q (grace-note c6)) 
     (g5 q (grace-note e6))
     )

  (staff treble 

     (g5 q (grace-note ef4 stem-down)) 
     (g5 q (grace-note gf4 stem-down)) 
     (g5 q (grace-note b4 stem-down)) 
     (g5 q (grace-note e5 stem-down)) 
     (g5 q (grace-note fs5 stem-down)) 
     (g5 q (grace-note a5 stem-down))
     (g5 q (grace-note c6 stem-down)) 
     (g5 q (grace-note e6 stem-down))
     )
  ))

(cmntest '(cmn (size 32)

  (staff treble 

     (gs4 q (grace-note a3))
     (gs4 q (grace-note c4)) 
     (gs4 q (grace-note ef4)) 
     (gs4 q (grace-note fs4)) 
     (gs4 q (grace-note gf4)) 
     (gs4 q (grace-note b4)) 
     (gs4 q (grace-note e5)) 
     (gs4 q (grace-note g5))
     )
     
  (staff treble

     (gs4 q (grace-note a3 stem-down))
     (gs4 q (grace-note c4 stem-down)) 
     (gs4 q (grace-note ef4 stem-down)) 
     (gs4 q (grace-note fs4 stem-down)) 
     (gs4 q (grace-note gf4 stem-down)) 
     (gs4 q (grace-note b4 stem-down)) 
     (gs4 q (grace-note e5 stem-down)) 
     (gs4 q (grace-note g5 stem-down))
     )
     
  (staff treble 

     (gs3 q (grace-note e3)) 
     (gs3 q (grace-note f3))
     (gs3 q (grace-note gf3))
     (gs3 q (grace-note a3)) 
     (gs3 q (grace-note ef4)) 
     (gs3 q (grace-note fs4)) 
     (gs3 q (grace-note gf4)) 
     (gs3 q (grace-note b4)) 
     )

  (staff (dy -1.0) treble 

     (gs5 q (grace-note ef4)) 
     (gs5 q (grace-note gf4)) 
     (gs5 q (grace-note b4)) 
     (gs5 q (grace-note e5)) 
     (gs5 q (grace-note fs5)) 
     (gs5 q (grace-note a5))
     (gs5 q (grace-note c6)) 
     (gs5 q (grace-note e6))
     )

  (staff treble 

     (gs5 q (grace-note ef4 stem-down)) 
     (gs5 q (grace-note gf4 stem-down)) 
     (gs5 q (grace-note b4 stem-down)) 
     (gs5 q (grace-note e5 stem-down)) 
     (gs5 q (grace-note fs5 stem-down)) 
     (gs5 q (grace-note a5 stem-down))
     (gs5 q (grace-note c6 stem-down)) 
     (gs5 q (grace-note e6 stem-down))
     )
  ))

(setf ax-note-size .6)

(defun ax (&rest args)
  (apply #'auxiliary-note no-beam (note-head :x) (scale ax-note-size ax-note-size) args))

(defun axacc (&rest args)
  (apply #'auxiliary-note no-beam (accent (dy -3.))(note-head :x) (scale ax-note-size ax-note-size) args))

(defun axe0 (xsh &rest args)
  (let* ((xtotal xsh))
    (engorge
     (loop for arg in args
	   collect (axacc arg q  (dx (- xtotal)))))))

(defun axes (xsh &rest args)
  (let* ((xdif .7)
	 (xtotal (+ xsh (+ .25 (* xdif (length args))))))
    (engorge
     (loop for arg in args and i from xtotal downto 0 by xdif 
	   collect (ax arg e (dx (- i)))))))

(defun axes2 (xsh &rest args)
  (let* ((xdif .7)
	 (xtotal (+ xsh (+ .25 (* xdif (length args))))))
    (engorge
     (loop for arg in args and i from 0 to xtotal by xdif 
	   collect 
	   (ax arg e (dx (+ i)))
	   ))))
					; (cmntest '(cmn staff treble (e5 q (axes 0. d4 d4 d4 d4))))

(defun ltext (&rest args)
  (apply #'text 
	  (font-scaler .5) 
	 (y #'(lambda (mark note score &optional justifying) 
		(- (staff-y0 note) .9)))
	 (x #'(lambda (mark note score &optional justifying) 
		(+ (x0 note) (if (sign note) .2 0))))
	 args))

(cmntest '(cmn  
 (free-expansion-factor 1.0)
 (size 20)  
 (slur-thickness .05)  
 (slur-curvature .33)  
 (tie-curvature	.225)  
 (left-margin 	1.25)
 (right-margin	1.0)
 (automatic-bars nil)
 (automatic-ties nil)
 (staff-separation 2.0)
 (automatic-line-breaks nil)
 (staff bass 
     
    ;(meter 3 4 (meter-size 0)) ; meter still  needed  -- bil sez it's a bug --
					; and begob it is -- I've added an "automatic-line-breaks" flag to the score object.


(line-mark (dx '((1 -1.5))))
	(d4 e  (axes 2.6 f2 f2 f2 f2) 
	    (text "etc." (dx -1.6) (dy -1.8)  (font-scaler .45))
	    (ltext "yo")
	    (text "Drum" (dx -5.) (dy -1.8)   (font-scaler .45))
            (text "(Whoop)" (font-scaler .45)
		  (y #'(lambda (mark note score &optional justifying) 
			 (+ (staff-y0 note) 2.25))))	    (text "Solo" (dx 0.) (dy .4)   (font-scaler .45)))
	
	(d4 q.(ltext "we")) (d4 q. begin-slur(text- "he" (y #'(lambda (mark note score &optional justifying) 
								(- (staff-y0 note) .9)))))
	(e4 e)
	(d4 q end-slur)
	
	(b3 q (-text "yo"))
	(a3 e (ltext "he"))
	(a3 q (ltext "no")) 
	(a3 e begin-slur (ltext "ne"))
	
	(a3 h. end-slur (axes2 0.5 f2 f2 f2 f2 f2))
	(eighth-rest (scale ax-note-size ax-note-size) (dx -1.1) (dy -.5))

	(bar (measure-number 2))
(line-mark)
	
	(d4 e  (axes2 0. f2) 
	    (ltext "yo")
	    (text "Chorus" (dx 0.) (dy .4)   (font-scaler .45)))
	
	(d4 q. (axes2 0. f2) 
	    (text "etc." (dx .9) (dy -1.8)  (font-scaler .45))
            (ltext "we")) 
	(d4 q. (text- "he" (y #'(lambda (mark note score &optional justifying) 
				  (- (staff-y0 note) .9)))) begin-slur)
	(e4 e)
	(d4 q end-slur)
	(b3 q(-text "yo"))
	(a3 e(ltext "he"))
	
	(a3 q(ltext "no")) 
	(a3 e begin-slur(ltext "ne")) (a3 h end-slur )
	(bar (measure-number 3))
(line-mark)
	
 	(b3 q (ltext "yo")) 
	(b3 e begin-slur (text- "we" (y #'(lambda (mark note score &optional justifying) 
					    (- (staff-y0 note) .9))))) 
	
	(a3 q end-slur ) (a3 h begin-slur (-text- "he")) 
	(b3 e end-slur )(a3 h(-text "yo"))
	(g3 e(ltext "he")) 
	(g3 q (ltext "no")) 
	(g3 e begin-slur(ltext "ne" )) 
	(g3 h end-slur )
	(bar (measure-number 4))
(line-mark)
	
	(g3 q (ltext "yo")) (g3 e (text- "we" (y #'(lambda (mark note score &optional justifying) 
						     (- (staff-y0 note) .9))))  begin-slur) 
	(e3 q end-slur ) 
	(g3 h.(-text- "he")) 
	(a3 e begin-slur(ltext "yo"))(a3 h end-slur )
	(g3 e(ltext "he")) 
	(g3 q (ltext "no")) 
	(g3 e begin-slur(ltext "ne")) 
	(g3 h. end-slur (axes2 0.5 f2 f2 f2 f2 f2)) 
	(eighth-rest (scale ax-note-size ax-note-size) (dx -1.1) (dy -.5))
	
	(bar (measure-number 5))
(line-mark)
	
	(a3 q.(ltext "we")) (b3 e(ltext "he")) 
	( a3 q (ltext "?e")) (a3 h(ltext "ya")) 
	(a3 h. (ltext "ha")(axes2 0.5 f2 f2 f2 f2 f2))
	(eighth-rest (scale ax-note-size ax-note-size) (dx -1.1) (dy -.5))
	
	(bar (measure-number 6))
	
	(a3 e(ltext "ka")) (b3 q.(ltext "yo")) (b3 e(ltext "we")) (a3 q.(ltext "ho")) 
	(a3 e(ltext "he")) (a3 q (ltext "no")) (a3 q.(ltext "ne"))
	(bar (measure-number 7))
(line-mark)
	
	(b3 q (ltext "yo"))  
	(b3 e (text- "we" (y #'(lambda (mark note score &optional justifying) 
				 (- (staff-y0 note) .9)))) begin-slur)
	(a3 q end-slur )(a3 e(-text- "he") begin-slur)
	(a3 h. end-slur )
	(a3 h(ltext "yo")) (g3 e(ltext "he")) (g3 h(ltext "no")) (g3 e(ltext "ne") begin-slur)
	(g3 h end-slur )
	(bar (measure-number 8))
(line-mark)
	
	(g3 q (ltext "yo")) (g3 e (text- "we" (y #'(lambda (mark note score &optional justifying) 
						    (- (staff-y0 note) .9))))begin-slur)
	(e3 q end-slur )(g3 e(-text- "he") begin-slur)(g3 h. end-slur )
	(a3 h(ltext "yo")) (g3 e(ltext "he")) (g3 h(ltext "no")) (g3 e (ltext "ne")begin-slur)
	(g3 h. end-slur (axes2 0.5 f2 f2 f2 f2 f2))
	(eighth-rest (scale ax-note-size ax-note-size) (dx -1.1) (dy -.5))
	
	
	(bar (measure-number 9))
	
 	(b3 q accent (ltext "wih")(axe0 0.0 f2 )) 
 	(b3 h. accent (glissando-to f3)(ltext "ya?") 
  	    (axe0 0.0 f2 ))
	(bar (measure-number 10))
(line-mark (dx '((1 -2.5))))
	
	(b3 e begin-slur (axes 2.6 f2 f2 f2 f2)
	    (text "etc." (dx -1.6) (dy -1.8)  (font-scaler .45))
	    (text- "we" (y #'(lambda (mark note score &optional justifying) 
			       (- (staff-y0 note) .9))))
	    (text "(Whoop)" (font-scaler .45)
		  (y #'(lambda (mark note score &optional justifying) 
			 (- (staff-y0 note) 1.5))))
	    (text "Solo" (dx 0.) (dy .6)   (font-scaler .45)))
	
	(a3 e end-slur ) 
	(a3 q (-text "ya"))
	
	(a3 e begin-slur 
	    (text- "wi" (y #'(lambda (mark note score &optional justifying) 
			       (- (staff-y0 note) .9))))
	    )
	
	(g3 e end-slur ) 
	(a3 w (-text "ye"))
	
	(a3 h (ltext "he") (axes2 0.5 f2 f2 f2))
	(eighth-rest (scale ax-note-size ax-note-size) (dx -1.1) (dy -.5))
	
	
	bar
	

	)	
 ))



(defun t32 (&rest args)
  (engorge 
   (loop for arg in args collect 
     (if (write-protected arg) 
	 (note arg (rq 1/8))
       (progn
	 (copy (rq 1/8) arg)
	 arg)))))

(defun t32up (&rest args)
  (engorge 
   (loop for arg in args collect 
     (if (write-protected arg) 
	 (note arg stem-up (rq 1/8))
       (progn
	 (setf (stem-direction arg) :up)
	 (copy (rq 1/8) arg)
	 arg)))))

(defun t64 (&rest args)
  (engorge 
   (loop for arg in args collect 
     (if (write-protected arg) 
	 (note arg (rq 1/16))
       (progn
	 (copy (rq 1/16) arg)
	 arg)))))

(defun t32s (&rest args)
  (engorge 
   (loop for arg in args collect 
     (if (write-protected arg) 
	 (note arg staccato (rq 1/8))
       (progn
	 (copy (rq 1/8) arg)
	 (add-to-marks arg (list (copy staccato)))
	 arg)))))

(defun t32sup (&rest args)
  (engorge 
   (loop for arg in args collect 
     (if (write-protected arg) 
	 (note arg staccato stem-up (rq 1/8))
       (progn
	 (setf (stem-direction arg) :up)
	 (copy (rq 1/8) arg)
	 (add-to-marks arg (list (copy staccato)))
	 arg)))))

;;; edit boundingbox to be 1 200 ...

(cmntest '(cmn (size 12) (old-style-beams t) (note-head-size 1.25)
     (header-margin 0) (footer-margin 0) 
     (left-margin 1) (right-margin 2)
     (staff-separation 2.0) (automatic-line-breaks nil)
     (system-separation 1.5)
 (setf vln (staff (staff-size 0.8) 
		  bar treble (key ef-major) (meter 6 8 (meter-size 0))
		  sixteenth-rest (chord  (notes c4 f4) e (diminuendo (duration .75)))
		  (chord (notes c4 f4) s staccato) (chord (notes c4 f4) s) (chord (notes d4 f4) s)
		  (en4 staccato s no-beam)
		  (chord (notes g3 g4) e) (chord (notes g4 g3) s)
		  (chord (notes f4 g3) s crescendo) (chord (notes e4 g3) s)
		  bar
		  (chord (notes an4 g3) e. accent) 
		  (t32 (g4 begin-slur) (a4 end-slur) (a4 begin-slur) (g4 end-slur) (g4 begin-slur) (f4 end-slur))
		  (chord (notes g3 en4) s no-beam) 
		  (chord (notes c4 e4) s staccato begin-slur begin-beam) (chord (notes c4 e4) staccato s end-slur) 
		  (chord (notes c4 e4) s begin-slur (crescendo (dy -.5) (duration .75))) (chord (notes bn3 f4) s)
		  (chord (notes bf3 g4) s end-slur end-beam)
		  bar
))
 (staff (staff-size .8) bar treble (key ef-major) (meter 6 8 (meter-size 0))
	sixteenth-rest (an4 staccato e (diminuendo (dy -.125) (duration .75)))
	(a4 s staccato) (a4 s begin-slur) (bn4 s end-slur)
	(c5 s no-beam) (en5 e) (e5 s) (d5 s crescendo) (c5 s)
	bar
	(f5 e. accent) (t32 (en5 begin-slur) (f5 end-slur) (f5 begin-slur) (e5 end-slur) (e5 begin-slur) (d5 end-slur))
	(c5 s no-beam) (c5 s staccato begin-slur) (c5 s staccato end-slur)
	(c5 s begin-slur (crescendo (duration .75))) (d5 s) (e5 s end-slur)
	bar
)
  (system
    (setf pno1 (staff 
		(text "from"  (font-size 10) (x0 -4) (y0 2.2))
		(text "franz.cmn"  (font-size 10) (x0 -4.5) (y0 1.3))
		treble (key ef-major) (meter 6 8 (meter-size 0))
		   (c6 e.)
		   (t32up (c6 (begin-slur (dy .1))) (bn5 end-slur) 
			  (b5 (begin-slur (dy .1))) (c6 end-slur) 
			  (b5 begin-slur) (an5 end-slur))
		   (gn5 e stem-up begin-slur) 
		   (g4 end-slur (begin-octave-up (dy 1.3)) (rq 1/8))
		   (t32sup a4 bn4 c5 d5 en5 f5 g5)
		   bar
		   (bn4 q begin-tie (trill (wavy-line t)))
		   (t64 (b4 end-tie begin-slur) c5 d5 en5 f5 g5 an5 bn5)
		   (c6 end-slur e) eighth-rest (c4 e end-octave-up)
		   bar
		   ))

 (staff (tied-to pno1) treble (key ef-major) (meter 6 8 (meter-size 0))
	(sixteenth-rest (scale 0 0))
	(an4 e stem-down (accent (dy -3)))
	(t32 (a4 begin-slur stem-down) (gs4 end-slur stem-down)
	     (g4 begin-slur stem-down) (a4 end-slur stem-down)
	     (gn4 begin-slur stem-down) (f4 end-slur stem-down))
	(en4 e (staccato (dy .6)) (setf inner-beam (beam-))))

 (staff (dy -.5) brace bar bass (key ef-major) (meter 6 8 (meter-size 0))
	(chord (notes c2 c3) s) (sixteenth-rest (scale 0 0)) (quarter-rest (scale 0 0)) 
	(eighth-rest (scale 0 0)) (c3 e (-beam inner-beam))
	eighth-rest (treble unjustified (dx -.875) (scale .8 .8) (dy .1))
	bar
	(g4 s staccato) (f5 s staccato) (d5 s staccato) (bn4 s staccato) (g4 s staccato) (g5 s staccato) 
	(en5 e staccato) (bass (scale .8 .8) (dy .125)) (c3 e staccato) eighth-rest
	bar
)
)))
(cmntest '(cmn (size 14) (automatic-measure-numbers :by-line) (always-show-staff-names nil) (first-measure-number 0) 
     (staff-name-font "Times-Italic") (staff-name-font-scaler .6) (automatic-line-breaks nil)
     bracket
     (staff (staff-name "clarinet in A" (dx -.5)) treble (meter 3 4) 
	    c5 e begin-slur e5 e bar g5 e e5 e c6 q end-slur g5 e begin-slur e5 e bar
	    d5 e f5 e a5 q end-slur f5 e begin-slur d5 e bar
	    c5 e begin-beam b4 e e5 e d5 e g5 e f5 e end-slur end-beam bar
	    ds5 q begin-slur e5 q end-slur c5 e begin-slur e5 e bar
	    g5 e e5 e c6 q end-slur g5 e begin-slur e5 e bar
	    dn5 e f5 e a5 q end-slur quarter-rest bar whole-rest bar line-break
	    quarter-rest quarter-rest d4 te begin-slur a3 te f3 te bar
	    a3 e end-slur begin-beam d4 e staccato f4 e staccato a4 e staccato d5 e staccato f5 e staccato end-beam bar
	    a5 e begin-slur begin-beam g5 e f5 e e5 e f5 e d5 e end-slur end-beam bar
	    c5 h begin-slur e5 e d5 e end-slur bar
	    c5 q quarter-rest (begin-and-end-repeat-bar (fences '(.2 .3))) quarter-rest bar
	    whole-rest bar whole-rest bar whole-rest bar quarter-rest quarter-rest g5 q (ring- (ring-length 1.5) unjustified) bar)
     (staff (staff-name "violino I" (dx -.5)) treble a-major (meter 3 4)
	    quarter-rest bar quarter-rest a4 q p a4 q bar 
	    quarter-rest a4 q a4 q bar quarter-rest g4 q g4 q bar 
	    quarter-rest a4 q a4 q bar quarter-rest a4 q a4 q bar 
	    f4 q quarter-rest c5 e begin-slur as4 e bar
	    b4 e d5 e f5 q end-slur c5 e begin-slur as4 e bar
	    b4 e d5 e f5 q end-slur quarter-rest bar whole-rest bar whole-rest bar
	    c4 e begin-slur begin-beam e4 e c4 e e4 e d4 e e4 e end-slur end-beam bar
	    c4 q quarter-rest begin-and-end-repeat-bar e4 e begin-slur g4 e bar
	    b4 e g4 e e5 q end-slur e4 e begin-slur a4 e bar
	    c5 e a4 e e5 q end-slur e4 e begin-slur b4 e bar
	    d5 e begin-beam b4 e e5 e d5 e c5 e a4 e end-slur end-beam bar
	    g4 e begin-slur b4 e e5 q end-slur e4 e begin-slur g4 e end-slur bar)
     (staff (staff-name "violino II" (dx -.5)) treble a-major (meter 3 4)
	    quarter-rest bar quarter-rest e4 q p e4 q bar 
	    quarter-rest f4 q f4 q bar quarter-rest d4 q d4 q bar 
	    quarter-rest c4 q c4 q bar quarter-rest e4 q e4 q bar 
	    d4 q quarter-rest gn4 begin-slur bar
	    f4 h gn4 q bar
	    f4 h end-slur quarter-rest bar whole-rest bar whole-rest bar
	    a3 h begin-slur gs3 q end-slur bar
	    a3 q quarter-rest begin-and-end-repeat-bar quarter-rest bar
	    (chord (notes b3 g4) q (text "pizz." (font-scaler .5) (dy 1.5)) p) (chord (notes b3 g4) q) quarter-rest bar
	    (chord (notes a3 a4) q) (chord (notes a3 a4) q) quarter-rest bar
	    (chord (notes g4 b4) q) (chord (notes g4 b4) q) (chord (notes a4 c5) q stem-up) bar
	    (chord (notes g4 b4) q) (chord (notes g4 b4) q) quarter-rest bar)
     (staff (staff-name "viola" (dx -.5)) alto a-major (meter 3 4)
	    quarter-rest bar quarter-rest c4 q p c4 q bar 
	    quarter-rest b3 q b3 q bar quarter-rest b3 q b3 q bar 
	    quarter-rest a3 q a3 q bar quarter-rest c4 q c4 q bar 
	    b3 q quarter-rest e4 begin-slur bar
	    d4 h e4 q bar
	    d4 h end-slur quarter-rest bar whole-rest bar whole-rest bar
	    f3 h. begin-tie bar
	    f3 q end-tie quarter-rest begin-and-end-repeat-bar quarter-rest bar
	    (chord (notes d4 e4) q (text "pizz." (font-scaler .5) (dy .5)) (p (dy .25))) (chord (notes d4 e4) q) quarter-rest bar
	    (chord (notes c4 e4) q) (chord (notes c4 e4) q) quarter-rest bar
	    e4 q e4 q e4 q bar e4 q e4 q quarter-rest bar)
     (staff (staff-name "violoncello" (dx -.5)) bar bass a-major (meter 3 4)
	    quarter-rest bar a3 q p quarter-rest quarter-rest bar 
	    d3 q quarter-rest quarter-rest bar 
	    e3 q quarter-rest quarter-rest bar 
	    f3 q quarter-rest quarter-rest bar 
	    c3 q quarter-rest quarter-rest bar 
	    d3 q quarter-rest quarter-rest bar whole-rest bar whole-rest bar whole-rest bar whole-rest bar
	    e2 q begin-slur staccato e2 q staccato e2 q staccato end-slur bar 
	    a2 q quarter-rest begin-and-end-repeat-bar quarter-rest bar
	    e3 q p (text "pizz." (font-scaler .5) (dy .5)) e3 q quarter-rest bar
	    e3 q e3 q quarter-rest bar
	    e3 q e3 q e3 q bar e3 q e2 q quarter-rest bar)))


(cmntest '(cmn (staff treble (c4 q) (bartok-bounce q (notes cs4 d4 ef4 f4 e4 d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e)))
(cmntest '(cmn (staff treble (c4 q) (bartok-bounce q (notes (chord c4 g4) d4 eighth-rest f4 e4 d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e)))
(cmntest '(cmn (staff treble (c4 q) 
       (bartok-bounce q (notes (chord cs4 g4) d4 eighth-rest f4 (chord e4 fs4) d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e)))

 (cmntest '(cmn (staff treble (meter 2 4) 
        (c4 q) 
        (bartok-bounce q (notes (c4 staccato) (d4 marcato) 
                                (e4 upside-down-fermata) (f4 (bartok-pizzicato (dy .5))) 
                                (e4 (fingering 7 9)) (d4 ppp) c4) 
          (spacing .5) (start-beams 1) (end-beams 3)) 
        (c4 q)) 
      (staff bass (meter 2 4) c3 e c3 e c3 e c3 e c3 e c3 e))
	  )
(cmntest '(cmn (staff treble (c4 q) 
       (bartok-bounce q (notes (chord cs4 ds4 g4) (chord d4 a4) eighth-rest fs4 (chord e4 fs4) (chord d4 e4) c4) 
		      (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e)))

(cmntest '(cmn (staff treble (c4 q) (bartok-bounce q (notes cs4 (d4 begin-crescendo) ef4 f4 (e4 end-crescendo) d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q))))

(cmntest '(cmn (setf s1 (staff treble c4 e c4 e c4 w line-break d4 w d4 w)) (staff (tied-to s1) (treble (scale 0 0)) d5 q d5 w d5 w d5 w)))

(cmntest '(cmn (automatic-line-breaks nil)
     (size 30)
     (regularize 1)
     (staff bar treble fs-minor common-time 
	    (cycle (list c4 c5 g4 b4 a4 f4 d4 d5 c4 c5 g4 b4 a4 g4 f4 f5 f4 f5 as4 c5 b4 g4 es4 es5 an4 a5 bs4 d5 c5 bn4 g4 g5)
		   stem-up 
		   (rq 1/8)
		   (list (note-head-size 1.25) (note-head-size .75) (note-head-size .75) (note-head-size .75) 
			 (note-head-size .75) (note-head-size .75) (note-head-size 1.25) (note-head-size .75))))))

(cmntest '(cmn (size 30) 
     (staff treble c4 q (d4 q (onset 0)) cs4 q (d4 q (onset 1)) c4 q (df4 q (onset 2)) cs4 q (ds4 q (onset 3))
	    (c4 w double-sharp (onset 4)) (d4 w double-flat (onset 4)) (c4 w (note-head-size 1.3) (onset 8)) (df4 w (note-head-size 1.3) (onset 8)))
     (staff bass c4 q (d4 q (onset 0)) cs4 q (d4 q (onset 1)) c4 q (df4 q (onset 2)) cs4 q (ds4 q (onset 3))
	    ds4 w (cs4 w (onset 4)) (cs4 w (note-head-size .75) (onset 8)) (df4 w (note-head-size .75) (onset 8)))
     (staff treble d4 h (c4 h (onset 0)) ds4 h (c4 h (onset 2)) 
		   d4 h (cf4 h (onset 4)) ds4 h (cs4 h (onset 6)) (c4 w (onset 8)) (d4 w (onset 8)))
     (staff bass c4 q stem-down (d4 q (onset 0)) cs4 q stem-down (d4 q (onset 1)) c4 q stem-down (df4 q (onset 2)) cs4 q stem-down (ds4 q (onset 3))
	    c4 w (c4 w (onset 4)) ds4 w (cf4 w (onset 8)))
     (staff treble c4 q (c4 q (onset 0)) (c4 h (onset 1)) (c4 h (onset 1)) (c4 w (onset 2)) (c4 w (onset 2)) 
	    (g4 q (onset 6) (note-head-size 1.5)) (g4 q (onset 6) (note-head-size 1.5)) 
	    (g4 h. (onset 7) (note-head-size .75)) (g4 h. (onset 7) (note-head-size .75)) 
	    (g4 q. (onset 10)) (a4 q. (onset 10)))
     (staff treble c4 e stem-down (d4 e (onset 0) stem-up) cs4 e stem-down (d4 e (onset .5) stem-up) 
	    c4 e stem-down (df4 e (onset 1) stem-up) cs4 e stem-down (ds4 e (onset 1.5) stem-up))))

(cmn (layout :new-style) (staff (measure-rest)))

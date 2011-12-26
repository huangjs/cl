;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; continuation of cmn1.lisp

(in-package :cmn)

;;;
;;; ----------------    notes (leger lines, dots)
;;;

(defclass rhythm-mixin ()
  ((dots :initarg :dots :initform 0 :reader dots :writer set-dots)
   (dots-locked :initform nil :reader dots-locked)
   (flags :initarg :flags :initform 0 :reader flags :writer set-flags)
   (flags-locked :initform nil :reader flags-locked)
   (quarters :initarg :quarters :initform nil :reader quarters)
   (ratified :initarg :ratified :initform nil :reader ratified)))

(defclass rhythm (rhythm-mixin)
  ((dots :reader dots :writer set-dots)
   (dots-locked :initform nil :accessor dots-locked)
   (flags :reader flags :writer set-flags)
   (flags-locked :initform nil :accessor flags-locked)
   (quarters :accessor quarters)
   (ratified :accessor ratified)))

(defclass write-protected-rhythm (write-protect rhythm-mixin) ())

(defmethod (setf dots) (val (obj rhythm))
  (if (not (dots-locked obj))
      (set-dots val obj))
  val)

(defmethod (setf flags) (val (obj rhythm))
  (if (not (flags-locked obj))
      (set-flags val obj))
  val)

#|
(cmn treble c4 q. (dots 0) (flags 5))
(cmn staff treble (meter 3 8)
     (c4 (rhythm 3/16) (dots 0) (flags 3) (setf sub (beat-subdivision- (subdivision "4:3"))))
     (c4 (rhythm 9/16) (dots 1) (flags 2))
     (c4 (rhythm 3/4) (dots 4) (flags 0) (-beat-subdivision sub)))
|#

(defmethod rhythm-p ((obj t)) nil)
(defmethod rhythm-p ((obj rhythm-mixin))
  (and (not (note-p obj))
       (not (chord-p obj))
       (not (rest-p obj))))

(defmethod rcopy (rhy obj)
  (setf (dots obj) (dots rhy))
  (setf (quarters obj) (fratify (quarters rhy)))
  (setf (ratified obj) (ratified rhy))
  (setf (flags obj) (flags rhy)))

(defmethod copy ((rhythm rhythm-mixin) &optional object)
  (let ((new-rhythm (if (not object) (make-instance 'rhythm)
		      (if (write-protected object) (copy object)
			object))))
    (setf (dots new-rhythm) (dots rhythm))
    (if (quarters rhythm)
	(setf (quarters new-rhythm) 
	  (fratify (quarters rhythm))))
    (setf (ratified new-rhythm) (ratified rhythm))
    (setf (flags new-rhythm) (flags rhythm))
    (if (next-method-p) (call-next-method rhythm new-rhythm))
    new-rhythm))

(defmethod descry ((rhythm rhythm-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A"
	  (if (not controller) "(rhythm" "")
	  (if (and (quarters rhythm) (not (zerop (quarters rhythm)))) (format nil " :quarters ~A" (quarters rhythm)) "")
	  (if (and (dots rhythm) (not (zerop (dots rhythm)))) (format nil " :dots ~A" (dots rhythm)) "")
	  (if (and (flags rhythm) (not (zerop (flags rhythm)))) (format nil " :flags ~A" (flags rhythm)) "")
	  (if (ratified rhythm) " :ratified t" "")
	  (if (next-method-p) (call-next-method rhythm stream (or controller rhythm)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((rhythm rhythm-mixin))
  (format nil "(rhythm ~A)" (rhythm-name (quarters rhythm))))

(defmethod notify ((rhy rhythm-mixin) &optional objects) 
  (declare (ignore objects))
  (cmn-error "~A seems to be unattached to anything" (identify rhy)))
;;; rhythm-mixin needed for notes, chords, rests -- never independent

(defmacro define-rhythm (name dots flags quarters)
  `(defvar ,name (make-instance 'write-protected-rhythm :dots ,dots :flags ,flags :quarters ,quarters)))

(define-rhythm double-whole      0 0 8)
(define-rhythm whole             0 0 4)
(define-rhythm half              0 0 2)
(define-rhythm quarter           0 0 1)
(define-rhythm eighth            0 1 1/2)
(define-rhythm sixteenth         0 2 1/4)
(define-rhythm thirty-second     0 3 1/8)
(define-rhythm sixty-fourth      0 4 1/16)
(define-rhythm one-twenty-eighth 0 5 1/32)

(define-rhythm 64th  0 4 1/16)		;"s"=16th
(define-rhythm 64th. 1 3 3/32)
(define-rhythm 32nd  0 3 1/8)		;can't use "t"
(define-rhythm 32nd. 1 3 3/16)
(define-rhythm 16th  0 2 1/4)
(define-rhythm 16th. 1 2 3/8)
(define-rhythm 8th   0 1 1/2)
(define-rhythm 8th.  1 1 3/4)

(define-rhythm s     0 2 1/4)
(define-rhythm s.    1 2 3/8)
(define-rhythm s..   2 2 7/16)
(define-rhythm e     0 1 1/2)
(define-rhythm e.    1 1 3/4)
(define-rhythm e..   2 1 7/8)
(define-rhythm e...  3 1 15/16)
(define-rhythm q     0 0 1)
(define-rhythm q.    1 0 3/2)
(define-rhythm q..   2 0 7/4)
(define-rhythm q...  3 0 15/8)
(define-rhythm h     0 0 2)
(define-rhythm h.    1 0 3)
(define-rhythm h..   2 0 7/2)
(define-rhythm h...  3 0 15/4)
(define-rhythm w     0 0 4)
(define-rhythm w.    1 0 6)
(define-rhythm dw    0 0 8)

(define-rhythm ts     0 2 1/6)
(define-rhythm te     0 1 1/3)
(define-rhythm tq     0 0 2/3)
(define-rhythm th     0 0 4/3)

(define-rhythm ts.    1 2 3/12)		;sadly this distinction (1/4 not the same as 3/12) is lost in ACL which reduces the fraction
(define-rhythm te.    1 1 3/6)
(define-rhythm tq.    1 0 3/3)

(defun rhythm-name (qrhy)
  (let ((quarters qrhy))
    (if (= quarters 1) "q"
      (if (= quarters 3/2) "q."
	(if (= quarters 7/4) "q.."
	  (if (= quarters 2) "h"
	    (if (= quarters 3) "h."
	      (if (= quarters 7/2) "h.."
		(if (= quarters 4) "w"
		  (if (= quarters 6) "w."
		    (if (= quarters 8) "dw"
		      (if (= quarters 1/2) "e"
			(if (= quarters 3/4) "e."
			  (if (= quarters 7/8) "e.."
			    (if (= quarters 1/4) "s"
			      (if (= quarters 3/8) "s."
				(if (= quarters 7/16) "s.."
				  (if (= quarters 1/6) "ts"
				    (if (= quarters 1/3) "te"
				      (if (= quarters 2/3) "tq"
					(if (= quarters 4/3) "th"
					  (if (= quarters 15/16) "e..."
					    (if (= quarters 15/8) "q..."
					      (if (= quarters 15/4) "h..."
						(if (not quarters) "none"
						  (format nil "(rq ~A)" (fratify quarters)))))))))))))))))))))))))))

;;; how many flags we use is partly dependent on whether we choose to use a dot 
;;; (that is, it depends on the overall phrase, whether 2's are mixed with 3's)
;;; so the base rhythm can either be taken in terms of the quarter (1/2 1/4 etc) or
;;; in terms of these dotted any number of times (sigh).  So, a 6/11 note would
;;; have no flags (being thought of as part of a set of 11 in the time of 3 where
;;; (per Read anyway) each of the 3/11ths has one flag.  But the much longer
;;; 3/4 has one flag (being thought of as a dotted eighth).  So we need to decide
;;; when we intend to be using dots...  Other rhythms require a "half" dot -- 
;;; 5/7 = quarter tied to 16th in 7:4.  I think the numerator can give us the
;;; dot decision (3=1, 7=2, 15=3, 31=4; in between numbers require various ties)
;;; but once again we have to decide when we're going to use a tie -- the only
;;; cases that seem reasonable here involve 5 (4+1) and 9 (8+1) -- this is getting
;;; silly...
;;;
;;; According to Read, 3/11 should be "eights" whereas in our case, we're
;;; using dotted "sixteenths". This makes more sense if a 2/11 is in the mix somewhere.
;;;
;;; the next problem is that there's no real support in cmn for the tied case
#|
(cmn treble c4 (rq 5/8) c4 (rq 1/8) c4 (rq 2/8) c4 (rq 5/8) c4 (rq 3/8) c4 (rq 1/8) c4 (rq 6/8) c4 (rq 1/8) c4 (rq 7/8) c4 (rq 1/8))
(cmn treble c4 (rq 2/5) c4 (rq 3/5) c4 (rq 1/5) c4 (rq 1/5) c4 (rq 2/5) c4 (rq 1/5) c4 (rq 4/5) c4 (rq 1/5))
(cmn treble c4 (rq 1/9) c4 (rq 7/9) c4 (rq 1/9) c4 (rq 2/9) c4 (rq 5/9) c4 (rq 2/9)) ;looks weird, but is consistent
(cmn treble c4 (rq 2/11) c4 (rq 3/11) c4 (rq 4/11) c4 (rq 2/11) c4 (rq 6/11) c4 (rq 1/11) c4 (rq 4/11)) ;ditto
(cmn treble c4 (rq 3/13) c4 (rq 3/13) c4 (rq 3/13) c4 (rq 4/13) c4 (rq 5/13) c4 (rq 4/13) c4 (rq 4/13))
(cmn treble c4 (rq 2/7) c4 (rq 3/7) c4 (rq 2/7) c4 (rq 4/7) c4 (rq 3/7) c4 (rq 1/7) c4 (rq 1/7) c4 (rq 5/7) c4 (rq 6/7) c4 (rq 1/7))

;;; broken: (loses dot)
(cmn treble c4 (rq 1/6) c4 (rq 2/6) c4 (rq 3/6))

;;; broken: loses dot and no "9" over beam (it's confused by 3/9 reduced to 1/3 by lisp)
(cmn treble c4 (rq 2/9) c4 (rq 3/9) c4 (rq 4/9))

|#

(defun factorize (n) 
  ;; for special cases involving nested subdivisions we need to find the actual factors of the denominator(!)
  (let ((beams 0)
	(div 2))
    (loop while (and (> n 1) (< div 100)) do
      (multiple-value-bind (int frac)
	  (floor n div)
	(if (zerop frac)
	    (progn
	      (incf beams (floor (log div 2)))
	      (setf n int))
	  (incf div (if (= div 2) 1 2)))))
    beams))

(defun how-many-dots (rat)
  (let ((num (if (ratiop rat) (numerator rat) (first (ratify rat)))))
    (case num
      ((3 6 12 24) 1)
      ((7 14 28) 2)
      ((15 30) 3)
      (31 4)
      (t 0))))

(defun how-many-flags (rat)
  (if (not (plusp rat)) (cmn-error "impossible duration: ~1,3F" rat))
  (if (>= rat 1.0) 0
    (let* ((num (if (ratiop rat) (numerator rat) (first (ratify rat))))
	   (den (if (ratiop rat) (denominator rat) (second (ratify rat))))
	   (base-flags (factorize den)))
      (max 0 (- base-flags (floor (log num 2)))))))

#|
;;; old form:
(defun how-many-flags (rat)
  (if (not (plusp rat)) (cmn-error "impossible duration: ~1,3F" rat))
  (if (>= rat 1.0) 0
    (let* ((num (if (ratiop rat) (numerator rat) (first (ratify rat))))
	   (den (if (ratiop rat) (denominator rat) (second (ratify rat))))
	   (new-num (if (= num 3) 2 (if (= num 5) 4 (if (= num 7) 4 (if (= num 9) 8 (if (= num 15) 8 (if (= num 31) 16 num)))))))
	   (new-rat (/ new-num den)))
      (if (> new-rat .5) 0
	(if (> new-rat .25) 1
	  (if (> new-rat .125) 2
	    (if (> new-rat .0625) 3
	      (if (> new-rat .03125) 4 
		(if (> new-rat 0.015625) 5
		  (if (> new-rat 0.0078125) 6
		    7))))))))))		;surely that's enough(?) -- (floor (log (/ den num) 2)) might be as good

;;; but this fails when 3's are imbedded in other non-standard rhythms (and presumably other cases) --
;;; we're looking at the logical (factored) situation, not the durations.  One case, courtesy
;;; Olivier Delerue is a triplet (3:2) imbedded in 7:2 -- 3 4/21's should have 1 beam (since the
;;; outer 1/7 has one, 2/7 logically has 0, so the triplet splitting it should have 1), in spite
;;; of the fact that 4/16 = 1/4 has 2 beams but is a longer note!  See the note above for similar
;;; confusion (3/11 has to be a dotted 16th, not a bare 8th as per Read).  The factored approach
;;; will put 3 beams on 1/27, or 4 on 1/81, but 4 on 1/16 and 6 on 1/82.  This will screw up
;;; when CMN is being asked to notate a note list created from some real-time or algorithmic
;;; setting where rhythms are not exact -- it probably should have a switch for irregular cases.

;;; The 1/9 is ambiguous at the level that cmn sees it -- is it 9:1 = 3 beams, 
;;; or 3:3:1 = 2 beams? The 1/9 = 3:3:1 case 
;;; goes something like: if you ask for (say) triplets (i.e. n/3), you' re 
;;; putting 3 (say 8ths) in a quarter, so you get, for example, q + e = q. 
;;; If you ask for 5 divisions in a quarter, that's q + s = q, (or is it h + e = q?) -- 
;;; since the beat is closer to a 16th, this gets two beams, but the problem for cmn 
;;; is that in nested n-lets so to speak, you need to keep the beams 
;;; logical (say a triplet within a triplet = 1/9) so the outer triplet, the 
;;; unbroken beam, has one beam, and the inner triplet is considered 
;;; 3 16ths in a triplet 1/8, so it ought to have 2 beams, but that means 
;;; a quarter at the outer level = h + s at the innermost level.

|#

(defun rq (rat)
  (make-instance 'rhythm 
   :dots (how-many-dots rat)
   :flags (how-many-flags rat)
   :quarters rat))

(defmethod (setf duration) (val (obj rhythm-mixin))
  (let ((dur (fratify val)))
    (setf (odb-duration obj) dur)
    ;;; (setf (dots obj) (how-many-dots dur))
    (if (= (dots obj) 0) (setf (dots obj) (how-many-dots dur)))
    (setf (quarters obj) dur)
    (setf (flags obj) (how-many-flags dur))
    dur))


(defun whole-note-p (note)
  (> (quarters note) 3.5))

(defmethod display ((dynamics dynamics) (note rhythm-mixin) score &optional justifying)
  ;; use rhythm-mixin to affect note, chord, and rest
  (when (and (or (not justifying)
                 (not (eq (visible-justification dynamics) :none)))
             (not (invisible-p dynamics)))
    (let* ((x0 (- (+ (x0 note) (vis-dx dynamics) (center note)) (* .4 (width dynamics))))
           (y0 (+ (staff-y0 note) (vis-dy dynamics)
                  (if (member (visible-justification dynamics) '(:up :above))
                      (max *dynamics-minimum-vertical-separation*
			   (* (max 10 (+ (maximum-line note)
                                         (if (member (stem-direction note) '(:up)) 8 4)))
                              *staff-line-separation*)
                           (+ (y1 note) 0.2))
		    (min (- *dynamics-minimum-vertical-separation*)
			 (* (min -6 (- (minimum-line note)
				       (if (member (stem-direction note) '(:down)) 9 4)))
			    *staff-line-separation*)))))
           (dss *dynamics-size*))
      (when (marks dynamics) (with-position dynamics x0 y0 (display-marks dynamics score justifying)))
      (show score dynamics 
            :matrix (scale-matrix (translate-matrix score dynamics x0 y0) dss dss)
            :data (dynamics-mark dynamics)))))


(defclass audible-mixin ()
  ((slurs :initarg :slurs :initform nil :reader slurs)
   (beams :initarg :beams :initform nil :reader beams)
   (beamed :initarg :beamed :initform nil :reader beamed)
   (flagged :initarg :flagged :initform nil :reader flagged)
   (octaved :initarg :octaved :initform nil :reader octaved)
   (tremolo :initarg :tremolo :initform nil :reader get-tremolo)
   (ties :initarg :ties :initform nil :reader ties)
   (stem-direction :initarg :stem-direction :initform nil :reader stem-direction :reader audible-stem-direction)
   (stem-end :initarg :stem-end :initform nil :reader stem-end)
   (outer-beam :initarg :outer-beam :initform nil :reader outer-beam)
   (store-data :initarg :store-data :initform nil :reader store-data)
   (tie-direction :initarg :tie-direction :initform nil :reader tie-direction :reader audible-tie-direction)
   (slur-direction :initarg :slur-direction :initform nil :reader slur-direction :reader audible-slur-direction)
   (leger-line-length :initarg :leger-line-length :reader leger-line-length :initform .1) ;on the wide side
   (leger-line-width :initarg :leger-line-width :reader leger-line-width :initform .04)
   (leger-line-breathing-space :initarg :leger-line-breathing-space :initform .05 :reader leger-line-breathing-space)
   (local-line-separation :initarg :local-line-separation :initform nil :reader local-line-separation)))

(defclass audible (audible-mixin)
  ((slurs :accessor slurs)
   (beams :accessor beams)
   (beamed :accessor beamed)
   (flagged :accessor flagged)
   (octaved :accessor octaved)
   (tremolo :accessor get-tremolo)
   (ties :accessor ties)
   (stem-direction :accessor stem-direction :accessor audible-stem-direction)
   (stem-end :accessor stem-end)
   (outer-beam :accessor outer-beam)
   (store-data :accessor store-data)
   (tie-direction :accessor audible-tie-direction)
   (slur-direction :accessor slur-direction :accessor audible-slur-direction)
   (leger-line-length :accessor leger-line-length)
   (leger-line-width :accessor leger-line-width)
   (leger-line-breathing-space :accessor leger-line-breathing-space)
   (local-line-separation :accessor local-line-separation)))

(defmethod audible-p ((obj t)) nil)
(defmethod audible-p ((obj audible-mixin)) t)


(deferred-action leger-line-length)
(deferred-action leger-line-width)
(deferred-action leger-line-breathing-space)

(defgeneric stem-is-up (object) )
(defgeneric stem-is-down (object) )
(defgeneric stem-is-locked (object) )
(defgeneric stem-is-up? (object) )
(defgeneric stem-is-down? (object) )

(defmethod stem-is-up ((object t)) nil)
(defmethod stem-is-down ((object t)) nil)
(defmethod stem-is-locked ((object t)) nil)
(defmethod stem-is-up? ((object t)) nil)
(defmethod stem-is-down? ((object t)) nil)

(defmethod stem-is-up ((object audible-mixin)) (member (audible-stem-direction object) '(:up :up?)))
(defmethod stem-is-down ((object audible-mixin)) (member (audible-stem-direction object) '(:down :down?)))
(defmethod stem-is-locked ((object audible-mixin)) (member (audible-stem-direction object) '(:up :down)))
(defmethod stem-is-up? ((object audible-mixin)) (if (audible-stem-direction object) (stem-is-up object) (< (head-line object) 4)))
(defmethod stem-is-down? ((object audible-mixin)) (not (stem-is-up? object)))

(defmethod descry ((note audible-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A"
	  (if (slurs note)  (format nil "~%~A :slurs (list~{ ~A~})" 
				    prewhitespace
				    (loop for slur in (slurs note) collect (descry slur))) "")
	  (if (beams note) (format nil "~%~A :beams ~A~%                    " prewhitespace (descry (beams note))) "")
	  (if (beamed note) (format nil "~%~A :beamed ~A" prewhitespace (beamed note)) "")
	  (if (flagged note) (format nil " :flagged ~A" (flagged note)) "")
	  (if (octaved note) " :octaved t" "")
	  (if (outer-beam note)
	      (format nil " :outer-beam '(~{~1,3F ~})~%                    " (outer-beam note))
	    "")
	  (if (get-tremolo note) (format nil " :tremolo ~A" (descry (get-tremolo note))) "")
	  (if (ties note) (format nil "~%~A :ties (list~{ ~A~})" 
				  prewhitespace (loop for tie in (ties note) collect (descry tie))) "")
	  (if (stem-direction note) (format nil "~%~A :stem-direction :~(~A~)" prewhitespace (stem-direction note)) "")
	  (if (audible-tie-direction note) (format nil " :tie-direction :~(~A~)" (audible-tie-direction note)) "")
	  (if (slur-direction note) (format nil " :slur-direction :~(~A~)" (slur-direction note)) "")
	  (if (stem-end note) (format nil "~%                     :stem-end ~1,3F" (stem-end note)) "")
	  (if (store-data note) (format nil "~%~A :store-data ~A" prewhitespace (store-data note)) "")
	  (if (/= (leger-line-length note) .1) (format nil " :leger-line-length ~1,3F" (leger-line-length note)) "")
	  (if (/= (leger-line-width note) .04) (format nil " :leger-line-width ~1,3F" (leger-line-width note)) "")
	  (if (/= (leger-line-breathing-space note) .05) (format nil " :leger-line-breathing-space ~1,3F" (leger-line-breathing-space note)) "")
	  (if (local-line-separation note) (format nil " :local-line-separation ~1,3F" (local-line-separation note)) "")
	  (if (next-method-p) (call-next-method note stream (or controller note)) "")))

#|
;;; old version -- I can't rememebr why it doesn't make a new object
(defmethod copy ((note audible-mixin) &optional new-chord)
  (when (slurs note) (copy-slurs note new-chord))
  (if (beams note) (setf (beams new-chord) (copy (beams note))))
  (setf (beamed new-chord) (beamed note))
  (setf (flagged new-chord) (flagged note))
  (setf (octaved new-chord) (octaved note))
  (setf (outer-beam new-chord) (outer-beam note))
  (if (get-tremolo note) (setf (get-tremolo new-chord) (copy (get-tremolo note))))
  (if (ties note) (setf (ties new-chord) (copy-ties new-chord note)))
  (setf (audible-stem-direction new-chord) (audible-stem-direction note))
  (setf (audible-tie-direction new-chord) (audible-tie-direction note))
  (setf (audible-slur-direction new-chord) (audible-slur-direction note))
  (setf (stem-end new-chord) (stem-end note))
  (setf (leger-line-length new-chord) (leger-line-length note))
  (setf (leger-line-width new-chord) (leger-line-width note))
  (setf (leger-line-breathing-space new-chord) (leger-line-breathing-space note))
  (setf (local-line-separation new-chord) (local-line-separation note))
  ;; (setf (store-data new-chord) (copy-tree (store-data note)))
  ;; why is this omitted?
  (if (next-method-p) (call-next-method note new-chord))
  new-chord)
|#

;;; new version from AV 22-Jul-03
(defmethod copy ((note audible-mixin) &optional object)
  (let ((new-audible (if (not object)
			 (make-instance 'audible)
		       (if (write-protected object) (copy object)
			 object))))
    (when (slurs note) (copy-slurs note new-audible))
    (if (beams note) (setf (beams new-audible) (copy (beams note))))
    (setf (beamed new-audible) (beamed note))
    (setf (flagged new-audible) (flagged note))
    (setf (octaved new-audible) (octaved note))
    (setf (outer-beam new-audible) (outer-beam note))
    (if (get-tremolo note) (setf (get-tremolo new-audible) (copy (get-tremolo note))))
    (if (ties note) (setf (ties new-audible) (copy-ties new-audible note)))
    (setf (audible-stem-direction new-audible) (audible-stem-direction note))
    (setf (audible-tie-direction new-audible) (audible-tie-direction note))
    (setf (audible-slur-direction new-audible) (audible-slur-direction note))
    (setf (stem-end new-audible) (stem-end note))
    (setf (leger-line-length new-audible) (leger-line-length note))
    (setf (leger-line-width new-audible) (leger-line-width note))
    (setf (leger-line-breathing-space new-audible) (leger-line-breathing-space note))
    (setf (local-line-separation new-audible) (local-line-separation note))
    ;; (setf (store-data new-audible) (copy-tree (store-data note)))
    ;; why is this omitted?
    (if (next-method-p) (call-next-method note new-audible))
    new-audible))

(defmethod display-in-parentheses ((mark sundry-mixin) (note audible-mixin) score &optional justifying)
  ;; improved by AV 28-Jun-02
  (declare (ignore justifying))
  (let* ((x0 (- (+ (box-x0 note) (vis-dx note) (x0 mark) (vis-dx mark)) .25 (if (sign note) (x1 (sign note)) 0)))
	 (x1 (+ (box-x0 note) (vis-dx mark) (x1 mark) (vis-dx note) (center note) .3 (* (dots note) 0.125)))
	 (y (+ (vis-dy mark) (- (box-y0 note) .2))))
    (draw-both-parens score mark x0 y x1 1)))

#|
(cmn (size 24)
     (free-expansion-factor 3.0)
     (glissando-thickness 0.03)
     (automatic-ties nil)
     (staff bar (treble (text "sul E" (font-size 10) (dy 1.25) ))
            (e6 q begin-glissando)
            (gs5 e end-glissando begin-glissando)
            (c6 e  end-glissando begin-glissando)
            (e6 h. (in-parentheses) end-glissando begin-glissando)
            (f5 q... (sign double-flat) (in-parentheses) end-glissando)
            (f5 q (in-parentheses))))
|#

(defclass note-mixin (audible-mixin rhythm-mixin staff-relative-mixin score-object-mixin)
  ((line :initarg :line :initform nil :reader line :reader note-line)
   (note-head :initarg :note-head :initform nil :reader note-head) ; a type like :diamond, nil = choose what seems best from context
   (sign :initarg :sign :initform nil :reader note-sign)
   (cclass :initarg :cclass :initform nil :reader cclass)
   (pitch :initarg :pitch :initform nil :reader pitch)
   (octave :initarg :octave :initform nil :reader octave)
   (head-quarters :initform nil :reader head-quarters)
   (stem-dy :initform nil :reader stem-dy)
   (stem-x0 :initform nil :reader stem-x0)
   (stem-mark :initform nil :reader stem-mark)
   (stem-tie :initarg :stem-tie :initform nil :reader stem-tie)
   (note-head-size :initarg :note-head-size :initform nil :accessor note-head-size)
   (collision :initarg :collision :initform nil :reader collision :reader note-collision)
   (collider :initarg :collider :initform nil :reader collider :reader note-collider)))

(defclass note (note-mixin audible rhythm staff-relative-mixin score-object) 
  ((line :accessor line :accessor note-line)
   (note-head :accessor note-head :initform nil)
   (sign :accessor note-sign)
   (cclass :accessor cclass)
   (pitch :accessor pitch)
   (octave :accessor octave)
   (head-quarters :accessor head-quarters)
   (stem-dy :accessor stem-dy)
   (stem-x0 :accessor stem-x0)
   (stem-mark :accessor stem-mark)
   (stem-tie :accessor stem-tie)
   (note-head-size :accessor note-head-size)
   (collision :accessor collision :accessor note-collision)
   (collider :accessor collider :accessor note-collider)
   (quarters :initform 1)))

#-excl
(defmethod print-object ((object note-mixin) stream)
  (format stream "#<~A: ~A>" 
	  (class-name (class-of object))
	  (brief-identify object)))

(deferred-action stem-direction)
;(deferred-action sign)
(deferred-action pitch)
(deferred-action octave)

;(deferred-action dots)
(defmethod dots ((val t)) (make-self-acting :action #'(lambda (obj val1) (set-dots val1 obj) (setf (dots-locked obj) t) val1) :argument val))

;(deferred-action flags)
(defmethod flags ((val t)) (make-self-acting :action #'(lambda (obj val1) (set-flags val1 obj) (setf (flags-locked obj) t) val1) :argument val))

(deferred-action note-head)
(deferred-action beams)
(deferred-action ratified)
(deferred-action quarters)
(deferred-action beamed)
(deferred-action stem-dy)
(deferred-action stem-x0)
(deferred-action stem-mark)
(deferred-action head-quarters)

(deferred-action note-head-size)
(defmethod (setf note-head-size) (n (scr score)) (setf *note-head-size* n) nil)

(defgeneric sign (val &rest args) )

(defmethod sign ((val t) &rest args)
  (declare (ignore args))
  nil)

(defmethod sign ((val accidental-mixin) &rest args)
  (apply #'ur-accidental (if (write-protected val) (copy val) val) args))

(defmethod sign ((val note-mixin) &rest args)
  (declare (ignore args))
  (note-sign val))


(defmethod (setf sign) (val (obj t))
  (declare (ignore val))
  (cmn-error "can't set the sign of ~A because it's a ~(~A~), not a note." (identify obj) (class-name (class-of obj))))

(defmethod (setf sign) (val (obj note-mixin))
  (setf (note-sign obj) val))


(defmethod add-to-marks ((note note) objects) (setf (marks note) (append (marks note) objects)))
(defmethod add-to-slurs ((note note) object) (push object (slurs note)))
(defmethod add-to-ties ((note note) object) (push object (ties note))) ; internal -- in automatic tie creation

(defmethod tied-to ((tie-note audible-mixin)) 
  (make-self-acting :action #'(lambda (note tied-note)
				(setf (stem-tie note) tied-note)
				(if (not (stem-tie tied-note))
				    (push tied-note (stem-tie tied-note)))
				(push note (stem-tie tied-note))
				;; so top of chain has a list of everyone on the chain,
				;;   each lower member has a pointer to the top,
				;;   now stem-x0 becomes the stem-x0 -- 
				;;     it can be used to adjust horizontally during housing,
				;;   with back-pointers from the top to harmonize everyone.
				(setf (stem-direction note) (or (stem-direction tied-note) :down))
				;; so top note determines stem-direction -- defaults to :down
				nil)
		    :argument tie-note))

(defvar old-octave 4)

(defmethod (setf rhythm) (ur-val (note note))
  (if (minusp ur-val) (cmn-error "attempt to set negative rhythm"))
  (let ((val (fratify ur-val)))
    (setf (odb-duration note) val)
    (setf (dots note) (how-many-dots val))
    (setf (flags note) (how-many-flags val))
    (setf (quarters note) val)))

(defun rhythm (dur) (make-self-acting :action #'(lambda (obj val) (setf (rhythm obj) val)) :argument dur))

(defgeneric head-line (note) )
(defgeneric maximum-line (note) )
(defgeneric minimum-line (note) )

(defmethod head-line ((note note-mixin))  (note-line note))
(defmethod maximum-line ((note note-mixin)) (note-line note))
(defmethod minimum-line ((note note-mixin)) (note-line note))

(defun stem-up () (stem-direction :up))
(defun stem-down () (stem-direction :down))
(defun no-stem () (stem-direction :none))
(defvar stem-up (make-self-acting :action #'(lambda (obj val) (setf (stem-direction obj) val)) :argument :up))
(defvar stem-down (make-self-acting :action #'(lambda (obj val) (setf (stem-direction obj) val)) :argument :down))
(defvar no-stem (make-self-acting :action #'(lambda (obj val) (setf (stem-direction obj) val)) :argument :none))
(defun dashed-stem (&optional (pat '(4 3))) (pattern pat))
(defvar dashed-stem (dashed-stem))
(defvar whisper (dashed-stem))

#|
(cmn treble
     (c4 e (begin-beam) (dashed-stem))
     (c4 e (end-beam) (dashed-stem))
     (chord (notes c5 g5) q whisper))
|#

(defun stem-eq (s1 s2)
  (or (eq s1 s2)
      (and (member s1 '(:up :up?)) (member s2 '(:up :up?)))
      (and (member s1 '(:down :down?)) (member s2 '(:down :down?)))))

(defclass write-protected-note (write-protect note-mixin) ())

(defmethod note-p ((obj t)) nil)
(defmethod note-p ((obj note-mixin)) t)

(defmethod descry ((note note-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) "(note" "")
	  (if (note-head note) 
	      (format nil " :note-head :~(~A~)~%~A" 
		      (note-head note)
		      prewhitespace) 
	    "")
	  (if (sign note) 
	      (format nil " :sign ~A~%~A" 
		      (descry (sign note))
		      prewhitespace) 
	    "")
	  (if (cclass note) (format nil " :cclass ~A" (cclass note)) "")
	  (if (pitch note) (format nil " :pitch ~A" (pitch note)) "")
	  (if (octave note) (format nil " :octave ~A" (octave note)) "")
	  (if (note-line note) (format nil " :line ~A" (note-line note)) "")
	  (if (head-quarters note) (format nil " :head-quarters ~A" (head-quarters note)) "")
	  (if (stem-dy note) (format nil " :stem-dy ~A" (stem-dy note)) "")
	  (if (stem-x0 note) (format nil " :stem-x0 ~A" (stem-x0 note)) "")
	  (if (stem-tie note) (format nil " :stem-tie '~A" (stem-tie note)) "")
	  (if (note-head-size note) (format nil " :note-head-size ~A" (note-head-size note)) "")
	  (if (collision note) (format nil " :collision ~A" (collision note)) "")
	  (if (collider note) (format nil " :collider ~A" (collider note)) "")
	  (if (next-method-p) (call-next-method note stream (or controller note)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((note note-mixin) &optional object)
  (let ((new-note (if (not object) (make-instance 'note)
		    (if (write-protected object) (copy object)
		      object))))
    (if (next-method-p) (call-next-method note new-note))
    (if (note-head note) (setf (note-head new-note) (note-head note)))
    (if (note-head-size note) (setf (note-head-size new-note) (note-head-size note)))
    (setf (note-sign new-note) (note-sign note))
    (setf (note-line new-note) (note-line note))
    (setf (cclass new-note) (cclass note))
    (setf (pitch new-note) (pitch note))
    (setf (head-quarters new-note) (head-quarters note))
    (setf (stem-dy new-note) (stem-dy note))
    (setf (stem-x0 new-note) (stem-x0 note))
    (setf (stem-tie new-note) (copy-list (stem-tie note)))
    (setf (stem-mark new-note) (stem-mark note))
    (setf (note-collision new-note) (note-collision note))
    (setf (note-collider new-note) (note-collider note))
    (if (octave note)
	(progn
	  (setf (octave new-note) (octave note))
	  (setf old-octave (octave note)))
      (setf (octave new-note) old-octave))
    new-note))

(defmethod short-note ((note note-mixin) &key x0)
  (make-instance 'note :x0 (or x0 (box-x0 note)) :x1 (box-x1 note) :y0 (box-y0 note) :y1 (box-y1 note)
	     :staff-y0 (staff-y0 note) :stem-direction (audible-stem-direction note)
	     :tie-direction (audible-tie-direction note) :slur-direction (audible-slur-direction note)
	     :center (center note) :ties (if (ties note) t nil) :quarters (quarters note)
	     :stem-end (stem-end note) :line (note-line note) :pitch (pitch note) :octave (octave note)))

(defun car-member (var lst)
  (and lst
       (or
	(and (listp (car lst)) (eq var (caar lst)) (cadar lst))
	(car-member var (cdr lst)))))

(defun note-name (note) 
  (let* ((sign (note-sign note))
	 (tied-sign (car-member :sign (store-data note)))
	 (nat-sign (and sign (marks sign) (find-if #'(lambda (n) 
						       (and (sundry-p n) 
							    (member (sundry-name n) (list :natural-sharp :natural-flat))))
						   (marks sign))))
	 (nat-sign-name (and nat-sign (sundry-name nat-sign)))
	 (funny-sign (and sign (not nat-sign) (not (member sign (list sharp flat natural)))))
	 (pitch (if (or (not tied-sign)
			(eq tied-sign natural))
		    (pitch note)
		  (if (eq tied-sign sharp)
		      (1- (pitch note))
		    (if (eq tied-sign flat)
			(1+ (pitch note))
		      (pitch note)))))
	 (name (case pitch
		 (0 (if (eq sign sharp) "bs" (if (eq sign natural) "cn" "c")))
		 (1 (if (eq sign sharp) "cs" "df"))
		 (2 (if (eq sign natural) "dn" "d"))
		 (3 (if (eq sign sharp) "ds" "ef"))
		 (4 (if (eq sign natural) "en" "e"))
		 (5 (if (eq sign natural) "fn" (if (eq sign sharp) "es" "f")))
		 (6 (if (eq sign sharp) "fs" "gf"))
		 (7 (if (eq sign natural) "gn" "g"))
		 (8 (if (eq sign sharp) "gs" "af"))
		 (9 (if (eq sign natural) "an" "a"))
		 (10 (if (eq sign sharp) "as" "bf"))
		 (11 (if (eq sign natural) "bn" (if (eq sign flat) "cf" "b")))
		 (12 (if (eq sign sharp) "bs" "c")))))
    (format nil "~A~D~A~(~A~)" name (octave note) 
	    (if (or nat-sign funny-sign) " " "") 
	    (if funny-sign (identify sign)
	      (if nat-sign nat-sign-name "")))))

(defmethod brief-identify ((note note-mixin))
  (format nil "(~A ~A~A)"
	  (note-name note) 
	  (if (not (halve-duration note)) 
	      (rhythm-name (quarters note))
	    (rhythm-name (* 2 (quarters note))))
	  (if (onset note) (format nil " (onset ~A)" (fratify (onset note))) "")))

(defmethod brief-identify ((whatever t))
  whatever)

(defmethod brief-identify ((note write-protected-note)) (note-name note))

(defmethod identify ((note write-protected-note)) (note-name note))

(defmethod identify ((note note-mixin))
  (format nil "(~A ~A~A~A~A~A~A~A~A~A~A~A~A~A~A)" 
	  (note-name note) 
	  (if (not (halve-duration note)) 
	      (rhythm-name (quarters note))
	    (rhythm-name (* 2 (quarters note))))
	  (if (onset note) (format nil " (onset ~A)" (fratify (onset note))) "")
	  (if (and (note-head note)
		   (not (eq (note-head note) :normal)))
	      (format nil " (note-head :~(~A~))" (note-head note))
	    "")
	  (if (eq (audible-stem-direction note) :up) " stem-up"
	    (if (eq (audible-stem-direction note) :down) " stem-down"
	      (if (eq (audible-stem-direction note) :none) " no-stem"
		"")))
	  (if (stem-dy note) (format nil " (stem-dy ~A)" (stem-dy note)) "")
	  (if (audible-tie-direction note) (format nil " (tie-direction :~(~A~))" (audible-tie-direction note)) "")
	  (if (audible-slur-direction note) (format nil " (slur-direction :~(~A~))" (audible-slur-direction note)) "")
	  (if (store-data note) (identify-store-data note) "")
	  (if (/= (leger-line-length note) .1) (format nil " (leger-line-length ~1,3F)" (leger-line-length note)) "")
	  (if (/= (leger-line-width note) .04) (format nil " (leger-line-width ~1,3F)" (leger-line-width note)) "")
	  (if (/= (leger-line-breathing-space note) .05) (format nil " (leger-line-breathing-space ~1,3F)" (leger-line-breathing-space note)) "")
	  (if (ties note) 
	      (format nil "~{ ~(~A~)~}" (sorted-ties (ties note)))
	    "")
	  (if (get-tremolo note) 
	      (identify (get-tremolo note))
	    "")
	  (the-usual-suspects note)))

(defvar with-errors t)

(defun ur-note (new-note &rest ur-objects)
  (let ((found-note nil)
	(found-rhythm nil))
    (setf *cmn-owning-object* new-note)
    (let ((objects ur-objects))
      (loop while objects do
	(let ((object (pop objects)))
	  (setf *cmn-object* object)
	  (when object
	    (if (self-acting-p object)
		(funcall (action object) new-note (argument object))
	      (if (rhythm-p object)
		  (if found-rhythm
		      (warn "more than one rhythm (~A) passed to ~A" (identify object) (identify new-note))
		    (progn
		      (setf found-rhythm object)
		      (rcopy object new-note)))
		(if (score-object-list-p object) 
		    (setf objects (append (disgorge object) objects))
		  (if (score-object-p object)
		      (if (accidental-p object)
			  (setf (note-sign new-note) object)
			(if (or (sundry-p object) (pause-p object) (dynamics-p object))
			    (add-to-marks new-note (list (if (write-protected object) (copy object) object)))
			  (if (note-p object)
			      (if found-note
				  (warn "more than one note designation (~A) passed to ~A" (identify object) (identify new-note))
				(progn
				  (setf found-note object)
				  (copy object new-note)))
			    (if with-errors
				(cmn-error "odd argument to ~A: ~A" (identify new-note) (identify object))))))
		    (if (visible-p object)
			(add-to-marks new-note (list object))
		      (if with-errors
			  (cmn-warn "odd argument to note: ~A to ~A" object new-note)))))))))))
    (when (halve-duration new-note)
      (setf (head-quarters new-note) (quarters new-note))
      (if (quarters new-note) (setf (quarters new-note) (* .5 (quarters new-note))))
      (if (odb-duration new-note) (setf (odb-duration new-note) (* .5 (odb-duration new-note)))))
    (setf (octaved new-note) (or (octaved new-note) octave-stack))
    (setf (flagged new-note) (and (not (flagged new-note)) (not beam-stack) (> (flags new-note) 0)))
    (if (not (beamed new-note)) (setf (beamed new-note) (and (> (flags new-note) 0) beam-stack)))
    (cmn-tick-pipe new-note)
    new-note))

(defun note (&rest objects)
  (apply #'ur-note (make-instance 'note) objects))

(defmethod notify ((note note) &optional objects)
  (if (listp objects)
      (apply #'ur-note note objects)
    (ur-note note objects)))

;;; define-accidental for non-special note heads since it is the simplest class for the job
(define-accidental whole-note #'draw-whole-note whole-note-bounds)
(define-accidental double-whole-note #'draw-double-whole-note double-whole-note-bounds)
(define-accidental half-note #'draw-half-note half-note-bounds)
(define-accidental quarter-note #'draw-quarter-note quarter-note-bounds)
(define-accidental open-diamond-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-diamond score t)) diamond-bounds)
(define-accidental filled-diamond-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-diamond score nil)) diamond-bounds)

(define-accidental open-diamond-1-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-diamond-1 score t)) diamond-1-bounds)
(define-accidental filled-diamond-1-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-filled-diamond-1 score nil)) diamond-1-bounds)

(define-accidental open-triangle-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-triangle score t)) triangle-bounds)
(define-accidental filled-triangle-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-triangle score nil)) triangle-bounds)
(define-accidental open-square-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-square score t)) square-bounds)
(define-accidental filled-square-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-square score nil)) square-bounds)
(define-accidental rhythmX-note #'draw-rhythmX rhythmX-bounds)
(define-accidental circled-x-note #'draw-circled-x rhythmX-bounds)
(define-accidental slash-note #'draw-slash slash-bounds)
(define-accidental headless-note #'(lambda (score &optional fill) (declare (ignore fill)) (rmoveto score .3 0)) '(0 0 .3 0))
(define-accidental flag-up #'draw-8th-flag-up up8th-bounds)
(define-accidental flag-down #'draw-8th-flag-down down8th-bounds)
(define-accidental add-flag-up #'draw-extend-flag-up extend-flag-up-bounds)
(define-accidental add-flag-down #'draw-extend-flag-down extend-flag-down-bounds)

;;; (cmn treble c4 q (note-head :square) c4 h (note-head :square))



;;; _______some glyphs___________________________________________
;;; (AV 3-Jul-00)

(defvar *centered-note-heads* '())      ; for use in #'note-head-x0-offset

(defun draw-breath-in (score &optional style whole)
  (comment score "breath-in")
  (setf (line-width score) .03)
  (moveto score (+ 0.2 (* 0.5 *stem-width*)) 0)
  (rlineto score 0 .12)
  (rlineto score -.4 -.12)
  (rlineto score .4 -.12)
  (rlineto score 0.0 .12)
  (if (not style) (fill-in score) (draw score style))
  (setf (line-width score) 0))

(defvar breath-in-bounds '(0.0 -.16 .4 .125))

(define-accidental whole-breath-in-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-breath-in score t t))
  breath-in-bounds)
(define-accidental open-breath-in-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-breath-in score t nil))
  breath-in-bounds)
(define-accidental filled-breath-in-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-breath-in score nil nil))
  breath-in-bounds)

(pushnew :breath-in *centered-note-heads*)

(defun draw-breath-out (score &optional style whole)
  (comment score "breath-out")
  (setf (line-width score) .03)
  (moveto score (- 0.2 (* 0.5 *stem-width*)) 0)
  (rlineto score 0 .12)
  (rlineto score .4 -.12)
  (rlineto score -.4 -.12)
  (rlineto score 0 .12)
  (if (not style) (fill-in score) (draw score style))
  (setf (line-width score) 0))

(defvar breath-out-bounds '(0.0 -.16 .4 .125))

(define-accidental filled-breath-out-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-breath-out score nil nil))
  breath-out-bounds)
(define-accidental open-breath-out-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-breath-out score t nil))
  breath-out-bounds)
(define-accidental whole-breath-out-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-breath-out score t t))
  breath-out-bounds)

(pushnew :breath-out *centered-note-heads*)

(defun draw-airy-head (score &optional style)
  (comment score "airy-head")
  (let ((radius .12))
    (setf (line-width score) .03)
    (circle score radius 0.0 radius 0 360 style)
    (setf (line-width score) *stem-width*)
    (moveto score radius 0.25)
    (rlineto score 0 -.5)
    (draw score style)  
    (setf (line-width score) 0)))

(defvar airy-head-bounds '(0.0 -.25 .24 .25))

(define-accidental open-airy-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-airy-head score nil))
  airy-head-bounds)
(define-accidental filled-airy-note
  #'(lambda (score &optional fill) (declare (ignore fill)) (draw-airy-head score t))
  airy-head-bounds)

(pushnew :airy-head *centered-note-heads*)

(defun draw-arrow-up (score &optional style)
  (comment score "arrow-up")
  (setf (line-width score) .03)
  (moveto score  0.14 -.125)
  (rlineto score .14 0)
  (rlineto score -.14 .25)
  (rlineto score -.14 -.25)
  (rlineto score .14 .0)
  (if (not style) (fill-in score) (draw score style))
  (setf (line-width score) 0))

(defvar arrow-up-bounds '(0.0 -0.15 .28 .135))

(define-accidental open-arrow-up-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-arrow-up score t)) arrow-up-bounds)
(define-accidental filled-arrow-up-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-arrow-up score nil)) arrow-up-bounds)

(pushnew :arrow-up *centered-note-heads*)

(defun draw-arrow-down (score &optional style)
  (comment score "arrow-down")
  (setf (line-width score) .03)
  (moveto score  .14 0.125)
  (rlineto score .14 0)
  (rlineto score -.14 -.25)
  (rlineto score -.14 .25)
  (rlineto score .14 .0)
  (if (not style) (fill-in score) (draw score style))
  (setf (line-width score) 0))

(defvar arrow-down-bounds '(0.0  -0.125 0.28 0.15))
(define-accidental open-arrow-down-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-arrow-down score t)) arrow-down-bounds)
(define-accidental filled-arrow-down-note #'(lambda (score &optional fill) (declare (ignore fill)) (draw-arrow-down score nil)) arrow-down-bounds)

(pushnew :arrow-down *centered-note-heads*)


(defun note-head-y0-offset (note up-stem)
  ;; note and chord
  (let ((head (note-head note)))
    (if (or (not head)
	    (eq head :normal))
	(if (>= 3.5 (quarters note) 2) (if up-stem .02 -.02) 0)
      (if (member head '(:arrow-down :arrow-up))
	  (if up-stem
              (- (* (y1 (get-note-head (quarters note) (note-head note))) *note-head-size*) *stem-width*)
            (+ (* (y0 (get-note-head (quarters note) (note-head note)))  *note-head-size*) *stem-width*))
        (if (member head '(:triangle))
            (* (y0 (get-note-head (quarters note) (note-head note)))  *note-head-size*) 
          (if (eq head :x)
              (if up-stem .1 -.1)
            (if (eq head :slash)
                (if up-stem .1 -.1)
              0)))))))

(defun note-head-x0-offset (note up-stem half-stem-width)
  ;;; denne versjonen trengs ved benytting av "sentrerte" note-hoder ("this version is needed for 'centered' note-heads")
  (if (note-p note)
      (let* ((matrix-x (first (matrix note)))
             (xscl (or (and matrix-x (* matrix-x matrix-x)) 1.0))
             (nh (get-note-head (or (head-quarters note) (quarters note)) (note-head note)))
             (offs (if up-stem
                       (- (* xscl (width nh))  half-stem-width)
                     *half-stem-width*)) ;was *stem-width* (24-Aug-00 Christophe Rhodes)
             (head (note-head note)))
        (if (member head *centered-note-heads*)
            (if up-stem
                (- (width nh) (* xscl (note-size note) *note-head-size* (width nh) 0.5))
              (* xscl (note-size note) *note-head-size* (width nh) 0.5))
          offs))
    .27))

(defun get-note-head (head-quarters &optional (head-type nil))
  (if (or (not head-type) 
	  (member head-type '(:normal :small)))
      (if (= head-quarters 8)
	  double-whole-note
	(if (>= head-quarters 4)
	    whole-note
	  (if (or (>= head-quarters 2) (= head-quarters 4/3))
	      half-note
	    quarter-note)))
    (if (sundry-p head-type)
	head-type
      (case head-type
	(:diamond (if (< head-quarters 2) filled-diamond-note open-diamond-note))
        (:diamond-1 (if (< head-quarters 2) filled-diamond-1-note open-diamond-1-note))
	(:tremolo half-note)
	(:artificial-harmonic open-diamond-note)
	(:triangle (if (< head-quarters 2) filled-triangle-note open-triangle-note))
	(:slash slash-note)
	(:square (if (< head-quarters 2) filled-square-note open-square-note))
	(:x rhythmX-note)
	(:circled-x circled-x-note)
        (:breath-in (if (< head-quarters 2)
                        filled-breath-in-note
                      (if (< head-quarters 4)
                          open-breath-in-note
                        whole-breath-in-note)))
        (:breath-out (if (< head-quarters 2)
                        filled-breath-out-note
                      (if (< head-quarters 4)
                          open-breath-out-note
                        whole-breath-out-note)))
        (:airy-head (if (< head-quarters 2)
                        filled-airy-note
                      open-airy-note))
        (:arrow-up (if (< head-quarters 2) filled-arrow-up-note open-arrow-up-note))
        (:arrow-down (if (< head-quarters 2) filled-arrow-down-note open-arrow-down-note))
	(:none headless-note)
	(otherwise (cmn-error "unknown note head type: ~A" head-type))))))

#|
(cmn
 (free-expansion-factor 2.0)
 (staff-separation 0.6)
 (staff (staff-lines 0)
        (engorge (loop repeat 11
                   collect (d4 q (note-head :arrow-down) (stem-dy -0.3)))))
 (staff 
  (g4 e (note-head :airy-head))
  (f4 e (note-head :arrow-down))
  (e4 e (note-head :diamond-1))
  (d4 e (note-head :square))
  (c5 q (note-head :arrow-up))
  (f4 h (note-head :diamond-1) (mouth-position 3))
  (f4 h (note-head :circled-x))
  (a4 w (note-head :square)(mouth-position :pursed))))
|#

(defvar shorter-stems nil)
(defvar *straight-flag-dx* .3)
(defvar *straight-flag-dy* 0.0)

(defun draw-flags (score note nflags up-stem stem-end x0)
  ;; this still isn't perfect -- the initial flag seems to be offset to the left by half-stem-width?
  (let* ((y0 (if (not shorter-stems)
		 (+ stem-end (if up-stem -.1 .05))
	       stem-end))
	 (incr (if up-stem (- *flag-vertical-spacing*) *flag-vertical-spacing*))
	 (ny0 (if (not shorter-stems)
		  (+ stem-end (* incr nflags) (if up-stem .1 -.175))
		(+ stem-end (* incr (1- nflags))))))
    (if (member (note-head note) *centered-note-heads*)
	(incf x0 (- (note-head-x0-offset note up-stem *half-stem-width*) *half-stem-width*))
        ;; -> (cmn treble c5 s)
      (if (and up-stem *curvy-flags*)
	  (incf x0 (note-head-x0-offset note up-stem *stem-width*))))
    (if *curvy-flags*
	(progn
	  (if up-stem
	      (show score flag-up :matrix (translate-matrix score note x0 ny0))
	    (show score flag-down :matrix (translate-matrix score note x0 ny0)))
#-(or gcl sbcl)	  (loop for i from 1 below nflags and y from y0 by incr do
	    (if up-stem
		(show score add-flag-up :matrix (translate-matrix score note x0 y))
	      (show score add-flag-down :matrix (translate-matrix score note x0 y))))
#+(or gcl sbcl)     (do ((i 1 (1+ i))
	       (y y0 (+ y incr)))
	      ((>= i nflags))
	    (if up-stem
		(show score add-flag-up :matrix (translate-matrix score note x0 y))
	      (show score add-flag-down :matrix (translate-matrix score note x0 y))))
         )

      ;; all the .3's below are probably wrong for unusual note heads or note head sizes
      (let* ((bx0 (+ x0 (if up-stem (- .3 *half-stem-width*) *half-stem-width*))) ; .3=head size
	     (by0 (if up-stem (- stem-end *beam-width*) stem-end)))
	#-(or gcl sbcl)	(loop for i from 0 below nflags and y from by0 by incr do
		  (moveto score bx0 y)

		  ;; this version from AV 7-Oct-02
		  (rlineto score *straight-flag-dx* (if up-stem (- *straight-flag-dy*) *straight-flag-dy*))
		  (rlineto score 0.0 *beam-width*)
		  (lineto score bx0 (+ y *beam-width*))
		  
;		  (lineto score (+ bx0 .3) y)
;		  (lineto score (+ bx0 .3) (+ y .1))
;		  (lineto score bx0 (+ y .1))
		  
		  (lineto score bx0 y)
		  (fill-in score))
	#+(or gcl sbcl)	(do ((i 0 (1+ i))
		     (y by0 (+ y incr)))
		    ((>= i nflags))
		  (moveto score bx0 y)
		  (lineto score (+ bx0 .3) y)
		  (lineto score (+ bx0 .3) (+ y .1))
		  (lineto score bx0 (+ y .1))
		  (lineto score bx0 y)
		  (fill-in score))
        ))))

;;; ----------------------------------------------------------------

#|
(cmn (automatic-ties nil) (automatic-beams nil) treble c4 e c4 s c4 thirty-second c4 sixty-fourth c4 one-twenty-eighth)
(cmn (automatic-ties nil) (automatic-beams nil) bass c4 e c4 s c4 thirty-second c4 sixty-fourth c4 one-twenty-eighth)
(cmn (size 100) (automatic-beams nil) (staff treble g4 e g4 s g4 (rq 1/8) g4 (rq 1/16)) (staff bass c4 e c4 s c4 (rq 1/8) c4 (rq 1/16)) )
|#

(defun draw-stem (score x0 y0 y1 width &optional pattern)
    (let ((old-width (line-width score)))
      (setf (line-width score) width)
      (moveto score x0 y0)
      (lineto score x0 y1 :pattern pattern)
      (draw score)
      (setf (line-width score) old-width)))

(defun note-size (n) (or (note-head-size n) 1.0))

(defun get-stem-end (score note up-stem grace-stem)
  (declare (ignore score))
  (let* ((mid-staff (+ (or (staff-y0 note) 0) 
		       (if grace-stem
			   (if up-stem
			       (/ .125 *grace-note-size*)
			     (/ .875 *grace-note-size*)) 
			 (* .5 *staff-dy*))))
	 (ideal *ideal-stem-length*)
	 (nflags (or (flags note) 0))
	 (dots (or (dots note) 0))
	 (note-y0 (+ (box-y0 note) (dy note)))
	 (note-line (if up-stem (minimum-line note) (maximum-line note)))
	 (dot-room (if (and up-stem
			    (plusp dots)
			    (plusp nflags))
		       (if (not shorter-stems)
			   .1
			 (if (oddp note-line) 0.09 0.14))
		     0))
	 (tremolo-room (if (get-tremolo note)
			   (* (or (tremolo-slashes (get-tremolo note)) 0) *staff-line-separation*)
			 0))
	 (note-size (* *note-head-size* (note-size note)))
	 (flag-room (if (not shorter-stems)
			(+ (* nflags *flag-vertical-spacing*)
			   (if (and (not up-stem)
				    (plusp nflags)
				    (> note-size 1.1))
			       (* .5 (- note-size 1.1))
			     0))
		      (* (max 0 (- nflags 1)) *flag-vertical-spacing*))))
    (if (or (<= -1 note-line 9)		; not leger lines, so just make stem long enough
	    (and up-stem (> note-line 9))
	    (and (not up-stem) (< note-line -1)))
	(if (not shorter-stems)
	    (if up-stem
		(+ note-y0 (max .3 (+ flag-room tremolo-room)) dot-room ideal)
	      (- note-y0 (max .3 (+ flag-room tremolo-room)) ideal))
	  (if up-stem
	      (+ note-y0 (if (= nflags 0)
			     (max tremolo-room 0.3)
			   (+ flag-room tremolo-room))
		 dot-room ideal)
	    (- note-y0 (if (= nflags 0)
			   (max tremolo-room 0.3)
			 (+ flag-room tremolo-room))
	       ideal)))
      (if (zerop nflags)
	  (+ mid-staff (* (if up-stem 1 -1) (min (max -.25 (- ideal .5)) .25)))
	(let* ((stem-length (+ (max 0.5 ideal) flag-room tremolo-room dot-room))
	       (cur-stem (abs (- note-y0 mid-staff)))
	       (extra-stem (max 0 (- stem-length cur-stem))))
	  (if up-stem
	      (incf mid-staff extra-stem)
	    (decf mid-staff extra-stem))
	  mid-staff)))))

(defun draw-tremolo (score note trem slashes x0 y0 y1 stem-dir &optional (y-offset 0.0))
  ;; tremolo is the number of tremolo slashes desired.  We assume on the stem here, or on the entire note.
  ;; "note" may be a chord.
  ;; x0 y0 y1 describe the stem position
  ;; if no stem (as in whole note with tremolo), y1 is nil but stem-dir tells us where to put the slashes
  (when (plusp slashes)
    (let* ((up-stem (member stem-dir '(:up :up?)))
	   (thickness (or (and trem (thickness trem)) .08))
	   (ur-incr (- (* 2 thickness) .01))
	   (incr (if up-stem ur-incr (- ur-incr)))
	   (slant (or (and trem (slant trem)) .1))
	   (width (or (and trem (width trem)) .3))
	   (x-off (- x0 (- (* .5 width) .01)))
	   (bottom (+ (if up-stem
			  (+ y0 .2 y-offset (if trem (vis-dy trem) 0))
			(- (+ y0 (if trem (vis-dy trem) 0)) (+ .2 thickness slant y-offset)))
		      (if (= slashes 1) incr 0))))
      
      #-(or gcl sbcl) (loop for i from 0 below slashes and y-off from bottom by incr do
	      (moveto score x-off y-off)
	      (lineto score x-off (+ y-off thickness))
	      (lineto score (+ x-off width) (+ y-off thickness slant))
	      (lineto score (+ x-off width) (+ y-off slant))
	      (lineto score x-off y-off)
	      (fill-in score))
      
      #+(or gcl sbcl) (do ((i 0 (1+ i))
		 (y-off bottom (+ y-off incr)))
		((>= i slashes))
	      (moveto score x-off y-off)
	      (lineto score x-off (+ y-off thickness))
	      (lineto score (+ x-off width) (+ y-off thickness slant))
	      (lineto score (+ x-off width) (+ y-off slant))
	      (lineto score x-off y-off)
	      (fill-in score))
      
      (when (and trem (paranoid trem))
	(show score (cmn-text :letters "trem." :font-name (font-name trem) :font-scaler (font-scaler trem))
	      :matrix (translate-matrix score trem 
					(- (if up-stem x-off x0) .25)
					(+ .125 (max (+ (staff-y0 note) 1.0) y0 (or y1 0)))))))))

(defun draw-two-note-tremolo (score note tremolo)
  ;; make three "beams" between note and (tremolo-note tremolo)
  ;; set stem-direction of next note too
  (if (not (measured tremolo))
      (let* ((next-note (tremolo-note tremolo))
	     (stem-dir (audible-stem-direction note))
	     (above (member stem-dir '(:up :up?)))
	     (x0 (if (or above (>= (head-quarters note) 4)) (box-x1 note) (box-x0 note)))
	     (y0 (+ (staff-y0 note) (* (line note) *staff-line-separation*)))
	     (y1 (+ (staff-y0 note) (* (line next-note) *staff-line-separation*)))
	     (y-diff (- y1 y0))
	     (x1 (if (and above (< y-diff .5)) (box-x1 next-note) (box-x0 next-note)))
	     (y-incr (min .2 (max -.2 (* .5 y-diff))))
	     (py0 (+ y0 (vis-dy tremolo) (* .25 y-diff) (if above .25 -.4)))
	     (py1 (+ py0 y-incr))
	     (x-incr (if (>= (head-quarters note) 4) .1 0))
	     (bx0 (+ x0 x-incr (vis-dx tremolo) (if above -.05 .125)))
	     (bx1 (- (+ x1 (vis-dx tremolo)) x-incr (if above .2 .1))))
	(if (and (/= (staff-y0 note) (staff-y0 next-note))
		 (< x1 x0))
	    ;; tremolo broken across line-break
	    (let ((len (min .4 (* .5 (maximum-length tremolo))))
		  (staff-diff (- (staff-y0 note) (staff-y0 next-note))))
	      (draw-beams score bx0 py0 (+ bx0 len) py1 3 stem-dir .75)
	      (incf bx1 .05)
	      (draw-beams score (- bx1 len) (- py0 staff-diff) bx1 (- py1 staff-diff) 3 stem-dir .75))
	  (progn
	    (when (> (- bx1 bx0) (maximum-length tremolo))
	      (let ((excess (- bx1 bx0 (maximum-length tremolo))))
		(incf bx0 (* .5 excess))
		(decf bx1 (* .5 excess))))
	    (if (not (stem-is-locked next-note)) (setf (audible-stem-direction next-note) stem-dir))
	    (draw-beams score bx0 py0 bx1 py1 3 stem-dir .75))))))

#|
(cmn staff treble (c5 w (begin-tremolo (dy .5))) (bf4 w end-tremolo) (c5 w (begin-tremolo (dx .25))) (bf4 w end-tremolo)
      (c5 w begin-tremolo) (bf4 w end-tremolo) (c5 w begin-tremolo) line-break (bf4 w end-tremolo) 
      (c5 w begin-tremolo) (bf4 w end-tremolo) (c5 h begin-tremolo) (bf4 h end-tremolo)
      (c4 q (tremolo (dy .5))) (chord (notes c4 g4) q (tremolo (dx .5)))
      (e4 e (tremolo) (begin-beam)) (e4 e (tremolo (dy -0.5)) (end-beam)) (e4 e (tremolo (dy 0.5))))
|#

(defun display-note-slurs-and-brackets (object score &optional justifying)
  (if (marks object) 
      (loop for mark in (marks object) do 
	(if (and (sundry-p mark)
		 (member (sundry-name mark) '(:slur :beat)))
	    (display mark object score justifying)))))


(defun floating-note (score quarters stem-up matrix)
  ;; draw notehead with stem/flags as needed (for metronome and others) (dots too)
  (let ((nh (get-note-head quarters))
	(nflags (how-many-flags quarters))
	(ndots (how-many-dots quarters))
	(stem-length (if stem-up .7 -.7)))
    (matrix-front score matrix)
    (moveto score 0 0)
    (funcall (draw-func nh) score)
    (when (< quarters 4)
      (let ((x0 (if stem-up (- (width nh) *half-stem-width*) 0)))
	(setf (line-width score) *stem-width*)
	(moveto score x0 0)
	(rlineto score 0 stem-length)
	(draw score)
	(setf (line-width score) 0)
	(when (plusp nflags) 
	  (draw-flags score nh nflags stem-up stem-length -0.01))))
    (when (plusp ndots)
      (loop for i from 1 to ndots and xd from (* 2 *first-dot-spacing*) by *dot-spacing* do
        (cmn-dot score xd (+ *dot-vertical-spacing* .125))))
    (matrix-back score)))
  

(defmethod width ((note note-mixin)) (width (or (note-head note) (get-note-head (or (head-quarters note) (quarters note))))))

(defun collider-sign-backup (colnote)
  (if colnote
      (* (or (note-head-size colnote) 1.0) (if (note-head colnote) (width (note-head colnote)) .29))
    0.0))

(defun collision-sign-backup (colnote)
  (if (and colnote
	   (note-sign colnote))
      (+ (width (note-sign colnote)) .04)
    ;; .04 to put some space between the successive signs
    0.0))


;;; special case here -- if durs are same and stem-dirs are different and signs/lines are same,
;;; then the noteheads should overlap completely (i.e. on note head, two stems, one set of augmentation dots if any)

(defun collision-head-backup (curnote colnote)
  (if colnote
      (- (* (or (note-head-size colnote) 1.0) (if (note-head colnote) (width (note-head colnote)) .29))
	 (if (/= (line curnote) (line colnote))
	     .025
	   0))
    ;; overlap slightly if not on the same line
    0.0))

(defmethod display ((note note-mixin) chord score &optional justifying)
  ;; AV 3-Jul-00
  (let* ((line (note-line note))
	 (nflags (or (flags note) 0))
	 (dots (or (dots note) 0))
	 (nh (get-note-head (or (head-quarters note) (quarters note)) (note-head note)))
	 (note-size (* *note-head-size* (note-size note)))
	 (nsx (* (width nh) (- 1.0 note-size)))
	 (sls *staff-line-separation*)
	 (x0-note (box-x0 note))
	 (leger-len (* note-size (leger-line-length note)))
	 (leger-wid (* note-size (leger-line-width note)))
	 (dx-note (if justifying 0 (vis-dx note)))
	 (dy-note (if justifying 0 (vis-dy note)))
	 (ny0 nil))
    
    (setf (box-y0 note) (+ (staff-y0 note) (* line sls)))
    (setf ny0 (+ (box-y0 note) dy-note))
    
    (when (not justifying)
      (if (and (beams note)
	       (not (audible-p (stem-tie note))))
	  (display (beams note) note score))
      (if (ties note) (loop for tie in (ties note) do (display tie note score))))
    (when (marks note) (with-position note (+ x0-note dx-note) ny0 (display-marks note score justifying)))
    
    (if (invisible-p note)
	(if (marks note) (display-note-slurs-and-brackets note score justifying))
      (let ((old-color (color score)))
	(if (color note) (setf (color score) (color note)))
	(matrix-front score (translate-matrix score note (+ x0-note dx-note nsx) ny0))
	(moveto score 0 0)
	
	(when (note-sign note)
	  (let* ((nsgn (if (eq (note-head note) :small)
			   (if (eq (note-sign note) sharp)
			       (setf (note-sign note) small-sharp)
			     (if (eq (note-sign note) flat)
				 (setf (note-sign note) small-flat)
			       (if (eq (note-sign note) natural)
				   (setf (note-sign note) small-natural)
				 (note-sign note))))
			 (note-sign note))))

	    (let ((ax0 (- (vis-dx nsgn)
			  (+ (width nsgn)
			     (* (+ *accidental-to-note-head-space*
				   (if (or (<= line -2) (<= 10 line))
				       (if (evenp line)
					   (leger-line-breathing-space note)
					 (* .5 (leger-line-breathing-space note)))
				     0))
				note-size)
			     (collider-sign-backup (note-collider note))
			     (collision-sign-backup (note-collision note)))))
		  ;;need to move sign back to accommodate nearby unrelated notehead (and its sign, if any)
		  (ay0 (vis-dy nsgn)))
	      (if justifying (moveto score ax0 ay0))
	      ;; (cmn (size 200) treble (meter 2 4) cs4 e c4 e c4 q ef4 h gf4 e af4 e) -- force room for accidentals
	      (show score nsgn :matrix (scale-matrix (translate-matrix score nsgn ax0 ay0) note-size note-size))
	      (when (marks nsgn)      ;in parens possibly
		(with-position nsgn ax0 ay0
		  (display-marks nsgn score justifying))))))
	  
	(if (sundry-p nh)
	    (with-position note 0 0 (display nh note score justifying))
	  (show score nh :matrix (scale-matrix (translate-matrix score note 0 0) note-size note-size)))

	(when (plusp dots)
 	  (let ((dx0 (+ (width nh) *first-dot-spacing*))
		(dy0 (+ *dot-vertical-spacing* (if (oddp line) 0 sls))))
	    (loop for i from 1 to dots and xd from dx0 by *dot-spacing* do
	      (cmn-dot score xd dy0))))
	(matrix-back score)
	
	(let* ((stem-dir (or (audible-stem-direction note)
			     (if (>= line 4) 
				 :down?
			       :up?)))
	       (up-stem (member stem-dir '(:up :up?)))
	       (x0 (+ x0-note dx-note))
	       (y0 (+ (box-y0 note) dy-note))
	       (need-stem (and (or (not chord) (grace-note-p chord))
			       (not (eq (beamed note) t))
			       (not (eq (audible-stem-direction note) :none))
			       (< (or (head-quarters note) (quarters note)) 4)))
	       (y1 (if need-stem (+ (get-stem-end score note up-stem (grace-note-p chord))
				    (or (stem-dy note) 0)))))
	  (setf (stem-x0 note) (+ x0 (note-head-x0-offset note up-stem *half-stem-width*)))
	  (when need-stem
	    (if (and (/= nsx 0.0) (not up-stem)) (incf x0 nsx))
	    (let* ((stem-width *stem-width*)
		   (half-stem-width *half-stem-width*)
		   (yy0 (+ y0 (note-head-y0-offset note up-stem)))
		   (xs (+ x0 (note-head-x0-offset note up-stem half-stem-width))))
	      (if (stem-mark note) (funcall (stem-mark note) score xs yy0 y1))
 	      (draw-stem score xs yy0 y1 stem-width (pattern note)))
	    
	    (setf (audible-stem-direction note) stem-dir)
	    (when (plusp nflags) 
	      (draw-flags score note nflags up-stem y1 x0))
	    (if (not justifying)
		(setf (stem-end note) y1)
	      (if (and up-stem (flagged note))
		  ;; need to leave room for the flags when stem is up -- .45 here because in this
		  ;;   case we are positioned currently at the left side of the note head.
		  (rmoveto score (+ (* (note-head-x0-offset note up-stem 0.0) note-size) .15) 0))))
	  (if (marks note) (display-note-slurs-and-brackets note score justifying))
	  
	  (if (and (get-tremolo note) 
		   (not (audible-p (stem-tie note))))
	      (let* ((trem (get-tremolo note))
		     (type (tremolo-type trem)))
		(if (not type)
		    (draw-tremolo score note trem
				  (or (tremolo-slashes trem)
				      (- 3 nflags))
				  (+ (vis-dx trem)
				     (if (< (quarters note) 4)
					 (+ x0 (note-head-x0-offset note up-stem *half-stem-width*))
				       (+ x0 (* .5 (width whole-note)))))
				  y0 y1 stem-dir
				  (if (and (measured trem)
					   (zerop nflags))
				      .15
				    0))
		  (if (eq type :left)
		      (if justifying
			  (setf (fences note) (list .05 (* (maximum-length trem) .5)))
			(if (tremolo-note trem)
			    (draw-two-note-tremolo score note trem)))))))
	  
	  (when (or (< line -1) (> line 9))
	    (let* ((x1 (+ x0 (width nh)))
		   (yline (if (oddp line) 
			      (if (minusp line) 
				  (+ y0 sls)
				(- y0 sls))
			    y0))
		   (factor (if (minusp line) 2 -2))
		   (lines (if (oddp line) 
			      (if (plusp line) 
				  (floor (- line 9) 2)
				(floor (- (abs line) 1) 2))
			    (if (plusp line)
				(floor (- line 8) 2)
			      (floor (abs line) 2))))
	           (special-leger-fixup-case (and need-stem (/= nsx 0.0) (member stem-dir '(:down :down?))))
		   (lx0 (if special-leger-fixup-case x0 (+ x0 nsx)))
		   (lx1 (if special-leger-fixup-case (- x1 nsx) x1)))
	      (setf (line-width score) leger-wid)

	      (if (not justifying)
		  (let* ((bx (if (and up-stem (oddp line)) -.03 0))
			 (fx (if (and (not up-stem) (oddp line)) -.03 0))
			 (xx0 (- lx0 leger-len bx))
			 (xx1 (+ lx1 leger-len fx)))
		    #-(or gcl sbcl) (loop for i from 0 below lines and y1 from yline by (* factor sls) do
			    (moveto score xx0 y1)
			    (lineto score xx1 y1))
		    #+(or gcl sbcl) (do ((i 0 (1+ i))
			       (y1 yline (+ y1 (* factor sls))))
			      ((>= i lines))
			    (moveto score xx0 y1)
			    (lineto score xx1 y1))
		    (draw score))
		(progn ;make sure we leave room for the leger line
		  (moveto score (- lx0 leger-len) yline)
		  (lineto score (+ lx1 leger-len (leger-line-breathing-space note)) yline)))
	      (setf (line-width score) 0))))
	(if (color note) (setf (color score) old-color))))
    (* .5 (width nh))))


#|
(cmn (size 100) cs4 q bf3 q ef4 q fs5 q af5 q bf5 q)
;;; note-head size tests
(cmn (note-head-size .75) staff treble f5 w fs5 h c5 w cs5 h f4 w fs4 h c4 w cs4 h)
(cmn (note-head-size 1.5) staff treble f5 w fs5 h c5 w cs5 h f4 w fs4 h c4 w cs4 h)
(cmn (note-head-size 1.5) staff treble f5 e fs5 e c5 e cs5 e f4 e fs4 e c4 e cs4 e)
(cmn (note-head-size 1.5) staff treble c6 e c6 e c6 q a3 q a3 e a3 e)
(cmn (note-head-size 1.5) staff treble (chord (notes c5 g5 c6) h) (chord (notes a3 d4) h) 
  (chord (notes c5 c6) e) (chord (notes c5 c6) e) (chord (notes a3 e4) e) (chord (notes a3 e4) e))
(cmn (note-head-size 1.5) (grace-note-size 1.0) staff treble (g4 h (appoggiatura (notes bn4 cs5))) 
     (bn4 e stem-up) (cs5 e stem-up) g4 h)
(cmn (size 200) (note-head-size 1.3) staff treble (c5 q stem-up) (b4 q (sign flat (dx -.1)) stem-down (onset 0)))
(cmn staff treble (c4 q (sharp invisible)))

;;; collisions:
(cmn (size 30) 
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
	    c4 e stem-down (df4 e (onset 1) stem-up) cs4 e stem-down (ds4 e (onset 1.5) stem-up)))
|#

(defvar note-walls '(.05 .05))
(defvar note-fences '(.05 .05))
(defvar note-expanders '(2 3))

(defmethod house ((note note) score)
  (let ((old-output (output score))
	(note-center 0))
    (clear-score score)
    (if (eq (flagged note) :none) (setf (flagged note) nil))
    (setf note-center (display note nil score t))
    (setf (output score) old-output)
    (when (not (eq (visible-justification note) :none))
      (copy-bounding-box score note)
      (setf (center note) note-center)
      (if (not (walls note)) (setf (walls note) note-walls))
      (if (not (fences note)) (setf (fences note) note-fences))
      (if (not (expanders note)) (setf (expanders note) note-expanders)))
    (when (audible-p (stem-tie note))
      (align-tied-notes score note))))
      

(defmacro define-note (name c-class p-class &optional octave sign)
  `(progn
     (defvar ,name (make-instance 'write-protected-note :cclass ,c-class :pitch ,p-class :octave ,octave :sign ,sign))
     (defun ,name (&rest objects) (apply #'ur-note (copy ,name) objects))))

(define-note cf0 0 -1 0 flat) (define-note c0 0 0 0) (define-note cn0 0 0 0 natural) (define-note cs0 0 1 0 sharp)
(define-note df0 1 1 0 flat)  (define-note d0 1 2 0) (define-note dn0 1 2 0 natural) (define-note ds0 1 3 0 sharp)
(define-note ef0 2 3 0 flat)  (define-note e0 2 4 0) (define-note en0 2 4 0 natural) (define-note es0 2 5 0 sharp)
(define-note ff0 3 4 0 flat)  (define-note f0 3 5 0) (define-note fn0 3 5 0 natural) (define-note fs0 3 6 0 sharp)
(define-note gf0 4 6 0 flat)  (define-note g0 4 7 0) (define-note gn0 4 7 0 natural) (define-note gs0 4 8 0 sharp)
(define-note af0 5 8 0 flat)  (define-note a0 5 9 0) (define-note an0 5 9 0 natural) (define-note as0 5 10 0 sharp)
(define-note bf0 6 10 0 flat) (define-note b0 6 11 0)(define-note bn0 6 11 0 natural)(define-note bs0 6 12 0 sharp)

(define-note cf1 0 -1 1 flat) (define-note c1 0 0 1) (define-note cn1 0 0 1 natural) (define-note cs1 0 1 1 sharp)
(define-note df1 1 1 1 flat)  (define-note d1 1 2 1) (define-note dn1 1 2 1 natural) (define-note ds1 1 3 1 sharp)
(define-note ef1 2 3 1 flat)  (define-note e1 2 4 1) (define-note en1 2 4 1 natural) (define-note es1 2 5 1 sharp)
(define-note ff1 3 4 1 flat)  (define-note f1 3 5 1) (define-note fn1 3 5 1 natural) (define-note fs1 3 6 1 sharp)
(define-note gf1 4 6 1 flat)  (define-note g1 4 7 1) (define-note gn1 4 7 1 natural) (define-note gs1 4 8 1 sharp)
(define-note af1 5 8 1 flat)  (define-note a1 5 9 1) (define-note an1 5 9 1 natural) (define-note as1 5 10 1 sharp)
(define-note bf1 6 10 1 flat) (define-note b1 6 11 1)(define-note bn1 6 11 1 natural)(define-note bs1 6 12 1 sharp)

(define-note cf2 0 -1 2 flat) (define-note c2 0 0 2) (define-note cn2 0 0 2 natural) (define-note cs2 0 1 2 sharp)
(define-note df2 1 1 2 flat)  (define-note d2 1 2 2) (define-note dn2 1 2 2 natural) (define-note ds2 1 3 2 sharp)
(define-note ef2 2 3 2 flat)  (define-note e2 2 4 2) (define-note en2 2 4 2 natural) (define-note es2 2 5 2 sharp)
(define-note ff2 3 4 2 flat)  (define-note f2 3 5 2) (define-note fn2 3 5 2 natural) (define-note fs2 3 6 2 sharp)
(define-note gf2 4 6 2 flat)  (define-note g2 4 7 2) (define-note gn2 4 7 2 natural) (define-note gs2 4 8 2 sharp)
(define-note af2 5 8 2 flat)  (define-note a2 5 9 2) (define-note an2 5 9 2 natural) (define-note as2 5 10 2 sharp)
(define-note bf2 6 10 2 flat) (define-note b2 6 11 2)(define-note bn2 6 11 2 natural)(define-note bs2 6 12 2 sharp)

(define-note cf3 0 -1 3 flat) (define-note c3 0 0 3) (define-note cn3 0 0 3 natural) (define-note cs3 0 1 3 sharp)
(define-note df3 1 1 3 flat)  (define-note d3 1 2 3) (define-note dn3 1 2 3 natural) (define-note ds3 1 3 3 sharp)
(define-note ef3 2 3 3 flat)  (define-note e3 2 4 3) (define-note en3 2 4 3 natural) (define-note es3 2 5 3 sharp)
(define-note ff3 3 4 3 flat)  (define-note f3 3 5 3) (define-note fn3 3 5 3 natural) (define-note fs3 3 6 3 sharp)
(define-note gf3 4 6 3 flat)  (define-note g3 4 7 3) (define-note gn3 4 7 3 natural) (define-note gs3 4 8 3 sharp)
(define-note af3 5 8 3 flat)  (define-note a3 5 9 3) (define-note an3 5 9 3 natural) (define-note as3 5 10 3 sharp)
(define-note bf3 6 10 3 flat) (define-note b3 6 11 3)(define-note bn3 6 11 3 natural)(define-note bs3 6 12 3 sharp)

(define-note cf4 0 -1 4 flat) (define-note c4 0 0 4) (define-note cn4 0 0 4 natural) (define-note cs4 0 1 4 sharp)
(define-note df4 1 1 4 flat)  (define-note d4 1 2 4) (define-note dn4 1 2 4 natural) (define-note ds4 1 3 4 sharp)
(define-note ef4 2 3 4 flat)  (define-note e4 2 4 4) (define-note en4 2 4 4 natural) (define-note es4 2 5 4 sharp)
(define-note ff4 3 4 4 flat)  (define-note f4 3 5 4) (define-note fn4 3 5 4 natural) (define-note fs4 3 6 4 sharp)
(define-note gf4 4 6 4 flat)  (define-note g4 4 7 4) (define-note gn4 4 7 4 natural) (define-note gs4 4 8 4 sharp)
(define-note af4 5 8 4 flat)  (define-note a4 5 9 4) (define-note an4 5 9 4 natural) (define-note as4 5 10 4 sharp)
(define-note bf4 6 10 4 flat) (define-note b4 6 11 4)(define-note bn4 6 11 4 natural)(define-note bs4 6 12 4 sharp)

(define-note cf5 0 -1 5 flat) (define-note c5 0 0 5) (define-note cn5 0 0 5 natural) (define-note cs5 0 1 5 sharp)
(define-note df5 1 1 5 flat)  (define-note d5 1 2 5) (define-note dn5 1 2 5 natural) (define-note ds5 1 3 5 sharp)
(define-note ef5 2 3 5 flat)  (define-note e5 2 4 5) (define-note en5 2 4 5 natural) (define-note es5 2 5 5 sharp)
(define-note ff5 3 4 5 flat)  (define-note f5 3 5 5) (define-note fn5 3 5 5 natural) (define-note fs5 3 6 5 sharp)
(define-note gf5 4 6 5 flat)  (define-note g5 4 7 5) (define-note gn5 4 7 5 natural) (define-note gs5 4 8 5 sharp)
(define-note af5 5 8 5 flat)  (define-note a5 5 9 5) (define-note an5 5 9 5 natural) (define-note as5 5 1 5 sharp)
(define-note bf5 6 10 5 flat) (define-note b5 6 11 5)(define-note bn5 6 11 5 natural)(define-note bs5 6 12 5 sharp)

(define-note cf6 0 -1 6 flat) (define-note c6 0 0 6) (define-note cn6 0 0 6 natural) (define-note cs6 0 1 6 sharp)
(define-note df6 1 1 6 flat)  (define-note d6 1 2 6) (define-note dn6 1 2 6 natural) (define-note ds6 1 3 6 sharp)
(define-note ef6 2 3 6 flat)  (define-note e6 2 4 6) (define-note en6 2 4 6 natural) (define-note es6 2 5 6 sharp)
(define-note ff6 3 4 6 flat)  (define-note f6 3 5 6) (define-note fn6 3 5 6 natural) (define-note fs6 3 6 6 sharp)
(define-note gf6 4 6 6 flat)  (define-note g6 4 7 6) (define-note gn6 4 7 6 natural) (define-note gs6 4 8 6 sharp)
(define-note af6 5 8 6 flat)  (define-note a6 5 9 6) (define-note an6 5 9 6 natural) (define-note as6 5 10 6 sharp)
(define-note bf6 6 10 6 flat) (define-note b6 6 11 6)(define-note bn6 6 11 6 natural)(define-note bs6 6 12 6 sharp)

(define-note cf7 0 -1 7 flat) (define-note c7 0 0 7) (define-note cn7 0 0 7 natural) (define-note cs7 0 1 7 sharp)
(define-note df7 1 1 7 flat)  (define-note d7 1 2 7) (define-note dn7 1 2 7 natural) (define-note ds7 1 3 7 sharp)
(define-note ef7 2 3 7 flat)  (define-note e7 2 4 7) (define-note en7 2 4 7 natural) (define-note es7 2 5 7 sharp)
(define-note ff7 3 4 7 flat)  (define-note f7 3 5 7) (define-note fn7 3 5 7 natural) (define-note fs7 3 6 7 sharp)
(define-note gf7 4 6 7 flat)  (define-note g7 4 7 7) (define-note gn7 4 7 7 natural) (define-note gs7 4 8 7 sharp)
(define-note af7 5 8 7 flat)  (define-note a7 5 9 7) (define-note an7 5 9 7 natural) (define-note as7 5 10 7 sharp)
(define-note bf7 6 10 7 flat) (define-note b7 6 11 7)(define-note bn7 6 11 7 natural)(define-note bs7 6 12 7 sharp)

(define-note cf8 0 -1 8 flat) (define-note c8 0 0 8) (define-note cn8 0 0 8 natural) (define-note cs8 0 1 8 sharp)
(define-note df8 1 1 8 flat)  (define-note d8 1 2 8) (define-note dn8 1 2 8 natural) (define-note ds8 1 3 8 sharp)
(define-note ef8 2 3 8 flat)  (define-note e8 2 4 8) (define-note en8 2 4 8 natural) (define-note es8 2 5 8 sharp)
(define-note ff8 3 4 8 flat)  (define-note f8 3 5 8) (define-note fn8 3 5 8 natural) (define-note fs8 3 6 8 sharp)
(define-note gf8 4 6 8 flat)  (define-note g8 4 7 8) (define-note gn8 4 7 8 natural) (define-note gs8 4 8 8 sharp)
(define-note af8 5 8 8 flat)  (define-note a8 5 9 8) (define-note an8 5 9 8 natural) (define-note as8 5 10 8 sharp)
(define-note bf8 6 10 8 flat) (define-note b8 6 11 8)(define-note bn8 6 11 8 natural)(define-note bs8 6 12 8 sharp)

(defun place-of-note-on-clef (note clef) ;note is '(pitch-class octave sign bare-note-name ...)
  (let* ((clef-relative-octave (- (octave note)
				  (floor (clef-base-pitch clef) 12)))
	 (cclass (cclass note)))
    ;; clef goes from -1 to 9 -- above or below we need leger lines or octave signs
    (+ (* 7 clef-relative-octave) 
       (- cclass (clef-base-line-note clef)))))

(defun place-of-note-given-note (known-note unknown-note)
  ;; given line number of known-note, find (and set) line number of unknown-note
  (let ((line (note-line known-note))
	(oct-off (- (octave unknown-note) (octave known-note)))
	(cls-off (- (cclass unknown-note) (cclass known-note))))
    (+ line cls-off (* 7 oct-off))))




;;;
;;; ----------------    chords
;;;

(defclass chord (audible rhythm staff-relative-mixin score-object)
  ((data :initarg :data :initform nil :accessor chord-data)
   (note-head :initarg :note-head :initform nil :accessor note-head)
   (stem-dy :initarg :stem-dy :initform nil :accessor stem-dy)
   (stem-x0 :initarg :stem-x0 :initform nil :accessor stem-x0)
   (stem-mark :initarg :stem-mark :initform nil :accessor stem-mark)
   (stem-tie :initarg :stem-tie :initform nil :accessor stem-tie)
   (note-head-size :initarg :note-head-size :initform 1.0 :accessor note-head-size)))

(defmethod chord-p ((obj t)) nil)
(defmethod chord-p ((obj chord)) t)

(defmethod descry ((chord chord) &optional stream controller)
  (format stream "(chord~A~A~A~A~A~A)"
	  (if (next-method-p) (call-next-method chord stream (or controller chord)) "")
	  (if (note-head chord) (format nil " :note-head :~(~A~)" (note-head chord)) "")
	  (if (stem-dy chord) (format nil " :stem-dy ~A" (stem-dy chord)) "")
	  (if (stem-x0 chord) (format nil " :stem-x0 ~A" (stem-x0 chord)) "")
	  (if (stem-tie chord) (format nil " :stem-tie '~A" (stem-tie chord)) "")
	  (if (chord-data chord) (format nil " :data (list~{~%              ~A~})" 
					 (loop for note in (chord-data chord) collect (descry note))) 
	    "")))

(defmethod brief-identify ((chord chord))
  (format nil "(chord ~A~A~A)"
	  (if (not (halve-duration chord))
	      (rhythm-name (quarters chord))
	    (rhythm-name (* 2 (quarters chord))))
	  (if (onset chord) (format nil " (onset ~A)" (fratify (onset chord))) "")
	  (if (chord-data chord)
	      (format nil " (notes~{ ~A~})"
		      (loop for note in (chord-data chord)
		       collect (note-name note)))
	    "")))

(defmethod identify ((chord chord))
  (format nil "(chord ~A~A~A~A~A~A~A~A~A~A~A~A)"
	  (if (not (halve-duration chord))
	      (rhythm-name (quarters chord))
	    (rhythm-name (* 2 (quarters chord))))
	  (if (onset chord) (format nil " (onset ~A)" (fratify (onset chord))) "")
	  (if (chord-data chord)
	      (format nil " (notes~{ ~A~})"
		      (loop for note in (chord-data chord)
		       collect (if (or (marks note) (store-data note) (ties note))
				   (identify note) 
				 (note-name note))))
	    "")
	  (if (and (note-head chord)
		   (not (eq (note-head chord) :normal)))
	      (format nil " (note-head :~(~A~))" (note-head chord))
	    "")
	  (if (eq (stem-direction chord) :up) " stem-up"
	    (if (eq (stem-direction chord) :down) " stem-down"
	      (if (eq (stem-direction chord) :none) " no-stem"
		"")))
	  (if (stem-dy chord) (format nil " (stem-dy ~A)" (stem-dy chord)) "")
	  (if (audible-tie-direction chord) (format nil " (tie-direction :~(~A~))" (audible-tie-direction chord)) "")
	  (if (audible-slur-direction chord) (format nil " (slur-direction :~(~A~))" (audible-slur-direction chord)) "")
	  (if (store-data chord) (identify-store-data chord) "")
	  (if (ties chord) (format nil "~{ ~(~A~)~}" (sorted-ties (ties chord))) "")
	  (if (get-tremolo chord) (identify (get-tremolo chord)) "")
	  (the-usual-suspects chord)))

(defmethod copy ((chord chord) &optional object)
  (let ((new-chord (if (not object) (make-instance 'chord)
		     (if (write-protected object) (copy object)
		       object))))
    (if (chord-data chord) (setf (chord-data new-chord) (loop for note in (chord-data chord) collect (copy note))))
    (setf (note-head new-chord) (note-head chord))
    (if (next-method-p) (call-next-method chord new-chord))
    new-chord))

(defun copy-chord-for-ties (chord)
  (let ((new-chord (make-instance 'chord)))
    (setf (dots new-chord) (dots chord))
    (setf (flags new-chord) (flags chord))
    (setf (quarters new-chord) (quarters chord))
    (setf (beamed new-chord) (beamed chord))
    (setf (flagged new-chord) (flagged chord))
    (setf (octaved new-chord) (octaved chord))
    (setf (note-head new-chord) (note-head chord))
    (setf (stem-dy new-chord) (stem-dy chord))
    (setf (stem-x0 new-chord) (stem-x0 chord))
    (setf (stem-tie new-chord) (copy-list (stem-tie chord)))
    (setf (stem-mark new-chord) (stem-mark chord))
    (if (get-tremolo chord) (setf (get-tremolo new-chord) (copy (get-tremolo chord))))
    (setf (audible-stem-direction new-chord) (audible-stem-direction chord))
    (setf (audible-tie-direction new-chord) (audible-tie-direction chord))
    (setf (audible-slur-direction new-chord) (audible-slur-direction chord))
    (setf (chord-data new-chord) (loop for note in (chord-data chord) 
				  collect (let ((n (copy note)))
					    (setf (ties n) nil)
					    n)))
    (loop for note in (chord-data new-chord) do 
      (push (list :sign (note-sign note)) (store-data note))
      (setf (onset note) nil)
      (setf (note-sign note) nil))
    new-chord))

(defmethod short-note ((chord chord) &key x0)
  (make-instance 'chord 
    :x0 (or x0 (box-x0 chord)) :x1 (box-x1 chord) :y0 (box-y0 chord) :y1 (box-y1 chord)
    :staff-y0 (staff-y0 chord) :stem-direction (audible-stem-direction chord)
    :tie-direction (audible-tie-direction chord) :slur-direction (audible-slur-direction chord)
    :center (center chord) :ties (if (ties chord) t nil) :quarters (quarters chord)
    :stem-end (stem-end chord) 
    :data (loop for note in (chord-data chord) collect (short-note note))))

(defmethod add-to-marks ((chord chord) objects) (setf (marks chord) (append (marks chord) objects)))
(defmethod add-to-ties ((chord chord) object) (push object (ties chord)))
(defmethod add-to-slurs ((chord chord) objects) (push objects (slurs chord)))

(defun chord (&rest objects)
  (let ((notes nil)
	(new-chord (make-instance 'chord))
	(object nil))
    (when (chord-p (first objects))
      (setf new-chord (pop objects)))
    (setf *cmn-owning-object* new-chord)
    (loop while objects do
      (setf object (pop objects))
      (setf *cmn-object* object)
      (when object
	(if (self-acting-p object)
	    (funcall (action object) new-chord (argument object))
	  (if (note-p object)
	      (push (if (write-protected object) (copy object) object) notes)
	    (if (rhythm-p object)
		(rcopy object new-chord)
	      (if (score-object-p object)
		  (if (or (sundry-p object) (pause-p object) (dynamics-p object))
		      (add-to-marks new-chord (list (if (write-protected object) (copy object) object)))
		    (copy object new-chord))
		(if (score-object-list-p object)
		    (setf objects (append (disgorge object) objects))
		  (if (visible-p object)
		      (add-to-marks new-chord (list object))
		    (cmn-warn "odd argument to chord: ~A" object)))))))))
    (if notes (setf (chord-data new-chord) (nreverse notes)))
    (if (note-head new-chord)
	(loop for note in (chord-data new-chord) do (setf (note-head note) (note-head new-chord))))
    (when (and (not (quarters new-chord))
	       (not (duration new-chord)))
      (let ((fnote (or (find-if-not #'null (chord-data new-chord) :key #'quarters)
		       (first (chord-data new-chord)))))
	(setf (quarters new-chord) (quarters fnote))
	(setf (odb-duration new-chord) (odb-duration fnote))
	(setf (odb-onset new-chord) (or (odb-onset new-chord) (odb-onset fnote)))
	(setf (dots new-chord) (dots fnote))
	(setf (flags new-chord) (flags fnote))))
    (loop for note in (chord-data new-chord) do
      (when (not (odb-onset note))	;might have been a notes list (was quarters -- defaults to 1)
	(setf (quarters note) (quarters new-chord))
	(setf (odb-duration note) (odb-duration new-chord))
	(setf (odb-onset note) (odb-onset new-chord)))
      (if (not (note-head-size note)) (setf (note-head-size note) (note-head-size new-chord)))
      (setf (dots note) (dots new-chord))
      (setf (flags note) (flags new-chord)))
    (setf (octaved new-chord) (or (octaved new-chord) octave-stack))
    (setf (flagged new-chord) (and (not (flagged new-chord)) (not beam-stack) (> (flags new-chord) 0)))
    (if (not (beamed new-chord)) (setf (beamed new-chord) (and (> (flags new-chord) 0) beam-stack)))
    (cmn-tick-pipe new-chord)
    new-chord))

(defun ur-chord (new-note &rest ur-objects)
  (apply #'chord new-note ur-objects))

(defun notes (&rest ns) (engorge ns))

(defmethod notify ((chord chord) &optional objects)
  (apply #'chord chord objects))

(defmethod (setf sign) (val (chord chord))
  (loop for note in (chord-data chord) do
    (setf (note-sign note) val)))

(defmethod sign ((chord chord) &rest args)
  (declare (ignore args))
  (let ((note (find-if-not #'null (chord-data chord) :key #'sign)))
    (and note (note-sign note))))

(defmethod line ((chord chord))
  (loop for note in (chord-data chord) collect (note-line note)))

(defmethod maximum-line ((chord chord)) (or (loop for note in (chord-data chord) maximize (note-line note)) 0))
(defmethod minimum-line ((chord chord)) (or (loop for note in (chord-data chord) minimize (note-line note)) 0))

(defmethod head-line ((chord chord))
  (if (audible-stem-direction chord)
      (if (member (audible-stem-direction chord) '(:up :up?))
	  (minimum-line chord)
	(maximum-line chord))
    (if (> (abs (- (maximum-line chord) 4)) (- 4 (minimum-line chord)))
	(maximum-line chord)
      (minimum-line chord))))

(defmethod (setf rhythm) (ur-val (chord chord))
  (let ((val (fratify ur-val)))
    (setf (odb-duration chord) val)
    (setf (dots chord) (how-many-dots val))
    (setf (flags chord) (how-many-flags val))
    (setf (quarters chord) val)
    (loop for note in (chord-data chord) do
      (setf (rhythm note) val)
      (setf (dots note) 0))))

(defun displace-chord (ordered-chord-1 lines-1 stem-direction)
  (let* ((stem-up (member stem-direction '(:up :up?)))
	 (normal (if stem-up :left :right))
	 (other (if stem-up :right :left))
	 (displaced normal)
	 (last-line nil)
	 (ordered-chord (if stem-up (reverse ordered-chord-1) ordered-chord-1))
	 (lines (if stem-up (reverse lines-1) lines-1))
	 (chords (loop for note in ordered-chord and 
		  line in lines
		  do (progn
		       (if (and last-line 
				(eq displaced normal)
				(or (= line last-line) 
				    (and stem-up
					 (= line (1+ last-line)))
				    (and (not stem-up)
					 (= (1+ line) last-line))))
			   (setf displaced other)
			 (setf displaced normal))
		       (setf last-line line))
		   ;; eventually we should probably respell the thing if on the same line and not the same sign
		  collect (list 0 displaced line (note-sign note) note))))
    (if stem-up (nreverse chords) chords)))

(defun accident-chord (chord stem-direction)
  ;; chord is a list of notes made up of "0", note-head displacements, line numbers, and the signs [original note values]
  ;; here we decide where to put the accidentals so that they don't collide with each other or with the note heads.
  ;; we return the old list with a new first element = accidental displacement levels (i.e. sign motion backwards)
  ;; problem is that the accidental signs are different sizes.
  ;; basic algorithm is that a seventh gives a room on any given layer of accidentals, layers can be any depth
  ;; a "seventh" is (oddly enough) a difference of 6 in line numbers
  ;; return the max (left) displacement and the nominal true-center displacement (where the note heads go modulo seconds)
  ;; each of the possible cases of collision has a different level map shape (i.e. flats go down very little, but
  ;;   sharps go down a lot, so we have different ways of deciding when a level is free)
  (let ((got-signs (some #'fourth chord)))
    (if (not got-signs)
	chord
      (let* ((all-done nil)
	     (level 1)
	     (notes (length chord))
	     (level-map (make-array notes :element-type 'float :initial-element 1.0))
	     (lines (make-array notes :element-type 'fixnum :initial-contents (map 'list #'third chord))))
	
	;; map out collisions with note heads on the "wrong" side of a downward stem (getting in accidental's way)
	(loop for note-data in chord and i from 0 do
	  (if (and (member stem-direction '(:down :down? :none))
		   (eq (second note-data) :left))
	      (loop for k from 0 below notes do
	        (let ((diff (abs (- (aref lines k) (aref lines i)))))
		  (if (or (= diff 3) (= diff -4))
		      (setf (aref level-map k) (max (aref level-map k) 1.5))
		    (if (< diff 3)
			(setf (aref level-map k) 2.0)))))))
	
	;; an accidental fits into a level if the current level-map is less than or equal to the current level
	(loop until all-done do		;keep filling levels until all signs dealt with 
	  (let ((unset-signs nil))
	    (loop for note-data in chord and i from 0 do
	      (let ((sign (fourth note-data)))
		(when (and sign		;there's an accidental
			   (zerop (first note-data))) ; that hasn't yet been placed
		  (if (<= (aref level-map i) level) ; and we have room on this level
		      (progn
			(setf (first note-data) level)
			(if (member sign flats)
			    (loop for k from 0 below notes do
			      (let ((diff (- (aref lines k) (aref lines i))))
				(if (or (= diff -3) (= diff -4) (= diff -5))
				    (let ((sign-below (fourth (nth k chord))))
				      (if (and (member sign-below sharps)
					       (/= diff -5))
					  (setf (aref level-map k) (max (aref level-map k) (+ level 1.0)))
					(setf (aref level-map k) (max (aref level-map k) (+ level .5)))))
				  (if (< (abs diff) 5)
				      (setf (aref level-map k) (max (aref level-map k) (+ level 1.0)))))))
			  (if (member sign sharps)
			      (loop for k from 0 below notes do
				(let ((diff (abs (- (aref lines k) (aref lines i)))))
				  (if (< diff 6)
				      (setf (aref level-map k) (max (aref level-map k) (+ level 1.0))))))
			    (if (member sign naturals)
				(loop for k from 0 below notes do
				  (let ((diff (- (aref lines k) (aref lines i))))
				    (if (or (= diff -4) (= diff -5))
					(setf (aref level-map k) (max (aref level-map k) (+ level .5)))
				      (if (< (abs diff) 6)
					  (setf (aref level-map k) (max (aref level-map k) (+ level 1.0)))))))
			      (if (member sign double-sharps)
				  (loop for k from 0 below notes do
				    (let ((diff (abs (- (aref lines k) (aref lines i)))))
				      (if (< diff 4)
					  (setf (aref level-map k) (max (aref level-map k) (+ level 1.0))))))
				(if (member sign double-flats)
				    (loop for k from 0 below notes do
				      (let ((diff (- (aref lines k) (aref lines i))))
					(if (or (= diff 3) (= diff 4) (= diff -3))
					    (setf (aref level-map k) (max (aref level-map k) (+ level 1.5)))
					  (if (< (abs diff) 5)
					      (setf (aref level-map k) (max (aref level-map k) (+ level 2.0)))))))))))))
		    (setf unset-signs t)))))
	    (setf all-done (not unset-signs))
	    (incf level .5)))
	chord))))

#|
(cmn (size 100) staff treble (chord q (notes ds4 gf4)) (chord q (notes df4 gf4)) (chord q (notes dn4 gf4))) 
(cmn 
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
	(chord (notes ds4 gs4 gn5 an5 bn5 cs7) q no-stem))) ;12
|#


(defun staff-pitch-> (note1 note2)
  ;; problem here is not which note has the higher pitch but which should be displayed higher on
  ;; the staff (consider a chord of bs4 and cf5).
  ;; (cmn staff treble (chord q (notes c4 bs4 cf5)))
  (or (> (octave note1) (octave note2))
      (and (= (octave note1) (octave note2))
	   (> (cclass note1) (cclass note2)))
      (and (= (octave note1) (octave note2))
	   (= (cclass note1) (cclass note2))
	   (> (pitch note1) (pitch note2)))))

#|
(cmn (automatic-ties nil)
     (staff treble
            (chord (notes f4 g4) e.)
            (chord (notes f4 g4 a4) e. (dy 2.0)
                   (tremolo)   ; does not work right
                   ) 
            (chord (notes e4 f4 g4 a4) q. (accent ) (tremolo )  (dx 2.0))
            (chord (notes e4 f4 g4) q. (accent)  (dy 2))))
|#

(defmethod display ((chord  chord) container score &optional justifying)
  ;; chord is a list of notes -- each note is a list just like place-note takes.
  ;; return the x0 loc relative to px0 of the main notes (for multi-staff alignment) and the bounding box of the chord
  (let* ((stf-y0 (staff-y0 chord))
	 (lines (line chord))
	 (dx-chord (if justifying 0 (vis-dx chord)))
	 (dy-chord (if justifying 0 (vis-dy chord)))
	 (x0 (+ (box-x0 chord) dx-chord))
	 (y0 (+ stf-y0 dy-chord))
	 ;; stem direction determined by note furthest from staff (in LINES)
	 (max-line (or (loop for line in lines maximize line) 0))
	 (min-line (or (loop for line in lines minimize line) 0))
	 ;; "4" is the center line of the staff
	 (direction-stem (or (audible-stem-direction chord)
			     (if (> (abs (- max-line 4)) (- 4 min-line)) :down? :up?)))
	 (up-stem (member direction-stem '(:up :up?)))
	 (down-stem (not up-stem))
	 (stem-start (if down-stem max-line min-line))
	 (stem-last-note (if down-stem min-line max-line))
	 (stem-extra (if (grace-note-p container) 4 (round (* 12 *ideal-stem-length*))))
					; i.e. 3 staff spaces = 6 "lines" extra on stem after last note
	 (stem-end-1 (if down-stem 
			 (- stem-last-note stem-extra) 
		       (+ stem-last-note stem-extra)))
	 ;; so stem goes from line STEM-START to STEM-END-1 relative to current staff
	 ;; this has not yet taken flags or leger lines into account (may be lengthened a lot)
	 ;; if max-line is > 10 or min-line < -2 we have leger lines, and the stem has to reach the mid line (4)
	 (stem-end-2 (if (and up-stem 
			      (< min-line -2))
			 (max stem-end-1 4)
		       (if (and down-stem
				(> max-line 10))
			   (min stem-end-1 4)
			 stem-end-1)))
	 ;; and room for flags...
	 
	 (true-head (get-note-head (quarters chord) (note-head (first (chord-data chord)))))
	 (nflags (or (flags chord) 0))
	 (ntrem-1 (or (and (get-tremolo chord) (or (tremolo-slashes (get-tremolo chord)) (- 3 nflags))) 0))
	 ;; the tremolo slashes stem fixup is later multiplied by *staff-line-separation* which depends on
	 ;; .15 (?) being close to .126 -- if user has set a new line separation we need to fixup the nterm amounts
	 ;; similarly, I guess for nflags (wotta mess!)
	 ;; or rather (- (* 2 (thickness tremolo)) .01) (normally .15)
	 (ntrem (* ntrem-1 (/ .126 *staff-line-separation*)))
	 ;; but the stem-end still looks silly in a case like this (see below) because it's assuming 5-line staves etc
	 ;; (cmn (size 24) staff (staff-lines 2) (line-separation 0.5) (chord q e4 (tremolo (tremolo-slashes 2))))
	 (note-size (* *note-head-size* (note-head-size chord)))
	 (stem-end (if (and (< nflags 2) (zerop ntrem))
		       stem-end-2
		     (if up-stem
			 (max stem-end-2 (+ stem-end-1 nflags ntrem))
		       (min stem-end-2 
			    (- stem-end-1 nflags ntrem 
			       (if (and (plusp nflags)
					(> note-size 1.1))
				   (* .5 (- note-size 1.1))
				 0))))))
         ;; (min stem-end-2 (- stem-end-2 nflags ntrem)) ; is this right?  It looks silly...
	 
	 (ordered-chord (sort (copy-list (chord-data chord)) #'staff-pitch->))
	 (ordered-lines (loop for note in ordered-chord collect (note-line note)))
	 ;; looks like best way to plot a chord is to start at the top and work down.
	 ;; we need to make space for accidentals and noteheads on the left (seconds)
	 ;; there can be duplicate notes, or notes on the same staff line (or space), 
	 ;; and dots may have to be displaced down rather than up.
	 (displaced-chord (displace-chord ordered-chord ordered-lines direction-stem))
	 (accidentals (accident-chord displaced-chord direction-stem))
	 (normal-note-side (if up-stem :left :right))
	 (other-note-side (if (eq normal-note-side :right) :left :right))
	 (head-width (width true-head))
	 (nsx (* .3 (- 1.0 note-size)))	;remember this is negative if note head is bigger!
	 (stem-half-width (if (>= (quarters chord) 4) 0.0 *half-stem-width*))
	 (max-displacement (or (loop for note in accidentals maximize (first note)) 0))
	 (min-displacement (or (loop for note in accidentals minimize (first note)) 0))
         (sign-displacement-right (* .25 max-displacement))
	 (sign-breathing-room (if (zerop max-displacement) 0.0 *accidental-to-note-head-space*))
	 (mark-displacement-right (if (marks chord)
				      (if (find-if #'arrow-p (marks chord))
					  .2
					(if (find-if #'arpeggio-p (marks chord))
					    .25
					  0))
				    0))
	 (normal-head-x0 (+ x0 
			    sign-displacement-right sign-breathing-room 
			    mark-displacement-right
			    (if up-stem
				0.0
			      (+ stem-half-width
				 (if (and (zerop sign-displacement-right)
					  (find other-note-side accidentals :key #'second))
				     head-width
				   0.0)))))
	 (other-head-x0 (+ normal-head-x0
			   (if (>= (quarters chord) 4)
			       (if up-stem
				   (- head-width nsx .075)
				 (- (+ .075 nsx) head-width))
			     (if up-stem
				 (- head-width nsx stem-half-width) ;used to be (+ stem-half-width head-width)
			       (- nsx head-width)))))
	 (other-note-x0-if-in-use-and-on-right (and (eq other-note-side :right) 
						    (find :right displaced-chord :key #'second)
						    other-head-x0)))

    (setf (audible-stem-direction chord) direction-stem)
    
    (setf (box-y0 chord) (+ (staff-y0 chord) (* *staff-line-separation* min-line)))
    
    (if (and (ties chord) (not (fences chord)) justifying) 
	(setf (fences chord) (list .05 .25)))

    (when (not justifying)
      (if (and (beams chord) 
	       (not (audible-p (stem-tie chord))))
	  (display (beams chord) chord score))
      (if (ties chord)			
	  (loop for tie in (ties chord) do 
	    (display-tied-chord score chord tie
				accidentals 
				(min normal-head-x0 other-head-x0)
				(max normal-head-x0 other-head-x0)
				direction-stem ordered-lines min-line max-line)))
      
      (if (marks chord)
          (loop for mrk in (marks chord) do
                   (incf (vis-dy mrk)  (+ dy-chord
                                          (if up-stem
                                              (* ntrem *staff-line-separation*)
                                            0)))
                   (incf (vis-dx mrk) dx-chord))))
      
    (if (marks chord) (display-marks chord score justifying))

    ;; put in stem and flags (if not beaming)
    (let ((old-color (color score)))
      (if (color chord) (setf (color score) (color chord)))

      (let* ((bm-x0 (if up-stem 
			(+ other-head-x0 nsx stem-half-width) ;(if (stem-tie chord) 0.0 stem-half-width)
		      (+ normal-head-x0 nsx stem-half-width))))
	(setf (stem-x0 chord) bm-x0)
	(if (invisible-p chord)
	    (if (marks chord) (display-note-slurs-and-brackets chord score justifying))
	  (progn
	    (if (eq (beamed chord) t)
		(if (marks chord) (display-note-slurs-and-brackets chord score justifying))
	      (let* ((fl-x0 (if up-stem 
				(- bm-x0 (- .29 *half-stem-width*) ) ;was .3 (24-Aug-00 C Rhodes)
			      (- (+ bm-x0 .005) *half-stem-width*))) ;added .005 (24-Aug-00 C Rhodes)
		     ;; .275 is nominal x offset, .0125 is half stem width => .2875
		     (sy0 (+ y0 
			     (* stem-start *staff-line-separation*) 
			     (if up-stem 
				 *half-stem-width* 
			       (- *half-stem-width*))))
		     (sy1 (+ y0 (* stem-end *staff-line-separation*))))
		(when (and (< (quarters chord) 4) 
			   (not (eq (audible-stem-direction chord) :none)))
		  (let ((up-stem (member direction-stem '(:up :up?))))
		    (when up-stem (decf bm-x0 *half-stem-width*) (decf fl-x0 *half-stem-width*))
		    (draw-stem score bm-x0 sy0 (+ sy1 (or (stem-dy chord) 0)) *stem-width* (pattern chord))
		    (if (stem-mark chord) (funcall (stem-mark chord) score bm-x0 sy0 sy1))
		    (setf (stem-end chord) sy1)
		    (when (plusp nflags) 
		      (draw-flags score chord nflags up-stem sy1 fl-x0)
		      (if (and up-stem (flagged chord) justifying) (rmoveto score (+ (* note-size .3) .15) 0)))))
		
		(if (marks chord) (display-note-slurs-and-brackets chord score justifying))
		
		(when (and (get-tremolo chord) 
			   (not (audible-p (stem-tie chord))))
		  (let ((trem (get-tremolo chord)))
		    (draw-tremolo score chord trem
				  (or (tremolo-slashes trem)
				      (- 3 nflags))
				  (+ (vis-dx trem)
				     (if (< (quarters chord) 4)
					 bm-x0
				       (if up-stem
					   (- bm-x0 (* .5 (width whole-note)))
					 (+ bm-x0 (* .5 (width whole-note))))))
				  (+ y0 (* stem-last-note *staff-line-separation*))
				  sy1 direction-stem
				  (if (and (measured trem)
					   (zerop nflags))
				      .15
				    .10))))))
	
	    (loop for ur-note in accidentals do ; cons up a fake note to get the note head and (possible) leger lines taken care of
	      (let* ((tnote (fifth ur-note))
		     (old-sign (note-sign tnote)))
		(when (not justifying)
		  (setf (flags tnote) 0)
		  (setf (dots tnote) 0)
		  (setf (quarters tnote) (quarters chord))
		  (setf (note-sign tnote) nil)
		  (setf (box-x0 tnote) (if (eq (second ur-note) normal-note-side) 
					   normal-head-x0 
					 (+ other-head-x0 (* stem-half-width 
							     (+ (if up-stem 0 1) 
								(if (> (count (note-line tnote) ordered-lines) 1) 1 0))))))
		  ;; anders: to accommodate #'dy messages to chords)
		  (incf (vis-dy tnote) dy-chord))
		(display tnote chord score justifying)
	      
		(if (not justifying) (setf (note-sign tnote) old-sign))))

	    (loop for note in accidentals do
	      (when (fourth note)         ;it's got an accidental
		(show score (fourth note)
		      :matrix (translate-matrix score chord
						(+ x0 mark-displacement-right ;dx accidental and dy below?
						   (* nsx min-displacement)
						   (if (or (and (= (first note) 1.0)
								(or (> (third note) 9) (< (third note) -1)))
							   (and (= (first note) 2.0)
								down-stem
								(eq (second note) :left)
								(or (> (third note) 9) (< (third note) -1))))
						       (- (leger-line-breathing-space chord)) ;try not to collide with the leger line
						     0.0)
						   (* .25 (- max-displacement (first note))))
						(+ y0 (* *staff-line-separation* (third note)))))))
	  
	    (let ((last-line 100))
	    
	      (loop for note in accidentals do
		(let* ((dots (dots chord)))
		  (when (and dots (plusp dots))
		    (let* ((our-line (third note))
			   (collision-above (find (1+ our-line) lines))
			   (collision-below (find (1- our-line) lines))
			   (dx0 (+ normal-head-x0 head-width *first-dot-spacing* (- nsx) ;dot-distance=.1
				   (if up-stem
				       (if (eq (second note) :right)
					   (if (and collision-above collision-below
						    (evenp our-line))
					       .35
					     .25)
					 (if (and (eq (second note) :left)
						  (or collision-above collision-below))
					     .25
					   0))
				     (if (eq (second note) :left)
					 (if (and collision-above collision-below
						  (evenp our-line))
					     .1
					   0)
				       0))))
			   (dy0 (+ y0 *dot-vertical-spacing*
				   (* *staff-line-separation* (third note)) 
				   (if (oddp (third note)) 
				       (progn (setf last-line our-line) 0)
				     (if (and collision-above collision-below)
					 (progn (setf last-line our-line) 0)
				       (if (or (and up-stem
						    (= (1- last-line) our-line))
					       (and down-stem
						    ;; this would be easier if we reversed accidentals and went top down
						    (evenp our-line)
						    (or collision-above
							(and (find (+ our-line 2) lines)
							     (find (+ our-line 3) lines)
							     (not (find (+ our-line 4) lines))))))
					   (progn (setf last-line (1- our-line)) (- *staff-line-separation*))
					 (progn (setf last-line (1+ our-line)) *staff-line-separation*)))))))
		      (loop for i from 1 to dots and xd from 0.0 by *dot-spacing* do
			(cmn-dot score (+ dx0 xd) dy0)))
		    ))))
	    )))
      (if (color chord) (setf (color score) old-color)))
      
    (values (+ normal-head-x0 (* .5 head-width)) other-note-x0-if-in-use-and-on-right)))

#|
;;; colored chord
(cmn treble (chord (notes cs4 gf4) q (color (list 1 0 0))))

;;; dot placement tests
(cmn (staff treble (chord (notes f4 g4) q.) (chord (notes f4 g4 a4) q.) 
	    (chord (notes e4 f4 g4 a4) q.) (chord (notes e4 f4 g4) q.) 
	    (chord (notes e4 g4 a4) q.) (chord (notes f4 a4 b4) q.) 
	    (chord (notes e4 f4) q.) (chord (notes f4 g4 a4 b4) q.) 
	    (chord (notes e4 f4 a4) q.) (chord (notes f4 g4 b4) q.))
     (staff treble (chord (notes f5 g5) q.) (chord (notes f5 g5 a5) q.) 
	    (chord (notes e5 f5 g5 a5) q.) (chord (notes e5 f5 g5) q.) 
	    (chord (notes e5 g5 a5) q.) (chord (notes f5 a5 b5) q.) 
	    (chord (notes e5 f5) q.) (chord (notes f5 g5 a5 b5) q.) 
	    (chord (notes e5 f5 a5) q.) (chord (notes f5 g5 b5) q.)))

;;; note-head-size tests
(cmn (note-head-size 1.5) staff treble (chord (notes a3 b3) q) (chord (notes a3 b3) e) (chord (notes a3 b3) e) )
(cmn (note-head-size 1.5) staff treble (chord (notes c5 g5 c6) h) (chord (notes c5 d5) e) (chord (notes c5 d5) e) (chord (notes c5 d5) q) )
(cmn (note-head-size 1.5) staff treble 
  (chord (notes cs5 gf5 cn6) h) (chord (notes cs5 ds5) e) 
  (chord (notes cf5 df5) e) (chord (notes cn5 ds5) q))
(cmn staff treble (meter 4 4) (chord (notes a4 e5) (duration 7)))
|#

(defmethod house ((chord chord) score)
  (let ((old-output (output score))
	(chord-center 0)
	(other-note-x0 0))
    (clear-score score)
    (if (eq (flagged chord) :none) (setf (flagged chord) nil))
    (multiple-value-setq
	(chord-center other-note-x0)
      (display chord nil score t))
    (setf (output score) old-output)
    (when (not (eq (visible-justification chord) :none))
      (copy-bounding-box score chord)
      (when (ties chord) 
	(if (find-if #'(lambda (tie) 
			 (eq (tie-type tie) :left))
		     (ties chord))
	    (incf (box-x1 chord) .125))
	(if (find-if #'(lambda (tie) 
			 (eq (tie-type tie) :right))
		     (ties chord))
	    (decf (box-x0 chord) .125)))

      (if other-note-x0 (setf (box-x1 chord) (max (box-x1 chord) (+ other-note-x0 .3))))

      (setf (center chord) chord-center)
      (if (not (walls chord)) (setf (walls chord) note-walls))
      (if (not (fences chord)) (setf (fences chord) note-fences))
      (if (not (expanders chord)) (setf (expanders chord) note-expanders)))
    (when (audible-p (stem-tie chord))
      (align-tied-notes score chord))))


(defun align-tied-notes (score note)
  (declare (ignore score))
  (let* ((top-note (stem-tie note))
	 (all-notes (stem-tie top-note)))
    (when (every #'stem-x0 all-notes)	;may be 0's from chord dummy placement -- try to coerce via dx
      (if (some #'beamed all-notes)
	  (loop for n in all-notes do (if (not (beamed n)) (setf (beamed n) t)))
	(let* ((flagged-note (if (stem-is-up top-note) top-note (first all-notes)))
	       (unflagged-note (if (stem-is-down top-note) top-note (first all-notes))))
	  (if (not (chord-p unflagged-note)) (setf (audible-stem-direction unflagged-note) :none))
	  (loop for n in all-notes do
	    (if (and (not (eq n top-note))
		     (not (eq n (first all-notes))))
		(setf (audible-stem-direction n) :none))
	    (if (and (not (eq n flagged-note))
		     (plusp (flags top-note)))
		(setf (flags n) 0)))))
      (add-to-marks 
       note 
       (list
	(make-instance 'sundry 
	 :name :stem-tie
	 :mark #'(lambda (mark note score &optional justifying)
		   (declare (ignore mark))
		   (when (not justifying)
		     (if (and (not (beamed note))
			      (beamed top-note))
			 (setf (beamed note) t))
		     ;; this depends on the top or bottom note to handle flags and the "outer" stem
		     (draw-stem 
		      score 
		      (stem-x0 top-note)
		      (+ (box-y0 note) (if (note-p note) (note-head-y0-offset note (stem-is-up note)) 0.0))
		      (+ (box-y0 top-note) (if (note-p top-note) (note-head-y0-offset top-note (stem-is-up top-note)) 0.0))
 		      *stem-width*
		      (pattern note))))))))))

#|
;;; alignment tests
(cmn (staff treble (chord (notes d4 e4 a4) q) (chord (notes a4 g5 a5) q)) 
     (staff bass (chord (notes e3 a3 b3) q) (chord (notes f2 g2 c3) q)))
(cmn (size 100) 
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
	 ))
(cmn (size 100) 
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
	 ))
(cmn (size 100) 
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
	 ))
(cmn (staff treble (setf mc (c4 q stem-up))) (staff bass c3 (tied-to mc)) (staff bass c3 (tied-to mc)) (staff bass c3 (tied-to mc)))
(cmn (staff treble (setf mc (c4 e stem-up))) (staff bass c3 e (tied-to mc)) (staff bass c3 e (tied-to mc)) (staff bass c3 e (tied-to mc)))
(cmn (staff treble (setf mc (c4 e stem-up)) (setf md (c4 e stem-up))) 
     (staff bass c3 e (tied-to mc) (d3 e (note-head :none) (tied-to md)))
     (staff bass cs3 e (tied-to mc) ef3 e (tied-to md)) 
     (staff bass c3 e (tied-to mc) (chord (notes c3 gf3) e (tied-to md))))
(cmn (size 100)
     (staff treble (chord (notes c4 g4) s) (setf midc3 (chord (notes d4 ef4 en5) s (stem-direction :down))))
     (staff bass sixteenth-rest (c3 s (tied-to midc3))))
(cmn (size 100) (staff treble sixteenth-rest sixteenth-rest
     (setf midc3 (chord (notes d4 ef4 an5) s (stem-direction :down))) sixteenth-rest sixteenth-rest)
     (staff bass sixteenth-rest sixteenth-rest (cs3 s (tied-to midc3)) sixteenth-rest sixteenth-rest))
(cmn (size 100) 
     (staff bar treble (setf arp1 (d4 s stem-up (note-head :x))) (setf arp2 (d4 s stem-down (note-head :x)))) 
     (staff bar bass (dn3 s (tied-to arp1)  (note-head :triangle)) (dn3 s (tied-to arp2)  (note-head :triangle))))
(cmn treble c4 q (note-head (make-instance 'sundry :mark #'(lambda (mark note score &optional hiho) (draw-rhythmX score nil)))))
|#


;;;
;;; ----------------    rests
;;;

(defclass rest-mixin (rhythm-mixin staff-relative-mixin score-object-mixin)
  ((mark :initform nil :initarg :mark :reader rest-mark)
   (line :initform nil :initarg :line :reader rest-line)
   (ties :initform nil :initarg :ties :reader ties)
   (tie-direction :initarg :tie-direction :initform nil :reader tie-direction)
   (beamed :initform nil :initarg :beamed :reader beamed)
   (outer-beam :initarg :outer-beam :initform nil :reader outer-beam)
   (store-data :initarg :store-data :initform nil :reader store-data)
   (beams :initarg :beams :initform nil :reader beams)))

(defclass wrest (rest-mixin rhythm score-object)
  ((mark :accessor rest-mark)
   (line :accessor rest-line)
   (ties :accessor ties)
   (tie-direction :accessor tie-direction)
   (beamed :accessor beamed)
   (outer-beam :accessor outer-beam)
   (store-data :accessor store-data)
   (beams :accessor beams)))

(defclass write-protected-rest (write-protect rest-mixin) ())

(defmethod note-line ((r rest-mixin)) (or (rest-line r) 0)) ; added note-line reader 18-Jul-06 at suggestion of AV
(defmethod stem-end ((r rest-mixin)) nil)                   ; AV 1-Aug-06

(defmacro define-rest (name mark dots flags quarters drawf bbox)
  `(progn
     (defvar ,name
       (make-instance 'write-protected-rest
		      :mark ,mark
		      :dots ,dots
		      :flags ,flags
		      :quarters ,quarters
		      :duration ,quarters
		      :draw-func ,drawf
		      :x0 (first ,bbox)
		      :x1 (third ,bbox)
		      :y0 (second ,bbox)
		      :y1 (fourth ,bbox)
		      :width (- (third ,bbox) (first ,bbox))))
     (defun ,name (&rest objects) (apply #'ur-rest (copy ,name) objects))))

(define-rest double-whole-rest       :rest.5      0 0 8     #'draw-double-whole-rest double-whole-rest-bounds)
(define-rest dotted-whole-rest       :rest1       1 0 6     #'draw-whole-rest whole-rest-bounds)
(define-rest whole-rest              :rest1       0 0 nil   #'draw-whole-rest whole-rest-bounds)
(define-rest whole-measure-rest      :measurerest 0 0 nil   #'draw-whole-rest whole-rest-bounds)
(define-rest measure-rest            :measurerest 0 0 nil   #'draw-measure-rest measure-rest-bounds)
(define-rest repeat-measure          :repsign     0 0 nil   #'draw-repeat-sign repeat-sign-bounds)
(define-rest half-rest               :rest2       0 0 2     #'draw-half-rest half-rest-bounds)
(define-rest dotted-half-rest        :rest2       1 0 3     #'draw-half-rest half-rest-bounds)
(define-rest quarter-rest            :rest4       0 0 1     #'draw-quarter-rest quarter-rest-bounds)
(define-rest dotted-quarter-rest     :rest4       1 0 3/2   #'draw-quarter-rest quarter-rest-bounds)
(define-rest eighth-rest             :rest8       0 1 1/2   #'draw-8th-rest rest8-bounds)
(define-rest dotted-eighth-rest      :rest8       1 1 3/4   #'draw-8th-rest rest8-bounds)
(define-rest sixteenth-rest          :rest16      0 2 1/4   #'draw-16th-rest rest16-bounds)
(define-rest dotted-sixteenth-rest   :rest16      1 2 3/8   #'draw-16th-rest rest16-bounds)
(define-rest thirty-second-rest      :rest32      0 3 1/8   #'draw-32nd-rest rest32-bounds)
(define-rest dotted-thirty-second-rest :rest32    1 3 3/16  #'draw-32nd-rest rest32-bounds)
(define-rest sixty-fourth-rest       :rest64      0 4 1/16  #'draw-64th-rest rest64-bounds)
(define-rest one-twenty-eighth-rest  :rest128     0 5 1/32  #'draw-128th-rest rest128-bounds)

(defun rest-name (rst)
  (let ((rhy (quarters rst)))
    (if (not rhy) 
	(if (eq (rest-mark rst) :repsign) 
	    "repeat-measure" 
	  "whole-rest")			;see above -- whole-rest and measure-rest have nil as quarters field
      (if (= rhy 8) "double-whole-rest"
	(if (= rhy 6) (if (= (dots rst) 1) "dotted-whole-rest" "whole-rest")
	  (if (= rhy 4) "whole-rest"
	    (if (= rhy 2) "half-rest"
	      (if (= rhy 3) "dotted-half-rest"
		(if (= rhy 1) "quarter-rest"
		  (if (= rhy 3/2) "dotted-quarter-rest"
		    (if (= rhy 1/2) "eighth-rest"
		      (if (= rhy 3/4) "dotted-eighth-rest"
			(if (= rhy 1/4) "sixteenth-rest"
			  (if (= rhy 3/8) "dotted-sixteenth-rest"
			    (if (= rhy 1/8) "thirty-second-rest"
			      (if (= rhy 3/16) "dotted-thirty-second-rest"
				(if (= rhy 1/16) "sixty-fourth-rest"
				  (format nil "rest (rq ~A)" (fratify rhy)))))))))))))))))))

(defmethod rest-p ((obj t)) nil)
(defmethod rest-p ((obj rest-mixin)) t)

(defmethod descry ((rest rest-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) "(rest" "")
	  (if (rest-mark rest) (format nil " :mark ~A" (rest-mark rest)) "")
	  (if (rest-line rest) (format nil " :line ~A" (rest-line rest)) "")
	  (if (ties rest) (format nil " :ties (list ~{~A ~})" (loop for tie in (ties rest) collect (descry tie))) "")
	  (if (tie-direction rest) (format nil " :tie-direction :~(~A~)" (tie-direction rest)) "")
	  (if (beamed rest) " :beamed t" "")
	  (if (outer-beam rest)
	      (format nil " :outer-beam '(~{~1,3F ~})" (outer-beam rest))
	    "")
	  (if (beams rest) (format nil " :beams ~A~%                    " (descry (beams rest))) "")
	  (if (store-data rest) (format nil " :store-data ~A" (store-data rest)) "")
	  (if (next-method-p) (call-next-method rest stream (or controller rest)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((rst rest-mixin))
  (format nil "(~A~A~A~A~A~A)"
	  (rest-name rst)
 	  (if (onset rst) (format nil " (onset ~A)" (fratify (onset rst))) "")
	  (if (ties rst) (format nil "~{ ~(~A~)~}" (sorted-ties (ties rst))) "")
	  (if (tie-direction rst) (format nil "(tie-direction :~(~A~))" (tie-direction rst)) "")
	  (if (store-data rst) (identify-store-data rst) "")
	  (the-usual-suspects rst)))

(defmethod copy ((old-rest rest-mixin) &optional object)
  (let ((new-rest (if (not object) (make-instance 'wrest)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (rest-mark new-rest) (rest-mark old-rest))
    (setf (rest-line new-rest) (rest-line old-rest))
    (setf (beamed new-rest) (beamed old-rest))
    (setf (outer-beam new-rest) (outer-beam old-rest))
    (if (beams old-rest) (setf (beams new-rest) (copy (beams old-rest))))
    (if (ties old-rest) (setf (ties new-rest) (loop for tie in (ties old-rest) collect (copy tie))))
    (setf (tie-direction new-rest) (tie-direction old-rest))
    (if (next-method-p) (call-next-method old-rest new-rest))
    new-rest))

(defun get-rest (dur)
  (if (>= dur 8) double-whole-rest
    (if (>= dur 4) whole-rest
      (if (or (>= dur 2) (= dur 4/3) (= dur 8/5)) half-rest
	(let ((flags (how-many-flags dur)))    
	  (if (= flags 0) quarter-rest
	    (if (= flags 1) eighth-rest
	      (if (= flags 2) sixteenth-rest
		(if (= flags 3) thirty-second-rest
		  sixty-fourth-rest)))))))))

(defun rest-copy (dur new-rest)
  (let ((rm (if (rest-p dur) dur
	      (if (rhythm-p dur) (get-rest (quarters dur))
		(get-rest dur)))))
    (setf (rest-mark new-rest) (rest-mark rm))
    (setf (draw-func new-rest) (draw-func rm))
    (setf (box-x0 new-rest) (box-x0 rm))
    (setf (box-x1 new-rest) (box-x1 rm))
    (setf (box-y0 new-rest) (box-y0 rm))
    (setf (box-y1 new-rest) (box-y1 rm))
    (setf (width new-rest) (width rm))
    new-rest))

(defmethod (setf duration) (val (obj rest-mixin))
  (let ((dur (fratify val)))
    (setf (odb-duration obj) dur)
    (if (not (eq (rest-mark obj) :rest1)) ; (cmn treble (meter 3 4) whole-rest)
	(setf (dots obj) (how-many-dots dur)))
    (setf (quarters obj) dur)
    (setf (flags obj) (how-many-flags dur))
    dur))

(defmethod (setf rhythm) (ur-val (rest rest-mixin))
  (let ((val (fratify ur-val)))
    (setf (odb-duration rest) val)
    (setf (dots rest) (how-many-dots val))
    (setf (flags rest) (how-many-flags val))
    (setf (quarters rest) val)
    (rest-copy (quarters rest) rest)))
  
(defmethod get-tremolo ((rest rest-mixin)) nil)
(defmethod (setf stem-direction) (val (rest rest-mixin)) (declare (ignore val)))
(defmethod stem-x0 ((rest rest-mixin)) 0)
(defmethod stem-dy ((rest rest-mixin)) nil)
(defmethod stem-tie ((obj t)) nil)
(defmethod stem-mark ((rest rest-mixin)) nil)
(defmethod collision ((rest rest-mixin)) 0)
(defmethod (setf collision) (val (rest rest-mixin)) (declare (ignore val)))
(defmethod note-head-size ((rest rest-mixin)) 1.0)
(defmethod flagged ((rest rest-mixin)) nil)
(defmethod (setf flagged) (val (rest rest-mixin)) (declare (ignore val)))

(defmethod maximum-line ((rest rest-mixin)) (or (rest-line rest) 4))
(defmethod minimum-line ((rest rest-mixin)) (or (rest-line rest) 4))
(defmethod line ((rest rest-mixin)) (or (rest-line rest) 4))

(defmethod stem-is-up? ((object rest-mixin)) (and (rest-line object) (< (rest-line object) 4)))
(defmethod stem-is-down? ((object rest-mixin)) (not (stem-is-up? object)))


(defmethod short-note ((note rest-mixin) &key x0)
  (make-instance 'note :x0 (or x0 (box-x0 note)) :x1 (box-x1 note) :y0 (box-y0 note) :y1 (box-y1 note)
		 :staff-y0 (staff-y0 note)
		 :quarters (quarters note)
		 :line (or (rest-line note) 4)))


(defmethod display ((rest rest-mixin) container score &optional justifying)
  (declare (ignore container))
  ;; line is the clef-relative center line of the rest (for dot placement)
  (let* ((x0 (if (not (member (rest-mark rest) (list :rest1 :measurerest :repsign)))
		 (+ (box-x0 rest) (vis-dx rest))
 	       (- (+ (* .5 (+ (box-x0 rest) (box-x1 rest))) (vis-dx rest)) (center rest))))
	 (rm (rest-mark rest))
	 (line (if (eq rm :repsign) 2
		 (or (rest-line rest) 4)))
	 (y0 (+ (staff-y0 rest) (vis-dy rest) (* line *staff-line-separation*)))
	 (dots (dots rest))
	 (flags (flags rest))
	 (ty0 (+ (if (zerop flags)
		     y0
		   (if (= flags 1)
		       (+ y0 *staff-line-separation*)
		     (if (evenp flags)
			 (- y0 (* (1- flags) *staff-line-separation*))
		       (if (= flags 3)
			   (- y0 *staff-line-separation*)
			 (- y0 (* flags *staff-line-separation*))))))
		 (if (oddp line) *staff-line-separation* 0)))) ;always position in a space, not on a line
    
    (if (= flags 1) (decf ty0 .125) (if (> flags 1) (incf ty0 .125) (if (eq rm :rest2) (incf ty0 .01))))
    
    (setf  (box-y0 rest) (+ (staff-y0 rest) (* line *staff-line-separation*)))
    
    (if (and (beams rest) 
	     (not justifying))
	(display (beams rest) rest score))
    
    (if (marks rest) (display-marks rest score justifying))
    (when (not (invisible-p rest))
      (if (eq (rest-mark rest) :measurerest)
	  (let* ((msx (- (box-x1 rest) (box-x0 rest)))
		 (mdx (max 0.0 (min .5 (/ (- msx 1.0) 4)))))
	    (decf x0 mdx)))
      (show score rest :matrix (translate-matrix score rest x0 ty0))
      (when (and dots (plusp dots)
		 (not (member rest '(whole-rest measure-rest one-twenty-eighth-rest))))
	(let* ((dx (+ *dot-spacing*
		      (if (eq (rest-mark rest) :rest2) .4
			(if (eq (rest-mark rest) :rest4) .25
			  (if (eq (rest-mark rest) :rest8) .3
			    (if (eq (rest-mark rest) :rest16) .35
			      (max .2 (width rest))))))))
	       (dy (if (oddp line)
		       0
		     *staff-line-separation* ))
	       (dx0 (+ x0 dx))
	       (dy0 (+ y0 dy (if (eq (rest-mark rest) :rest32) .25 0))))
	  (loop for i from 1 to dots and xd from 0.0 by *dot-spacing* do
		(cmn-dot score (+ dx0 xd) dy0)))))
    (let ((mn (and (store-data rest) 
		   (find-if #'(lambda (n) (and (listp n) (eq (first n) :measure-number))) (store-data rest)))))
      (when mn
	(let ((num (second mn)))
	  (show score (make-instance 'meter :num num)
		:data (format nil "~A" num)
		:matrix (translate-matrix score rest 
					  (- x0 (if (> num 10) (if (> num 100) .3 .15) 0)) 
					  (+ (staff-y0 rest) (vis-dy rest) 1.45))))))))

;;; (cmn (automatic-ties nil) treble dotted-whole-rest dotted-half-rest dotted-quarter-rest dotted-eighth-rest dotted-sixteenth-rest dotted-thirty-second-rest)
(defun ur-rest (new-rest &rest objects)
  (loop for act in objects do
    (if (self-acting-p act)
	(funcall (action act) new-rest (argument act))
      (if (rest-p act)
	  (copy act new-rest)
	(if (rhythm-p act)
	    (progn
	      (rcopy act new-rest)
	      (rest-copy act new-rest))
	  (if (visible-p act)
	      (add-to-marks new-rest (list (if (write-protected act) (copy act) act)))
	    (if (numberp act)
		(push (list :measure-number act) (store-data new-rest))
	      (if act
		  (cmn-warn "odd argument to rest: ~A" act))))))))
  (when (and (not (rest-mark new-rest))
	     (duration new-rest))
    (rcopy (rq (duration new-rest)) new-rest)
    (rest-copy (quarters new-rest) new-rest))
  new-rest)

(defmethod notify ((rest rest-mixin) &optional objects)
  (apply #'ur-rest rest objects))

(defun rest (&rest objects)
  (apply #'ur-rest (make-instance 'wrest) objects))


(defvar rest-walls '(.05 .05))
(defvar rest-fences '(.1 .1))
(defvar rest-expanders '(2 2))

(defmethod house ((rest rest-mixin) score)
  (declare (ignore score))
  (when (and (not (eq (visible-justification rest) :none))
	     (not (invisible-p rest)))
    (setf (box-x1 rest) (+ (max .2 (width rest))
			   (if (and (dots rest) (plusp (dots rest)))
			       (+ .1	;dot-distance
				  (* (dots rest) .1)
				  (* (1- (dots rest)) .15))
			     0)))
    (setf (center rest) (* .5 (max .2 (width rest)))) ;i.e. ignore dots when aligning vertically
    (if (not (walls rest)) (setf (walls rest) rest-walls))
    (if (not (fences rest)) (setf (fences rest) rest-fences))
    (if (not (expanders rest)) (setf (expanders rest) rest-expanders))))

;;; (cmn staff treble (c4 (rq 2/5)) (c4 (rq 2/5)) (rest (rq 2/5)) (c4 (rq 2/5)) (c4 (rq 2/5)))


;;;
;;; ----------------   grace notes
;;;
;;; these are normal notes with small heads, beams, stems, sometimes with a slash
;;; they have no duration, but take up lots of horizontal space, so they are
;;; implemented here as marks -- in this case the mark (i.e. sundry) contains
;;; notes to be displayed.  stem-direction defaults to :up. Slashed if one note.
;;; slurred is default.
;;;
;;; (c4 e (grace-note d4)) (c4 e (grace-notes d4 fs4 e4)) (c4 e (grace-notes (slashed nil) (stem-direction :down) a4 f4))
;;; similarly, a chord can have grace-notes: (chord (notes c4 e4 g4) q (grace-notes (chord d4 f4 a4)))

(defclass grace-note (score-object)
  ((data :initarg :data :initform nil :accessor grace-data) ;a list of notes and chords, like chord-data list
   (stem-direction :initarg :stem-direction :initform :up :accessor stem-direction)
   ;; has to be :up, not :up? -- otherwise gets upended at dumb times
   (slashed :initarg :slashed :initform t :accessor slashed) ;t if one note, nil otherwise is default
   (slurred :initarg :slurred :initform t :accessor slurred) ;if nil, no slur to main note
   (user-data :initarg :user-data :initform nil :accessor slur-user-data)
   (direction :initarg :direction :initform nil :accessor slur-direction)
   (thickness :initarg :thickness :initform nil :accessor slur-thickness)
   (curvature :initarg :curvature :initform nil :accessor slur-curvature)
   (slash-thickness :initarg :slash-thickness :initform .025 :accessor slash-thickness)))

(defmethod grace-note-p ((obj t)) nil)
(defmethod grace-note-p ((obj grace-note)) t)

(deferred-action slashed)
(deferred-action slurred)
(deferred-action slash-thickness)

(defun grace-note (&rest objects)
  (let ((new-grace (make-instance 'grace-note))
	(nlen 0)
	(bt 0.0))
    (let ((new-staff (make-instance 'staff)))
      ;; let the args to grace-notes mimic the staff style input (for appoggiaturas primarily)
      (multiple-value-bind
	  (data actions local-brace)
	  (notify new-staff objects)
	(declare (ignore local-brace))
	(loop for act in actions do
	      (funcall (action act) new-grace (argument act)))
	(setf (grace-data new-grace) data)))
    
    (setf nlen (length (grace-data new-grace)))
    (loop for note in (grace-data new-grace) do
      (if (not (audible-stem-direction note)) 
	  (setf (audible-stem-direction note) (stem-direction new-grace)))
      (if (or (not (flags note)) 
	      (zerop (flags note)))
	  (if (= nlen 1) (setf (flags note) 1)
	    (if (< nlen 4) (setf (flags note) 2)
	      (setf (flags note) 3))))
      (if (not (quarters note)) 
	  (setf (quarters note) 
	    (if (= nlen 1) 1/2
	      (if (= nlen 2) 1/4
		1/8))))
      (setf (odb-beat note) bt)
      (incf bt (quarters note)))
    (make-self-acting :action #'add-to-marks :argument (list new-grace))))

(defun grace-notes (&rest objects) (apply #'grace-note objects))
(defun appoggiatura (&rest objects) (apply #'grace-note (slashed nil) objects))

(defmethod descry ((grace grace-note) &optional stream controller)
  (format stream "(grace-note :slashed ~(~A~)~A :slurred ~(~A~) :stem-direction :~(~A~)~A :data (list~{ ~A~}))"
	  (slashed grace) 
	  (if (/= (slash-thickness grace) .025) (format nil " :slash-thickness ~1,3F" (slash-thickness grace)) "")
	  (slurred grace) (stem-direction grace)
	  (if (next-method-p) (call-next-method grace stream (or controller grace)) "")
	  (loop for note in (grace-data grace) collect (descry note stream grace))))

(defmethod identify ((gn grace-note))
  (format nil " (~A~A~A~A~A~{ ~A~})"
	  (if (not (slashed gn)) "appoggiatura" "grace-note")
	  (if (/= (slash-thickness gn) .025) (format nil " (slash-thickness ~1,3F)" (slash-thickness gn)) "")
	  (if (not (slurred gn)) " (slurred nil)" "")
	  (if (not (eq (stem-direction gn) :up)) " stem-down" "")
	  (if (eq (visible-justification gn) :none) " unjustified" "")
	  (loop for note in (grace-data gn) 
	   collect (if (or (marks note)
			   (/= (quarters note) 1)
			   (not (note-p note)))
		       (identify note)
		     (note-name note)))))

(defmethod copy ((grace grace-note) &optional object)
  (let ((new-grace (if (not object) (make-instance 'grace-note)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (slashed new-grace) (slashed grace))
    (setf (slash-thickness new-grace) (slash-thickness grace))
    (setf (slurred new-grace) (slurred grace))
    (setf (stem-direction new-grace) (stem-direction grace))
    (setf (grace-data new-grace) (loop for note in (grace-data grace) collect (copy note)))
    (if (next-method-p) (call-next-method grace new-grace))
    new-grace))

(defmethod display ((grace grace-note) outer-note score &optional justifying)
  (let* ((grace-scale *grace-note-size*)
	 (inverse-scale (/ 1.0 grace-scale))
	 (old-separation *staff-line-separation*)
	 (glen (length (grace-data grace)))
	 (outer-line (minimum-line outer-note))
	 (grace-dxs (loop for note in (grace-data grace) 
		     collect (if (note-p note)
				 (+ .5 
				    (if (sign note) (+ (width (sign note)) *accidental-to-note-head-space*) 0)
				    (if (or (< (line note) -1) 
					    (> (line note) 9))
					.1
				      0))
			       (+ .5 
				  (if (find-if #'sign (chord-data note))
				      (let ((all-sign-lines (sort (loop for cdn in (chord-data note) 
								   if (note-sign cdn) collect (line cdn)) 
								  #'>)))
					(if (and (> (length all-sign-lines) 1)
						 (< (or (loop for n1 in all-sign-lines and n2 in (cdr all-sign-lines) 
							 minimize (abs (- n2 n1)))
							0)
						    3))
					    1.0
					  .5))
				    0)))))
	 (x-space (+ .1 (* grace-scale (apply #'+ grace-dxs)))))
    (if justifying
	(if (not (eq (visible-justification grace) :none))
	    (moveto score (- (+ (box-x0 outer-note) (vis-dx grace)) x-space) (box-y0 outer-note)))
      (progn
	(setf *staff-line-separation* (* inverse-scale old-separation))
	(with-scaling score 
		      grace-scale 
		      (* (- (+ (box-x0 outer-note) (vis-dx grace)) x-space) (scr-size score))
		      (* (staff-y0 outer-note) (scr-size score))
	  (let ((fc nil)
		(xorig 0.0)
		(backpatches nil))
	    (loop for note in (grace-data grace) and dx in grace-dxs do
	      (setf (staff-y0 note) 0.0)
	      (setf (box-y0 note) (* (minimum-line note) *staff-line-separation*))
		  
	      (if (or (note-p note)
		      (not (find-if #'sign (chord-data note))))
		  (setf (center note) .143)

		(let ((old-output (output score)))
		  ;; chords with accidentals can be all over the map, so we have to draw them to figure out where the stem is
		  (clear-score score)
		  (setf (center note) (display note grace score justifying))
		  (setf (output score) old-output)))

	      (setf (box-x0 note) xorig)
			  
	      (if backpatches 
		  (loop for patch in backpatches do
		    (if (eq (first patch) note)
			(setf (box-x1 (second patch)) xorig))))
	      (if (marks note)
		  (loop for m in (marks note) do
		    (if (finger-mark-p m)
			(setf (fingering-size m) inverse-scale))
		    (if (and (crescendo-p m)
			     (tag-note m))
			(push (list (tag-note m) m) backpatches))))
	      (incf xorig dx))
			
	    (if (> glen 1)
		(annotate-beam score nil 0 
			       (grace-data grace) nil 
			       (stem-direction grace) 
			       (* grace-scale *staff-line-separation*)))
	    ;; beam-unit happens in display
			
	    (if (slurred grace)
		(if (> glen 1)
		    (prepare-slur score 
				  (make-instance 'slur :type :left 
						 :curvature (slur-curvature grace) 
						 :thickness (slur-thickness grace))
				  (append (grace-data grace) 
					  (list (make-instance 'note :stem-direction :up?
							       :y0 (- (* outer-line *staff-line-separation*) (/ .15 grace-scale))
							       :staff-y0 0
							       :x0 (+ xorig (/ (if (stem-is-up? outer-note) .2 0) grace-scale))
							       :line (if (stem-is-down? outer-note) outer-line (1- outer-line))
							       :sign (and (sign outer-note) (copy (sign outer-note)))
							       :note-head (note-head outer-note)))))
		  ;; two note "slur" is so problematic that I've decided (after n failures)
		  ;;   to handle them directly -- no intervening two-note-slur calls and so on
		  (let* ((gn (first (grace-data grace))) ;the actual grace note
			 (on outer-note)
			 (gn-stem (or (stem-direction gn) (stem-direction grace) :up?))
			 (gn-stem-up (member gn-stem '(:up :up?)))
			 (on-stem (or (stem-direction outer-note)
				      (and (< (head-line outer-note) 4) :up?)
				      :down?))
			 (on-stem-up (member on-stem '(:up :up?)))
			 (stems-agree (or (and gn-stem-up on-stem-up)
					  (and (not gn-stem-up) (not on-stem-up))))
			 (gn-stem-end (+ (get-stem-end score gn (member gn-stem '(:up :up?)) t)
					 (or (stem-dy gn) 0)))
			 (gn-line (if (not gn-stem-up) (maximum-line gn) (minimum-line gn)))
			 (on-line (if (not on-stem-up) (maximum-line on) (minimum-line on)))
			 (line-diff (- gn-line on-line)) ;true (unscaled) line diff
			 (close-heads (< (abs line-diff) 4))
			 (slur-dir (or (slur-direction grace)
				       (slur-direction gn)
				       (if (or stems-agree close-heads)
					   (if gn-stem-up 
					       :down 
					     :up)
					 ;; here we have the weird cases -- opposite stems, wide leaps
					 (if (or (minusp line-diff)
						 (> gn-line 9))
					     :up
					   :down))))
			 (connection-position (if (eq slur-dir :up)
						  (if (and (minusp line-diff) gn-stem-up)
						      :stem-head
						    :head-head)
						(if (and (plusp line-diff) (not gn-stem-up))
						    :stem-head
						  :head-head)))
			 (sudat (slur-user-data grace))
			 (sux0 (or (and sudat (first sudat)) 0))
			 (suy0 (or (and sudat (second sudat)) 0))
			 (sux1 (or (and sudat (third sudat)) 0))
			 (suy1 (or (and sudat (fourth sudat)) 0))
			 (sux2 (or (and sudat (fifth sudat)) 0))
			 (suy2 (or (and sudat (sixth sudat)) 0))
			 (sux3 (or (and sudat (seventh sudat)) 0))
			 (suy3 (or (and sudat (nth 7 sudat)) 0)) ; "eighth" confuses sbcl here
			 
			 (gny0 (+ suy0 
				  (if (eq connection-position :head-head) 
				      (if (note-p gn) 
					  (y0 gn) 
					(* *staff-line-separation* gn-line))
				    gn-stem-end)
				  (if (eq slur-dir :up) .2 -.3)
				  (if (and (< gn-line -2) (eq slur-dir :up))
				      (* (- (+ 2 gn-line)) *staff-line-separation*)
				    0)
				  (if (and (> gn-line 10) (eq slur-dir :down))
				      (* (- 10 gn-line) *staff-line-separation*)
				    0)
				  (if (and stems-agree
					   (or (and gn-stem-up (< line-diff -4))
					       (and (not gn-stem-up) (> line-diff 4))))
				      (if gn-stem-up .2 -.2)
				    0)))
			 
			 (gnxscl (/ .5 *grace-note-size*))
			 
			 (gny3 (+ suy3 
				  (* on-line *staff-line-separation*)
				  (* gnxscl
				     (if (eq slur-dir :down)
					 (if (and (not close-heads)
						  (plusp line-diff)
						  (or (eq connection-position :head-head)
						      (> line-diff 6)))
					     .1
					   (if (sign on)
					       -.8
					     -.4))
				       (if (and (not close-heads)
						(minusp line-diff)
						(or (eq connection-position :head-head)
						    (< line-diff -6)))
					   .1
					 (if (sign on)
					     .8
					   .4))))))
			 
			 (gnx0 (+ sux0 (box-x0 gn) (center gn) 
				  (if (and (chord-p gn) (sign gn)) .25 0)
				  (if (or (and gn-stem-up 
					       (eq slur-dir :up) 
					       (plusp line-diff)) 
					  (and stems-agree
					       (or (and gn-stem-up (< line-diff -4))
						   (and (not gn-stem-up) (> line-diff 4)))))
				      (+ .3 (if (or (> gn-line 10) (< gn-line -2)) .1 0))
				    0)))
			 (gnx3 (+ sux3 xorig 
				  (* gnxscl
				     (if (or (and (or (> line-diff 6)
						      (and (> line-diff 3)
							   (eq connection-position :head-head)))
						  (eq slur-dir :down))
					     (and (or (< line-diff -6)
						      (and (< line-diff -3)
							   (eq connection-position :head-head)))
						  (eq slur-dir :up))
					     (and (eq slur-dir :down)
						  (not on-stem-up)))
					 .10
				       (if (sign on)
					   1.0
					 .4)))))
			 ;; now take into account curvature, thickness, and user-data, and general "nice" shapes
			 (dx (- gnx3 gnx0))
			 (dy (- gny3 gny0))
			 (len (sqrt (+ (* dx dx) (* dy dy))))
			 (gnslope (divide dy dx))
			 (gnx1 (+ gnx0 sux1 (* .33 dx)))
			 (gnx2 (+ gnx0 sux2 (* .67 dx)))
			 (curve (* (or (slur-curvature grace)
				       (and (slurs gn)
					    (slur-curvature (first (slurs gn))))
				       *slur-curvature*)
				   (if (< len .7) .5 1.0)))
			 (curve-fixup (if (> (abs gnslope) 6)
					  1.0
					(if (> (abs gnslope) 3)
					    0.5
					  0)))
			 (bow-curve (if on-stem-up
					(if gn-stem-up
					    (- curve)
					  (if (eq slur-dir :up)
					      curve
					    (- curve)))
				      (if (not gn-stem-up)
					  curve
					(if (eq slur-dir :down)
					    (- curve)
					  curve))))
			 (bow-curve-fixup (if (minusp bow-curve) (- curve-fixup) curve-fixup))
			 (thick (or (slur-thickness grace)
				    (and (slurs gn)
					 (slur-thickness (first (slurs gn))))
				    *slur-thickness*))
			 (gny1 (+ gny0 suy1 (* (- gnx1 gnx0) gnslope) bow-curve bow-curve-fixup))
			 (gny2 (+ gny0 suy2 (* (- gnx2 gnx0) gnslope) bow-curve)))
		    
		    (moveto score gnx0 gny0)
		    (curveto score gnx1 gny1 gnx2 gny2 gnx3 gny3)
		    (curveto score gnx2 (- gny2 thick) gnx1 (- gny1 thick) gnx0 gny0)
		    (fill-in score))))
	    
	    (loop for note in (grace-data grace) do
	      (if (not fc)	;first note flag (for slash mark)
		  (progn
		    (setf fc (/ (display note grace score justifying) (if (chord-p note) 1.0 (scr-size score))))
		    (when (slashed grace)
		      (if (= glen 1)
			  (progn
			    (moveto score 
				    (+ fc (if (stem-is-up? note) .05 -.2) (if (chord-p note) -.15 0))
				    (if (and (note-p note)
					     (<= 0 (minimum-line note) (maximum-line note) 9))
					(+ (box-y0 note)
					   (if (stem-is-up? note) .35 -.4))
				      (if (stem-is-up? note)
					  (- (or (stem-end note) 0)
					     .7)
					(+ (or (stem-end note) 0) 
					   .7))))
			    (setf (line-width score) (slash-thickness grace))
			    (rlineto score 
				     (+ .6 (if (> (flags note) 2) .1 0))
				     (* (if (stem-is-up? note) 1.0 -1.0)
					(+ .6 
					   (if (> (flags note) 2) 
					       (max 0 (- (stem-end note) (box-y0 note) 1.25))
					     0))))
			    (draw score)
			    (setf (line-width score) 0))
			(let* ((beam-data (outer-beam note))
			       ;; draw slash at front going up if stem up and beam flat or down
			       ;;            at front going down if stem down and beam up
			       ;;            at back going up if stem down and beam flat or down
			       ;;            at back going down if stem up and beam up
			       ;; we assume we've already called beamify which has set up outer-beam
			       (bx0 (first beam-data))
			       (by0 (second beam-data))
			       (bx1 (third beam-data))
			       (by1 (fourth beam-data))
			       (beam-up (> by1 by0))
			       (gstem-up (stem-is-up? note))
			       (slash-at-front (or (and gstem-up (not beam-up)) (and (not gstem-up) beam-up)))
			       (slash-going-up (or (and gstem-up (not beam-up)) (and (not gstem-up) (not beam-up))))
			       (beams (flags note))
			       (sx0 (if slash-at-front 
					(- bx0 .25) 
				      (if (and slash-going-up (> beams 2))
					  (- bx1 .75)
					(- bx1 .5))))
			       (bspace (* (1+ beams) *beam-spacing*))
			       (dist (+ .2 (* bspace (if (or slash-going-up slash-at-front) 1.0 .75))))
			       (sx1 (+ sx0 dist))
			       (sy0 (if slash-going-up 
					(if slash-at-front
					    (- by0 -.2 bspace)
					  (- by1 .25))
				      (if slash-at-front
					  (+ by0 .25 bspace)
					(+ by1 .3))))
			       (sy1 (if slash-going-up
					(+ sy0 dist)
				      (- sy0 dist))))
			  (moveto score sx0 sy0)
			  (setf (line-width score) (slash-thickness grace))
			  (lineto score sx1 sy1)
			  (draw score)
			  (setf (line-width score) 0)))))
		(display note grace score)))))
	(setf *staff-line-separation* old-separation)))))

#|
(cmn (size 40) (free-expansion-factor 1.0) staff treble 
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
     (g4 q (grace-note stem-down (chord ef4 bf4))))

;;; see grace.cmn for many 2 note cases
(cmn (size 40) staff treble 
     (g4 q (grace-note fs4)) 
     (g4 q (grace-note gf4)) 
     (g4 q (grace-note ef4)) 
     (g4 q (grace-note b4)) 
     (b4 q (grace-note e5)) 
     (b4 q (grace-note d4)) 
     (g4 q (grace-note ef5)))

(cmn (size 40) staff treble 
     (g4 q (grace-note e4 fs4)) 
     (g4 q (grace-note gf4 ef4)) 
     (g4 q (grace-note ef4 c4)) 
     (g4 q (grace-note b4 a4)) 
     (b4 q (grace-note e5 d5 cs5)) 
     (b4 q (grace-note d4 e4 fs4 g4 a4)) 
     (g4 q (grace-note ef5 bf4 ef4)))

(cmn (size 60) staff treble a4 q. (grace-note f4 g4) g4 e f4 q g5 q. g5 e (grace-note a5) begin-slur f5 e g5 e end-slur)

(cmn (size 60) staff treble (c6 q (appoggiatura b5 a5)) (c6 q (appoggiatura bf5 as5)) (c5 q (appoggiatura b4 d5)))
(cmn staff treble (c4 q (grace-note (notes d4 (chord fs4 bf4)))))
(cmn staff treble (c4 q (grace-note (notes d4 a4))))

(cmn (size 200) staff treble (c4 q (grace-note (notes ds4 af4))))
(cmn (size 200) (note-head-size .5) ds4 e af4 e)
|#



(defun auxiliary-note-1 (at-end &rest objects)
  (let ((nt (apply #'note objects)))
    (make-self-acting 
     :argument nt
     :action #'(lambda (owner note) 
		 (add-to-marks 
		  owner
		  (list (make-instance 'sundry 
			 :name :auxiliary-note
			 :source (format nil "(auxiliary-note ~A)" note)
			 :mark #'(lambda (mark outer-note score &optional justifying)
				   (declare (ignore mark))
				   (let* ((grace-scale 
					   (if (and (matrix note) (not (identity-matrix-p note)))
					       (first (matrix note))
					     *grace-note-size*))
					  (inverse-scale (/ 1.0 grace-scale))
					  (old-separation *staff-line-separation*))
				     (if (not justifying)
					 (progn
					   (setf *staff-line-separation* (* inverse-scale old-separation))
					   (with-scaling score grace-scale 
					     (* (+ (if at-end
						       (box-x1 outer-note)
						     (box-x0 outer-note))
						   (vis-dx outer-note))
						(scr-size score))
					     (* (staff-y0 outer-note) (scr-size score))
					     (setf (staff-y0 note) 0.0)
					     (setf (line note) (place-of-note-given-note outer-note note))
					     (setf (box-y0 note) (* (line note) *staff-line-separation*))
					     (setf (center note) .143)
					     ;; can this be a chord? if so, see grace note centering case.
					     (setf (box-x0 note) 0)
					     (display note nil score))
					   (setf *staff-line-separation* old-separation))
				       (moveto score (+ (if at-end
							    (box-x0 outer-note)
							  (box-x1 outer-note))
							(vis-dx outer-note)
							(vis-dx note))
					       0)))))))))))


(defun auxiliary-note (&rest objects) (apply #'auxiliary-note-1 nil objects))
(defun auxiliary-note-at-end (&rest objects) (apply #'auxiliary-note-1 t objects))


;;;
;;; ----------------   cycle
;;;

;;; also need a reader of note names that doesn't demand the explicit octave number
;;; (e f ff d g problems)

(defun cycle (&rest objects-or-lists)
  ;; take a sequence of sequences of cmn objects and parcel them out into a single full list (score-object-list)
  ;; these are expanded by notify staff during scorify
  (let* ((len (length objects-or-lists))
	 (args (make-array len))
	 (max-list 0)
	 (data nil)
	 (cur-args (make-array len)))
    (loop for i from 0 below len and object in objects-or-lists do
      (setf (aref args i) (if (listp object) object (list object)))
      (setf (aref cur-args i) (aref args i)))
    (setf max-list (or (loop for i from 0 below len maximize (length (aref args i))) 0))
    (loop for i from 0 below max-list do
      (let ((owner (first (aref cur-args 0))))
	(push (funcall #'notify (if (write-protected owner)
				    (copy owner)
				  owner)
		       (loop for j from 1 below len collect (first (aref cur-args j)))) data))
      (loop for j from 0 below len do
	(setf (aref cur-args j) (cdr (aref cur-args j)))
	(if (not (aref cur-args j)) 
	    (setf (aref cur-args j) (aref args j)))))
    (engorge (nreverse data))))


(defun cue (&rest args)
  (flet ((scale-object (obj x y)
	   (setf (matrix obj) (scale-matrix (or (matrix obj) (list 1 0 0 1 0 0)) x y))
	   obj))
    (let ((hidden-section (make-instance 'score))
	  (data (staff-data (apply #'staff args))))
      (push hidden-section *cmn-hidden-sections*)
      (loop for n in data do 
	(when (audible-p n)
	  (setf (visible-section n) hidden-section)
	  (if (note-p n) 
	      (if (sign n) 
		  (setf (note-sign n) (scale-object (sign n) .8 .8)))
	    (if (chord-p n) 
		(loop for nn in (chord-data n) do 
		      (if (sign nn) 
			  (setf (note-sign nn) (scale-object (sign nn) .8 .8))))))))
      (engorge data))))


;;;
;;; ----------------   add-note, display-notes
;;;
;;; clm/cm/stella interface -- each instrument gives us its name, begin time, duration, frequency, and maybe dynamics etc
;;; we turn out the equivalent cmn input stream (at level of notified staff data)
;;;
;;; examples:
;;;
;;; (display-notes nil (list (list 'hi 0 1 440 .1 staccato) '(hi 1 1 660 .1)))
;;; (display-notes (list (metronome 75) (size 24) (title "Opus 1")) (list (list 'hi 0 .2 440 .1 staccato) '(hi .2 .6 660 .1)))

(defvar *exact-rhythms* nil)
;;; t if user (or cm) sending in exact rhythms and doesn't want us to try to approximate anything

(defun freq-to-pitch (freq)
  (floor (* 12 (+ (log (/ (if (numberp freq) freq (eval freq)) 16.351) 2.0) (/ 1.0 24)))))

(defvar last-clm-sign nil)

(defun pitch-to-note-octave-and-accidental (pitch) ;for clm note lists
  (let* ((pclass (mod pitch 12))
	 (octave (floor pitch 12))
	 (cclass (case pclass 
		   (0 0) 
		   (1 (if (not (eq last-clm-sign flat)) 0 1))
		   (2 1) 
		   (3 (if (not (eq last-clm-sign sharp)) 2 1))
		   (4 2) 
		   (5 3) 
		   (6 (if (not (eq last-clm-sign flat)) 3 4)) 
		   (7 4) 
		   (8 (if (not (eq last-clm-sign sharp)) 5 4))
		   (9 5) 
		   (10 (if (not (eq last-clm-sign sharp)) 6 5) )
		   (11 6))))
    (if (member pclass '(0 2 4 5 7 9 11))
	(list pclass octave (setf last-clm-sign nil) cclass)
      (if (or (and (= pclass 1) (= cclass 0))
	      (and (= pclass 3) (= cclass 1))
	      (and (= pclass 6) (= cclass 3))
	      (and (= pclass 8) (= cclass 4))
	      (and (= pclass 10) (= cclass 5)))
	  (list pclass octave (setf last-clm-sign sharp) cclass)
	(list pclass octave (setf last-clm-sign flat) cclass)))))

(defun freq-to-note-octave-and-accidental (freq)
  (pitch-to-note-octave-and-accidental (freq-to-pitch freq)))

(defun name-to-note-and-accidental (name)
  ;; give score-style note name (optional sign and octave), returns pitch, c-class, sign
  (let* ((string-name (format nil "~s" name))
	 (cclass-name (elt string-name 0))
	 (name-len (length string-name))
	 (csign (and (> name-len 1) 
		     (member (elt string-name 1) '(#\s #\s #\f #\f #\n #\n)) 
		     (elt string-name 1)))
	 (sign (if csign (if (member csign '(#\s #\s)) sharp 
			   (if (member csign '(#\f #\f)) flat
			     natural))
		 nil))
	 (octave (or (and (> name-len (if csign 2 1)) 
			  (char<= #\0 (elt string-name (if csign 2 1)) #\9)
			  (- (char-code (elt string-name (if csign 2 1))) 48))
		     old-octave))
	 (cclass (if (upper-case-p cclass-name)
		     (position cclass-name '(#\c #\d #\e #\f #\g #\a #\b))
		   (position cclass-name '(#\c #\d #\e #\f #\g #\a #\b))))
	 (pclass (+ (case cclass (0 0) (1 2) (2 4) (3 5) (4 7) (5 9) (6 11))
		    (if (and sign (eq sign sharp)) 1
		      (if (and sign (eq sign flat)) -1
			0)))))
    (setf old-octave octave)
    (list pclass octave sign cclass)))

(defun duration-to-rhythm (score dur)
  (let* ((qnotes (* dur (/  (metronome score) (* (beats-per-quarter-note score) 60))))
	 (ttnotes (floor (* qnotes 8))))
    (case ttnotes
      (1 thirty-second)
      (2 s)
      (3 s.)
      ((4 5) eighth)
      (6 e.)
      ((7 8 9 10 11) q)
      ((12 13 14) q.)
      ((15 16 17 18 19 20 21 22) h)
      ((23 24 25 26) h.)
      ((27 28 29) h..)
      (otherwise w))))


(defvar staff-aliases nil)
(defvar staff-descriptors nil)

(defun find-staff (score name)
  (when (not staff-aliases)
    (setf staff-aliases (make-hash-table :test #'equal))
    (push (make-instance 'system) (systems score)))
  (or (gethash name staff-aliases)
      (let ((stf (make-instance 'staff :name name)))
	(setf (gethash name staff-aliases) stf)
	(push stf (staves (first (systems score))))
	stf)))

(defun add-staff (score staff-name other-names &rest acts)
  (let ((stf (make-instance 'staff :name staff-name)))
    (if (not staff-aliases) (setf staff-aliases (make-hash-table :test #'equal)))
    (setf (gethash staff-name staff-aliases) stf)
    (loop for name in other-names do
      (setf (gethash name staff-aliases) stf))
    (loop for act in acts do
      (if (self-acting-p act)
	  (funcall (action act) stf (argument act))))
    (if (not (systems score)) (push (make-instance 'system) (systems score)))
    (push stf (staves (first (systems score))))
    stf))

(defvar accumulated-tempo-offset 0.0)
(defvar last-tempo-true-begin-time 0.0)
;;; these help handle tempo changes while collecting score data.
;;; the first is the integrated cmn-time at the point of the last tempo change.
;;; the second is the user's idea of the corresponding musical time of that tempo change.

(defun add-note (score name begin-time duration frequency &rest rest)
  (apply #'add-note-to-staff score (find-staff score name) begin-time duration frequency rest))

(defun score-relative-onset (score onset)
  (if *exact-rhythms*
      (if (floatp onset)
	  (iratify onset)
	onset)
    (+ accumulated-tempo-offset 
       (* .25 (round (* 4 (* (/ (metronome score) 60) 
			     (- onset last-tempo-true-begin-time))))))))

(defvar staff-meters nil)

(defun set-staff-meter (stf mtr)
  (let ((dat (find stf staff-meters :key #'first)))
    (if (not dat)
	(push (list stf mtr) staff-meters)
      (setf (second dat) mtr))))

(defun not-metered (stf)
  (let ((dat (find stf staff-meters :key #'first)))
    (or (not dat)
	(not (meter-p (second dat)))
	(eq (meter-name (second dat)) :unmetered))))

(defun add-note-to-staff (score staff begin-time duration frequency &rest rest)
  (let* ((pitch-data (if (numberp frequency)
			 (freq-to-note-octave-and-accidental frequency)
		       (list (pitch frequency) (octave frequency) (sign frequency) (cclass frequency))))
	 (nt (make-instance 'note :onset (score-relative-onset score begin-time)
			:octave (second pitch-data)
			:pitch (first pitch-data)
			:sign (third pitch-data)
			:cclass (fourth pitch-data))))
    (setf (rhythm nt) (if *exact-rhythms*
			  (if (floatp duration)
			      (iratify duration)
			    duration)
			(max .125 (* .25 (round (* 4 (/ (metronome score) 60) duration))))))

    (let ((last-obj (and (staff-data staff) (first (staff-data staff)))))
      (if (and last-obj (meter-p last-obj) (not (onset last-obj)) (not (beat last-obj)))
	  (if (not-metered staff)
	      (setf (onset last-obj) (score-relative-onset score begin-time))
	    (setf (beat last-obj) 1))))

    (if rest
	(let ((with-errors nil))
	  (push (apply #'note nt rest) (staff-data staff)))
      (push nt (staff-data staff)))))

(defun add-data (score name obj)
  (add-data-1 score (find-staff score name) obj))

(defun add-data-1 (score staff obj &key onset beat)
  (let* ((nobj (if (write-protected obj) (copy obj) obj))
	 (last-obj (and (staff-data staff) (first (staff-data staff)))))
    (if onset 
	(setf (onset nobj) (score-relative-onset score onset)))
    (if beat (setf (beat nobj) beat))
    (if (meter-p nobj) (set-staff-meter staff nobj))
    (if (self-acting-p nobj)
	(funcall (action nobj) 
		 (or (find-if #'note-p (staff-data staff))
		     (error "Can't find note for ~S in ~S" obj staff))
		 (argument nobj))
      (if (or (sundry-p nobj) (pause-p nobj) (dynamics-p nobj) (accidental-p nobj))
	  (add-to-marks (or last-obj staff) (list nobj))
	(push nobj (staff-data staff))))))

;; here if the datum calls the onset method (as in (bar (onset ...))), there should
;; first be some sort of fixup done on the onset value.


(defun set-cmn-tempo (score onset new-tempo)
  (let ((apparent-onset (+ accumulated-tempo-offset 
			   (* .25 (round (* 4 (* (/ (metronome score) 60) 
						 (- onset last-tempo-true-begin-time))))))))
    (setf accumulated-tempo-offset apparent-onset)
    (setf last-tempo-true-begin-time onset)
    (setf (metronome score) new-tempo)
    new-tempo))

(defun reinitialize-score (score)
  (setf (output-type score) :postscript)
  ;; reset internal slots and return score for reuse. hope i got all of them.
  (dolist (slot '(staves pages lines line-data time-lines time-line systems draw-list))
    (setf (slot-value score slot) nil))
  score)

(defun init-clm-input (&optional score) 
  (setf last-clm-sign nil)
  (setf staff-aliases nil)
  (setf staff-descriptors nil)
  (setf staff-meters nil)
  (setf accumulated-tempo-offset 0.0)
  (setf last-tempo-true-begin-time 0.0)
  (setf *cmn-score* (if score
                        (reinitialize-score score)
			(make-instance 'score))))

(defun begin-delayed-slur () (make-instance 'sundry :name :start-slur))
(defvar begin-delayed-slur (make-instance 'write-protected-sundry :name :start-slur))
(defun end-delayed-slur () (make-instance 'sundry :name :finish-slur))
(defvar end-delayed-slur (make-instance 'write-protected-sundry :name :finish-slur))

(defun deal-with-delayed-slurs (obj cnote)
  (if (marks cnote)
    (flet ((sun-name (n) (and (sundry-p n) (sundry-name n))))
      (let ((ss (find :start-slur (marks cnote) :key #'sun-name))
	    (fs (find :finish-slur (marks cnote) :key #'sun-name)))
	(when ss
	  (user-add-to-left-slurs obj nil)
	  (setf (marks cnote) (remove ss (marks cnote))))
	(when fs
	  (user-add-to-right-slurs obj nil)
	  (setf (marks cnote) (remove fs (marks cnote))))))))

(defun check-for-delayed-slurs (obj)
  (when obj
    (if (chord-p obj)
	(loop for cnote in (chord-data obj) do
	  (deal-with-delayed-slurs obj cnote))
      (deal-with-delayed-slurs obj obj)))
  obj)
		       

(defun check-for-chords (objects)
  (let* ((new-data nil)
	 (current-chord nil)
	 (last-object nil)
	 (last-onset nil)
	 (robjects (reverse objects)))
    (loop for object in robjects do
      (let ((this-onset (odb-onset object)))
	(if (and (audible-p object)
		 last-onset
		 (= last-onset this-onset))
	    (if current-chord
		(push object (chord-data current-chord))
	      (if (audible-p last-object)
		  (setf current-chord (chord (notes last-object object) (onset this-onset) (duration (odb-duration object))))
		(progn
		  (push last-object new-data)
		  (setf last-object object))))
	  (progn
	    (if current-chord
		(progn
		  (push (check-for-delayed-slurs current-chord) new-data)
		  (setf current-chord nil))
	      (if last-object
		  (push (check-for-delayed-slurs last-object) new-data)))
	    (setf last-object object)
	    (setf last-onset this-onset)))))
    (push (check-for-delayed-slurs (or current-chord last-object)) new-data)
    (nreverse new-data)))

(defstruct stfdat top-freq bottom-freq above-c4 below-c4 mean-freq staff tstf bstf uclf uclfs ustfs clefs)

(defun set-staff-clef (staff main-clefs &rest other-clefs)
  (let ((stfd (find staff staff-descriptors :key #'stfdat-staff)))
    (setf (stfdat-uclf stfd) main-clefs)
    (setf (stfdat-uclfs stfd) other-clefs)))

(defun set-staff-number (staff number)
  (let ((stfd (find staff staff-descriptors :key #'stfdat-staff)))
    (setf (stfdat-ustfs stfd) number)))

(defun finish-clm-input (score output-type with-onsets &rest descriptors)
  ;; until we get a better idea, descriptors is a list of lists,
  ;; each list being (function-name staff-name &rest other args)
  ;; we will run through that list, matching the staff-name to the appropriate staff,
  ;; then applying the first name to the resultant arg list.
  (let ((new-staves nil))
    (setf staff-descriptors nil)
    (map-over-staves #'(lambda (score stf)
			 (declare (ignore score))
			 (let ((topf 0) 
			       (botf 100000)
			       (sumf 0)
			       (ctf 0)
			       (ac4 0)
			       (bc4 0))
			   (when (staff-data stf) ;if no data at all, just silently flush that staff
			     (loop for object in (staff-data stf) do
			       (when (audible-p object)
				 (let ((pcl (+ (pitch object) (* 12 (octave object)))))
				   (incf sumf pcl)
				   (incf ctf)
				   (setf topf (max topf pcl))
				   (setf botf (min botf pcl))
				   (if (> pcl 48) (incf ac4))
				   (if (< pcl 48) (incf bc4)))))
			     (push (make-stfdat :top-freq topf :bottom-freq botf :mean-freq (divide sumf ctf)
						:above-c4 (divide ac4 ctf) :below-c4 (divide bc4 ctf) :staff stf)
				   staff-descriptors))))
		     score)

    (setf staff-descriptors (sort staff-descriptors #'> :key #'stfdat-mean-freq))
    (loop for desc in descriptors do
      (let ((func (first desc))
	    (args (cddr desc))
	    (staff (find-staff score (second desc))))
	(apply func staff args)))

    (loop for stf in staff-descriptors do
      ;; here check first for any user-set decisions about number of staves and respective clefs
      (if (stfdat-ustfs stf) 
	  (if (= (stfdat-ustfs stf) 0) (setf (stfdat-clefs stf) :neither)
	    (if (= (stfdat-ustfs stf) 2) (setf (stfdat-clefs stf) :both)
	      (if (= (stfdat-ustfs stf) 1) 
		  (if (stfdat-uclf stf) (setf (stfdat-clefs stf) (stfdat-uclf stf))))))) ;:treble for example
      (if (and (not (stfdat-clefs stf))
	       (stfdat-uclf stf) 
	       (not (stfdat-uclfs stf)))
	  (setf (stfdat-clefs stf) (stfdat-uclf stf)))
      (if (not (stfdat-clefs stf))	;i.e. either not ustfs or uclfs+ustfs leave us undecided so far
	  (if (and (or (> (stfdat-bottom-freq stf) 48)
		       (> (stfdat-top-freq stf) 57)
		       (> (stfdat-above-c4 stf) .25))
		   (or (not (stfdat-uclfs stf))
		       (member :treble (stfdat-uclfs stf))))
	      (setf (stfdat-clefs stf) :treble?)
	    (setf (stfdat-clefs stf) :bass?)))
      (if (not (stfdat-clefs stf))
	  (if (stfdat-uclf stf) (setf (stfdat-clefs stf) (stfdat-uclf stf))
	    (if (stfdat-uclfs stf)
		(if (member :bass (stfdat-uclfs stf)) (setf (stfdat-clefs stf) :bass)))))
      (if (and (member (stfdat-clefs stf) '(:bass? :treble?))
	       (not (stfdat-ustfs stf)))
	  (if (or (and (eq (stfdat-clefs stf) :treble?)
		       (or (< (stfdat-top-freq stf) 48)
			   (< (stfdat-bottom-freq stf) 40)))
		  (and (eq (stfdat-clefs stf) :bass?)
		       (or (> (stfdat-bottom-freq stf) 48)
			   (> (stfdat-top-freq stf) 57))))
	      (setf (stfdat-clefs stf) :both)))
      (if (eq (stfdat-clefs stf) :treble?) (setf (stfdat-clefs stf) :treble)
	(if (eq (stfdat-clefs stf) :bass?) (setf (stfdat-clefs stf) :bass)))
      (if (not (stfdat-clefs stf)) (setf (stfdat-clefs stf) :treble))
      
      (if (or (member (stfdat-clefs stf) '(:treble :both))
	      (not (member (stfdat-clefs stf) '(:bass :neither))))
	  (progn
	    (setf (stfdat-tstf stf) (make-instance 'staff :name (staff-name (stfdat-staff stf)) :staff-hooks (staff-hooks (stfdat-staff stf))))
	    (add-to-marks (stfdat-tstf stf) (list (make-staff-name)))
	    (push (stfdat-tstf stf) new-staves)))
      (if (member (stfdat-clefs stf) '(:bass :both))
	  (progn
	    (setf (stfdat-bstf stf) (make-instance 'staff :name (staff-name (stfdat-staff stf)) :staff-hooks (staff-hooks (stfdat-staff stf))))
	    (if (eq (stfdat-clefs stf) :bass) (add-to-marks (stfdat-bstf stf) (list (make-staff-name))))
	    (if (stfdat-tstf stf) (setf (staff-local-brace (stfdat-bstf stf)) brace))
	    (push (stfdat-bstf stf) new-staves)))

      (if (marks (stfdat-staff stf)) 
	  (add-to-marks (or (stfdat-tstf stf) (stfdat-bstf stf)) 
			(marks (stfdat-staff stf))))

      (let ((current-clef (if (stfdat-tstf stf) :treble :bass))
	    (last-note nil)
	    (last-onset 0.0))
	(setf (staff-data (stfdat-staff stf)) (reverse (staff-data (stfdat-staff stf))))
	(let ((next-note (cdr (staff-data (stfdat-staff stf)))))
	  (loop for note in (staff-data (stfdat-staff stf)) do
	    (when (eq (stfdat-clefs stf) :both)
	      (if (and (stfdat-uclfs stf)
		       (audible-p note)
		       (/= last-onset (odb-onset note))
		       (not (and (stfdat-tstf stf) (stfdat-bstf stf))))
		  (if (and (eq current-clef :bass)
			   (or (> (+ (pitch note) (* 12 (octave note))) 58)
			       (and (> (+ (pitch note) (* 12 (octave note))) 47)
				    next-note
				    (or (not last-note)
					(< (octave last-note) (octave note))
					(< (pitch last-note) (pitch note)))
				    (audible-p (first next-note))
				    (/= (onset note) (onset (first next-note)))
				    (> (+ (pitch (first next-note)) (* 12 (octave (first next-note)))) 48))))
		      (progn
			(push (clef treble (onset (odb-onset note))) (staff-data (or (stfdat-tstf stf) (stfdat-bstf stf))))
			(setf current-clef :treble))
		    (if (and (eq current-clef :treble)
			     (or (< (+ (pitch note) (* 12 (octave note))) 39)
				 (and (< (+ (pitch note) (* 12 (octave note))) 44)
				      next-note
				      (audible-p (first next-note))
				      (/= (onset note) (onset (first next-note)))
				      (< (+ (pitch (first next-note)) (* 12 (octave (first next-note)))) 48))))
			(progn
			  (push (clef bass (onset (odb-onset note))) (staff-data (or (stfdat-tstf stf) (stfdat-bstf stf))))
			  (setf current-clef :bass))))))
	    (if (and (or (bar-p note) (meter-p note)) ;try to catch explicit bars and meters on split staff
		     (eq (stfdat-clefs stf) :both)
		     (stfdat-tstf stf)
		     (stfdat-bstf stf))
		(progn
		  (push note (staff-data (stfdat-tstf stf)))
		  (push (copy note) (staff-data (stfdat-bstf stf))))
	      (if (and (stfdat-tstf stf)
		       (or (not (audible-p note))
			   (and (> (+ (pitch note) (* 12 (octave note))) 47)
				(stfdat-tstf stf))
			   (not (stfdat-bstf stf))))
		  (push note (staff-data (stfdat-tstf stf)))
		(push note (staff-data (stfdat-bstf stf)))))
		     
	    (if (audible-p note) (setf last-onset (odb-onset note))
	      (if (clef-p note) (setf current-clef (clef-name note))))
	    (setf next-note (cdr next-note))
	    (setf last-note note)
	    ))
	)
      )
    
    (setf (systems score) (list (make-instance 'system :staves (nreverse new-staves))))
    (map-over-staves #'(lambda (score stf)
			 (declare (ignore score))
			 (setf (staff-data stf) (check-for-chords (staff-data stf))))
		     score)
    
    (loop for stf in staff-descriptors do
      (when (not (member (stfdat-clefs stf) '(:neither :none)))
	(if (stfdat-tstf stf) 
	    (if (not (clef-p (first (staff-data (stfdat-tstf stf)))))
		(push (clef (if (member (stfdat-clefs stf) '(:treble :both)) treble
			      (case (stfdat-clefs stf)
				(:bass bass)
				(:alto alto)
				(:tenor tenor)
				(:percussion percussion)
				(:sub-bass sub-bass)
				(:double-bass double-bass)
				(:quad-bass quad-bass)
				(:baritone-F baritone-F)
				(:baritone-C baritone-C)
				(:baritone baritone)
				(:mezzo-soprano mezzo-soprano)
				(:soprano soprano)
				(:tenor-treble tenor-treble)
				(:double-treble double-treble)
				(:quad-treble quad-treble)
				(:french-violin french-violin)
				(otherwise (error "unknown clef type: ~A" type))))
			    (onset 0) (duration 0)) (staff-data (stfdat-tstf stf)))))
	(if (and (stfdat-bstf stf) 
		 (staff-data (stfdat-bstf stf)))
	    (if (not (clef-p (first (staff-data (stfdat-bstf stf)))))
		(push (clef bass (onset 0) (duration 0)) (staff-data (stfdat-bstf stf)))))))

    (map-over-staves #'(lambda (score staff)
			 (declare (ignore score))
			 (loop for hook in (staff-hooks staff) do
			   (funcall (first hook) staff (second hook))))
		     score)

    (if (eq output-type :cmn)
	(let ((file (output-file score)))
	  (setf (output-file score) "hi.eps") ;a garbage file...
	  (cmn-store (cmn score)	;this is just to get the rests added explicitly
		     (if (string-equal (pathname-type file) "cmn") file "hi.cmn")
		     with-onsets))
      (cmn score))))

#|
 (progn (setf hi (init-clm-input)) (setf ho (add-staff hi "HIHO" nil)) (setf ha (add-staff hi "HAHA" nil)) 
  (setf hu (add-staff hi "BABA" nil)) (add-note-to-staff hi ho 0 1 440) (add-note-to-staff hi ho 0 1 660)
  (add-note-to-staff hi ho 2 2 330) (add-note-to-staff hi ha 0 3 110) (add-note-to-staff hi hu 0 3 110) 
  (add-note-to-staff hi hu 1 2 550) (finish-clm-input hi nil nil))

 (progn
  (setf hi (init-clm-input))
  (setf ho (add-staff hi "HIHO" nil
             (make-self-acting
               :action #'(lambda (staff &rest rest)
                           (push (list #'(lambda (staff &rest rest)
		                           (loop for obj in (staff-data staff) do 
                                             (if (or (note-p obj) (chord-p obj)) 
	   	                               (setf (stem-direction obj) :down)))))
                             (staff-hooks staff)))
               :argument nil)))
  (add-data-1 hi ho treble) (add-data-1 hi ho (meter 3 4))
  (add-data-1 hi ho (note a4 q)) (add-data-1 hi ho begin-delayed-slur) (add-data-1 hi ho (note e5 q)) (add-data-1 hi ho end-delayed-slur)
  (add-data-1 hi ho (note e4 h)) (finish-clm-input hi nil nil))

  (defun deferred-stem-up () ;not tested yet
    (make-self-acting
      :action #'(lambda (staff &rest rest)
                  (push (list #'(lambda (staff &rest rest)
	                          (loop for obj in (staff-data staff) do 
                                    (if (or (note-p obj) (chord-p obj)) 
	                                (setf (stem-direction obj) :up)))))
                  (staff-hooks staff)))
               :argument nil))

  (defun deferred-tied-to (main)
    (make-self-acting
      :action #'(lambda (staff &rest rest)
                  (push (list
                          #'(lambda (staff main)
                             (let ((old (loop for stf in staff-descriptors if (eq main (stfdat-staff stf)) return stf)))
	                       (setf (true-staff staff) (or (stfdat-tstf old) (stfdat-bstf old)))))
                          main)
                    (staff-hooks staff)))
     :argument nil))

  (defun add-harmony (texts)
    (make-self-acting
     :action #'(lambda (score &rest rest)
	       (setf filter-hook
		     #'(lambda (score)
			 (let* ((bottom-staff (first (last (staves (first (last (systems score)))))))
				(last-obj nil)
				(last-cx 0)
				(beg -1)
				(cx 0)
				(objs (staff-data bottom-staff))
				(obj nil))
			   (loop for time in (time-line score) and txt in texts do
			     (when (> (tld-time time) beg)
			       (setf last-obj obj)
			       (setf last-beg beg)
			       (setf last-cx cx)
			       (setf objs (cdr objs))
			       (loop while (and objs (not (note-p (first objs))) (not (chord-p (first objs)))) do (setf objs (cdr objs))))
			     (when objs
			       (setf obj (first objs))
			       (setf cx (tld-cx time))
			       (setf beg (onset obj))
			       (if (= (tld-time time) beg)
				   (add-to-marks obj (list (apply #'fingering txt)))
				 (if last-obj
				     (add-to-marks last-obj (list (apply #'fingering (append txt (list (dx (- cx last-cx))))))))))))
			 score)))))


  ;;; (cmn (add-harmony '((1 2) ("g" "4#") ("I" 7))) treble c4 e c4 e g4 e)
						       

  ;;; and for a global last-minute meter specification:

  (defun set-meter (staff meter) 
    (loop for stf in staff-descriptors do 
      (setf (staff-data (stfdat-staff stf)) 
        (append (staff-data (stfdat-staff stf)) (list (eval meter))))))

  (progn (setf hi (init-clm-input)) (setf ho (add-staff hi "HIHO" nil)) (setf ha (add-staff hi "HA" nil (deferred-tied-to ho))) 
  (add-note-to-staff hi ha 0 4 220) (add-note-to-staff hi ho 0 1 440) (add-data hi "HIHO" begin-delayed-slur)
  (add-note-to-staff hi ho 1 1 660) (add-data hi "HIHO" end-delayed-slur) (add-note-to-staff hi ho 2 2 330)
  (finish-clm-input hi nil nil '(set-staff-clef "HIHO" :treble) '(set-meter "HIHO" (meter 3 4))))

  (progn (setf hi (init-clm-input)) (setf ho (add-staff hi "HIHO" nil)) (add-data-1 hi ho (meter 2 4))
  (add-note-to-staff hi ho 0 1 440) (add-note-to-staff hi ho 2.5 1 660) (add-data-1 hi ho (meter 3 4 (beat 1)))
  (add-note-to-staff hi ho 5 2 330) (finish-clm-input hi nil nil))

  ;; two clefs with barline
  (progn (setf hi (init-clm-input)) (setf ho (add-staff hi "HIHO" nil)) 
  (add-note-to-staff hi ho 0 1 110) (add-note-to-staff hi ho 1 1 440) (add-data hi "HIHO" bar)
  (add-note-to-staff hi ho 2 1 660) (add-note-to-staff hi ho 3 2 330)
  (finish-clm-input hi nil nil))
|#

(defun make-name-a-string (name) (format nil "~S" name))

(defun display-notes (args notes)
  (let ((new-score (init-clm-input)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) new-score (argument arg))))
    (loop for note in notes do (apply #'add-note new-score (make-name-a-string (first note)) (cdr note)))
    (finish-clm-input new-score nil nil)))

#|
;;; (defun set-staff-clef (staff main-clefs &rest other-clefs)
;;; (defun set-staff-number (staff number)
;;; (defun add-staff (score staff-name other-names &rest acts)
;;; (defun add-note (score name begin-time duration frequency &rest rest)
;;; (defun add-data (score name data)

;;; (dpyit (list (metronome 75)) (list (list 'hi (bar (onset 0))) (list 'hi (bass (onset 0))) (list 'hi (meter 3 4 (onset 0))) (list 'hi 0 .2 440 .1 staccato) '(silver .2 .6 660 .1) (list 'hi (bar (onset .8))) (list :score 'set-cmn-tempo .8 60) '(hi .8 1.0 440) '(ho 1.8 .5 220)))

(defun dpyit (args notes)
  (let ((new-score (init-clm-input)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) new-score (argument arg))))
    (let ((hi-staff (add-staff new-score "HI" '("HO" "SILVER"))))
      (loop for note in notes do 
	(if (eq (first note) :score)
	    (apply (second note) *cmn-score* (cddr note))
	  (if (numberp (second note))
	      (apply #'add-note new-score (make-name-a-string (first note)) (cdr note))
	    (apply #'add-data new-score (make-name-a-string (first note)) (cdr note)))))
      (finish-clm-input new-score nil nil '(set-staff-clef "HI" :none)))))
|#

(defun add-clef (score staff type &optional size-factor)
  (declare (ignore score))
  (push (clef (if (clef-p type) type 
		(case type
		  (:bass bass)
		  (:treble treble)
		  (:alto alto)
		  (:tenor tenor)
		  (:percussion percussion)
		  (:sub-bass sub-bass)
		  (:double-bass double-bass)
		  (:quad-bass quad-bass)
		  (:baritone-F baritone-F)
		  (:baritone-C baritone-C)
		  (:baritone baritone)
		  (:mezzo-soprano mezzo-soprano)
		  (:soprano soprano)
		  (:tenor-treble tenor-treble)
		  (:double-treble double-treble)
		  (:quad-treble quad-treble)
		  (:french-violin french-violin)
		  (otherwise (error "unknown clef type: ~A" type))))
	      (if size-factor (scale size-factor size-factor)))
	(staff-data staff)))

(defun add-bar (score staff &optional (type :bar) bar-meter)
  (declare (ignore score))
  (push (bar (if (bar-p type) type
	       (case type
		 (:bar bar)
		 (:double-bar double-bar)
		 (:interior-double-bar interior-double-bar)
		 (:dashed-bar dashed-bar)
		 (:begin-repeat-bar begin-repeat-bar)
		 (:end-repeat-bar end-repeat-bar)
		 (:begin-and-end-repeat-bar begin-and-end-repeat-bar))))
	(staff-data staff))
  (if bar-meter
      (push (apply #'meter bar-meter) (staff-data staff))))

(defun set-tempo (score staff time tempo &optional pulse draw?)
  (let ((rhy (if (not pulse) q
	       (case pulse
		 (:q q)
		 (:e e)
		 (:q. q.)
		 (:h h)
		 (:w w)
		 (:h. h.)
		 (:s s)
		 (otherwise (error "unknown tempo pulse: ~A" pulse))))))
    (set-cmn-tempo score time (* tempo (quarters rhy)))
    (when draw?
      (if (not (staff-data staff))
	  (add-to-marks staff (list (mm tempo rhy)))
	(if (meter-p (first (staff-data staff)))
	    (add-to-marks (second (staff-data staff)) (list (mm tempo rhy)))
	  (add-to-marks (first (staff-data staff)) (list (mm tempo rhy))))))))
    
#|

(progn (setf hi (init-clm-input)) (setf ho (add-staff hi "HIHO" nil)) (add-clef hi ho :treble) (add-note-to-staff hi ho 0 1 440)
  (add-data hi "HIHO" begin-delayed-slur) (add-note-to-staff hi ho 1 1 660) (add-data hi "HIHO" end-delayed-slur)
  (add-note-to-staff hi ho 2 2 330) (finish-clm-input hi nil nil '(set-staff-clef "HIHO" :none)))
|#
      


;;;
;;; ----------------   cmn-store
;;;

;(defvar cmn-store-tags nil)		;a list of (obj str) pairs (in cmn1)
(defvar cmn-store-tag-ctr 0)
(defun new-cmn-store-tag (&optional (name "tag-")) (format nil "~A~D" name (incf cmn-store-tag-ctr)))

(defun check-for-cmn-store-tag (obj)	;returns a string suitable for format ~A
  (let ((pair (find obj cmn-store-tags :key #'first :test #'equal)))
    (if pair
	(progn
	  (setf cmn-store-tags (remove pair cmn-store-tags))
	  (format nil " ~A~A" 
		  (second pair)
		  (check-for-cmn-store-tag obj)))
      "")))

(defun add-to-cmn-store-tags (obj str)
  (push (list obj str) cmn-store-tags))

(defun cmn-store (&optional (scr *cmn-score*) (file "hi.cmn") (with-onsets t))
  (setf cmn-store-tags nil)
  (setf cmn-store-tag-ctr 0)
  (with-open-file (s file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s ";;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-~%~
               ;;; cmn structure stored ~A~%~
               (in-package :cmn)~%~%~
               (cmn" (creation-date))
    (identify-score scr s with-onsets)
    (format s ")~%")))

(defun identify-score (scr s with-onsets)
  (if (title scr) 
      (if (not (text-p (title scr)))
	  (format s " (title ~S)" (title scr))
	(format s " (title ~A)" (identify (title scr)))))
  (format s "~{~A~}" (style-list scr nil t))
  (identify-marks scr s)
  (let ((tied-to-staves (remove-duplicates 
			 (loop for system in (systems scr) append
			       (loop for staff in (staves system)
				 if (true-staff staff) collect (true-staff staff)))))
	(tied-to-notes nil)
	(tied-to-ctr 0))
    (loop for system in (systems scr) do
      (format s "~%  (system")
      (identify-marks system s)
      (if (bracketing system) (format s " ~A " (identify (bracketing system))))
      (loop for staff in (staves system) do
	(let ((staff-y0 (staff-y0 staff))
	      (last-object nil)
	      (possible-line-break nil)
	      (first-object t))
	  (if (not (member staff tied-to-staves))
	      (format s "~%    (staff")
	    (format s "~%    (setf staff-~D (staff" (position staff tied-to-staves)))
	  (if (true-staff staff)
	      (format s " (tied-to staff-~D)" (position (true-staff staff) tied-to-staves))
	    (progn
	      (format s "~A" (identify-visible staff))
	      (if (staff-size staff) (format s " (staff-size ~1,3F)" (staff-size staff)))
	      (if (staff-lines staff) (format s " (staff-lines ~D)" (staff-lines staff)))
	      (if (staff-user-data staff) (identify-user-data (staff-user-data staff) s) "")
	      (if (staff-name staff) 
		  (if (not (text-p (staff-name staff)))
		      (format s " (staff-name ~S)" (staff-name staff))
		    (format s " (staff-name ~A)" (identify (staff-name staff)))))))
	  (identify-marks staff s)
	  (loop for object in (staff-data staff) do
	    (let ((cur-staff-y0 (or (staff-y0 object) (box-y0 object))))
	      (if (and (not (staff-p object)) ;might want to put line-mark here
		       (or (not (bar-p object))
			   (not (bar-p last-object))) ;there's an extra left-justified bar sometimes
		       (or (not (brace-p object)) ; -> (cmn (system brace (staff treble) (staff bass)))
			   (not (bracketing system))
			   (not (brace-p (bracketing system))))
		       (or first-object
			   (= cur-staff-y0 staff-y0)
			   (not (or (clef-p object)
				    (key-p object)
				    (brace-p object)
				    (bracket-p object)
				    (bar-p object)))))
		  (let ((cur-onset (odb-onset object))
			(cur-stem (and (audible-p object) (audible-stem-direction object))))
		    (setf first-object nil)
		    (setf staff-y0 cur-staff-y0)
		    (setf last-object object)
		    (when (not with-onsets) 
		      (setf (odb-onset object) nil)
		      (if (chord-p object)
			  (loop for n in (chord-data object) do (setf (odb-onset n) nil)))
		      (if (audible-p object) 
			  (setf (audible-stem-direction object) nil)))
		    (if (stem-tie object)
			(if (not (audible-p (stem-tie object)))
			    (let ((note-name (format nil "tied-note-~D" (incf tied-to-ctr))))
			      (push (list object note-name) tied-to-notes)
			      (format s "~%      (setf ~A ~A)" note-name (identify object)))
			  (let ((tied-note-name (second (find (stem-tie object) tied-to-notes :test #'eq :key #'first)))
				(tied-tag (find :tied-to (store-data object))))
			    (if (not tied-tag)
				(push (list :tied-to tied-note-name) (store-data object))
			      (setf (second tied-tag) tied-note-name))
			    (format s "~%      ~A" (identify object))))
		      (if (or with-onsets
			      (and (not (line-p object))
				   (not (page-p object))))
			  (format s  "~%      ~A" (identify object))))
		    (when (not with-onsets) 
		      (setf (odb-onset object) cur-onset)
		      (if (chord-p object)
			  (loop for n in (chord-data object) do (setf (odb-onset n) cur-onset)))
		      (if (audible-p object) 
			  (setf (audible-stem-direction object) cur-stem)))
		    (if (audible-p object)
			(setf possible-line-break (and (store-data object)
						       (member :line-break (store-data object)))))
		    ))
	      (if (staff-p object)
		  (progn
		    (if possible-line-break
			(if with-onsets (format s "~%      (line-mark)")))
		    (setf possible-line-break nil)))
	      ))
	  (if (not (member staff tied-to-staves))
	      (format s ")")
	    (format s "))"))))
      (format s ")"))))

;; Written by Sam Heisz, January 1998.
;; Idea and RMS formulas taken from Csound unit generators 
;; balance, rms, and gain.
;; see notes at bottom of file

;; an rmsgain structure is used by the rms, gain, and balance generators
(def-clm-struct rmsgain 
    c1 c2				; filter coeffiecients 
    q					; rms of the input (compared) signal
    r					; rms of the output (gained) signal
    avg avgc)			; average gain reporting

;; hp is the half-power point (in cps) of an internal low-pass filter
(defun make-rms-gain (hp)
  (let* ((b (- 2.0 (cos (* hp (/ two-pi *srate*)))))
	 (c2 (- b (sqrt (- (* b b) 1))))
	 (c1 (- 1 c2)))
    
    (make-rmsgain :c1 c1 :c2 c2 :q 0 :r 0 :avg 0.0 :avgc 0)))


(defun make-rms (&optional (hp 10)) (make-rms-gain hp))
(defun make-gain (&optional (hp 10)) (make-rms-gain hp))

(defun make-balance (&optional (hp 10)) (make-rms-gain hp))

(defmacro rms (b sig)
  `(sqrt (setf (rmsgain-q ,b) (+ (* (rmsgain-c1 ,b) ,sig ,sig)
				 (* (rmsgain-c2 ,b) (rmsgain-q ,b))))))
	 

(defmacro gain (b sig rms)
  `(progn (setf (rmsgain-r ,b) (+ (* (rmsgain-c1 ,b) ,sig ,sig)
				  (* (rmsgain-c2 ,b) (rmsgain-r ,b))))
	  (let ((g_a+i_n (if (zerop (rmsgain-r ,b))
				   ,rms 
				 (/ ,rms (sqrt (rmsgain-r ,b))))))
	    (setf (rmsgain-avg ,b)  (+ (rmsgain-avg ,b) g_a+i_n))
	    (setf (rmsgain-avgc ,b) (+ (rmsgain-avgc ,b) 1))
	    (* ,sig g_a+i_n ))))

(defmacro balance (b signal compare)
  `(gain ,b ,signal (rms ,b ,compare)))

(defmacro gain-avg (b) `(/ (rmsgain-avg ,b) (rmsgain-avgc ,b)))
(defmacro balance-avg (b) `(gain-avg ,b))
  
#| NOTE =======================================================

sudden increases in the amplitude of either signals to balance can cause
amplitude to be too high for a brief time. If experiencing a short burst of 
high amplitude (i.e. higher than the compare signal) right at the onset of 
the output try putting a small attack envelope on the compare signal.

i.e if your compare signal looks like:

1.0| 
   |_______
0.5|       \___________
   |
1.0|________________________


the sharp attack will cause the balance generator to overcompensate in trying 
bring the output signal up.

If the signal gets suddenly much greater than the compare signal, it
will take a bit of time for the compare signal to bring it back down
and there will be a peak.

I did some experiments with this, and the only cases that seemed to 
cause problems were:

the compare signal is high at the very onset (ie sample 0), & the signal is low 
the signal makes a sudden jump high when the compare stays low.

My testing, however, was not exhaustive.

I tried running a second balance generator on the output of the first
i.e: (balance b2 (balance b1 sig comp) comp)
and this made the peaks in the above two cases smaller but made peaks in
almost all sudden changes.

|#

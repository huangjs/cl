(in-package :sa)

(defparameter *vector-size* 256)
(defparameter *fft-size* 1024)
(defparameter *sample-rate* 44100.0)
(defparameter *table-length* 512)
(defparameter *amp-scaling-factor* 0.00003052)
(defparameter *max-scale* 32767.0)

(defun get-option (kw alist)
  (let ((res (getf alist kw)))
    (if res
	res
	nil)))

(defmethod foreign-float-array ((obj t))
  (if (cffi:pointerp obj)
      obj
      (error "cannot convert ~s to float array" obj)))

(defmethod foreign-float-array ((obj list))
  (let* ((len (length obj))
	 (farray (cffi:foreign-alloc ':float :count len)))
    (loop for f in obj for i from 0
	 do
	 (setf (cffi:mem-aref farray ':float i) (float f)))
    farray))

(defmethod foreign-float-array ((obj array))
  (let* ((len (length obj))
	 (farray (cffi:foreign-alloc ':float :count len)))
    (loop for f across obj for i from 0
	 do
	 (setf (cffi:mem-aref farray ':float i) (float f)))
    farray))

(defmethod double-array ((obj t))
  (if (cffi:pointerp obj)
      obj
      (error "cannot convert ~s to double array" obj)))

(defmethod double-array ((obj list))
  (let* ((len (length obj))
	 (farray (cffi:foreign-alloc ':double :count len)))
    (loop for f in obj for i from 0
	 do
	 (setf (cffi:mem-aref farray ':double i) (float f)))
    farray))

(defmethod foreign-double-array ((obj array))
  (let* ((len (length obj))
	 (farray (cffi:foreign-alloc ':double :count len)))
    (loop for f across obj for i from 0
	 do
	 (setf (cffi:mem-aref farray ':double i) (float f)))
    farray))

(defun valid-ptr (obj)
  (if obj
      (if (cffi:pointerp obj)
	  obj
	  (if (slot-value obj 'object-pointer)
	      (slot-value obj 'object-pointer)
	      (cffi:null-pointer)))
      (cffi:null-pointer)))

(defun object-ptr (obj)
  (if obj
      (if (cffi:null-pointer-p (slot-value obj 'object-pointer))
	  nil
	  t)
      nil))




(defgeneric init-pointer (obj))
(defgeneric do-process (obj))
(defmethod do-process ((obj t))
  (error "no"))
(defgeneric destroy (obj))


;
; SndObj
;


(defclass sndobj ()
  ((object-pointer :initform nil :accessor object-pointer)
   (name :initform nil :initarg :name)
   (vecsize :initform *vector-size* :initarg :vecsize)
   (sr :initform *sample-rate* :initarg :sr)
   (sig-inputs :initform nil)
   (visited-p :initform nil)))


;;;;;;;fills this
(defmethod init-pointer ((obj sndobj))
  ())

(defmethod initialize-instance :after ((obj sndobj) &rest initargs)
  (declare (ignore slot-names initargs))
  (funcall #'init-pointer obj))


(defmethod processing? ((obj sndobj))
  (SndObj_IsProcessing (valid-ptr obj)))

(defmethod get-error ((obj sndobj))
  (SndObj_GetError (valid-ptr obj)))

(defmethod push-in ((obj sndobj) vec)
  (SndObj_PushIn (valid-ptr obj)
		 (foreign-float-array vec)
		 (length vec)))

(defmethod pop-out ((obj sndobj) vec)
  (SndObj_PopOut (valid-ptr obj)
		 (foreign-float-array vec)
		 (length vec)))

(defmethod add-out ((obj sndobj) vec)
  (SndObj_AddOut (valid-ptr obj)
		 (foreign-float-array vec)
		 (length vec)))

(defmethod do-process ((obj sndobj))
  (SndObj_DoProcess (valid-ptr obj)))

;;get-msg-list

(defmethod enable ((obj sndobj))
  (SndObj_Enable (valid-ptr obj)))

(defmethod disable ((obj sndobj))
  (SndObj_Disable (valid-ptr obj)))

#|
(defmethod get-output ((obj sndobj) pos)
  (SndObj_Output (valid-ptr obj) pos))
|#
(defmethod get-vector-size ((obj sndobj))
  (SndObj_GetVectorSize (valid-ptr obj)))

(defmethod set-vector-size ((obj sndobj) size)
  (SndObj_SetVectorSize (valid-ptr obj) size))

(defmethod get-sr ((obj sndobj))
  (SndObj_GetSr (valid-ptr obj)))

(defmethod set-sr ((obj sndobj) sr)
  (SndObj_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj sndobj) (str string) val)
  (SndObj_Set (valid-ptr obj)
	      str (float val)))

;;how would this be used??
(defmethod connect ((obj sndobj) (str string) ptr)
  (SndObj_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))


(defmethod do-process ((obj sndobj))
  (if (object-ptr obj)
      (SndObj_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sndobj))
  (if (object-ptr obj)
      (delete_SndObj (object-pointer obj))))


(defun make-sndobj-list (lst)
  (let ((arr (cffi:foreign-alloc :pointer :count (length lst))))
    (loop for so in lst for i from 0
	 do
	 (setf (cffi:mem-aref arr :pointer i) (object-pointer so)))
    arr))


;
; Table
;

(defclass table ()
  ((object-pointer :initform nil :accessor object-pointer)
   (length :initform *table-length* :initarg :length)))

(defmethod init-pointer ((obj table))
  ())

(defmethod initialize-instance :after ((obj table) &rest initargs)
  (declare (ignore slot-names initargs))
  (funcall #'init-pointer obj))

(defmethod get-length ((obj table))
  (Table_GetLen (valid-ptr obj)))

;;returns pointer of float array
(defmethod get-table ((obj table))
  (Table_GetTable (valid-ptr obj)))

(defmethod lookup-index ((obj table) pos)
  (Table_Lookup (valid-ptr obj) pos))

(defmethod make-table ((obj table))
  (Table_MakeTable (valid-ptr obj)))



;;
;; ADSR
;;

(defclass adsr (sndobj)
  ((att :initform nil :initarg :att)
   (max-amp :initform nil :initarg :max-amp)
   (dec :initform nil :initarg :dec)
   (sus :initform nil :initarg :sus)
   (rel :initform nil :initarg :rel)
   (dur :initform nil :initarg :dur)
   (input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj adsr))
  (with-slots (att max-amp dec sus rel dur input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ADSR (float att) (float max-amp) (float dec) (float sus) (float rel) (float dur) 
			      (valid-ptr input) vecsize (float sr))))))

(defmethod set-sr ((obj adsr) sr)
  (ADSR_SetSr (valid-ptr obj) (float sr)))

(defmethod set-max-amp ((obj adsr) max-amp)
  (ADSR_SetMaxAmp (valid-ptr obj) (float max-amp)))

(defmethod (setf max-amp) (amp (obj adsr))
  (set-max-amp obj amp)
  (setf (slot-value obj 'max-amp) (float amp)))

(defmethod sustain ((obj adsr))
  (ADSR_Sustain (valid-ptr obj)))

(defmethod release ((obj adsr))
  (ADSR_Release (valid-ptr obj)))

(defmethod restart ((obj adsr))
  (ADSR_Restart (valid-ptr obj)))

(defmethod set-adsr ((obj adsr) att dec sus rel)
  (ADSR_SetADSR (valid-ptr obj) (float att) (float dec) (float sus) (float rel)))

(defmethod set-parameter ((obj adsr) (str string) val)
  (ADSR_Set (valid-ptr obj)
	    str (float val)))

(defmethod do-process ((obj adsr))
  (if (object-ptr obj)
      (ADSR_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj adsr))
  (if (object-ptr obj)
      (delete_ADSR (object-pointer obj))))



;;;
;;;  IADSR
;;;

(defclass iadsr (adsr)
  ((init :initform nil :initarg :init)
   (end :initform nil :initarg :end)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj iadsr))
  (with-slots (init att max-amp dec sus rel end dur input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_IADSR (float init) (float att) (float max-amp) (float dec) (float sus)
		       (float rel) (float end) (float dur) (valid-ptr input)
		       vecsize (float sr))))))

(defmethod set-init ((obj iadsr) init)
  (IADSR_SetInit (valid-ptr obj) (float init)))

(defmethod set-end ((obj iadsr) end)
  (IADSR_SetEnd (valid-ptr obj) (float end)))

(defmethod set-parameter ((obj iadsr) (str string) val)
  (IADSR_Set (valid-ptr obj)
	     str (float val)))

(defmethod do-process ((obj iadsr))
  (if (object-ptr obj)
      (IADSR_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj iadsr))
  (if (object-ptr obj)
      (delete_IADSR (object-pointer obj))))




;;
;; Balance
;;

(defclass balance (sndobj)
  ((input1 :initform nil :initarg :input1)
   (input2 :initform nil :initarg :input2)
   (lp-freq :initform 10.0 :initarg :lp-freq)
   (sig-inputs :initform '(input1 input2))))

(defmethod init-pointer ((obj balance))
  (with-slots (input1 input2 lp-freq vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Balance (valid-ptr input1) (valid-ptr input2)
				 (float lp-freq) vecsize (float sr))))))

(defmethod set-inputs ((obj balance) (input1 sndobj) (input2 sndobj))
  (Balance_SetInput (valid-ptr obj) (valid-ptr input1) (valid-ptr input2)))

(defmethod set-lp-freq ((obj balance) freq)
  (Balance_SetLPFreq (valid-ptr obj) (float freq)))

(defmethod set-sr ((obj balance) sr)
  (Balance_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj balance) (str string) val)
  (Balance_Set (valid-ptr obj)
	       str (float val)))

(defmethod connect ((obj balance) (str string) ptr)
  (Balance_Connect (valid-ptr obj)
		   str (valid-ptr ptr)))

(defmethod do-process ((obj balance))
  (if (object-ptr obj)
      (Balance_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj balance))
  (if (object-ptr obj)
      (delete_Balance (object-pointer obj))))



;;
;; Buzz
;;

(defclass buzz (sndobj)
  ((freq :initform nil :initarg :freq)
   (amp :initform nil :initarg :amp)
   (harms :initform nil :initarg :harms)
   (freq-mod :initform nil :initarg :freq-mod)
   (amp-mod :initform nil :initarg :amp-mod)
   (sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj buzz))
  (with-slots (freq amp harms freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Buzz (float freq) (float amp) harms (valid-ptr freq-mod)
			      (valid-ptr amp-mod) vecsize (float sr))))))

(defmethod set-freq ((obj buzz) freq &optional (freq-mod nil))
  (Buzz_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-amp ((obj buzz) amp &optional (amp-mod nil))
  (Buzz_SetAmp (valid-ptr obj) (float amp) (valid-ptr amp-mod)))

(defmethod set-harm ((obj buzz) harm &optional ignore)
  ignore
  (Buzz_SetHarm (valid-ptr obj) harm))

(defmethod set-sr ((obj buzz) sr)
  (Buzz_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj buzz) (str string) val)
  (Buzz_Set (valid-ptr obj)
	      str (float val)))

(defmethod connect ((obj buzz) (str string) ptr)
  (Buzz_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod do-process ((obj buzz))
  (if (object-ptr obj)
      (Buzz_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj buzz))
  (if (object-ptr obj)
      (delete_Buzz (object-pointer obj))))

;;
;; Convol
;; 

(defclass convol (sndobj)
  ((impulse :initform nil :initarg :impulse)
   (input :initform nil :initarg :input)
   (scale :initform nil :initarg :scale)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj convol))
  (with-slots (impulse input chan scale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Convol (valid-ptr impulse)
			(valid-ptr input)
			(float scale) vecsize (float sr))))))

(defmethod connect ((obj convol) (str string) ptr)
  (Convol_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod set-parameter ((obj convol) (str string) val)
  (Convol_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-impulse ((obj convol) (impulse table) scale)
  (Convol_SetImpulse (valid-ptr obj) (valid-ptr impulse) scale))

(defmethod do-process ((obj Convol))
  (if (object-ptr obj)
      (Convol_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj Convol))
  (if (object-ptr obj)
      (delete_Convol (object-pointer obj))))



;;
;; DelayLine
;;

(defclass delay-line (sndobj)
  ((delaytime :initform nil :initarg :delaytime)
   (input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj delay-line))
  (with-slots (delaytime input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_DelayLine (float delaytime) (valid-ptr input)
				   vecsize (float sr))))))

(defmethod set-delaytime ((obj delay-line) delaytime)
  (DelayLine_SetDelayTime (valid-ptr obj) (float delaytime)))

(defmethod get-delaytime ((obj delay-line))
  (DelayLine_GetDelayTime (valid-ptr obj)))

(defmethod reset-delay-line ((obj delay-line))
  (DelayLine_Reset (valid-ptr obj)))

(defmethod set-sr ((obj delay-line) sr)
  (DelayLine_SetSr (valid-ptr obj) (float sr)))
 
(defmethod set-parameter ((obj delay-line) (str string) val)
  (DelayLine_Set (valid-ptr obj)
		 str (float val)))

(defmethod do-process ((obj delay-line))
  (if (object-ptr obj)
      (DelayLine_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj delay-line))
  (if (object-ptr obj)
      (delete_DelayLine (object-pointer obj))))

;;;
;;; Comb
;;;

(defclass comb (delay-line)
  ((gain :initform nil :initarg :gain)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj comb))
  (with-slots (delaytime gain input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Comb (float gain) (float delaytime)
		      (valid-ptr input)
		      vecsize sr)))))

(defmethod set-gain ((obj comb) gain)
  (Comb_SetGain (valid-ptr obj) gain))

(defmethod set-parameter ((obj comb) (str string) val)
  (Comb_Set (valid-ptr obj)
	    str (float val)))

(defmethod do-process ((obj comb))
  (if (object-ptr obj)
      (Comb_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj comb))
  (if (object-ptr obj)
      (delete_Comb (object-pointer obj))))


;;;;
;;;; Allpass
;;;;

(defclass all-pass (comb)
  ((gain :initform nil :initarg :gain)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj all-pass))
  (with-slots (gain delaytime input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_AllPass (float gain) (float delaytime) (valid-ptr input)
			 vecsize (float sr))))))

(defmethod do-process ((obj all-pass))
  (if (object-ptr obj)
      (Allpass_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj all-pass))
  (if (object-ptr obj)
      (delete_Allpass (object-pointer obj))))



;;;
;;; FIR
;;;

(defclass fir (delay-line)
  ((table :initform nil :initarg :table)
   (input :initform nil :initarg :input)
   (impulse :initform nil :initarg :impulse)
   (impulse-size :initform nil :initarg :impulse-size)
   (sig-inputs :initform '(input))))


(defmethod init-pointer ((obj fir))
  (with-slots (table input impulse impulse-size vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if impulse
		(new_FIR_impulse (foreign-float-array impulse)
				 impulse-size
				 (valid-ptr input)
				 vecsize (float sr))
		(new_FIR (valid-ptr table)
			 (valid-ptr input)
			 vecsize (float sr)))))))

(defmethod set-table ((obj fir) (tab table))
  (FIR_SetTable (valid-ptr obj) (valid-ptr tab)))

(defmethod set-impulse ((obj fir) impulse impulse-size)
  (FIR_SetImpulse (valid-ptr obj) (foreign-float-array impulse)
		  (floor impulse-size)))

(defmethod set-delaytime ((obj fir) delaytime)
  (FIR_SetDelayTime (valid-ptr obj) (float delaytime)))

(defmethod set-parameter ((obj fir) (str string) val)
  (FIR_Set (valid-ptr obj)
		 str (float val)))

(defmethod connect ((obj fir) (str string) ptr)
  (FIR_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj fir))
  (if (object-ptr obj)
      (FIR_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fir))
  (if (object-ptr obj)
      (delete_FIR (object-pointer obj))))


;;;
;;; Pitch Transpose
;;;

(defclass pitch-transpose (delay-line)
  ((pitch :initform nil :initarg :pitch)
   (semitones :initform nil :initarg :semitones)
   (sig-inputs :initform '(input))))


(defmethod init-pointer ((obj pitch-transpose))
  (with-slots (delaytime pitch semitones input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if semitones
		(new_Pitch_semitones (float delaytime) (valid-ptr input)
				     semitones vecsize (float sr))
		(new_Pitch (float delaytime) (valid-ptr input) pitch
			   vecsize (float sr)))))))

(defmethod set-pitch ((obj pitch-transpose) pitch &optional ignore) 
  ignore
  (Pitch_SetPitch (valid-ptr obj) (float pitch)))

(defmethod set-semitones ((obj pitch-transpose) st)
  (Pitch_SetPitch_semitones (valid-ptr obj) (floor st)))

(defmethod set-parameter ((obj pitch-transpose) (str string) val)
  (Pitch_Set (valid-ptr obj)
	     str (float val)))

(defmethod do-process ((obj pitch-transpose))
  (if (object-ptr obj)
      (Pitch_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pitch-transpose))
  (if (object-ptr obj)
      (delete_Pitch (object-pointer obj))))



;;;
;;; SndLoop 
;;;

(defclass sndloop (delay-line)
  ((xfadetime :initform nil :initarg :xfadetime)
   (looptime :initform nil :initarg :looptime)
   (input :initform nil :initarg :input)
   (pitch :initform 1.0 :initarg :pitch)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj sndloop))
  (with-slots (xfadetime looptime input pitch vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Comb (float xfadetime) (float looptime)
		      (valid-ptr input)
		      vecsize (float sr))))))

(defmethod set-xfade ((obj sndloop) xfadetime)
  (SndLoop_SetXFade (valid-ptr obj) (float xfadetime)))

(defmethod set-pitch ((obj sndloop) pitch &optional ignore)
  ignore
  (SndLoop_SetPitch (valid-ptr obj) (float pitch)))

(defmethod re-sample ((obj sndloop))
  (SndLoop_ReSample (valid-ptr obj)))

(defmethod set-parameter ((obj sndloop) (str string) val)
  (SndLoop_Set (valid-ptr obj)
	       str (float val)))

(defmethod do-process ((obj sndloop))
  (if (object-ptr obj)
      (SndLoop_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sndloop))
  (if (object-ptr obj)
      (delete_SndLoop (object-pointer obj))))


;;;
;;; StringFlt
;;;

(defclass string-flt (delay-line)
  ((freq :initform nil :initarg :freq)
   (fdbgain :initform nil :initarg :fdbgain)
   (input :initform nil :initarg :input)
   (freq-mod :initform nil :initarg :freq-mod)
   (decay :initform nil :initarg :decay)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj string-flt))
  (with-slots (freq fdbgain decay input freq-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if decay
		(new_StringFlt_decay (float freq) (valid-ptr input)
				     (float decay) 
				     (valid-ptr freq-mod)
				     vecsize (float sr))
		(new_StringFlt (float freq) (float fdbgain)
			       (valid-ptr input)
			       (valid-ptr freq-mod)
			       vecsize (float sr)))))))

(defmethod set-sr ((obj string-flt) sr)
  (StringFlt_SetSr (valid-ptr obj) (float sr)))

(defmethod set-decay ((obj string-flt) decay)
  (StringFlt_SetDecay (valid-ptr obj) (float decay)))

(defmethod set-freq ((obj string-flt) freq &optional (freq-mod nil))
  (StringFlt_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-fdbgain ((obj string-flt) fdbgain)
  (StringFlt_SetDecay (valid-ptr obj) (float fdbgain)))

(defmethod set-parameter ((obj string-flt) (str string) val)
  (StringFlt_Set (valid-ptr obj)
		 str (float val)))

(defmethod connect ((obj string-flt) (str string) ptr)
  (StringFlt_Connect (valid-ptr obj)
		     str (valid-ptr ptr)))

(defmethod do-process ((obj string-flt))
  (if (object-ptr obj)
      (StringFlt_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj string-flt))
  (if (object-ptr obj)
      (delete_StringFlt (object-pointer obj))))


;;;;
;;;; Pluck
;;;;

(defclass pluck (string-flt)
  ((amp :initform nil :initarg :amp)
   (maxscale :initform 1.0 :initarg :maxscale)
   (sig-inputs :initform '(freq-mod))))

(defmethod init-pointer ((obj pluck))
  (with-slots (freq amp fdbgain decay input freq-mod maxscale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if decay
		(new_Pluck_decay (float freq) (float amp) 
				 (valid-ptr freq-mod)
				 (float decay) (float maxscale)
				 vecsize (float sr))
		(new_Pluck (float freq) (float amp) (float fdbgain)
			   (valid-ptr freq-mod)
			   (float maxscale)
			   vecsize (float sr)))))))

(defmethod re-pluck ((obj pluck))
  (Pluck_RePLuck (valid-ptr obj)))

(defmethod set-parameter ((obj pluck) (str string) val)
  (Pluck_Set (valid-ptr obj)
	     str (float val)))

(defmethod set-amp ((obj pluck) amp &optional maxscale)
  (Pluck_SetAmp (valid-ptr obj) amp
		(if maxscale
		    (float maxscale)
		    (slot-value obj 'maxscale))))


(defmethod do-process ((obj pluck))
  (if (object-ptr obj)
      (Pluck_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pluck))
  (if (object-ptr obj)
      (delete_Pluck (object-pointer obj))))


;;;
;;; Tap
;;;

(defclass tap (delay-line)
  ((delaytime :initform nil :initarg :delaytime)
   (delay-line :initform nil :initarg :delay-line)
   (sig-inputs :initform '(delay-line))))

(defmethod init-pointer ((obj tap))
  (with-slots (delaytime delay-line vecsize sr) obj
    (unless (object-pointer obj)
      (let ((dly-time (get-delaytime delay-line)))
	(if (> delaytime dly-time)
	    (error "attempt to create Tap with delaytime greater than delay line")
	    (setf (object-pointer obj) 
		  (new_Tap delaytime
				 (valid-ptr delay-line)
				 vecsize (float sr))))))))

(defmethod set-delaytime ((obj tap) delaytime)
  (let ((dly-time (get-delaytime (slot-value obj 'delay-line))))
    (if (> delaytime dly-time)
	(error "attempt to change Tap delaytime to greater than delay line")
    (Tap_SetDelayTime (valid-ptr obj) (float delaytime)))))

(defmethod set-delay-tap ((obj tap) (delay delay-line))
  (Tap_SetDelayTap (valid-ptr obj) (valid-ptr delay))
  (setf (slot-value obj 'delay-line) delay))

(defmethod set-parameter ((obj tap) (str string) val)
  (Tap_Set (valid-ptr obj)
	   str (float val)))

(defmethod connect ((obj tap) (str string) ptr)
  (Tap_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj tap))
  (if (object-ptr obj)
      (Tap_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj tap))
  (if (object-ptr obj)
      (delete_Tap (object-pointer obj))))


;;;;
;;;; Tapi
;;;;

(defclass tapi (tap)
  ((delay-input :initform nil :initarg :delay-mod)
   (delay-line :initform nil :initarg :delay-line)
   (sig-inputs :initform '(delay-line delay-input))))

(defmethod init-pointer ((obj tapi))
  (with-slots (delay-input delay-line vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj)
	    (new_Tapi (valid-ptr delay-input)
			      (valid-ptr delay-line)
			      vecsize (float sr))))))

(defmethod set-delay-input ((obj tapi) (delay-input sndobj))
  (Tapi_SetDelayInput (valid-ptr obj) (valid-ptr delay-input)))

(defmethod connect ((obj tap) (str string) ptr)
  (Tap_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj tapi))
  (if (object-ptr obj)
      (Tapi_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj tapi))
  (if (object-ptr obj)
      (delete_Tapi (object-pointer obj))))



;;;
;;; VDelay
;;;

(defclass vdelay (delay-line)
  ((max-delaytime :initform nil :initarg :max-delaytime)
   (fdbgain :initform nil :initarg :fdbgain)
   (fwdgain :initform nil :initarg :fwdgain)
   (dirgain :initform nil :initarg :dirgain)
   (vdtime-mod :initform nil :initarg :vdtime-mod)
   (fdbgain-mod :initform nil :initarg :fdbgain-mod)
   (fwdgain-mod :initform nil :initarg :fwdgain-mod)
   (dirgain-mod :initform nil :initarg :dirgain-mod)
   (sig-inputs :initform '(input vdtime-mod fdbgain-mod fwdgain-mod dirgain-mod))))

(defmethod init-pointer ((obj vdelay))
  (with-slots (max-delaytime delaytime input fdbgain fwdgain dirgain vdtime-mod
			     fdbgain-mod fwdgain-mod dirgain-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if delaytime
		(new_VDelay_delaytime (float max-delaytime) (float delaytime)
				      (float fdbgain) (float fwdgain) (float dirgain)
				      (valid-ptr input)
				      (valid-ptr vdtime-mod)
				      (valid-ptr fdbgain-mod)
				      (valid-ptr fwdgain-mod)
				      (valid-ptr dirgain-mod)
				      vecsize sr)
		(new_VDelay (float max-delaytime) (float fdbgain)
			    (float fwdgain) (float dirgain)
			    (valid-ptr input)
			    (valid-ptr vdtime-mod)
			    (valid-ptr fdbgain-mod)
			    (valid-ptr fwdgain-mod)
			    (valid-ptr dirgain-mod)
			    vecsize (float sr)))))))

(defmethod set-parameter ((obj vdelay) (str string) val)
  (VDelay_Set (valid-ptr obj)
	      str (float val)))

(defmethod connect ((obj vdelay) (str string) ptr)
  (VDelay_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod set-max-delaytime ((obj vdelay) max-delaytime)
  (VDelay_SetMaxDelayTime (valid-ptr obj) (float max-delaytime)))

(defmethod set-delaytime ((obj vdelay) delaytime)
  (VDelay_SetDelayTime (valid-ptr obj) (float delaytime)))

(defmethod set-vdtime-mod ((obj vdelay) (mod sndobj))
  (VDelay_SetVdtInput (valid-ptr obj) (valid-ptr mod)))

(defmethod set-fdbgain-mod ((obj vdelay) fdbgain (mod sndobj))
  (VDelay_SetFdbgain (valid-ptr obj) (float fdbgain) (valid-ptr mod)))

(defmethod set-fwdgain-mod ((obj vdelay) fwdgain (mod sndobj))
  (VDelay_SetFwdgain (valid-ptr obj) (float fwdgain) (valid-ptr mod)))

(defmethod set-dirgain-mod ((obj vdelay) dirgain (mod sndobj))
  (VDelay_SetDirgain (valid-ptr obj) (float dirgain) (valid-ptr mod)))

(defmethod do-process ((obj vdelay))
  (if (object-ptr obj)
      (VDelay_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj vdelay))
  (if (object-ptr obj)
      (delete_VDelay (object-pointer obj))))


;;
;;FastOsc
;;

(defclass fast-osc (sndobj)
  ((table :initform nil :initarg :table)
   (freq :initarg :freq :initform 440.0)
   (amp :initarg :amp :initform 1.0)))


(defmethod init-pointer ((obj fast-osc))
  (with-slots (table freq amp vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_FastOsc (valid-ptr table)
			 (float freq) (float amp) vecsize (float sr))))))
  
(defmethod set-freq ((obj fast-osc) freq &optional ignore)
  ignore
  (FastOsc_SetFreq (valid-ptr obj) (float freq)))

(defmethod set-amp ((obj fast-osc) amp &optional ignore)
  ignore
  (FastOsc_SetAmp (valid-ptr obj) (float amp)))

(defmethod set-phase ((obj fast-osc) phase)
  (FastOsc_SetPhase (valid-ptr obj) (float phase)))

(defmethod set-table ((obj fast-osc) (tab table))
  (FastOsc_SetTable (valid-ptr obj)
		    (valid-ptr tab)))

(defmethod connect ((obj fast-osc) (str string) ptr)
  (FastOsc_Connect (valid-ptr obj)
		   str (valid-ptr ptr)))

(defmethod set-parameter ((obj fast-osc) (str string) val)
  (FastOsc_Set (valid-ptr obj)
	       str (float val)))

(defmethod set-sr ((obj fast-osc) sr)
  (FastOsc_SetSr (valid-ptr obj) sr))

(defmethod do-process ((obj fast-osc))
  (if (object-ptr obj)
      (FastOsc_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fast-osc))
  (if (object-ptr obj)
      (delete_FastOsc (object-pointer obj))))


;;;
;;; Osc
;;;

(defclass osc (fast-osc)
  ((freq-mod :initarg :freq-mod :initform nil)
   (amp-mod :initarg :amp-mod :initform nil)
   (sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj osc))
  (with-slots (table freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Osc (valid-ptr table) (float freq) (float amp)
			     (valid-ptr freq-mod) (valid-ptr amp-mod)
			     vecsize (float sr))))))

(defmethod set-freq ((obj osc) freq &optional ignore)
  ignore
  (Osc_SetFreq (valid-ptr obj) (float freq)))

(defmethod set-amp ((obj osc) amp &optional ignore)
  ignore
  (Osc_SetAmp (valid-ptr obj) (float amp)))

(defmethod connect ((obj osc) (str string) ptr)
  (Osc_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod do-process ((obj osc))
  (if (object-ptr obj)
      (Osc_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj osc))
  (if (object-ptr obj)
      (delete_Osc (object-pointer obj))))


;;;;
;;;; Osci
;;;;

(defclass osci (osc)
  ((sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj osci))
  (with-slots (table freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Osci (valid-ptr table)
		      (float freq) (float amp)
		      (valid-ptr freq-mod)
		      (valid-ptr amp-mod)
		      vecsize (float sr))))))

(defmethod set-table ((obj osci) (tab table))
  (Osci_SetTable (valid-ptr obj)
		 (valid-ptr tab)))

(defmethod connect ((obj osci) (str string) ptr)
  (Osci_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod do-process ((obj osci))
  (if (object-ptr obj)
      (Osci_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj osci))
  (if (object-ptr obj)
      (delete_Osci (object-pointer obj))))


;;
;; FFT
;;

(defclass fft (sndobj)
  ((window :initform nil :initarg :window)
   (input :initform nil :initarg :input)
   (scale :initform 1.0 :initarg :scale)
   (fftsize :initform *fft-size* :initarg :fftsize)
   (vecsize :initarg :hopsize)
   (sig-inputs :initform '(input))))


(defmethod init-pointer ((obj fft))  
  (with-slots (window input scale fftsize vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_FFT (valid-ptr window)
			     (valid-ptr input)
			     (float scale) fftsize vecsize (float sr))))))

(defmethod get-fft-size ((obj fft))
  (FFT_GetFFTSize (valid-ptr obj)))

(defmethod get-hop-size ((obj fft))
  (FFT_GetHopSize (valid-ptr obj)))

(defmethod set-window ((obj fft) (window table))
  (FFT_SetWindow (valid-ptr obj) (valid-ptr window)))

(defmethod set-parameter ((obj fft) (str string) val)
  (FFT_Set (valid-ptr obj)
	   str (float val)))

(defmethod connect ((obj fft) (str string) ptr)
  (FFT_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod set-scale ((obj fft) scale)
  (FFT_SetScale (valid-ptr obj) (float scale)))

(defmethod set-fft-size ((obj fft) size)
  (FFT_SetFFTSize (valid-ptr obj) (float size)))

(defmethod set-hop-size ((obj fft) size)
  (FFT_SetHopSize (valid-ptr obj) (float size)))

(defmethod do-process ((obj fft))
  (if (object-ptr obj)
      (FFT_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fft))
  (if (object-ptr obj)
      (delete_FFT (object-pointer obj))))


;;;
;;; PVA
;;;

(defclass pva (fft)
  ((sig-inputs :initform '(input))
   (hopsize :initform *vector-size*)
   (fftsize :initform *fft-size*)))

(defmethod init-pointer ((obj pva))
  (with-slots (window input scale fftsize hopsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVA (valid-ptr window) (valid-ptr input)
		     (float scale) fftsize hopsize (float sr))))))

(defmethod get-phases ((obj pva) pos)
  (PVA_Outphases (valid-ptr obj) pos))

(defmethod set-parameter ((obj pva) (str string) val)
  (PVA_Set (valid-ptr obj)
	     str (float val)))

(defmethod set-fft-size ((obj pva) size)
  (PVA_SetFFTSize (valid-ptr obj) size))

(defmethod set-hop-size ((obj pva) size)
  (PVA_SetHopSize (valid-ptr obj) size))

(defmethod do-process ((obj pva))
  (if (object-ptr obj)
      (PVA_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pva))
  (if (object-ptr obj)
      (delete_PVA (object-pointer obj))))


;;
;; IFGram
;;

(defclass if-gram (pva)
  ((sig-inputs :initform '(input))))

(defmethod init-pointer ((obj if-gram))
  (with-slots (window input scale fftsize vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_IFGram (valid-ptr window)
				(valid-ptr input)
				(float scale) fftsize vecsize (float sr))))))

(defmethod set-parameter ((obj if-gram) (str string) val)
  (IFGram_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-fft-size ((obj if-gram) size)
  (IFGram_SetFFTSize (valid-ptr obj) size))

(defmethod connect ((obj if-gram) (str string) ptr)
  (IFGram_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj if-gram))
  (if (object-ptr obj)
      (IFGram_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj if-gram))
  (if (object-ptr obj)
      (delete_IFGram (object-pointer obj))))


;;
;; BP-filter
;;

(defclass bp-filter (sndobj)
  ((freq :initform nil :initarg :freq)
   (bw :initform nil :initarg :bw)
   (input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj bp-filter))
  (with-slots (freq bw input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Filter (float freq) (float bw)
			(valid-ptr input)
			vecsize (float sr))))))

(defmethod set-freq ((obj bp-filter) freq &optional ignore)
  ignore
  (Filter_SetFreq (valid-ptr obj) (float freq)))

(defmethod set-bw ((obj bp-filter) freq &optional ignore)
  ignore
  (Filter_SetFreq (valid-ptr obj) (float freq)))
   
(defmethod set-sr ((obj bp-filter) sr)
  (Filter_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj bp-filter) (str string) val)
  (Filter_Set (valid-ptr obj)
	      str (float val)))

(defmethod do-process ((obj bp-filter))
  (if (object-ptr obj)
      (Filter_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj bp-filter))
  (if (object-ptr obj)
      (delete_Filter (object-pointer obj))))


;;;
;;; HiPass
;;;

(defclass hipass (bp-filter)
  ((sig-inputs :initform '(input))))

(defmethod init-pointer ((obj hipass))
  (with-slots (freq input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Hipass (float freq)
			(valid-ptr input)
			vecsize (float sr))))))

(defmethod set-freq ((obj hipass) freq &optional ignore)
  ignore
  (HiPass_SetFreq (valid-ptr obj) (float freq)))

(defmethod set-sr ((obj hipass) sr)
  (HiPass_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj hipass) (str string) val)
  (HiPass_Set (valid-ptr obj)
	      str (float val)))

(defmethod do-process ((obj hipass))
  (if (object-ptr obj)
      (HiPass_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj hipass))
  (if (object-ptr obj)
      (delete_HiPass (object-pointer obj))))



;;;
;;; LoPass
;;;

(defclass lopass (bp-filter)
  ((sig-inputs :initform '(input))))

(defmethod init-pointer ((obj lopass))
  (with-slots (freq input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_LoPass (float freq)
			(valid-ptr input)
			vecsize (float sr))))))

(defmethod set-freq ((obj lopass) freq &optional ignore)
  ignore
  (LoPass_SetFreq (valid-ptr obj) (float freq)))

(defmethod set-sr ((obj lopass) sr)
  (LoPass_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj lopass) (str string) val)
  (LoPass_Set (valid-ptr obj)
	      str (float val)))

(defmethod do-process ((obj lopass))
  (if (object-ptr obj)
      (LoPass_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj lopass))
  (if (object-ptr obj)
      (delete_LoPass (object-pointer obj))))

;;;
;;; Lp-reson
;;;

(defclass lp-reson (bp-filter)
  ((freq-mod :initform nil :initarg :freq-mod)
   (bw-mod :initform nil :initarg :bw-mod)
   (sig-inputs :initform '(input freq-mod bw-mod))))

(defmethod init-pointer ((obj lp-reson))
  (with-slots (freq bw input freq-mod bw-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj)
	    (new_Lp (float freq) (float bw)
		    (valid-ptr input)
		    (valid-ptr freq-mod)
		    (valid-ptr bw-mod)
		    vecsize (float sr))))))

  
(defmethod set-sr ((obj lp-reson) sr)
  (Lp_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj lp-reson) (str string) val)
  (Lp_Set (valid-ptr obj)
	  str (float val)))

(defmethod do-process ((obj lp-reson))
  (if (object-ptr obj)
      (Lp_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj lp-reson))
  (if (object-ptr obj)
      (delete_Lp (object-pointer obj))))


;;;
;;; Bp-reson
;;;

(defclass bp-reson (bp-filter)
  ((freq-mod :initform nil :initarg :freq-mod)
   (bw-mod :initform nil :initarg :bw-mod)
   (sig-inputs :initform '(input freq-mod bw-mod))))

(defmethod init-pointer ((obj bp-reson))
  (with-slots (freq bw input freq-mod bw-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Reson (float freq) (float bw)
		       (valid-ptr input)
		       (valid-ptr freq-mod)
		       (valid-ptr bw-mod)
		       vecsize (float sr))))))

(defmethod set-freq ((obj bp-reson) freq &optional (freq-mod nil))
  (Reson_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-bw ((obj bp-reson) bw &optional (bw-mod nil))
  (Reson_SetBW (valid-ptr obj) (float bw) (valid-ptr bw-mod)))

(defmethod connect ((obj bp-reson) (str string) ptr)
  (Reson_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj bp-reson))
  (if (object-ptr obj)
      (Reson_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj bp-reson))
  (if (object-ptr obj)
      (delete_Reson (object-pointer obj))))


;;
;; TpTz
;;

(defclass tptz (sndobj)
  ((a :initform nil :initarg :a)
   (a1 :initform nil :initarg :a1)
   (a2 :initform nil :initarg :a2)
   (b1 :initform nil :initarg :b1)
   (b2 :initform nil :initarg :b2)
   (input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))


(defmethod init-pointer ((obj tptz))
  (with-slots (a a1 a2 b1 b2 input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_TpTz (coerce a 'double-float)
		      (coerce a1 'double-float)
		      (coerce a2 'double-float)
		      (coerce b1 'double-float)
		      (coerce b2 'double-float)
		      (valid-ptr input)
		      vecsize (float sr))))))

(defmethod set-coeffs ((obj tptz) a a1 a2 b1 b2)
  (TpTz_SetParam (valid-ptr obj) 
		 (coerce a 'double-float)
		 (coerce a1 'double-float)
		 (coerce a2 'double-float)
		 (coerce b1 'double-float)
		 (coerce b2 'double-float)))

(defmethod set-parameter ((obj tptz) (str string) val)
  (TpTz_Set (valid-ptr obj)
	      str (float val)))

(defmethod do-process ((obj tptz))
  (if (object-ptr obj)
      (TpTz_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj tptz))
  (if (object-ptr obj)
      (delete_TpTz (object-pointer obj))))


;;;
;;; Ap
;;;

(defclass ap (tptz)
  ((freq :initform nil :initarg :freq)
   (r :initform nil :initarg :r)
   (input :initform nil :initarg :input)
   (freq-mod :initform nil :initarg :freq-mod)
   (r-mod :initform nil :initarg :r-mod)
   (sig-inputs :initform '(input freq-mod r-mod))))

(defmethod init-pointer ((obj ap))
  (with-slots (freq r input freq-mod r-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Ap (float freq) (float r)
		    (valid-ptr input)
		    (valid-ptr freq-mod)
		    (valid-ptr r-mod)
		    vecsize (float sr))))))

(defmethod set-freq ((obj ap) freq &optional (freq-mod nil))
  (Ap_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-r ((obj ap) r &optional (r-mod nil))
  (Ap_SetR (valid-ptr obj) (float r) (valid-ptr r-mod)))

(defmethod set-sr ((obj ap) sr)
  (Ap_SetSr (valid-ptr obj) (float sr)))

(defmethod set-parameter ((obj ap) (str string) val)
  (Ap_Set (valid-ptr obj)
	  str (float val)))

(defmethod connect ((obj ap) (str string) ptr)
  (Ap_Connect (valid-ptr obj)
	      str (valid-ptr ptr)))

(defmethod do-process ((obj ap))
  (if (object-ptr obj)
      (Ap_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj ap))
  (if (object-ptr obj)
      (delete_Ap (object-pointer obj))))


;;
;; ButtBP
;;

(defclass butt-bp (tptz)
  ((freq-mod :initform nil :initarg :freq-mod)
   (bw-mod :initform nil :initarg :bw-mod)
   (freq :initform nil :initarg :freq)
   (bw :initform nil :initarg :bw)
   (sig-inputs :initform '(input freq-mod bw-mod))))

(defmethod init-pointer ((obj butt-bp))
  (with-slots (freq bw input freq-mod bw-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ButtBP (float freq) (float bw)
			(valid-ptr input)
			(valid-ptr freq-mod)
			(valid-ptr bw-mod)
			vecsize (float sr))))))

(defmethod set-freq ((obj butt-bp) freq &optional (freq-mod nil))
  (ButtBP_SetFreq_mod (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-bw ((obj butt-bp) bw &optional (bw-mod nil))
  (ButtBP_SetBW_mod (valid-ptr obj) (float bw) (valid-ptr bw-mod)))

(defmethod set-sr ((obj butt-bp) sr)
  (ButtBP_SetSr (valid-ptr obj) (float sr)))

(defmethod connect ((obj butt-bp) (str string) ptr)
  (ButtBP_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod do-process ((obj butt-bp))
  (if (object-ptr obj)
      (ButtBP_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj butt-bp))
  (if (object-ptr obj)
      (delete_ButtBP (object-pointer obj))))

;;;
;;; ButtBR
;;;

(defclass butt-br (butt-bp)
  ((sig-inputs :initform '(input freq-mod bw-mod))))

(defmethod init-pointer ((obj butt-br))
  (with-slots (freq bw input freq-mod bw-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ButtBR (float freq) (float bw)
			(valid-ptr input)
			(valid-ptr freq-mod)
			(valid-ptr bw-mod)
			vecsize (float sr))))))

(defmethod do-process ((obj butt-br))
  (if (object-ptr obj)
      (ButtBR_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj butt-br))
  (if (object-ptr obj)
      (delete_ButtBR (object-pointer obj))))

;;;
;;; ButtHP
;;;

(defclass butt-hp (butt-bp)
  ((sig-inputs :initform '(input freq-mod))))

(defmethod init-pointer ((obj butt-hp))
  (with-slots (freq input freq-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ButtHP (float freq)
			(valid-ptr input)
			(valid-ptr freq-mod)
			vecsize (float sr))))))

(defmethod do-process ((obj butt-hp))
  (if (object-ptr obj)
      (ButtHP_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj butt-hp))
  (if (object-ptr obj)
      (delete_ButtHP (object-pointer obj))))

;;;
;;; ButtLP
;;;

(defclass butt-lp (butt-bp)
  ((sig-inputs :initform '(input freq-mod))))

(defmethod init-pointer ((obj butt-lp))
  (with-slots (freq input freq-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ButtLP (float freq)
			(valid-ptr input)
			(valid-ptr freq-mod)
			vecsize (float sr))))))

(defmethod do-process ((obj butt-lp))
  (if (object-ptr obj)
      (ButtLP_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj butt-lp))
  (if (object-ptr obj)
      (delete_ButtLP (object-pointer obj))))


;;
;; Gain
;;

(defclass gain (sndobj)
  ((gain :initform nil :initarg :gain)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj gain))
  (with-slots (gain input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Gain (float gain)
		      (valid-ptr input)
		      vecsize (float sr))))))


(defmethod set-gain ((obj gain) gaindb)
  (Gain_SetGain (valid-ptr obj) (float gaindb)))

(defmethod set-gain-multiplier ((obj gain) gainm)
  (Gain_SetGainM (valid-ptr obj) gainm))

(defmethod do-process ((obj gain))
  (if (object-ptr obj)
      (Gain_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj gain))
  (if (object-ptr obj)
      (delete_Gain (object-pointer obj))))


;;
;; Hilb
;;

(defclass hilb (sndobj)
  ((input :initform nil :initarg :input)
   (sig-inputs :initform '(input))
   (real :initform nil)
   (imag :initform nil)))

(defmethod init-pointer ((obj hilb))
  (with-slots (freq input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Hilb (valid-ptr input)
		      vecsize (float sr))))))

(defmethod get-real ((obj hilb))
  (if (slot-value obj 'real)
      (slot-value obj 'real)
      (let ((ptr (Hilb_real_get (valid-ptr obj)))
	    (res))
	(if (not (cffi:null-pointer-p ptr))
	    (progn
	      (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				       :sr (get-sr ptr)))
	      (setf (slot-value res 'object-pointer) ptr)
	      (setf (slot-value obj 'real) res))
	    (progn
	      (setf res (make-instance 'sndobj))
	      (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
	res)))

(defmethod get-imaginary ((obj hilb))
  (if (slot-value obj 'imag)
      (slot-value obj 'imag)
      (let ((ptr (Hilb_imag_get (valid-ptr obj)))
	    (res))
	(if (not (cffi:null-pointer-p ptr))
	    (progn
	      (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				       :sr (get-sr ptr)))
	      (setf (slot-value res 'object-pointer) ptr)
	      (setf (slot-value obj 'imag) res))
	    (progn
	      (setf res (make-instance 'sndobj))
	      (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
	res)))

(defmethod do-process ((obj hilb))
  (if (object-ptr obj)
      (Hilb_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj hilb))
  (if (object-ptr obj)
      (progn
	(delete_Hilb (object-pointer obj))
	(if (slot-value obj 'real)
	    (destroy (slot-value obj 'real)))
	(if (slot-value obj 'imag)
	    (destroy (slot-value obj 'imag))))))



;;
;; IFFT
;;

(defclass ifft (sndobj)
  ((window :initform nil :initarg :window)
   (input :initform nil :initarg :input)
   (fftsize :initform *fft-size* :initarg :fftsize)
   (vecsize :initarg :hopsize)
   (sig-inputs :initform '(input))))


(defmethod init-pointer ((obj ifft))
  (with-slots (window input fftsize vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_IFFT (valid-ptr window)
		      (valid-ptr input)
		      fftsize
		      vecsize (float sr))))))

(defmethod get-fft-size ((obj ifft))
  (IFFT_GetFFTSize (valid-ptr obj)))

(defmethod get-hop-size ((obj ifft))
  (IFFT_GetHopSize (valid-ptr obj)))

(defmethod set-window ((obj ifft) (window table))
  (IFFT_SetWindow (valid-ptr obj) (valid-ptr window)))

(defmethod set-parameter ((obj ifft) (str string) val)
  (IFFT_Set (valid-ptr obj)
	    str (float val)))

(defmethod connect ((obj ifft) (str string) ptr)
  (IFFT_Connect (valid-ptr obj)
		str (valid-ptr ptr)))

(defmethod set-fft-size ((obj ifft) size)
  (IFFT_SetFFTSize (valid-ptr obj) size))

(defmethod set-hop-size ((obj ifft) size)
  (IFFT_SetHopSize (valid-ptr obj) size))

(defmethod do-process ((obj ifft))
  (if (object-ptr obj)
      (IFFT_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj ifft))
  (if (object-ptr obj)
      (delete_IFFT (object-pointer obj))))


;;;
;;; PVS
;;;

(defclass pvs (ifft)
  ((sig-inputs :initform '(input))
   (hopsize :initform *vector-size* :initarg :hopsize)))

(defmethod init-pointer ((obj pvs))
  (with-slots (window input scale hopsize fftsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj)
	    (new_PVS (valid-ptr window)
		     (valid-ptr input)
		     fftsize
		     hopsize (float sr))))))

(defmethod set-parameter ((obj pvs) (str string) val)
  (PVS_Set (valid-ptr obj)
	   str (float val)))

(defmethod set-fft-size ((obj pvs) size)
  (PVS_SetFFTSize (valid-ptr obj) size))

(defmethod set-hop-size ((obj pvs) size)
  (PVS_SetHopSize (valid-ptr obj) size))

(defmethod do-process ((obj pvs))
  (if (object-ptr obj)
      (PVS_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pvs))
  (if (object-ptr obj)
      (delete_PVS (object-pointer obj))))


;;;;
;;;; PVRead
;;;;

(defclass pvread (pvs)
  ((file :initform nil :initarg :file)
   (timescale :initform 1.0 :initarg :timescale)))

(defmethod init-pointer ((obj pvread))
  (with-slots (file timescale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVRead file
			(float timescale) vecsize (float sr))))))

(defmethod get-outchannel ((obj pvread) chan)
  (let ((ptr (PVRead_Outchannel (valid-ptr obj) chan))
	(res))
    (if (not (cffi:null-pointer-p ptr))
	(progn
	  (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				   :sr (get-sr ptr)))
	  (setf (slot-value res 'object-pointer) ptr))
	(progn
	  (setf res (make-instance 'sndobj))
	  (setf (slot-value res 'object-pointer) (cffi:null-pointer))))))

(defmethod set-parameter ((obj pvs) (str string) val)
  (PVRead_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-input ((obj pvread) file &optional ignore)
  ignore
  (PVRead_SetInput (valid-ptr obj) file))

(defmethod set-timescale ((obj pvread) timescale)
  (PVRead_SetTimescale (valid-ptr obj) (float timescale)))

(defmethod do-process ((obj pvread))
  (if (object-ptr obj)
      (PVRead_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pvread))
  (if (object-ptr obj)
      (delete_PVRead (object-pointer obj))))



;;
;; Interp-line
;;

(defclass interp-line (sndobj)
  ((init :initform nil :initarg :init)
   (final :initform nil :initarg :final)
   (dur :initform nil :initarg :dur)
   (type :initform 0.0 :initarg :type)))

(defmethod init-pointer ((obj interp-line))
  (with-slots (init final dur type vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Interp (float init) (float final) (float dur)
				(float type) vecsize (float sr))))))

(defmethod set-sr ((obj interp-line) sr)
  (Interp_SetSr (valid-ptr obj) (float sr)))

(defmethod restart  ((obj interp-line))
  (Interp_Restart (valid-ptr obj)))

(defmethod set-interp-curve ((obj interp-line) init final type)
  (Interp_SetCurve (valid-ptr obj) (float init) (float final) (float type)))

(defmethod set-parameter ((obj interp-line) (str string) val)
  (Interp_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-dur ((obj interp-line) dur)
  (Interp_SetDur (valid-ptr obj) (float dur)))

(defmethod do-process ((obj interp-line))
  (if (object-ptr obj)
      (Interp_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj interp-line))
  (if (object-ptr obj)
      (delete_Interp (object-pointer obj))))


;;
;; Table-lookup
;;

(defparameter *lookup-modes* (list :limit 0 :wrap 1))
(defparameter *lookup-norm* (list :raw 0 :normalised 1 :normalized 1))

(defclass table-lookup (sndobj)
  ((table :initform nil :initarg :table)
   (offset :initform nil :initarg :offset)
   (input :initform nil :initarg :input)
   (mode :initform ':wrap :initarg :mode)
   (normal :initform ':raw :initarg :normal)
   (sig-inputs :initform '(input))))
   
(defmethod init-pointer ((obj table-lookup))
  (with-slots (table offset input mode normal vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Lookup (valid-ptr table)
			offset
			(valid-ptr input)
			(get-option mode *lookup-modes*)
			(get-option normal *lookup-norm*)
			vecsize (float sr))))))

(defmethod set-mode ((obj table-lookup) mode &optional (normal :normalised))
  (Lookup_SetMode (valid-ptr obj) (get-option mode *lookup-modes*) (get-option normal *lookup-norm*)))

(defmethod set-offset ((obj table-lookup) offset)
  (Lookup_Offset (valid-ptr obj) offset))

(defmethod set-table ((obj table-lookup) (tab table))
  (Lookup_SetTable (valid-ptr obj) (valid-ptr tab)))

(defmethod set-parameter ((obj table-lookup) (str string) val)
  (Lookup_Set (valid-ptr obj)
	     str (float val)))

(defmethod connect ((obj table-lookup) (str string) ptr)
  (Lookup_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod do-process ((obj table-lookup))
  (if (object-ptr obj)
      (Lookup_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj table-lookup))
  (if (object-ptr obj)
      (delete_Lookup (object-pointer obj))))


;;;
;;; Lookupi
;;;


(defparameter *lookup-modes* (list :limit 0 :wrap 1))
(defparameter *lookup-norm* (list :raw 0 :normalised 1 :normalized 1))

(defclass table-lookupi (lookup)
  ((sig-inputs :initform '(input))))

(defmethod init-pointer ((obj table-lookupi))
  (with-slots (table offset input mode normal vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Lookupi (valid-ptr table)
			 offset
			 (valid-ptr input)
			 (get-option mode *lookup-modes*)
			 (get-option normal *lookup-norm*)
			 vecsize (float sr))))))

(defmethod do-process ((obj table-lookupi))
  (if (object-ptr obj)
      (Lookupi_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj table-lookupi))
  (if (object-ptr obj)
      (delete_Lookupi (object-pointer obj))))



;;;;;;;Mixer


;;
;; Oscil
;;

(defclass oscil (sndobj)
  ((table :initform nil :initarg :table)
   (freq :initarg :freq :initform 440.0)
   (amp :initarg :amp :initform 1.0)
   (freq-mod :initarg :freq-mod :initform nil)
   (amp-mod :initarg :amp-mod :initform nil)
   (sig-inputs :initform '(freq-mod amp-mode))))
   

(defmethod init-pointer ((obj oscil))
  (with-slots (table freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Oscil (valid-ptr table)
			       (float freq) (float amp)
			       (valid-ptr freq-mod)
			       (valid-ptr amp-mod)
			       vecsize (float sr))))))

(defmethod set-freq ((obj oscil) freq &optional (freq-mod nil))
  (Oscil_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-amp ((obj oscil) amp &optional (amp-mod nil))
  (Oscil_SetAmp (valid-ptr obj) (float amp) (valid-ptr amp-mod)))

(defmethod set-phase ((obj oscil) phase)
  (Oscil_SetPhase (valid-ptr obj) (float phase)))

(defmethod set-table ((obj oscil) (tab table))
  (Oscil_SetTable (valid-ptr obj)
		  (valid-ptr tab)))

(defmethod connect ((obj oscil) (str string) ptr)
  (Oscil_Connect (valid-ptr obj)
		 str (valid-ptr ptr)))

(defmethod set-parameter ((obj sndobj) (str string) val)
  (Oscil_Set (valid-ptr obj)
	     str (float val)))

(defmethod do-process ((obj oscil))
  (if (object-ptr obj)
      (Oscil_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj oscil))
  (if (object-ptr obj)
      (delete_Oscil (object-pointer obj))))


;;;
;;; Oscili
;;;


(defclass oscili (oscil)
  ((sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj oscili))
  (with-slots (table freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Oscili (valid-ptr table)
			(float freq) (float amp)
			(valid-ptr freq-mod)
			(valid-ptr amp-mod)
			vecsize (float sr))))))

(defmethod do-process ((obj oscili))
  (if (object-ptr obj)
      (Oscili_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj oscili))
  (if (object-ptr obj)
      (delete_Oscili (object-pointer obj))))



;;;;
;;;; PhOscili
;;;;

(defclass phoscili (oscili)
  ((phase-mod :initform nil :initarg :phase-mod)
   (sig-inputs :initform '(freq-mod amp-mod phase-mod))))

(defmethod init-pointer ((obj phoscili))
  (with-slots (table freq amp freq-mod amp-mod phase-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) (new_PhOscili (valid-ptr table)
					       (float freq) (float amp)
					       (valid-ptr freq-mod)
					       (valid-ptr amp-mod)
					       (valid-ptr phase-mod)
					       vecsize (float sr))))))

(defmethod do-process ((obj phoscili))
  (if (object-ptr obj)
      (PhOscili_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj phoscili))
  (if (object-ptr obj)
      (delete_PhOscili (object-pointer obj))))


;;;
;;; Oscilt
;;;

(defclass oscilt (oscil)
  ((sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj oscilt))
  (with-slots (table freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Oscilt (valid-ptr table)
			(float freq) (float amp)
			(valid-ptr freq-mod)
			(valid-ptr amp-mod)
			vecsize (float sr))))))

(defmethod do-process ((obj oscilt))
  (if (object-ptr obj)
      (Oscilt_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj oscilt))
  (if (object-ptr obj)
      (delete_Oscilt (object-pointer obj))))


;;
;; Pan 
;;

(defclass pan (sndobj)
  ((pan :initform nil :initarg :pan)
   (input :initform nil :initarg :input)
   (pan-mod :initform nil :initarg :pan-mod)
   (resolution :initform 1024 :initarg :resolution)
   (left :initform nil)
   (right :initform nil)
   (sig-inputs :initform '(input pan-mod))))

(defmethod init-pointer ((obj pan))
  (with-slots (pan input pan-mod resolution vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Pan (float pan)
		     (valid-ptr input)
		     (valid-ptr pan-mod)
		     resolution vecsize (float sr))))))

(defmethod set-pan ((obj pan) pan &optional (pan-mod nil))
  (Pan_SetPan (valid-ptr obj) (float pan) (valid-ptr pan-mod)))

(defmethod set-parameter ((obj pan) (str string) val)
  (Pan_Set (valid-ptr obj)
	   str (float val)))

(defmethod connect ((obj pan) (str string) ptr)
  (Pan_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod get-left ((obj pan))
  (if (slot-value obj 'left)
      (slot-value obj 'left)
      (let ((ptr (Pan_left_get (valid-ptr obj)))
	    (res))
	(if (not (cffi:null-pointer-p ptr))
	    (progn
	      (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				       :sr (get-sr ptr)))
	      (setf (slot-value res 'object-pointer) ptr)
	      (setf (slot-value obj 'left) res))
	    (progn
	      (setf res (make-instance 'sndobj))
	      (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
	res)))

(defmethod get-right ((obj pan))
  (if (slot-value obj 'right)
      (slot-value obj 'right)
      (let ((ptr (Pan_right_get (valid-ptr obj)))
	    (res))
	(if (not (cffi:null-pointer-p ptr))
	    (progn
	      (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				       :sr (get-sr ptr)))
	      (setf (slot-value res 'object-pointer) ptr)
	      (setf (slot-value obj 'right) res))
	    (progn
	      (setf res (make-instance 'sndobj))
	      (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
	res)))

(defmethod do-process ((obj Pan))
  (if (object-ptr obj)
      (Pan_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pan))
  (if (object-ptr obj)
      (progn
	(delete_Pan (object-pointer obj))
	(if (destroy (slot-value obj 'left))
	    (destroy (slot-value obj 'left)))
	(if (destroy (slot-value obj 'right))
	    (destroy (slot-value obj 'right))))))
		 
  
	    

;;
;; Phase
;;

(defclass phase (sndobj)
  ((freq :initform nil :initarg :freq)
   (freq-mod :initform nil :initarg :freq-mod)
   (offset :initform 0.0 :initarg :offset)
   (sig-inputs :initform '(freq-mod))))

(defmethod init-pointer ((obj phase))
  (with-slots (freq freq-mod offset vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Phase (float freq)
		       (valid-ptr freq-mod)
		       (float offset) vecsize (float sr))))))

(defmethod set-freq ((obj phase) freq &optional (freq-mod nil))
  (Phase_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-phase ((obj phase) offset)
  (Phase_SetPhase (valid-ptr obj) (float offset)))

(defmethod set-parameter ((obj phase) (str string) val)
  (Phase_Set (valid-ptr obj)
	     str (float val)))

(defmethod connect ((obj phase) (str string) ptr)
  (Phase_Connect (valid-ptr obj)
		 str (valid-ptr ptr)))

(defmethod do-process ((obj phase))
  (if (object-ptr obj)
      (Phase_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj phase))
  (if (object-ptr obj)
      (delete_Phase (object-pointer obj))))


;;
;; Rand
;;

(defclass rand (sndobj)
  ((amp :initform nil :initarg :amp)
   (amp-mod :initform nil :initarg :amp-mode)
   (sig-inputs :initform '(amp-mod))))

(defmethod init-pointer ((obj rand))
  (with-slots (amp amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Rand (float amp)
		      (valid-ptr amp-mod)
		      vecsize (float sr))))))

(defmethod set-amp ((obj rand) amp &optional (amp-mod nil))
  (Rand_SetAmp (valid-ptr obj) (float amp) (valid-ptr amp-mod)))

(defmethod set-parameter ((obj rand) (str string) val)
  (Rand_Set (valid-ptr obj)
	    str (float val)))

(defmethod connect ((obj rand) (str string) ptr)
  (Rand_Connect (valid-ptr obj)
		str (valid-ptr ptr)))

(defmethod do-process ((obj rand))
  (if (object-ptr obj)
      (Rand_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj rand))
  (if (object-ptr obj)
      (delete_Rand (object-pointer obj))))



;;;
;;; Randh
;;;

(defclass randh (rand)
  ((freq :initform nil :initarg :freq)
   (freq-mod :initform nil :initarg :freq-mod)
   (sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj randh))
  (with-slots (freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Randh (float freq) (float amp)
		       (valid-ptr freq-mod)
		       (valid-ptr amp-mod)
		       vecsize (float sr))))))

(defmethod set-amp ((obj randh) amp &optional (amp-mod nil))
  (Rand_SetAmp (valid-ptr obj) (float amp) (valid-ptr amp-mod)))

(defmethod set-sr ((obj randh) sr)
  (Randh_SetSr (valid-ptr obj) (float sr)))

(defmethod set-freq ((obj randh) freq &optional (freq-mod nil))
  (Randh_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-parameter ((obj randh) (str string) val)
  (Randh_Set (valid-ptr obj)
	     str (float val)))

(defmethod connect ((obj randh) (str string) ptr)
  (Randh_Connect (valid-ptr obj)
		 str (valid-ptr ptr)))

(defmethod do-process ((obj randh))
  (if (object-ptr obj)
      (Randh_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj randh))
  (if (object-ptr obj)
      (delete_Randh (object-pointer obj))))





;;;;
;;;; Randi
;;;;

(defclass randi (randh)
  ((freq :initform nil :initarg :freq)
   (sig-inputs :initform '(freq-mod amp-mod))))

(defmethod init-pointer ((obj randi))
  (with-slots (freq amp freq-mod amp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Randi (float freq) (float amp)
		       (valid-ptr freq-mod)
		       (valid-ptr amp-mod)
		       vecsize (float sr))))))

(defmethod do-process ((obj randi))
  (if (object-ptr obj)
      (Randi_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj randi))
  (if (object-ptr obj)
      (delete_Randi (object-pointer obj))))

;;
;; Ring
;;

(defclass ring-mod (sndobj)
  ((input1 :initform nil :initarg :input1)
   (input2 :initform nil :initarg :input2)
   (sig-inputs :initform '(input1 input2))))

(defmethod init-pointer ((obj ring-mod))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Ring (valid-ptr input1)
		      (valid-ptr input2)
		      vecsize (float sr))))))

(defmethod set-input1 ((obj ring-mod) (input1 sndobj))
  (Ring_SetInput1 (valid-ptr obj) (valid-ptr input1)))

(defmethod set-input2 ((obj ring-mod) (input2 sndobj))
  (Ring_SetInput2 (valid-ptr obj) (valid-ptr input2)))

(defmethod connect ((obj ring-mod) (str string) ptr)
  (Ring_Connect (valid-ptr obj)
		str (valid-ptr ptr)))

(defmethod do-process ((obj ring-mod))
  (if (object-ptr obj)
      (Ring_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj ring-mod))
  (if (object-ptr obj)
      (delete_Ring (object-pointer obj))))


;;
;; SinAnal
;;

(defclass sin-anal (sndobj)
  ((input :initform nil :initarg :input)
   (threshold :initform nil :initarg :threshold)
   (maxtracks :initform nil :initarg :maxtracks)
   (minpoints :initform 1 :initarg :minpoints)
   (maxgap :initform 3 :initarg :maxgap)
   (sig-inputs :initform '(input))))


(defmethod init-pointer ((obj sin-anal))
  (with-slots (input threshold maxtracks minpoints maxgap vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SinAnal (valid-ptr input)
			 (float threshold) maxtracks minpoints
			 maxgap (float sr))))))

(defmethod get-track-id ((obj sin-anal) track &optional ignore)
  ignore
  (SinAnal_GetTrackID (valid-ptr obj) track))

(defmethod get-tracks ((obj sin-anal) &optional ignore)
  ignore
  (SinAnal_GetTracks (valid-ptr obj)))

(defmethod set-parameter ((obj sin-anal) (str string) val)
  (SinAnal_Set (valid-ptr obj)
	       str (float val)))

(defmethod connect ((obj sin-anal) (str string) ptr)
  (SinAnal_Connect (valid-ptr obj)
		   str (valid-ptr ptr)))

(defmethod set-threshold ((obj sin-anal) thresh)
  (SinAnal_SetThreshold (valid-ptr obj) (float thresh)))

(defmethod set-if-gram ((obj sin-anal) (in sndobj))
  (SinAnal_SetIFGram (valid-ptr obj) (valid-ptr in)))

(defmethod set-max-tracks ((obj sin-anal) maxtracks)
  (SinAnal_SetMaxTracks (valid-ptr obj) maxtracks))

(defmethod do-process ((obj sin-anal))
  (if (object-ptr obj)
      (SinAnal_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sin-anal))
  (if (object-ptr obj)
      (delete_SinAnal (object-pointer obj))))

;;
;; SinSyn
;;

(defclass sin-syn (sndobj)
  ((input :initform nil :initarg :input)
   (maxtracks :initform nil :initarg :maxtracks)
   (table :initform nil :initarg :table)
   (scale :initform 1.0 :initarg :scale)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj sin-syn))
  (with-slots (input maxtracks table scale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SinSyn (valid-ptr input)
			maxtracks
			(valid-ptr table)
			(float scale) vecsize (float sr))))))

(defmethod set-table ((obj sin-syn) (tab table))
  (SinSyn_SetTable (valid-ptr obj) (valid-ptr tab)))

(defmethod set-max-tracks ((obj sin-syn) maxtracks)
  (SinSyn_SetMaxTracks (valid-ptr obj) maxtracks))

(defmethod set-scale ((obj sin-syn) scale)
  (SinSyn_SetScale (valid-ptr obj) (float scale)))

(defmethod set-parameter ((obj sin-syn) (str string) val)
  (SinSyn_Set (valid-ptr obj)
	      str (float val)))

(defmethod connect ((obj sin-syn) (str string) ptr)
  (SinSyn_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj sin-syn))
  (if (object-ptr obj)
      (SinSyn_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sin-syn))
  (if (object-ptr obj)
      (delete_SinSyn (object-pointer obj))))

;;;
;;; ReSyn
;;;

(defclass re-syn (sinsyn)
  ((pitch :initform 1.0 :initarg :pitch)
   (tscale :initform 1.0 :initarg :tscale)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj re-syn))
  (with-slots (input maxtracks table pitch scale tscale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ReSyn (valid-ptr input)
		       maxtracks
		       (valid-ptr table)
		       (float pitch) (float scale) (float tscale) vecsize (float sr))))))

(defmethod set-pitch ((obj re-syn) pitch &optional ignore)
  ignore
  (ReSyn_SetPitch (valid-ptr obj) (float pitch)))

(defmethod set-timescale ((obj re-syn) scale)
  (ReSyn_SetTimeScale (valid-ptr obj) scale))

(defmethod set-parameter ((obj re-syn) (str string) val)
  (ReSyn_Set (valid-ptr obj)
	     str (float val)))

(defmethod do-process ((obj re-syn))
  (if (object-ptr obj)
      (ReSyn_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj re-syn))
  (if (object-ptr obj)
      (delete_ReSyn (object-pointer obj))))

;;;;
;;;; AdSyn
;;;;

(defclass ad-syn (resyn)
  ((sig-inputs :initform '(input))))

(defmethod init-pointer ((obj ad-syn))
  (with-slots (input maxtracks table pitch scale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_AdSyn (valid-ptr input)
			       maxtracks
			       (valid-ptr table)
			       (float pitch) (float scale) vecsize (float sr))))))

(defmethod do-process ((obj ad-syn))
  (if (object-ptr obj)
      (AdSyn_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj ad-syn))
  (if (object-ptr obj)
      (delete_AdSyn (object-pointer obj))))

;;;;
;;;; IFAdd
;;;;

(defclass if-add (resyn)
  ((sig-inputs :initform '(input))))

(defmethod init-pointer ((obj if-add))
  (with-slots (input maxtracks table pitch tscale scale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_IFAdd (valid-ptr input)
			       maxtracks
			       (valid-ptr table)
			       (float pitch) (float scale) (float tscale) vecsize (float sr))))))

(defmethod do-process ((obj if-add))
  (if (object-ptr obj)
      (IFAdd_DoProcess (object-pointer obj))
      (error "instance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj if-add))
  (if (object-ptr obj)
      (delete_IFAdd (object-pointer obj))))

;;
;; SndIn
;;

(defclass snd-in (snd-io)
  ((input :initform nil :initarg :input)
   (channel :initform 1 :initarg :channel)))


(defmethod init-pointer ((obj snd-in))
    (with-slots (input channel vecsize sr) obj
      (unless (object-pointer obj)
	(setf (object-pointer obj) 
	      (new_SndIn (valid-ptr input)
			 channel vecsize (float sr))))))

(defmethod set-input ((obj snd-in) (input snd-io) &optional (channel 1))
  (SndIn_SetInput (valid-ptr obj) (valid-ptr input) channel))

(defmethod connect ((obj snd-in) (str string) ptr)
  (SndIn_Connect (valid-ptr obj)
			 str (valid-ptr ptr)))

(defmethod set-parameter ((obj snd-in) (str string) val)
  (SndIn_Set (valid-ptr obj)
	     str (float val)))

(defmethod destroy ((obj snd-in))
  (if (object-ptr obj)
      (delete_SndIn (object-pointer obj))))

(defmethod do-process ((obj snd-in))
  (SndIn_DoProcess (valid-ptr obj)))


;;
;; SndRead
;;

(defclass snd-read (sndobj)
  ((file :initform nil :initarg :file)
   (pitch :initform 1.0 :initarg :pitch)
   (scale :initform 1.0 :initarg :scale)))

(defmethod init-pointer ((obj snd-read))
  (with-slots (file pitch scale vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndRead (namestring file)
			 (float pitch) 
			 (*  *amp-scaling-factor* scale)
			 vecsize (float sr))))))

(defmethod get-outchannel ((obj snd-read) chan)
  (let ((ptr (SndRead_Outchannel (valid-ptr obj) chan))
	(res nil))
    (if (not (cffi:null-pointer-p ptr))
	(progn
	  (setf res (make-instance 'sndobj 
				   :vecsize (SndObj_GetVectorSize ptr)
				   :sr (SndObj_GetSr ptr)))
	  (setf (slot-value res 'object-pointer) ptr))
	(progn
	  (setf res (make-instance 'sndobj))
	  (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
    res))

(defmethod set-input ((obj snd-read) file &optional ignore)
  ignore
  (SndRead_SetInput (valid-ptr obj) file))

(defmethod set-scale ((obj snd-read) scale)
  (SndRead_SetScale (valid-ptr obj) (float scale)))

(defmethod set-pitch ((obj snd-read) pitch &optional ignore)
  (declare (ignore ignore))
  (SndRead_SetPitch (valid-ptr obj) (float pitch)))

(defmethod set-parameter ((obj snd-read) (str string) val)
  (SndRead_Set (valid-ptr obj)
	       str (float val)))

(defmethod do-process ((obj snd-read))
  (if (object-ptr obj)
      (SndRead_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj snd-read))
  (if (object-ptr obj)
      (delete_SndRead (object-pointer obj))))


;;
;; SpecIn
;;

(defclass spec-in (sndobj)
  ((input :initform nil :initarg :input)
   (chan :initform 1 :initarg :chan)))


(defmethod init-pointer ((obj spec-in))
  (with-slots (input chan vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecIn (valid-ptr input)
			chan vecsize (float sr))))))

(defmethod set-parameter ((obj spec-in) (str string) val)
  (SpecIn_Set (valid-ptr obj)
	      str (float val)))

(defmethod connect ((obj spec-in) (str string) ptr)
  (SpecIn_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod do-process ((obj spec-in))
  (if (object-ptr obj)
      (SpecIn_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-in))
  (if (object-ptr obj)
      (delete_SpecIn (object-pointer obj))))



;;
;; SpecMult
;;

(defclass spec-mult (sndobj)
  ((input1 :initform nil :initarg :input1)
   (input2 :initform nil :initarg :input2)
   (spec-table :initform nil :initarg :spec-table)
   (vecsize :initform *fft-size*)
   (sig-inputs :initform '(input1 input2))))

(defmethod init-pointer ((obj spec-mult))
  (with-slots (input1 input2 spec-table vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if spec-table
		(new_SpecMult_table (valid-ptr spec-table)
				    (valid-ptr input1)
				    vecsize (float sr))
		(new_SpecMult (valid-ptr input1)
			      (valid-ptr input2)
			      vecsize (float sr)))))))

(defmethod set-input2 ((obj spec-mult) (in sndobj))
  (SpecMult_SetInput2 (valid-ptr obj) (valid-ptr in)))

(defmethod set-table ((obj spec-mult) (tab table))
  (SpecMult_SetTable (valid-ptr obj) (valid-ptr tab)))

(defmethod connect ((obj spec-mult) (str string) ptr)
  (SpecMult_Connect (valid-ptr obj)
	       str (valid-ptr ptr)))

(defmethod do-process ((obj spec-mult))
  (if (object-ptr obj)
      (SpecMult_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-mult))
  (if (object-ptr obj)
      (delete_SpecMult (object-pointer obj))))


;;;
;;; PVBlur
;;;

(defclass pv-blur (spec-mult)
  ((input :initform nil :initarg :input)
   (blurtime :initform nil :initarg :blurtime)
   (hopsize :initform *vector-size* :initarg :hopsize)
   (vecsize :initform *fft-size*)
   (sig-inputs :initform '(input))))
   

(defmethod init-pointer ((obj pv-blur))
  (with-slots (input blurtime hopsize vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVBlur (valid-ptr input)
			(float blurtime) hopsize 
			vecsize (float sr))))))

(defmethod set-parameter ((obj pv-blur) (str string) val)
  (PVBlur_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-blurtime ((obj pv-blur) time)
  (PVBlur_SetBlurTime (valid-ptr obj) (float time)))

(defmethod set-hop-size ((obj pv-blur) hopsize)
  (PVBlur_SetHopSize (valid-ptr obj) (floor hopsize)))

(defmethod do-process ((obj pv-blur))
  (if (object-ptr obj)
      (PVBlur_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pv-blur))
  (if (object-ptr obj)
      (delete_PVBlur (object-pointer obj))))


;;;
;;; PVMix
;;;

(defclass pv-mix (spec-mult)
  ((sig-inputs :initform '(input1 input2))))

(defmethod init-pointer ((obj pv-mix))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVMix (valid-ptr input1)
		       (valid-ptr input2)
		       vecsize (float sr))))))

(defmethod do-process ((obj pv-mix))
  (if (object-ptr obj)
      (PVMix_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pv-mix))
  (if (object-ptr obj)
      (delete_PVMix (object-pointer obj))))


;;;
;;; PVTransp
;;;

(defparameter *pv-transp-modes* (list :normal 0 :formant 1))

(defclass pv-transp (spec-mult)
  ((input :initform nil :initarg :input)
   (pitch :initform nil :initarg :pitch)
   (mode :initform ':normal :initarg :mode)
   (pitch-mod :initform nil :initarg :pitch-mod)
   (sig-inputs :initform '(input pitch-mod))))

(defmethod init-pointer ((obj pv-transp))
  (with-slots (input pitch mode pitch-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVTransp (valid-ptr input)
			  (float pitch)
			  (get-option mode *pv-transp-modes*)
			  (valid-ptr pitch-mod)
			  vecsize (float sr))))))


(defmethod connect ((obj pv-transp) (str string) ptr)
  (PVTransp_Connect (valid-ptr obj)
		    str (valid-ptr ptr)))

(defmethod set-parameter ((obj pv-transp) (str string) val)
  (PVTransp_Set (valid-ptr obj)
		str (float val)))

(defmethod set-pitch ((obj pv-transp) pitch &optional (pitch-mod nil))
  (PVTransp_SetPitch (valid-ptr obj) (float pitch) (valid-ptr pitch-mod)))

(defmethod set-mode ((obj pv-transp) mode &optional ignore)
  ignore
  (PVTransp_SetMode (valid-ptr obj) (get-option mode *pv-transp-modes*)))

(defmethod do-process ((obj pv-transp))
  (if (object-ptr obj)
      (PVTransp_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pv-transp))
  (if (object-ptr obj)
      (delete_PVTransp (object-pointer obj))))



;;;
;;; SpecCart
;;;

(defclass spec-cart (spec-mult)
  ((input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj spec-cart))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecCart (valid-ptr input)
			  vecsize (float sr))))))

(defmethod do-process ((obj spec-cart))
  (if (object-ptr obj)
      (SpecCart_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-cart))
  (if (object-ptr obj)
      (delete_SpecCart (object-pointer obj))))


;;;;
;;;; SpecCombine
;;;;

(defclass spec-combine (spec-cart)
  ((mag-input :initform nil :initarg :mag-in)
   (phase-input :initform nil :initarg :phase-in)
   (sig-inputs :initform '(mag-input phase-input))))

(defmethod init-pointer ((obj spec-combine))
  (with-slots (mag-input phase-input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecCombine (valid-ptr mag-input)
			     (valid-ptr phase-input)
			     vecsize (float sr))))))

(defmethod connect ((obj spec-combine) (str string) ptr)
  (SpecCombine_Connect (valid-ptr obj)
		       str (valid-ptr ptr)))

(defmethod set-phase-input ((obj spec-combine) (phase-in sndobj))
  (SpecCombine_SetPhaseInput (valid-ptr obj) (valid-ptr phase-in)))

(defmethod set-mag-input ((obj spec-combine) (mag-in sndobj))
  (SpecCombine_SetmagInput (valid-ptr obj) (valid-ptr mag-in)))

(defmethod do-process ((obj spec-combine))
  (if (object-ptr obj)
      (SpecCombine_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-combine))
  (if (object-ptr obj)
      (delete_SpecCombine (object-pointer obj))))

;;;
;;; SpecInterp
;;;

(defclass spec-interp (spec-mult)
  ((interp :initform nil :initarg :interp)
   (interp-mod :initform nil :initarg :interp-mod)
   (sig-inputs :initform '(input1 input2 interp-mod))))

(defmethod init-pointer ((obj spec-interp))
  (with-slots (interp input1 input2 interp-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecInterp interp
			    (valid-ptr input1)
			    (valid-ptr input2)
			    (valid-ptr interp-mod)
			    vecsize (float sr))))))

(defmethod set-parameter ((obj spec-interp) (str string) val)
  (SpecInterp_Set (valid-ptr obj)
		  str (float val)))

(defmethod connect ((obj spec-interp) (str string) ptr)
  (SpecInterp_Connect (valid-ptr obj)
		      str (valid-ptr ptr)))

(defmethod set-interp ((obj spec-interp) offset &optional (interp-mod nil))
  (SpecInterp_SetInterp (valid-ptr obj) (float offset) (valid-ptr interp-mod)))

(defmethod do-process ((obj spec-interp))
  (if (object-ptr obj)
      (SpecInterp_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-interp))
  (if (object-ptr obj)
      (delete_SpecInterp (object-pointer obj))))

;;;;
;;;; PVMorph
;;;;

(defclass pv-morph (spec-interp)
  ((freq-morph :initform nil :initarg :freq-morph)
   (amp-morph :initform nil :initarg :amp-morph)
   (freq-morph-mod :initform nil :initarg :freq-morph-mod)
   (amp-morph-mod :initform nil :initarg :amp-morph-mod)
   (sig-inputs :initform '(freq-morph amp-morph freq-morph-mod amp-morph-mod))))

(defmethod init-pointer ((obj pv-morph))
  (with-slots (input1 input2 freq-morph amp-morph freq-morph-mod amp-morph-mod vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVMorph (float freq-morph) (float amp-morph)
			 (valid-ptr freq-morph)
			 (valid-ptr amp-morph)
			 (valid-ptr freq-morph-mod)
			 (valid-ptr amp-morph-mod)
			 vecsize (float sr))))))

(defmethod connect ((obj pv-morph) (str string) ptr)
  (PVMorph_Connect (valid-ptr obj)
		    str (valid-ptr ptr)))

(defmethod set-parameter ((obj pv-morph) (str string) val)
  (PVMorph_Set (valid-ptr obj)
	       str (float val)))

(defmethod set-freq-morph ((obj pv-morph) morph &optional (freq-morph-mod nil))
  (PVMorph_SetFreqMorph (valid-ptr obj) (float morph) (valid-ptr freq-morph-mod)))

(defmethod set-amp-morph ((obj pv-morph) morph &optional (amp-morph-mod nil))
  (PVMorph_SetAmpMorph (valid-ptr obj) (float morph) (valid-ptr amp-morph-mod)))

(defmethod do-process ((obj pv-morph))
  (if (object-ptr obj)
      (PVMorph_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pv-morph))
  (if (object-ptr obj)
      (delete_PVMorph (object-pointer obj))))


;;;;
;;;; PVMask
;;;;

(defclass pv-mask (spec-interp)
  ((maskgain :initform nil :initarg :maskgain)
   (input :initform nil :initarg :input)
   (mask :initform nil :initarg :mask)
   (mask-mod :initform nil :initarg :mask-mod)
   (masktable :initform nil :initarg :masktable)
   (vecsize :initform *fft-size*)
   (sig-inputs :initform '(input mask-mod))))

(defmethod init-pointer ((obj pv-mask))
  (with-slots (maskgain input mask mask-mod masktable vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if masktable
		(new_PVMask_table (float maskgain)
				  (valid-ptr masktable)
				  (valid-ptr input)
				  (valid-ptr mask-mod)
				  vecsize (float sr))
		(new_PVMask (float maskgain)
			    (valid-ptr input)
			    (valid-ptr mask)
			    (valid-ptr mask-mod)
			    vecsize (float sr)))))))

(defmethod connect ((obj pv-mask) (str string) ptr)
  (PVMask_Connect (valid-ptr obj)
		  str (valid-ptr ptr)))

(defmethod set-parameter ((obj pv-mask) (str string) val)
  (PVMask_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-mask ((obj pv-mask) (mask sndobj))
  (PVMask_SetMaskInput (valid-ptr obj) (valid-ptr mask)))

(defmethod set-mask-table ((obj pv-mask) (tab table))
  (PVMask_SetMaskTable (valid-ptr obj) (valid-ptr tab)))

(defmethod set-mask-gain ((obj pv-mask) maskgain &optional (mask-mod nil))
  (PVMask_SetMaskGain (valid-ptr obj) (float maskgain) (valid-ptr mask-mod)))

(defmethod do-process ((obj pv-mask))
  (if (object-ptr obj)
      (PVMask_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pv-mask))
  (if (object-ptr obj)
      (delete_PVMask (object-pointer obj))))


;;;;
;;;; PVFilter
;;;;

(defclass pv-filter (spec-interp)
  ((input :initform nil :initarg :input)
   (filter-spec :initform nil :initarg :filter-spec)
   (amount :initform 1.0 :initarg :amount)
   (amount-mod :initform nil :initarg :amount-mod)
   (filter-table :initform nil :initarg :filter-table)
   (sig-inputs :initform '(input amount-mod filter-spec))))

(defmethod init-pointer ((obj pv-filter))
  (with-slots (input filter-spec amount amount-mod filter-table vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if filter-table
		(new_PVFilter_table (valid-ptr filter-table)
				    (valid-ptr input)
				    (float amount)
				    (valid-ptr amount-mod)
				    vecsize (float sr))
		(new_PVFilter (valid-ptr input)
			      (valid-ptr filter-spec)
			      (float amount)
			      (valid-ptr amount-mod)
			      vecsize (float sr)))))))

(defmethod connect ((obj pv-filter) (str string) ptr)
  (PVFilter_Connect (valid-ptr obj)
		    str (valid-ptr ptr)))

(defmethod set-parameter ((obj pv-filter) (str string) val)
  (PVFilter_Set (valid-ptr obj)
	      str (float val)))

(defmethod set-filter-spec ((obj pv-filter) (spec sndobj))
  (PVFilter_SetFilterInput (valid-ptr obj) (valid-ptr spec)))

(defmethod set-filter-table ((obj pv-filter) (tab table))
  (PVFilter_SetFilterTable (valid-ptr obj) (valid-ptr tab)))

(defmethod set-amount ((obj pv-filter) amount &optional (amount-mod nil))
  (PVFilter_SetAmount (valid-ptr obj) (float amount) (valid-ptr amount-mod)))

(defmethod do-process ((obj pv-filter))
  (if (object-ptr obj)
      (PVFilter_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pv-filter))
  (if (object-ptr obj)
      (delete_PVFilter (object-pointer obj))))



;;;
;;; SpecPolar
;;;

(defclass spec-polar (spec-mult)
  ((input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj spec-polar))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecPolar (valid-ptr input)
			   vecsize (float sr))))))

(defmethod do-process ((obj spec-polar))
  (if (object-ptr obj)
      (SpecPolar_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-polar))
  (if (object-ptr obj)
      (delete_SpecPolar (object-pointer obj))))

;;;;
;;;; SpecSplit
;;;;

(defclass spec-split (spec-polar)
  ((input :initform nil :initarg :input)
   (sig-inputs :initform '(input))
   (magnitude :initform nil)
   (phase :initform nil)))

(defmethod init-pointer ((obj spec-split))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecSplit (valid-ptr input)
			   vecsize (float sr))))))

(defmethod get-magnitude ((obj spec-split))
  (if (slot-value obj 'magnitude)
      (slot-value obj 'magnitude)
      (let ((ptr (SpecSplit_magnitude_get (valid-ptr obj)))
	    (res nil))
	(if (not (cffi:null-pointer-p ptr))
	    (progn
	      (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				       :sr (get-sr ptr)))
	      (setf (slot-value res 'object-pointer) ptr)
	      (setf (slot-value obj 'magnitude) res))
	    (progn
	      (setf res (make-instance 'sndobj))
	      (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
	res)))

(defmethod get-phase ((obj spec-split))
  (if (slot-value obj 'phase)
      (slot-value obj 'phase)
      (let ((ptr (SpecSplit_phase_get (valid-ptr obj)))
	    (res))
	(if (not (cffi:null-pointer-p ptr))
	    (progn
	      (setf res (make-instance 'sndobj :vecsize (get-vector-size ptr)
				       :sr (get-sr ptr)))
	      (setf (slot-value res 'object-pointer) ptr)
	      (setf (slot-value obj 'phase) res))
	    (progn
	      (setf res (make-instance 'sndobj))
	      (setf (slot-value res 'object-pointer) (cffi:null-pointer))))
	res)))


;;;;
;;;; SpecThresh
;;;;

(defclass spec-thresh (spec-polar)
  ((threshold :initform nil :initarg :threshold)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj spec-thresh))
  (with-slots (threshold input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecThresh (float threshold)
			    (valid-ptr input)
			    (floor vecsize)
			    (float sr))))))

(defmethod set-parameter ((obj spec-thresh) (str string) val)
  (SpecThresh_Set (valid-ptr obj)
		  str (float val)))

(defmethod set-threshold ((obj spec-thresh) thresh)
  (SpecThresh_SetThreshold (valid-ptr obj) (float thresh)))

(defmethod do-process ((obj spec-thresh))
  (if (object-ptr obj)
      (SpecThresh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-thresh))
  (if (object-ptr obj)
      (delete_SpecThresh (object-pointer obj))))


;;;;
;;;; SpecVoc
;;;;

(defclass spec-voc (spec-polar)
  ((sig-inputs :initform '(input1 input2))))

(defmethod init-pointer ((obj spec-voc))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecThresh (valid-ptr input1)
			    (valid-ptr input2)
			    vecsize (float sr))))))

(defmethod do-process ((obj spec-polar))
  (if (object-ptr obj)
      (SpecPolar_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj spec-polar))
  (if (object-ptr obj)
      (delete_SpecPolar (object-pointer obj))))


;;
;; SyncGrain
;;

(defclass sync-grain (sndobj)
  ((wavetable :initform nil :initarg :wavetable)
   (envtable :initform nil :initarg :envtable)
   (freq :initform nil :initarg :freq)
   (amp :initform nil :initarg :amp)
   (pitch :initform nil :initarg :pitch)
   (grsize :initform nil :initarg :grsize)
   (prate :initform 1.0 :initarg :prate)
   (freq-mod :initform nil :initarg :freq-mod)
   (amp-mod :initform nil :initarg :amp-mod)
   (pitch-mod :initform nil :initarg :pitch-mod)
   (grsize-mod :initform nil :initarg :grsize-mod)
   (overlaps :initform 100 :initarg :overlaps)
   (sig-inputs :initform '(freq-mod amp-mod pitch-mod grsize-mod))))

(defmethod init-pointer ((obj sync-grain))
  (with-slots (wavetable envtable freq amp pitch 
			 grsize prate freq-mod amp-mod pitch-mod
			 grsize-mod overlaps vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SyncGrain (valid-ptr wavetable)
				   (valid-ptr envtable)
				   (float freq) (float amp) (float pitch) (float grsize) (float prate)
				   (valid-ptr freq-mod)
				   (valid-ptr amp-mod)
				   (valid-ptr pitch-mod)
				   (valid-ptr grsize-mod)
				   overlaps vecsize sr)))))

(defmethod set-wavetable ((obj sync-grain) wavetable)
  (SyncGrain_SetWaveTable (valid-ptr obj) (valid-ptr wavetable)))

(defmethod set-envtable ((obj sync-grain) envtable)
  (SyncGrain_SetEnvelopeTable (valid-ptr obj) (valid-ptr envtable)))

(defmethod set-freq ((obj sync-grain) freq &optional (freq-mod nil))
  (SyncGrain_SetFreq (valid-ptr obj) (float freq) (valid-ptr freq-mod)))

(defmethod set-amp ((obj sync-grain) amp &optional (amp-mod nil))
  (SyncGrain_SetAmp (valid-ptr obj) (float amp) (valid-ptr amp-mod)))

(defmethod set-pitch ((obj sync-grain) pitch &optional (pitch-mod nil))
  (SyncGrain_SetPitch (valid-ptr obj) (float pitch) (valid-ptr pitch-mod)))

(defmethod set-grain-size ((obj sync-grain) grsize &optional (grsize-mod nil))
  (SyncGrain_SetGrainSize (valid-ptr obj) (float grsize) (valid-ptr grsize-mod)))

(defmethod set-pointer-rate ((obj sync-grain) prate)
  (SyncGrain_SetPointerRate (valid-ptr obj) (float prate)))

(defmethod set-parameter ((obj sync-grain) (str string) val)
  (SyncGrain_Set (valid-ptr obj)
		 str (float val)))

(defmethod connect ((obj sync-grain) (str string) ptr)
  (SyncGrain_Connect (valid-ptr obj)
		     str (valid-ptr ptr)))

(defmethod do-process ((obj sync-grain))
  (if (object-ptr obj)
      (SyncGrain_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sync-grain))
  (if (object-ptr obj)
      (delete_SyncGrain (object-pointer obj))))


;;
;; Unit
;;






;
; SndIO
;


(defclass snd-io (sndobj)
  ((channels :initform 1 :initarg :channels)
   (bits :initform 16 :initarg :bits)
   (inputlist :initform nil :initarg :inputlist)))

(defmethod init-pointer ((obj snd-io))
  (with-slots (channels bits inputlist vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndIO channels bits
		       inputlist
		       vecsize (float sr))))))

(defmethod get-sr ((obj snd-io))
  (SndIO_GetSr (valid-ptr obj)))

(defmethod get-vector-size ((obj snd-io))
  (SndIO_GetVectorSize (valid-ptr obj)))

(defmethod get-channels ((obj snd-io))
  (SndIO_GetChannels (valid-ptr obj)))

(defmethod get-size ((obj snd-io))
  (SndIO_GetSize (valid-ptr obj)))

(defmethod get-output ((obj snd-io) pos chan)
  (SndIO_Output (valid-ptr obj) pos chan))

(defmethod set-output ((obj snd-io) chan (input sndobj))
  (SndIO_SetOutput (valid-ptr obj) chan (valid-ptr input)))

(defmethod destroy ((obj snd-io))
  (if (object-ptr obj)
      (delete_SndIO (object-pointer obj))))


;;
;; SndBuffer
;;

(defclass snd-buffer (snd-io)
  ((buffsize :initform *vector-size* :initarg :buffsize)))

(defmethod init-pointer ((obj snd-buffer))
  (with-slots (channels buffsize inputlist vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndBuffer channels
			   buffsize
			   (valid-ptr inputlist)
			   vecsize (float sr))))))

(defmethod read-file ((obj snd-buffer))
  (SndBuffer_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-buffer))
  (SndBuffer_Write (valid-ptr obj)))

(defmethod destroy ((obj snd-buffer))
  (if (object-ptr obj)
      (delete_SndBuffer (object-pointer obj))))



;;
;; SndFIO
;;

(defparameter *file-open-modes* (list :overwrite 0 :append 1 :insert 2 :read 3))

(defclass snd-fio (snd-io)
  ((file :initform nil :initarg :file)
   (mode :initform nil :initarg :mode)
   (start :initform 0.0 :initarg :start)))


(defmethod init-pointer ((obj snd-fio))
  (with-slots (file mode channels bits inputlist start vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndFIO (namestring file)
				(get-option mode *file-open-modes*) 
				channels
				bits
				(valid-ptr inputlist)
				(float start) vecsize (float sr))))))

;;returns pointer of FILE
(defmethod get-file ((obj snd-fio))
  (SndFIO_GetFile (valid-ptr obj)))

(defmethod get-mode ((obj snd-fio))
  (SndFIO_GetMode (valid-ptr obj)))

(defmethod set-position ((obj snd-fio) seconds)
  (SndFIO_SetPos_float (valid-ptr obj) (float seconds)))

(defmethod set-position ((obj snd-fio) (bytes integer))
  (SndFIO_SetPos (valid-ptr obj) bytes))

(defmethod eof? ((obj snd-fio))
  (SndFIO_Eof (valid-ptr obj)))

(defmethod get-data-frames ((obj snd-fio))
  (SndFIO_GetDataFrames (valid-ptr obj)))

(defmethod get-position ((obj snd-fio))
  (SndFIO_GetPos (valid-ptr obj)))

(defmethod get-status ((obj snd-fio))
  (SndFIO_GetStatus (valid-ptr obj)))

(defmethod read-file ((obj snd-fio))
  (SndFIO_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-fio))
  (SndFIO_Write (valid-ptr obj)))

;;;
;;; SndWave
;;;

(defclass snd-wave (snd-fio)
  ())

(defmethod init-pointer ((obj snd-wave))
  (with-slots (file mode channels bits inputlist start vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndWave (namestring file)
			 (get-option mode *file-open-modes*)
			 channels bits 
			 (valid-ptr inputlist)
			 (float start) vecsize (float sr))))))

;;returns pointer 
(defmethod get-header ((obj snd-wave))
  (SndWave_GetHeader (valid-ptr obj)))

(defmethod read-file ((obj snd-wave))
  (SndWave_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-wave))
  (SndWave_Write (valid-ptr obj)))

(defmethod wave? ((obj snd-wave))
  (SndWave_IsWave (valid-ptr obj)))

(defmethod destroy ((obj snd-wave))
  (if (object-ptr obj)
      (delete_SndWave (object-pointer obj))))


;;;;
;;;; SndWaveX
;;;;

(defparameter *sndwavex-format* (list :pcm #x0001 :float #x0003))

(defparameter *sndwavex-channel-masks* 
  (list :none 0 :front-left #x1 :front-right #x2 :front-center #x4
	:low-frequency #x8 :back-left #x10 :back-right #x20
	:front-left-of-center #x40 :front-right-of-center #x80
	:back-center #x100 :side-left #x200 :side-right #x400
	:top-center #x800 :top-front-left #x1000
	:top-front-center #x2000 :top-front-right #x4000
	:top-back-left #x8000 :top-back-center #x10000
	:top-back-right #x20000))

(defclass snd-wavex (snd-wave)
  ((channel-mask :initform ':none :initarg :channel-mask)
   (format :initform ':pcm :initarg :format)))

(defmethod init-pointer ((obj snd-wavex))
  (with-slots (file mode channels channel-mask bits format inputlist format start vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndWaveX (namestring file)
			  (get-option mode *file-open-modes*)
			  channels 
			  (get-option channel-mask *sndwavex-channel-masks*)
			  bits (get-option format *sndwavex-format*)
			  (valid-ptr inputlist)
			  (float start) vecsize (float sr))))))

;;returns pointer 
					;(defmethod get-header ((obj snd-wavex))
					; (SndWaveX_GetHeader (valid-ptr obj)))

(defmethod read-file ((obj snd-wavex))
  (SndWaveX_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-wavex))
  (SndWaveX_Write (valid-ptr obj)))

(defmethod wave-extensible? ((obj snd-wavex))
  (SndWaveX_IsWaveExtensible (valid-ptr obj)))

(defmethod wavex? ((obj snd-wavex))
  (SndWaveX_IsWaveExtensible (valid-ptr obj)))

(defmethod destroy ((obj buzz))
  (if (object-ptr obj)
      (delete_SndWaveX (object-pointer obj))))



;;;;
;;;; SndPVOCEX
;;;;

(defparameter *pvocex-analysis-formats* (list :amp-freq 0 :amp-phase 1 :complex 2))

(defparameter *fft-window-types* (list :default 0 :hamming 1 :hanning 2 :kaiser 3
				      :rectangular 4 :custom 5))

(defclass snd-pvocex (snd-wavex)
  ((mode :initform ':read)
   (analysis-format :initform ':amp-freq :initarg :analysis-format)
   (window-type :initform ':hanning :initarg :window-type)
   (bits :initform 32 :initarg :bits)
   (format :initform ':pcm :initarg :format)
   (hopsize :initform *vector-size* :initarg :hopsize)
   (fftsize :initform *fft-size* :initarg :fftsize)))

(defmethod init-pointer ((obj snd-pvocex))
  (with-slots (file mode analysis-format window-type channels channel-mask bits format inputlist start hopsize fftsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndPVOCEX (namestring file)
			   (get-option mode *file-open-modes*)
			   (get-option analysis-format *pvocex-analysis-formats*)
			   (get-option window-type *fft-window-types*)
			   channels 
			   (get-option channel-mask *sndwavex-channel-masks*)
			   bits 
			   (get-option format *sndwavex-format*)
			   (valid-ptr inputlist)
			   (float start) hopsize fftsize (float sr))))))

(defmethod get-fft-size ((obj snd-pvocex))
  (SndPVOCEX_GetFFTSize (valid-ptr obj)))

(defmethod get-hop-size ((obj snd-pvocex))
  (SndPVOCEX_GetHopSize (valid-ptr obj)))

(defmethod get-window-type ((obj snd-pvocex))
  (SndPVOCEX_GetWindowType (valid-ptr obj)))

(defmethod get-window-length ((obj snd-pvocex))
  (SndPVOCEX_GetWindowLength (valid-ptr obj)))

#| need struct to pass
(defmethod get-header ((obj snd-pvocex))

  (SndPVOCEX_GetHeader (valid-ptr obj) 
|#
   
(defmethod set-position ((obj snd-pvocex) pos)
  (SndPVOCEX_SetTimePos (valid-ptr obj) (float pos)))

(defmethod read-file ((obj snd-pvocex))
  (SndPVOCEX_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-pvocex))
  (SndPVOCEX_Write (valid-ptr obj)))

(defmethod pvocex? ((obj snd-pvocex))
  (SndPVOCEX_isPVOCEX (valid-ptr obj)))

(defmethod destroy ((obj snd-pvocex))
  (if (object-ptr obj)
      (delete_SndPVOCEX (object-pointer obj))))



;;;;
;;;; SndSinusex or SndSinIO
;;;;

(defclass snd-sinusex (snd-wavex)
  ((maxtracks :initform nil :initarg :maxtracks)
   (threshold :initform .01 :initarg :threshold)
   (window-type :initform ':hanning :initarg :window-type)
   (bits :initform 32 :initarg :bits)
   (mode :initform ':overwrite)
   (format :initform ':pcm)
   (hopsize :initform *vector-size* :initarg :hopsize)
   (fftsize :initform *fft-size* :initarg :fftsize)))

(defclass snd-sin-io (snd-sinusex)
  ())

(defmethod init-pointer ((obj snd-sinusex))
  (with-slots (file maxtracks threshold mode window-type channels channel-mask bits format inputlist start hopsize fftsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndSinIO (namestring file)
				  maxtracks threshold
				  (get-option window-type *fft-window-types*)
				  (get-option mode *file-open-modes*)
				  channels
				  (get-option channel-mask *sndwavex-channel-masks*)
				  bits
				  (get-option format *sndwavex-format*)
				  (valid-ptr inputlist)
				  (float start) hopsize fftsize (float sr))))))

(defmethod read-file ((obj snd-sinusex))
  (SndSinIO_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-sinusex))
  (SndSinIO_Write (valid-ptr obj)))

(defmethod get-track-id ((obj snd-sinusex) track &optional (channel 1))
  (SndSinIO_GetTrackID (valid-ptr obj) track channel))

(defmethod get-tracks ((obj snd-sinusex) &optional (chan 1))
  (SndSinIO_GetTracks (valid-ptr obj) chan))

(defmethod get-fft-size ((obj snd-sinusex))
  (SndSinIO_GetFFTSize (valid-ptr obj)))

(defmethod get-hop-size ((obj snd-sinusex))
  (SndSinIO_GetHopSize (valid-ptr obj)))

(defmethod get-window-type ((obj snd-sinusex))
  (SndSinIO_GetWindowType (valid-ptr obj)))

(defmethod get-max-tracks ((obj snd-sinusex))
  (SndSinIO_GetMaxTracks (valid-ptr obj)))


(defmethod destroy ((obj snd-sinusex))
  (if (object-ptr obj)
      (delete_SndSinIO (object-pointer obj))))


#|

get header
need pointer to pass
|# 

(defmethod set-position ((obj snd-sinusex) time)
  (SndSinIO_SetTimePos (valid-ptr obj) time))


;;;
;;; SndAIFF
;;;

(defclass snd-aiff (snd-fio)
  ((bits :initform 16 :initarg :bits)))

(defmethod init-pointer ((obj snd-aiff))
  (with-slots (file mode channels bits inputlist start vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndAiff (namestring file)
			 (get-option mode *file-open-modes*)
			 channels
			 (floor bits)
			 (valid-ptr inputlist)
			 (float start) vecsize (float sr))))))

(defmethod aiff? ((obj snd-aiff))
  (SndAiff_IsAiff (valid-ptr obj)))

(defmethod read-file ((obj snd-aiff))
  (SndAiff_Read (valid-ptr obj)))

(defmethod write-file ((obj snd-aiff))
  (SndAiff_Write (valid-ptr obj)))

(defmethod destroy ((obj snd-aiff))
  (if (object-ptr obj)
      (delete_SndAiff (object-pointer obj))))


;;
;; EnvTable
;;

(defclass env-table (table)
  ((segments :initform nil :initarg :segments)
   (start :initform nil :initarg :start)
   (points :initform '() :initarg :points)
   (lengths :initform '() :initarg :lengths)
   (type :initform 0.0 :initarg :type)))


(defmethod init-pointer ((obj env-table))
  (with-slots (length segments start points lengths type) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_EnvTable length segments (float start) (foreign-float-array points)
			  (foreign-float-array lengths) (float type))))))

(defmethod set-env-envelope ((obj env-table) segments start 
			     points lengths &optional (type 0.0))
  (EnvTable_SetEnvelope (valid-ptr obj) segments (float start) (foreign-float-array points)
			(foreign-float-array lengths) (float type)))

(defmethod make-table ((obj env-table))
  (EnvTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj env-table))
  (if (object-ptr obj)
      (delete_EnvTable (object-pointer obj))))


;;
;; HammingTable
;;

(defclass hamming-table (table)
  ((alpha :initform .54 :initarg :alpha)))

(defmethod init-pointer ((obj hamming-table))
  (with-slots (length alpha) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_HammingTable length (float alpha))))))

(defmethod set-parameter ((obj hamming-table) length alpha)
  (HammingTable_SetParam (valid-ptr obj)
			 length (float alpha)))

(defmethod destroy ((obj hamming-table))
  (if (object-ptr obj)
      (delete_HammingTable (object-pointer obj))))


;;
;; HarmTable 
;;

(defparameter *wave-forms* (list :sine 1 :saw 2 :square 3 :buzz 4))

(defclass harm-table (table)
  ((harm :initform 1 :initarg :harm)
   (type :initform nil :initarg :type)
   (phase :initform 0.0 :initarg :phase)))


(defmethod init-pointer ((obj harm-table))
  (with-slots (length harm type phase) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_HarmTable length harm (get-option type *wave-forms*) (float phase))))))



(defmethod set-harm ((obj table) harm &optional (type ':sine))
  (HarmTable_SetHarm (valid-ptr obj) harm (get-option type)))

(defmethod set-phase ((obj harm-table) phase)
  (HarmTable_SetPhase (valid-ptr obj) (float phase)))

(defmethod make-table ((obj harm-table))
  (HarmTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj harm-table))
  (if (object-ptr obj)
      (delete_HarmTable (object-pointer obj))))


;;
;; ImpulseTable
;;

(defclass impulse-table (table)
  ((segments :initform nil :initarg :segments)
   (start :initform nil :initarg :start)
   (points :initform nil :initarg :points)
   (lengths :initform nil :initarg :lengths)
   (type :initform 0.0 :initarg :type)
   (window :initform nil :initarg :window)
   (nyquist-amp :initform 0.0 :initarg :nyquist-amp)))

(defmethod init-pointer ((obj impulse-table))
  (with-slots (length segments start points lengths type window nyquist-amp) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_ImpulseTable length segments (float start) (foreign-float-array points)
			      (foreign-float-array lengths) (float type)
			      (valid-ptr window) (float nyquist-amp))))))

(defmethod set-window ((obj impulse-table) (tab table))
  (ImpulseTable_SetWindow (valid-ptr obj) (valid-ptr tab)))

(defmethod make-table ((obj impulse-table))
  (ImpulseTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj impulse-table))
  (if (object-ptr obj)
      (delete_ImpulseTable (object-pointer obj))))



;;
;; LoPassTable
;;

(defclass lopass-table (table) 
  ((freq :initform nil :initarg :freq)
   (sr :initform 44100.0)))

(defmethod init-pointer ((obj lopass-table))
  (with-slots (length freq sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_LoPassTable length freq sr)))))

(defmethod set-freq ((obj lopass-table) freq &optional ignore)
  ignore
  (LoPassTable_SetFreq (valid-ptr obj) freq))

(defmethod set-sr ((obj lopass-table) sr)
  (LoPassTable_SetSr (valid-ptr obj) sr))

(defmethod make-table ((obj lopass-table))
  (LoPassTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj lopass-table))
  (if (object-ptr obj)
      (delete_LoPass (object-pointer obj))))


;;
;; NoteTable
;;

(defclass note-table (table)
  ((lower-note :initform nil :initarg :lower-note)
   (upper-note :initform nil :initarg :upper-note)
   (lower-freq :initform nil :initarg :lower-freq)
   (upper-freq :initform nil :initarg :upper-freq)))

(defmethod init-pointer ((obj note-table))
  (with-slots (lower-note upper-note lower-freq upper-freq) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_NoteTable lower-note upper-note (float lower-freq) (float upper-freq))))))
  
(defmethod set-freq-interval ((obj note-table) lf uf)
  (NoteTable_SetFreqInterval (valid-ptr obj) (float lf) (float uf)))

(defmethod set-note-interval ((obj note-table) ln un)
  (NoteTable_SetNoteInterval (valid-ptr obj) ln un))

(defmethod make-table ((obj note-table))
  (NoteTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj note-table))
  (if (object-ptr obj)
      (delete_NoteTable (object-pointer obj))))


;;
;; PlnTable
;;
			
(defclass pln-table (table)
  ((order :initform nil :initarg :order)
   (coefs :initform '() :initarg :coefs)
   (range :initform 1.0 :initarg :range)))

(defmethod init-pointer ((obj pln-table))
  (with-slots (length order coefs range) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PlnTable length order (foreign-double-array coefs) (float range))))))

(defmethod set-pln ((obj pln-table) order coefs &optional (range 1.0))
  (PlnTable_SetPln (valid-ptr obj) order (foreign-double-array coefs) (float range)))

(defmethod destroy ((obj pln-table))
  (if (object-ptr obj)
      (delete_PlnTable (object-pointer obj))))


;;
;; PVEnvTable
;;

(defclass pv-env-table (table)
  ((segments :initform nil :initarg :segments)
   (start :initform nil :initarg :start)
   (points :initform '() :initarg :points)
   (lengths :initform '() :initarg :lengths)
   (type :initform 0.0 :initarg :type)
   (phi :initform 0.0 :initarg :phi)
   (sr :initform 44100.0)
   (nyquist-amp :initform 0.0 :initarg :nyquist-amp)))

(defmethod init-pointer ((obj pv-env-table))
  (with-slots (length segments start points lengths type phi sr nyquist-amp) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVEnvTable length segments (float start)
			    (foreign-float-array points)
			    (foreign-float-array lengths)
			    (float type) (float sr) (float nyquist-amp))))))

(defmethod set-pvenv-envelope ((obj pv-env-table) segments start points lengths type ny)
  (PVEnvTable_SetEnvelope (valid-ptr obj) segments (float start) (foreign-float-array points)
			  (foreign-float-array lengths) (float type) (float ny)))

(defmethod set-sr ((obj pv-env-table) sr)
  (PVEnvTable_SetSr (valid-ptr obj) (float sr)))

(defmethod make-table ((obj pv-env-table))
  (PVEnvTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj pv-env-table))
  (if (object-ptr obj)
      (delete_PVEnvTable (object-pointer obj))))



;;;
;;; SpecEnvTable
;;;

(defclass spec-env-table (table)
  ((segments :initform nil :initarg :segments)
   (start :initform nil :initarg :start)
   (points :initform '() :initarg :points)
   (lengths :initform '() :initarg :lengths)
   (type :initform 0.0 :initarg :type)
   (nyquist-amp :initform 0.0 :initarg :nyquist-amp)))

(defmethod init-pointer ((obj spec-env-table))
  (with-slots (length segments start points lengths type nyquist-amp) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SpecEnvTable length segments (float start)
			      (foreign-float-array points)
			      (foreign-float-array lengths)
			      (float type) (float nyquist-amp))))))

(defmethod make-table ((obj spec-env-table))
  (SpecEnvTable_MakeTable (valid-ptr obj)))



;;
;; PVTable
;;

(defclass pv-table (table)
  ((input :initform nil :initarg :input)
   (window :initform nil :initarg :window)
   (start :initform nil :initarg :start)
   (end :initform nil :initarg :end)))

(defmethod init-pointer ((obj pv-table))
  (with-slots (length input window start end) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVTable length (valid-ptr input) (valid-ptr window) (float start) (float end))))))

(defmethod make-table ((obj pv-table))
  (PVTable_MakeTable (valid-ptr obj)))

(defmethod set-pv-table ((obj pv-table) (fle snd-fio) (tab table) start end)
  (PVTable_SetTable (valid-ptr obj) (valid-ptr fle) (valid-ptr tab)
		    (float start) (float end)))

;;
;; TriSegTable
;;

(defclass triseg-table (table)
  ((init :initform nil :initarg :init)
   (seg1 :initform nil :initarg :seg1)
   (p1 :initform nil :initarg :p1)
   (seg2 :initform nil :initarg :seg2)
   (p2 :initform nil :initarg :p2)
   (seg3 :initform nil :initarg :seg3)
   (fin :initform nil :initarg :fin)
   (type :initform nil :initarg :type)
   (points :initform '() :initarg :points)))


(defmethod init-pointer ((obj triseg-table))
  (with-slots (length init seg1 p1 seg2 p2 seg3 fin type points) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if points
		(new_TrisegTable_points length (foreign-float-array points) (float type))
		(new_TrisegTable length (float init) (float seg1) (float p1) (float seg2)
				 (float p2) (float seg3) (float fin) (float type)))))))

(defmethod set-curve ((obj triseg-table) init seg1 p1 seg2 p2 seg3 fin &optional (type 0.0))
  (TrisegTable_SetCurve (valid-ptr obj) (float init) (float seg1) (float p1) (float seg2) (float p2)
			(float seg3) (float fin) (float type)))

(defmethod set-curve-points ((obj triseg-table) points &optional (type 0.0))
  (TrisegTable_SetCurve_points (valid-ptr obj) (foreign-float-array points) (float type)))

(defmethod make-table ((obj triseg-table))
  (TrisegTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj triseg-table))
  (if (object-ptr obj)
      (delete_TrisegTable (object-pointer obj))))



;;
;; SndTable
;;

(defclass snd-table (table)
  ((input :initform nil :initarg :input)
   (channel :initform 1 :initarg :channel)))

(defmethod init-pointer ((obj snd-table))
  (with-slots (length input channel) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_SndTable length input channel)))))

(defmethod set-snd-input ((obj snd-table) len (input snd-fio) &optional (chan 1))
  (SndTable_SetInput (valid-ptr obj) len (valid-ptr input) chan))

(defmethod make-table ((obj snd-table))
  (SndTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj snd-table))
  (if (object-ptr obj)
      (delete_SndTable (object-pointer obj))))

;;
;; UsrDefTable
;;

(defclass usr-def-table (table) 
  ((length :initform nil :initarg :length)
   (values :initform '() :initarg :values)))

(defmethod init-pointer ((obj usr-def-table))
  (with-slots (length values) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_UsrDefTable length (foreign-float-array values))))))

(defmethod set-usr-table ((obj usr-def-table) length values)
  (UsrDefTable_SetTable (valid-ptr obj) length (foreign-float-array values)))

(defmethod make-table ((obj usr-def-table))
  (UsrDefTable_MakeTable (valid-ptr obj)))

;;
;; UsrHarmTable
;; 
   

(defclass usr-harm-table (table)
  ((harm :initform 1 :initarg :harm)
   (amps :initform '() :initarg :amps)))

(defmethod init-pointer ((obj usr-harm-table))
  (with-slots (length harm amps) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_UsrHarmTable length harm (foreign-float-array amps))))))

(defmethod set-harm ((obj usr-harm-table) harm &optional amps)
  (UsrHarmTable_SetHarm (valid-ptr obj) harm (foreign-float-array amps)))

(defmethod make-table ((obj usr-harm-table))
  (UsrHarmTable_MakeTable (valid-ptr obj)))

(defmethod destroy ((obj usr-harm-table))
  (if (object-ptr obj)
      (delete_UsrHarmTable (object-pointer obj))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass unary-operator (sndobj)
  ((input :initform nil :initarg :input)
   (sig-inputs :initform '(input))))

(defclass binary-operator (sndobj)
  ((input1 :initform nil :initarg :input1)
   (input2 :initform nil :initarg :input2)
   (sig-inputs :initform '(input1 input2))))

;; acos~

(defclass acos~ (unary-operator)
  ())

(defmethod init-pointer ((obj acos~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Acos (valid-ptr input) vecsize (float sr))))))
  
(defmethod acos~ ((in sndobj))
  (make-instance 'acos~ :input in))

(defmethod do-process ((obj acos~))
  (if (object-ptr obj)
      (Acos_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj buzz))
  (if (object-ptr obj)
      (delete_Acos (object-pointer obj))))

;; asin~

(defclass asin~ (unary-operator)
  ())

(defmethod init-pointer ((obj asin~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Asin (valid-ptr input) vecsize (float sr))))))

(defmethod asin~ ((in sndobj))
  (make-instance 'asin~ :input in))

(defmethod do-process ((obj asin~))
  (if (object-ptr obj)
      (Asin_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj asin~))
  (if (object-ptr obj)
      (delete_Asin (object-pointer obj))))



;; atan~

(defclass atan~ (unary-operator)
  ())

(defmethod init-pointer ((obj atan~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Atan (valid-ptr input) vecsize (float sr))))))

(defmethod atan~ ((in sndobj))
  (make-instance 'atan~ :input in))

(defmethod do-process ((obj atan~))
  (if (object-ptr obj)
      (Atan_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj atan~))
  (if (object-ptr obj)
      (delete_Atan (object-pointer obj))))

;; atan2~

(defclass atan2~ (binary-operator)
  ())

(defmethod init-pointer ((obj atan2~))
  (with-slots (input1 input2  vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Atan2 (valid-ptr input1) (valid-ptr input2) vecsize (float sr))))))

(defmethod atan2~ ((in1 sndobj) (in2 sndobj))
  (make-instance 'atan2~ :input1 in1 :input2 in2))


(defmethod do-process ((obj atan2~))
  (if (object-ptr obj)
      (Atan2_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj atan2~))
  (if (object-ptr obj)
      (delete_Atan2 (object-pointer obj))))

;; cos~

(defclass cos~ (unary-operator)
  ())

(defmethod init-pointer ((obj cos~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Cos (valid-ptr input) vecsize (float sr))))))

(defmethod cos~ ((in sndobj))
  (make-instance 'cos~ :input in))

(defmethod do-process ((obj cos~))
  (if (object-ptr obj)
      (Cos_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj cos~))
  (if (object-ptr obj)
      (delete_Cos (object-pointer obj))))

;; sin~

(defclass sin~ (unary-operator)
  ())

(defmethod init-pointer ((obj sin~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Sin (valid-ptr input) vecsize (float sr))))))

(defmethod sin~ ((in sndobj))
  (make-instance 'sin~ :input in))

(defmethod do-process ((obj sin~))
  (if (object-ptr obj)
      (Sin_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sin~))
  (if (object-ptr obj)
      (delete_Sin (object-pointer obj))))


;; tan~

(defclass tan~ (unary-operator)
  ())

(defmethod init-pointer ((obj tan~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Tan (valid-ptr input) vecsize (float sr))))))

(defmethod tan~ ((in sndobj))
  (make-instance 'tan~ :input in))


(defmethod do-process ((obj tan~))
  (if (object-ptr obj)
      (Tan_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj tan~))
  (if (object-ptr obj)
      (delete_Tan (object-pointer obj))))

;; acosh~

(defclass acosh~ (unary-operator)
  ())

(defmethod init-pointer ((obj acosh~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Acosh (valid-ptr input) vecsize (float sr))))))

(defmethod acosh~ ((in sndobj))
  (make-instance 'acosh~ :input in))

(defmethod do-process ((obj acosh~))
  (if (object-ptr obj)
      (Acosh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj acosh~))
  (if (object-ptr obj)
      (delete_Acosh (object-pointer obj))))

;; asinh~

(defclass asinh~ (unary-operator)
  ())

(defmethod init-pointer ((obj asinh~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Asinh (valid-ptr input) vecsize (float sr))))))

(defmethod asinh~ ((in sndobj))
  (make-instance 'asinh~ :input in))


(defmethod do-process ((obj asinh~))
  (if (object-ptr obj)
      (Asinh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj asinh~))
  (if (object-ptr obj)
      (delete_Asinh (object-pointer obj))))


;; atanh~

(defclass atanh~ (unary-operator)
  ())

(defmethod init-pointer ((obj atanh~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Atanh (valid-ptr input) vecsize (float sr))))))

(defmethod atanh~ ((in sndobj))
  (make-instance 'atanh~ :input in))

(defmethod do-process ((obj atanh~))
  (if (object-ptr obj)
      (Atanh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj atanh~))
  (if (object-ptr obj)
      (delete_Atanh (object-pointer obj))))


;; cosh~

(defclass cosh~ (unary-operator)
  ())

(defmethod init-pointer ((obj cosh~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Cosh (valid-ptr input) vecsize (float sr))))))

(defmethod cosh~ ((in sndobj))
  (make-instance 'cosh~ :input in))

(defmethod do-process ((obj cosh~))
  (if (object-ptr obj)
      (Cosh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj cosh~))
  (if (object-ptr obj)
      (delete_Cosh (object-pointer obj))))


;; sinh~

(defclass sinh~ (unary-operator)
  ())

(defmethod init-pointer ((obj sinh~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Sinh (valid-ptr input) vecsize (float sr))))))

(defmethod sinh~ ((in sndobj))
  (make-instance 'sinh~ :input in))

(defmethod do-process ((obj sinh~))
  (if (object-ptr obj)
      (Sinh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sinh~))
  (if (object-ptr obj)
      (delete_Sinh (object-pointer obj))))


;; tanh~

(defclass tanh~ (unary-operator)
  ())

(defmethod init-pointer ((obj tanh~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Tanh (valid-ptr input) vecsize (float sr))))))

(defmethod tanh~ ((in sndobj))
  (make-instance 'tanh~ :input in))

(defmethod do-process ((obj tanh~))
  (if (object-ptr obj)
      (Tanh_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj tanh~))
  (if (object-ptr obj)
      (delete_Tanh (object-pointer obj))))


;; exp~

(defclass exp~ (unary-operator)
  ())

(defmethod init-pointer ((obj exp~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Exp (valid-ptr input) vecsize (float sr))))))

(defmethod exp~ ((in sndobj))
  (make-instance 'exp~ :input in))

(defmethod do-process ((obj exp~))
  (if (object-ptr obj)
      (Exp_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj exp~))
  (if (object-ptr obj)
      (delete_Exp (object-pointer obj))))


;; exp2~

(defclass exp2~ (unary-operator)
  ())

(defmethod init-pointer ((obj exp2~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Exp2 (valid-ptr input) vecsize (float sr))))))

(defmethod exp2~ ((in sndobj))
  (make-instance 'exp2~ :input in))

(defmethod do-process ((obj exp2~))
  (if (object-ptr obj)
      (Exp2_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj exp2~))
  (if (object-ptr obj)
      (delete_Exp2 (object-pointer obj))))


;; expm1~

(defclass expm1~ (unary-operator)
  ())

(defmethod init-pointer ((obj expm1~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Expm1 (valid-ptr input) vecsize (float sr))))))

(defmethod expm1~ ((in sndobj))
  (make-instance 'expm1~ :input in))

(defmethod do-process ((obj expm1~))
  (if (object-ptr obj)
      (Expm1_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj expm1~))
  (if (object-ptr obj)
      (delete_Expm1 (object-pointer obj))))

;; log~

(defclass log~ (unary-operator)
  ())

(defmethod init-pointer ((obj log~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Log (valid-ptr input) vecsize (float sr))))))

(defmethod log~ ((in sndobj))
  (make-instance 'log~ :input in))

(defmethod do-process ((obj log~))
  (if (object-ptr obj)
      (Log_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj log~))
  (if (object-ptr obj)
      (delete_Log (object-pointer obj))))

;; log10~

(defclass log10~ (unary-operator)
  ())

(defmethod init-pointer ((obj log10~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Log10 (valid-ptr input) vecsize (float sr))))))

(defmethod log10~ ((in sndobj))
  (make-instance 'log10~ :input in))

(defmethod do-process ((obj log10~))
  (if (object-ptr obj)
      (Log10_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj log10~))
  (if (object-ptr obj)
      (delete_Log10 (object-pointer obj))))

;; log2~

(defclass log2~ (unary-operator)
  ())

(defmethod init-pointer ((obj log2~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Log2 (valid-ptr input) vecsize (float sr))))))

(defmethod log2~ ((in sndobj))
  (make-instance 'log2~ :input in))

(defmethod do-process ((obj log2~))
  (if (object-ptr obj)
      (Log2_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj log2~))
  (if (object-ptr obj)
      (delete_Log2 (object-pointer obj))))

;; log1p~

(defclass log1p~ (unary-operator)
  ())

(defmethod init-pointer ((obj log1p~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Log1p (valid-ptr input) vecsize (float sr))))))

(defmethod log1p~ ((in sndobj))
  (make-instance 'log1p~ :input in))

(defmethod do-process ((obj log1p~))
  (if (object-ptr obj)
      (Log1p_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj log1p~))
  (if (object-ptr obj)
      (delete_Log1p (object-pointer obj))))

;; logb~

(defclass logb~ (unary-operator)
  ())

(defmethod init-pointer ((obj logb~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Logb (valid-ptr input) vecsize (float sr))))))

(defmethod logb~ ((in sndobj))
  (make-instance 'logb~ :input in))

(defmethod do-process ((obj logb~))
  (if (object-ptr obj)
      (Logb_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj logb~))
  (if (object-ptr obj)
      (delete_Logb (object-pointer obj))))


;; fabs~

(defclass fabs~ (unary-operator)
  ())

(defmethod init-pointer ((obj fabs~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Fabs (valid-ptr input) vecsize (float sr))))))

(defmethod fabs~ ((in sndobj))
  (make-instance 'fabs~ :input in))

(defmethod do-process ((obj fabs~))
  (if (object-ptr obj)
      (Fabs_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fabs~))
  (if (object-ptr obj)
      (delete_Fabs (object-pointer obj))))



;; cbrt~

(defclass cbrt~ (unary-operator)
  ())

(defmethod init-pointer ((obj cbrt~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Cbrt (valid-ptr input) vecsize (float sr))))))

(defmethod cbrt~ ((in sndobj))
  (make-instance 'cbrt~ :input in))

(defmethod do-process ((obj cbrt~))
  (if (object-ptr obj)
      (Cbrt_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj cbrt~))
  (if (object-ptr obj)
      (delete_Cbrt (object-pointer obj))))



;; hypot~

(defclass hypot~ (binary-operator)
  ())

(defmethod init-pointer ((obj hypot~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Hypotf (valid-ptr input1) input2 vecsize (float sr))
		(new_Hypot (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod hypot~ ((in1 sndobj) in2)
  (make-instance 'hypot~ :input1 in1 :input2 in2))

(defmethod do-process ((obj hypot~))
  (if (object-ptr obj)
      (Hypot_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj hypot~))
  (if (object-ptr obj)
      (delete_Hypot (object-pointer obj))))


;; pow~

(defclass pow~ (binary-operator)
  ())

(defmethod init-pointer ((obj pow~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Powf (valid-ptr input1) input2 vecsize (float sr))
		(new_Pow (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod pow~ ((in1 sndobj) in2)
  (make-instance 'pow~ :input1 in1 :input2 in2))

(defmethod do-process ((obj pow~))
  (if (object-ptr obj)
      (Pow_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pow~))
  (if (object-ptr obj)
      (delete_Pow (object-pointer obj))))


;; sqrt~

(defclass sqrt~ (unary-operator)
  ())

(defmethod init-pointer ((obj sqrt~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Sqrt (valid-ptr input) vecsize (float sr))))))

(defmethod sqrt~ ((in sndobj))
  (make-instance 'sqrt~ :input in))

(defmethod do-process ((obj sqrt~))
  (if (object-ptr obj)
      (Sqrt_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj sqrt~))
  (if (object-ptr obj)
      (delete_Sqrt (object-pointer obj))))


;; ceil~

(defclass ceil~ (unary-operator)
  ())

(defmethod init-pointer ((obj ceil~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Ceil (valid-ptr input) vecsize (float sr))))))

(defmethod ceil~ ((in sndobj))
  (make-instance 'ceil~ :input in))

(defmethod do-process ((obj ceil~))
  (if (object-ptr obj)
      (Ceil_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj ceil~))
  (if (object-ptr obj)
      (delete_Ceil (object-pointer obj))))

;; floor~

(defclass floor~ (unary-operator)
  ())

(defmethod init-pointer ((obj floor~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Floor (valid-ptr input) vecsize (float sr))))))

(defmethod floor~ ((in sndobj))
  (make-instance 'floor~ :input in))

(defmethod do-process ((obj floor~))
  (if (object-ptr obj)
      (Floor_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj floor~))
  (if (object-ptr obj)
      (delete_Floor (object-pointer obj))))


;; fdim~

(defclass fdim~ (unary-operator)
  ())

(defmethod init-pointer ((obj fdim~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Fdim (valid-ptr input) vecsize (float sr))))))

(defmethod fdim~ ((in sndobj))
  (make-instance 'fdim~ :input in))

(defmethod do-process ((obj fdim~))
  (if (object-ptr obj)
      (Fdim_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fdim~))
  (if (object-ptr obj)
      (delete_Fdim (object-pointer obj))))


;; fmax~

(defclass fmax~ (binary-operator)
  ())

(defmethod init-pointer ((obj fmax~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Fmaxf (valid-ptr input1) input2 vecsize (float sr))
		(new_Fmax (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod fmax~ ((in1 sndobj) in2)
  (make-instance 'fmax~ :input1 in1 :input2 in2))

(defmethod do-process ((obj fmax~))
  (if (object-ptr obj)
      (Fmax_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fmax~))
  (if (object-ptr obj)
      (delete_Fmax (object-pointer obj))))


;; fmin~

(defclass fmin~ (binary-operator)
  ())

(defmethod init-pointer ((obj fmin~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Fminf (valid-ptr input1) input2 vecsize (float sr))
		(new_Fmin (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod fmin~ ((in1 sndobj) in2)
  (make-instance 'fmin~ :input1 in1 :input2 in2))

(defmethod do-process ((obj fmin~))
  (if (object-ptr obj)
      (Fmin_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj fmin~))
  (if (object-ptr obj)
      (delete_Fmin (object-pointer obj))))


;; mulitply~

(defclass multiply~ (binary-operator)
  ())

(defmethod init-pointer ((obj multiply~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj)
	    (if (numberp input2)
		(new_Multiplyf (valid-ptr input1) (float input2) vecsize (float sr))
		(new_Multiply (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod *~ ((in1 sndobj) in2)
  (make-instance 'multiply~ :input1 in1 :input2 in2))

(defmethod do-process ((obj multiply~))
  (if (object-ptr obj)
      (Multiply_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj multiply~))
  (if (object-ptr obj)
      (delete_Multiply (object-pointer obj))))

;; divide~

(defclass divide~ (binary-operator)
  ())

(defmethod init-pointer ((obj divide~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Dividef (valid-ptr input1) input2 vecsize (float sr))
		(new_Divide (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod /~ ((in1 sndobj) in2)
  (make-instance 'divide~ :input1 in1 :input2 in2))

(defmethod do-process ((obj divide~))
  (if (object-ptr obj)
      (Divide_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj divide~))
  (if (object-ptr obj)
      (delete_Divide (object-pointer obj))))


;; add~

(defclass add~ (binary-operator)
  ())

(defmethod init-pointer ((obj add~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Addf (valid-ptr input1) input2 vecsize (float sr))
		(new_Add (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod +~ ((in1 sndobj) in2)
  (make-instance 'add~ :input1 in1 :input2 in2))

(defmethod do-process ((obj add~))
  (if (object-ptr obj)
      (Add_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj add~))
  (if (object-ptr obj)
      (delete_Add (object-pointer obj))))


;; subtract~

(defclass subtract~ (binary-operator)
  ())

(defmethod init-pointer ((obj subtract~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Subtractf (valid-ptr input1) input2 vecsize (float sr))
		(new_Subtract (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod -~ ((in1 sndobj) in2)
  (make-instance 'subtract~ :input1 in1 :input2 in2))

(defmethod do-process ((obj subtract~))
  (if (object-ptr obj)
      (Subtract_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj subtract~))
  (if (object-ptr obj)
      (delete_Subtract (object-pointer obj))))


;; greaterthan~

(defclass greaterthan~ (binary-operator)
  ())

(defmethod init-pointer ((obj greaterthan~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Greaterthanf (valid-ptr input1) input2 vecsize (float sr))
		(new_Greaterthan (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod >~ ((in1 sndobj) in2)
  (make-instance 'greaterthan~ :input1 in1 :input2 in2))

(defmethod do-process ((obj greaterthan~))
  (if (object-ptr obj)
      (Greaterthan_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj greaterthan~))
  (if (object-ptr obj)
      (delete_Greaterthan (object-pointer obj))))

;; greaterthanequal~

(defclass greaterthanequal~ (binary-operator)
  ())

(defmethod init-pointer ((obj greaterthanequal~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Greaterthanequalf (valid-ptr input1) input2 vecsize (float sr))
		(new_Greaterthanequal (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod >=~ ((in1 sndobj) in2)
  (make-instance 'greaterthanequal~ :input1 in1 :input2 in2))

(defmethod do-process ((obj greaterthanequal~))
  (if (object-ptr obj)
      (GreaterthanEqual_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj greaterthanequal~))
  (if (object-ptr obj)
      (delete_GreaterthanEqual (object-pointer obj))))



;; lessthan~

(defclass lessthan~ (binary-operator)
  ())

(defmethod init-pointer ((obj lessthan~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Lessthanf (valid-ptr input1) input2 vecsize (float sr))
		(new_Lessthan (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod <~ ((in1 sndobj) in2)
  (make-instance 'lessthan~ :input1 in1 :input2 in2))


;; lessthanequal~

(defclass lessthanequal~ (binary-operator)
  ())

(defmethod init-pointer ((obj lessthanequal~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Lessthanequalf (valid-ptr input1) input2 vecsize (float sr))
		(new_Lessthanequal (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))


(defmethod <=~ ((in1 sndobj) in2)
  (make-instance 'lessthanequal~ :input1 in1 :input2 in2))

(defmethod do-process ((obj lessthanequal~))
  (if (object-ptr obj)
      (Lessthanequal_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj lessthanequal~))
  (if (object-ptr obj)
      (delete_Lessthanequal (object-pointer obj))))



;; and~

(defclass and~ (binary-operator)
  ())

(defmethod init-pointer ((obj and~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Andf (valid-ptr input1) input2 vecsize (float sr))
		(new_And (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod and~ ((in1 sndobj) in2)
  (make-instance 'and~ :input1 in1 :input2 in2))

(defmethod do-process ((obj and~))
  (if (object-ptr obj)
      (And_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj and~))
  (if (object-ptr obj)
      (delete_And (object-pointer obj))))


;; or~

(defclass or~ (binary-operator)
  ())

(defmethod init-pointer ((obj or~))
  (with-slots (input1 input2 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (if (floatp input2)
		(new_Orf (valid-ptr input1) input2 vecsize (float sr))
		(new_Or (valid-ptr input1) (valid-ptr input2) vecsize (float sr)))))))

(defmethod or~ ((in1 sndobj) in2)
  (make-instance 'or~ :input1 in1 :input2 in2))

(defmethod do-process ((obj or~))
  (if (object-ptr obj)
      (Or_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj or~))
  (if (object-ptr obj)
      (delete_Or (object-pointer obj))))


;; not~

(defclass not~ (unary-operator)
  ())

(defmethod init-pointer ((obj not~))
  (with-slots (input vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_Not (valid-ptr input) vecsize (float sr))))))

(defmethod not~ ((in sndobj))
  (make-instance 'not~ :input in))

(defmethod do-process ((obj not~))
  (if (object-ptr obj)
      (Not_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj not~))
  (if (object-ptr obj)
      (delete_Not (object-pointer obj))))


(defclass ternary-operator (sndobj)
  ((input1 :initform nil :initarg :input1)
   (input2 :initform nil :initarg :input2)
   (input3 :initform nil :initarg :input3)))


;;  if~

(defclass if~ (ternary-operator)
  ())


(defmethod init-pointer ((obj if~))
  (with-slots (input1 input2 input3 vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (cond ((not (and (floatp input1) (floatp input2) (floatp input3)))
		   (new_If (valid-ptr input1) (valid-ptr input2) (valid-ptr input3) vecsize (float sr)))
		  ((and (not (and (floatp input1) (floatp input3))) (floatp input2))
		   (new_Iffs (valid-ptr input1) input2 (valid-ptr input3) vecsize (float sr)))
		  ((and (not (and (floatp input1) (floatp input2))) (floatp input3))
		   (new_Iffs (valid-ptr input1) (valid-ptr input2) input3 vecsize (float sr)))
		  ((and (not (floatp input2)) (floatp input2) (floatp input3))
		   (new_Ifff (valid-ptr input1) input2 input3 vecsize (float sr))))))))


(defmethod if~ ((in1 sndobj) in2 in3)
  (make-instance 'if~ :input1 in1 :input2 in2 :input3 in3))

(defmethod do-process ((obj if~))
  (if (object-ptr obj)
      (If_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj if~))
  (if (object-ptr obj)
      (delete_If (object-pointer obj))))




;; rtoutput~


(defclass rtoutput (sndobj)
  ((input :initform nil :initarg :input)
   (chan :initform nil :initarg :chan)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj rtoutput))
  (with-slots (input chan vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_RTOutput chan (valid-ptr input) vecsize (float sr))))))
  
(defmethod set-channel ((obj rtoutput) chan)
  (RTOutput_SetChan (valid-ptr obj) chan))


(defmethod do-process ((obj rtoutput))
  (if (object-ptr obj)
      (RTOutput_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj rtoutput))
  (if (object-ptr obj)
      (delete_RTOutput (object-pointer obj))))

;;
;; BusWrite
;;

(defclass bus-write (sndobj)
  ((input :initform nil :initarg :input)
   (bus :initform nil :initarg :bus)
   (sig-inputs :initform '(input))))

(defmethod init-pointer ((obj bus-write))
  (with-slots (input bus vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_BusWrite  bus (valid-ptr input) vecsize (float sr))))))
  
(defmethod set-channel ((obj bus-write) bus)
  (BusWrite_SetBus (valid-ptr obj) bus))


(defmethod do-process ((obj bus-write))
  (if (object-ptr obj)
      (BusWrite_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj bus-write))
  (if (object-ptr obj)
      (delete_BusWrite (object-pointer obj))))


;;
;; BusRead
;;

(defclass bus-read (sndobj)
  ((bus :initform nil :initarg :bus)))


(defmethod init-pointer ((obj bus-read))
  (with-slots (input bus vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_BusRead bus vecsize (float sr))))))
  
(defmethod set-channel ((obj bus-read) bus)
  (BusRead_SetBus (valid-ptr obj) bus))


(defmethod do-process ((obj bus-read))
  (if (object-ptr obj)
      (BusRead_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj bus-read))
  (if (object-ptr obj)
      (delete_BusRead (object-pointer obj))))

;;;PVOCEXRead

(defclass pvocexread (sndobj)
  ((file :initform nil :initarg :file)))

(defmethod init-pointer ((obj pvocexread))
  (with-slots (file vecsize sr) obj
    (unless (object-pointer obj)
      (setf (object-pointer obj) 
	    (new_PVOCEXRead file vecsize (float sr))))))


(defmethod do-process ((obj pvocexread))
  (if (object-ptr obj)
      (PVOCEXRead_DoProcess (object-pointer obj))
      (error "intance ~s does not have a valid object pointer" obj)))

(defmethod destroy ((obj pvocexread))
  (if (object-ptr obj)
      (delete_PVOCEXRead (object-pointer obj))))

   
   
;;;;

(defun out~ (&rest rest)
  (let ((gens '()))
    (labels ((depth-first-collect (lr)
	     (dolist (gen lr)
	       (unless (floatp gen)
		 (pushnew gen gens)
		 (dolist (input (slot-value gen 'sig-inputs))
		   (if (slot-value gen input) 
		       (progn
			 (if (not (slot-value gen 'visited-p))
			     (setf (slot-value gen 'visited-p) t))
			 (depth-first-collect (list (slot-value gen input))))))))))
      (depth-first-collect rest)
    gens)))

#|
(defmacro new (obj &rest rest)
  `(make-instance ',obj ,@rest))
|#

(defun make-env (env &key duration (scaler 1.0) (offset 0.0) (type 0.0))
  (let ((segments (1- (floor (/ (length env) 2))))
	(points (list ))
	(lengths (list ))
	(max-val)
	(osc))
    (setf max-val (loop for y in (cdr env) by #'cddr 
		     maximize y))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr with lastlen = 0.0
       do
	 (if (not (= x 0.0))
	     (push (- x lastlen) lengths))
	 (setf lastlen  x)
	 (push (/ (+ offset (* scaler y)) max-val)  points))
    
    (setf env (make-instance 'env-table :segments segments :start (float (first (reverse points)))
			     :points (foreign-float-array (rest (reverse points)))
			     :lengths (foreign-float-array (reverse lengths))
			     :type type :length 1024))
    (setf osc (make-instance 'fast-osc :freq (/ 1.0 duration)
			     :amp (+ offset (* scaler max-val))  :table env))
    osc))

#|
(setf g (make-env '(0.0 0.0 .25 1.0 .3 1.0 1.0 0.0) :duration .3 :scaler sa::*max-scale*))

(table g 0)
|#
(defun sym->keyword (sym)
  (let ((str (symbol-name sym)))
    (or (find-symbol str ':keyword)
        (intern str :keyword))))

(defclass synth ()
  ((object-pointer :initform nil :accessor object-pointer)
   (id :initform nil)
   (duration :initform -1 :initarg :duration)
   (name :initform nil)
   (init-func :initform nil)
   (inputs-list :initform '())
   (sndobjs :initform '())))

(defmethod init-synth ((obj synth))
  ())

(defmethod initialize-instance :after ((obj synth) &rest initargs)
  (declare (ignore slot-names initargs))
  (funcall #'init-synth obj))

(defmethod destroy-synth ((obj synth))
  (if (object-ptr obj)
      (progn
	(Synth_Free (slot-value obj 'object-pointer))
	t)
      nil))

(defmacro make-synth-function (llist &body body)
  `(function (lambda ,llist
     (progn
       duration
       ,@body))))
       

(defmacro defsynth (name arg-list &body body)
  (let ((slot-list '())
	(inputs-list '()))
    (dolist (i arg-list)
      (if (listp i)
	  (progn
	     (push (list (first i)
		      :initarg (sym->keyword (first i)) 
		      :initform (second i)) slot-list)
	     (push  (first i) inputs-list)))
       (if (atom i)
	   (progn
	     (push (list i :initarg (sym->keyword i)) slot-list)
	     (push i inputs-list))))
    `(progn 
       (defclass ,name (synth)
	 (,@slot-list 
	  (init-func :initform #'(lambda (,@inputs-list) 
				   ,(find-symbol "DURATION")
				   ,@body ))
	  (inputs-list :initform ',inputs-list)))
       (defmethod init-synth ((obj ,name))
	 (with-slots ,inputs-list obj
	   (setf (slot-value obj 'sndobjs) 
		 (apply (slot-value obj 'init-func) (list ,@inputs-list)))
	   (if (> (length (slot-value obj 'sndobjs)) 0)
	       (progn
		 (let ((gens (cffi:foreign-alloc ':pointer :count (length (slot-value obj 'sndobjs)))))
		   (loop for gen in (slot-value obj 'sndobjs) for i from 0
		      do
			(setf (cffi:mem-aref gens :pointer i) (slot-value gen 'object-pointer)))
		   (setf (slot-value obj 'object-pointer) 
			 (new_Synth (length (slot-value obj 'sndobjs))
				    gens
				    (if (> ,(find-symbol "DURATION") 0)
					(floor (* ,(find-symbol "DURATION") *sample-rate*))
					,(find-symbol "DURATION"))
				    (cffi::null-pointer)
				    *vector-size* *sample-rate*))))))))))



(defmethod map-sndobjs ((obj synth) (func function))
  (dolist (ob (slot-value obj 'sndobjs))
    (apply func ob)))
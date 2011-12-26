(in-package #:cm)

(use-system :sa :symbols t)

(defparameter *sa* (sa-open :busses 2))
;; this starts audio thread. input and outputs can be defined here
;; and soon devices

(use-system :rts)
;; make sure rts is loaded

(rts *sa*)
;;start up rts with sa stream as default output

;; this will import symbols into cm. if this causes conflicts
;; you may want to forego this step and just prepend sa: to all the 
;; relevant symbols. 

(defsynth sine-osc ((freq 400.0) (amp .1) (duration 1))
  (let* ((wave-tab (new harm-table :length 1024 :harm 1 :type :sine))
	 (osc (new fast-osc :freq freq :amp amp :table wave-tab)))
    (out~ (new rtoutput :input osc :chan 0)
	  (new rtoutput :input osc :chan 1))))


(sprout (new sine-osc :freq 500.0 :amp .5 :duration 1.0))
;; you should hear a sine wave 
#|
breakdown of syntax

all synths are defined by the defsynth macro. the format is

defsynth <name> <arguments>

the arguments are all slots in the newly defined synths and 
one can assign default values to them. in the case of
sine-osc all arguments have default values.

one special note - the duration argument must be in the argument
list for the synth to have a duration, otherwise it will 
continuously be processing until explicitedly deleted. more on 
this below.

next we define the basics of the synth
a wave-table of size 1024 with a wavetype of sine and
1 harmonic. this is read by a fast-osc sndobj.

the last form in the macro needs to be out~ . this 
indicates the final node of the processing chain. in this
are two rtoutputs. the :input keyword arg determine 
what is the sndobj directly hooked to this output and what
channel, starting with 0, to output on.

|#

;; 
;; here is a more flexible version of the above
;; in which one can alter wave type and number of
;; harmonics (note: there are other types of table
;; generators) also the number of harmonics seems
;; to limit the number of harmonics which go into 
;; contructing the "target" waveform. e.g. a
;; square wave with one harmonic is a sine wave.


(defsynth osc-synth ((freq 400.0) (amp .1) (duration 1) (type :sine) (harm 1))
  (let* ((wave-tab (new harm-table :length 1024 :harm harm :type type))
	 (osc (new fast-osc :freq freq :amp amp :table wave-tab)))
    (out~ (new rtoutput :input osc :chan 0)
	  (new rtoutput :input osc :chan 1))))


(sprout (new osc-synth :freq 500.0 :amp .5 :duration 1.0))
(sprout (new osc-synth :freq 500.0 :amp .5 :duration 1.0 :type :square :harm 10))
(sprout (new osc-synth :freq 500.0 :amp .5 :duration 1.0 :type :square :harm 40))
(sprout (new osc-synth :freq 500.0 :amp .5 :duration 1.0 :type :buzz :harm 10))
(sprout (new osc-synth :freq 500.0 :amp .5 :duration 1.0 :type :saw :harm 7))


;; below is an example using rts create
;; realtime events



(defun tt1 (num)
  (process repeat num for i from 1
	   output (new osc-synth :freq (hertz (pickl '(60 62 58 65 67 56))) :amp .2 :type :square :harm i)
	   wait .5))

(sprout (tt1 20))

;; we get clicks when the synth is started and stopped
;; so let's add an amplitude envelope to correct this.
;; there are several ways to do this, but here is one
;; using a traditional adsr model.

(defsynth osc-adsr-synth ((freq 400.0) (attack .1) (decay .1) 
			 (sustain .1) (release .1) (amp .1) (duration 1.0)
			 (type :sine) (harm 1))
  (let* ((wave-tab (new harm-table :length 1024 :harm harm :type type))
	 (osc (new fast-osc :freq freq :amp 1.0 :table wave-tab))
	 (adsr-env (new adsr :att attack :max-amp amp :dec decay 
			:sus sustain :rel release :dur duration ))
	 (output nil))
    (setf output (*~ adsr-env osc))
    (out~ (new sa::rtoutput :input output :chan 0)
	  (new sa::rtoutput :input output :chan 1))))

;; notice the use of *~ to multiply two sndobjs
;; (and right now it is only binary not n-ary)
;;

(sprout (new osc-adsr-synth :freq 500.0 :amp .5 :sustain .5 :duration 3.0))
(sprout (new osc-adsr-synth :freq 500.0 :amp .5 :duration 3.0 :attack .01 :sustain .03 :decay 1.0 :release .3))
(sprout (new osc-adsr-synth :freq 500.0 :amp .5 :duration 3.0 :attack .01 :sustain .03 :decay 1.0 :release .3
	     :type :saw :harm 20))


(defun tt2 (num)
  (process repeat num 
	   output (new osc-adsr-synth :freq (hertz (pickl '(60 62 58 65 67 56))) 
		       :amp .2 :attack .3 :decay .5 :release .1 :type :saw :harm 5)
	   wait .5))

(sprout (tt2 20))
;; I still hear some faint clicks - need to check into


(defsynth fm (car-freq ratio mod-index (amp .5) (duration 1.0) (channel 0))
  (let* ((wave-tab (new harm-table :length 1024 :harm 1 :type :sine))
	 (mod (new oscili :table wave-tab :freq (* car-freq ratio) :amp mod-index))
	 (car (new oscili :table wave-tab :freq car-freq :freq-mod mod :amp amp)))
    (sa::out~ (new rtoutput :input car :chan channel))))

(sprout (new fm :car-freq 500.0 :amp .5 :ratio 1.7 :mod-index 100.0 :duration 3.0))
(sprout (new fm :car-freq 500.0 :amp .5 :ratio 3.1 :mod-index 166.0 :duration 3.0 :channel 1))

(defun tt3 (num)
  (process repeat num 
	   output (new fm :car-freq (hertz (pickl '(60 62 58 65 67 56))) :amp .2 
		       :mod-index (between 20.0 200.0) :ratio (between .4 4.5)
		       :channel (odds .3 0 1))
	   wait .5))

(sprout (tt3 20))


;;
;; note the use of lower sample rate for the adsr
;;


(defsynth fm-adsr (car-freq ratio mod-index (attack .1) (decay .1) 
			    (sustain-offset -.1) (release .5) (amp .5) (duration 1.0))
  (let* ((wave-tab (new harm-table :length 1024 :harm 1 :type :sine))
	 (mod (new oscili :table wave-tab :freq (* car-freq ratio) :amp mod-index))
	 (car (new oscili :table wave-tab :freq car-freq :freq-mod mod :amp 1.0))
	 (adsr-env (new adsr :att attack :max-amp amp :dec decay :sus (+ amp sustain-offset) :rel release :dur duration 
			:vecsize 64 :sr (/ 44100.0 16.0)))
	 (output nil))
    (setf output (*~ car adsr-env ))
    (out~ (new rtoutput :input output :chan 0)
	  (new rtoutput :input output :chan 1))))

(sprout (new fm-adsr :car-freq 500.0 :amp .2 :ratio 1.7 :mod-index 100.0 :duration 3.0))

(defun tt4 (num)
  (process repeat num 
	   output (new fm-adsr :car-freq (hertz (pickl '(60 62 58 65 67 56))) :amp .15 
		       :mod-index (between 20.0 200.0) :ratio (between .4 4.5) :duration 3.0)
	   wait .5))

(sprout (tt4 20))


;; another examples this time using pluck

(defsynth plucky (freq amp (fdbgain .9) duration)
  (let ((pl (new pluck :freq freq :amp amp :fdbgain fdbgain)))
    (out~ (new rtoutput :input pl :chan 0)
	  (new rtoutput :input pl :chan 1))))

(sprout (new plucky :freq 400.0 :amp .5 :duration .5 :fdbgain .9))


(defun tt5 (num)
  (process repeat num 
	   output (new plucky :freq (hertz (pickl '(60 62 58 65 67 56))) :amp (between .4 .7)
		       :duration 2 :fdbgain .9)
	   wait .1))

(sprout (tt5 20))

(defsynth plucky-vib (freq amp duration (vib-rate 2.0) (vib-ratio .01))
  (let* ((wave-tab (new harm-table :length 1024 :harm 1 :type :sine))
	 (osc (new fast-osc :freq vib-rate :amp (* freq vib-ratio)  :table wave-tab ))
	 (pl (new pluck :freq freq :amp amp :freq-mod osc :decay 20.0)))
    (out~ (new rtoutput :input pl :chan 0)
	  (new rtoutput :input pl :chan 1))))

(sprout (new plucky-vib :freq 400.0 :amp .5 :duration 1))

(defun tt6 (num)
  (process repeat num 
	   output (new plucky-vib :freq (hertz (pickl '(60 62 58 65 67 56))) :amp (between .4 .7)
		       :duration 2 :vib-rate (between 2.0 8.0))
	   wait (pickl '(.3 .3 .6))))

(sprout (tt6 20))


(defsynth reson-noise (amp freq bw duration)
  (let* ((ns (new rand :amp amp))
	 (rs (new bp-reson :freq freq :bw bw :input ns)))
    (sa::out~ (new rtoutput :input rs
	       :chan 0)
	      (new rtoutput :input rs
	       :chan 1))))

(sprout (new reson-noise :amp .4 :freq 600.0 :bw 20.0 :duration 1))

(defun tt7 (num)
  (process repeat num 
	   output (new reson-noise :freq (* 2 (hertz (pickl '(60 62 58 65 67 56)))) :amp (between .6 .9)
		       :duration 2 :bw 100.0)
	   wait .3))

(sprout (tt7 20))


;; note the use of make-env below. this will 
;; use a user-defined normalized breakpoint envelope.
;; users of clm with find this very familiar



(defsynth reson-noise-slide (amp freq bw duration 
				 (fmod '(0.0 900.0 .5 400.0 .7 600.0 .8 500.0 .9 800.0 1.0 0.0)))
  (let* ((freq-env (make-env fmod :duration duration))
	 (ns (new rand :amp amp))
	 (rs (new bp-reson :freq freq :bw bw :input ns :freq-mod freq-env)))
    (sa::out~ (new rtoutput :input rs :chan 0)
	      (new rtoutput :input rs :chan 1))))

(sprout (new reson-noise-slide :amp .8 :freq 440.0 :bw 20.0 :duration 7))

(defun tt7 (num)
  (process repeat num 
	   output (new reson-noise-slide :freq (* 2 (hertz (pickl '(60 62 58 65 67 56)))) :amp (between .6 .9)
		       :duration 2 :bw 100.0 :fmod (list 0.0 (* 2 (hertz (pickl '(60 62 58 65 67 56)))) 1.0 (* 2 (hertz (pickl '(60 62 58 65 67 56))))))
	   wait 1))

(sprout (tt7 20))


;;outputing to busses

;;
;; The following give a simple example 
;; of using busses to route audio to 
;; affect the signal

(defsynth sine-osc ((freq 400) (amp .1) (duration 1))
  (let* ((wave-tab (new harm-table :length 1024 :harm 1 :type :sine))
	 (osc (new fast-osc :freq freq :amp amp :table wave-tab)))
    (out~ (new bus-write :input osc :bus 0)
	  (new bus-write :input osc :bus 1))))

;; in sine-osc and noise the usual rtoutput has been 
;; replace with bus-write.

(defsynth noise ((amp .4) (duration 1))
  (out~ (new bus-write :input (new rand :amp amp) :bus 0)))

;; here comby is using bus-read to read
;; from a bus. 

(defsynth comby ((duration 20))
  (let* ((in (new bus-read :bus 0))
	 (comb (new comb :gain .8 :delaytime .1 :input in)))
    (out~ (new rtoutput :input comb :chan 0)
	  (new rtoutput :input comb :chan 1))))

;;start effect

;; i am going to assign the instance of comby
;; so i can delete it later

(defparameter *combfilt* (new comby)) 
(sprout *combfilt*)

(sprout (new sine-osc :freq 500.0 :amp .3 :duration .3))
(sprout (new noise :amp .4))


;; maybe you like comby so much you 
;; want it to last forever. then use 
;; no duration argument in defining comby or make the 
;; duration less than 0

(defparameter *combfilt* (new comby :duration -1)) 
(sprout *combfilt*)

(sprout (new sine-osc :freq 500.0 :amp .3 :duration .3))
(sprout (new noise :amp .4))

(sprout (new sine-osc :freq 400.0 :amp .3 :duration .3))

(sprout (new noise :amp .4))




;; bye, comby.

(destroy-synth *combfilt*)


;;
;; non-realtime break. 
;; an example of using sndobjs in 
;; non-realtime. this could be easier
;; also amplitude envelop is scaled to 
;; sa::*max-scale* because currently
;; this is not normalized

(defun make-fm-file (filename dur car-freq ratio mod-index)
  (let* ((wave-tab (new harm-table :length 1024 :harm 1 :type :sine))
	 (mod (new oscili :table wave-tab :freq (* car-freq ratio) :amp mod-index))
	 (car (new oscili :table wave-tab :freq car-freq :freq-mod mod :amp 1.0))
	 (env (make-env '(0.0 0.0 .25 1.0 .5 0.2 1.0 0.0) :duration dur :scaler sa::*max-scale*))

	 (input  (*~ car env ) )
	 (output (new snd-aiff  :file filename :channels 1 :mode :overwrite)))
    (sa::set-output output 1 input)
    (loop for i from 0 below (* sa::*sample-rate* dur) by sa::*vector-size*
       do
	 (do-process mod)
	 (do-process car)
	 (do-process env)
	 (do-process input)
	 (write-file output))
    (destroy output)))
	 

(make-fm-file "fm1.aif" 3.0 200 .26 778)
(play "fm1.aif")
(make-fm-file "fm2.aif" .3 200 4.5 289)
(play "fm2.aif")

;;;;using soundfiles
;; now that we have some sounds files, we can look at one
;; method of playing them using snd-read. 
;; files can be played through at different rate
;; and amp can be scaled/

(defsynth playsnd (file pitch amp duration)
  (let ((snd (new snd-read :file file :pitch pitch :scale amp)))
    (out~ (new rtoutput :chan 0 :input snd))))

(sprout (new playsnd :file "fm1.aif"
	     :pitch .44 :amp 1.0 :duration 7.0))

(sprout (new playsnd :file "fm1.aif"
	     :pitch 1.44 :amp 1.0 :duration 2.0))

(sprout (new playsnd :file "fm2.aif"
	     :pitch .8 :amp 1.0 :duration 1.0))

(sprout (new playsnd :file "fm2.aif"
	     :pitch 1.2 :amp 1.0 :duration 1.0))

(defun tt8 (num)
  (process repeat num
	   with scaler = 1.0
	   with cents-pat = (new heap :of '(100 400 600 700 1100 -200 -500 -800))
	   
	   set scaler = (cents->scaler (next cents-pat))
	   output (new playsnd :file "fm2.aif"
		       :pitch scaler :amp .5 :duration (* .33 (/ 1.0 scaler)))
	   wait (pickl '(.1 .2 .3))))

(sprout (tt8 20))

(defsynth pvfile (file (pitch 1.0) (amp 1.0) (duration 3))
  (let* ((win (new hamming-table :length 1024 :alpha .5))
	 (file-read (new snd-read :file file :scale amp))
	 (pv (new pva :input file-read :window win))
	 (pvt (new pv-transp :input pv :pitch pitch))
	 (syn (new pvs :window win :input pvt)))
    (out~ (new rtoutput :input syn :chan 0))))

(sprout (new pvfile :file "fm2.aif" :duration 3.0 :pitch 1.0))

(defun tt9 (num)
  (process repeat num
	   with scaler = 1.0
	   with cents-pat = (new heap :of '(100 400 600 700 1100 -200 -500 -800))
	   
	   set scaler = (cents->scaler (next cents-pat))
	   output (new pvfile :file "fm2.aif"
		       :pitch scaler :duration .3)

	   wait (pickl '(.1 .2 .3))))

(sprout (tt9 20))



;;; create pv analysis file 

(defun pvocex-create (aifinfile pvocexfile)
  (let* ((file (new snd-aiff :file aifinfile :mode :read)) ; only to get length info
	 (len (get-data-frames file))
	 (win (new hamming-table :length 1024 :alpha .5))
	 (file-read (new snd-read :file aifinfile :scale .5))
	 (pv (new pva :input file-read :window win))
	 (output (new snd-pvocex :file pvocexfile :channels 1 :mode :overwrite)))
    (set-output output 1 pv)
    (loop for i from 0 below len by sa::*vector-size*
       do
	 (do-process file-read)
	 (do-process pv)
	 (write-file output))
    (destroy output)))


(pvocex-create "fm1.aif" "fm1.pvx")
(pvocex-create "fm2.aif" "fm2.pvx")


;;
;; 
;;	 

(defsynth play-pv (file timescale duration)
  (let* ((pvr (new pvread :file file :timescale timescale)))
    (out~ (new rtoutput :input pvr :chan 0))))



(sprout (new play-pv :file "fm2.pvx" :duration 3 :timescale 1.0))

;; pvocexread will read a pvocex file 
;; for resynthesis. right now the rate
;; can't be scaled. this should be available
;; shortly
;; this also demonstrates using pv-mix

(defsynth play-pvmix (file1 file2 (duration 5))
  (let* ((win (new hamming-table :length 1024 :alpha .5))
	(pvf1 (new pvocexread :file file1))
	(pvf2 (new pvocexread :file file2))
	(pvm (new pv-mix :input1 pvf1 :input2 pvf2))
	(pv (new pvs :input pvm :window win)))
    (out~ (new rtoutput :chan 0 :input pv))))

(sprout (new play-pvmix :file1 "fm2.pvx" :file2 "fm1.pvx" :duration 3))


;; this will transpose and do time blurring
;; of spectral data. don't make the blurring
;; below .1 - there seems to be an error.
;;

(defsynth play-pvblur (file (pitch 1.0) (blur .2) duration)
  (let* ((win (new hamming-table :length 1024 :alpha .5))
	 (pvf (new pvocexread :file file))
	 (pvt (new pv-transp :input pvf :pitch pitch))
	 (pvb (new pv-blur :input pvt :blurtime blur))
	 (pv (new pvs :input pvb :window win)))
    (out~ (new rtoutput :input pv :chan 0))))

(sprout (new play-pvblur :file "fm1.pvx" :pitch .4 :blur .1 :duration 5))

(sprout (new play-pvblur :file "fm1.pvx" :pitch .4 :blur 1.4 :duration 5))

(sprout (new play-pvblur :file "fm1.pvx" :pitch 1.4 :blur 1.4 :duration 5))

(sprout (new play-pvblur :file "fm1.pvx" :pitch 1.4 :blur .1 :duration 5))


;;
;; sound playback with envelop
;;

(defsynth playr (file (pitch 1.0) (amp 1.0) (env '(0.0 1.0 1.0 1.0))  duration)
  (let* ((env (make-env env :duration duration))
	 (snd (new snd-read :file file :pitch pitch :scale amp))
	 (output (*~ snd env)))
    (out~ (new rtoutput :input output :chan 0)
	  (new rtoutput :input output :chan 1))))


(sprout (new playr :file "fm1.aif" :pitch 1.0 :amp 1.0 :duration 8))

(sprout (new playr :file "fm1.aif" :pitch .5 :amp 1.0 :duration 8))

(sprout (new playr :file "fm1.aif" :pitch .5 :env '(0.0 0.0 .5 1.0 1.0 1.0) :amp .5 :duration 8))



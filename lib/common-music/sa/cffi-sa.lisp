(in-package :cl-user)

(defvar *libsa*
  (let ((type #+(or darwin macos macosx) "dylib"
              #+(or linux linux-target (and unix pc386) freebsd) "so"
              #+(or win32 microsoft-32 cygwin) "dll")
        (paths (list (truename *load-pathname*) "/usr/local/lib"
		     "/usr/lib/")))
    (loop for d in paths
       for p = (make-pathname :name "libsa" :type type :defaults d)
       when (probe-file p) do (return p)
       finally  
         (error "Library \"libsa\".~A\" not found. Fix cl-user::*libsa*."
                type))))

(cffi:load-foreign-library *libsa*)

(defpackage :sa
  (:use :common-lisp :cffi)
  (:export :adsr :iadsr :balance :buzz :convol :delay-line :comb :all-pass :fir :pitch-transpose
	   :sndloop :string-flt :pluck :tap :tapi :vdelay :fast-osc :osc :osci :fft :pva
	   :if-gram :bp-filter :hipass :lopass :lp-reson :bp-reson :tptz :ap :butt-hp :butt-br :butt-hp
	   :butt-lp :gain :hilb :ifft :pvs :pvread :interp-line :table-lookup :table-lookupi :oscil :oscili
	   :phoscili :oscilt :pan :phase :rand :randh :randi :ring-mod :set-input1 :set-input2
	   :sin-anal :sin-syn :re-syn :ad-syn :if-add :snd-in :snd-read :spec-in :spec-mult 
	   :pv-blur :pv-mix :pv-transp :spec-cart :spec-combine :spec-interp :pv-morph 
	   :pv-mask :pv-filter :spec-polar :spec-split :spec-thresh :spec-voc :sync-grain
	   :snd-io :snd-buffer :snd-fio :snd-wave :snd-wavex :snd-pvocex :snd-sin-io :snd-aiff
	   :env-table :hamming-table :harm-table :impulse-table :lopass-table :note-table 
	   :pln-table :pv-env-table :spec-env-table :pv-table :triseg-table :snd-table 
	   :usr-def-table :usr-harm-table :acos~ :asin~ :atan~ :atan2 :cos~ :sin~ :tan~ 
	   :acosh~ :asinh~ :atanh~ :cash~ :sinh~ :tanh~ :exp~ :exp2~ :expm1~ :log~ :log10~
	   :log2~ :log1p~ :logb :fabs~ :cbrt~ :hypot~ :pow~ :sqrt~ :ceil~ :floor~ :fdim~
	   :fmax~ :fmin~ :*~ :/~ :+~ :-~ :>~ :>=~ :<~ :<=~ :and~ :or~ :not~ :if~ :rtoutput
	   :bus-write :bus-read :pvocexread :out~ :make-env :synt :defsynth :map-sndobjs
	   :set-sr :set-max-amp :sustain :release :set-adsr :set-parameter :do-process :destroy :set-init 
	   :set-end :set-inputs :set-lp-freq :connect :set-amp :set-harm :set-impulse :set-delaytime 
	   :get-delaytime :reset-delay-line :set-gain :set-semitones :set-xfade :set-pitch :re-sample :set-decay 
	   :set-freq :set-fdbgain :re-pluck :set-delay-tap :set-delay-input :set-max-delaytime 
	   :set-vdtime-mode :set-fdbgain-mod :set-fwdgain-mod :set-dirgain-mod :set-phase 
	   :get-fft-size :get-hop-size :set-window :set-fft-size :get-phases :set-bw :set-coeffs 
	   :set-r :set-gain-multiplier :get-real :get-imgaginary :get-outchannel :restart :set-interp-curve 
	   :set-dur :set-mode :set-offset :set-table :set-pan :get-left :get-right :get-track-id :get-tracks 
	   :set-threshold :set-if-gram :set-max-tracks :set-scale :set-timescale :set-input1 :set-input2 
	   :set-blurtime :set-hop-size :set-phase-input :set-mag-input :set-interp :set-freq-morph 
	   :set-amp-morph :set-mask :set-mask-table :set-mask-gain :set-filter-spec :set-filter-table 
	   :set-amount :get-magnitude :get-phase :set-wavetable :set-envtable :set-grain-size :set-pointer-rate 
	   :get-channels :get-size :get-output :read-file :write-file :get-file :get-mode :set-position :eof? 
	   :get-data-frames :get-position :get-status :get-header :wave? :wave-extensible? :wavex? :get-window-type 
	   :get-window-length :pvocex? :aiff? :set-env-envelope :make-table :set-freq-interval :set-note-interval 
	   :set-pvenv-envelope :set-pv-table :set-curve :set-curve-points :set-snd-input :set-usr-table :get-length
	   :get-table :lookup-index :destroy-synth :set-output))
  
  
(in-package #:sa)

(defctype :sndobj :pointer)
(defctype :sndobj_sndobj :pointer)
(defctype :sndobj_sndio :pointer)
(defctype :sndobj_table :pointer)
(defctype :sndobj_oscil :pointer)
(defctype :sndobj_oscilt :pointer)
(defctype :sndobj_oscili :pointer)
(defctype :sndobj_phoscili :pointer)
(defctype :sndobj_fastosc :pointer)
(defctype :sndobj_osc :pointer)
(defctype :sndobj_osci :pointer)
(defctype :sndobj_sndin :pointer)
(defctype :sndobj_sndread :pointer)
(defctype :sndobj_adsr :pointer)
(defctype :sndobj_iadsr :pointer)
(defctype :sndobj_buzz :pointer)
(defctype :sndobj_balance :pointer)
(defctype :sndobj_delayline :pointer)
(defctype :sndobj_tap :pointer)
(defctype :sndobj_tapi :pointer)
(defctype :sndobj_comb :pointer)
(defctype :sndobj_allpass :pointer)
(defctype :sndobj_stringflt :pointer)
(defctype :sndobj_pluck :pointer)
(defctype :sndobj_vdelay :pointer)
(defctype :sndobj_pitch :pointer)
(defctype :sndobj_loop :pointer)
(defctype :sndobj_fir :pointer)
(defctype :sndobj_filter :pointer)
(defctype :sndobj_tptz :pointer)
(defctype :sndobj_reson :pointer)
(defctype :sndobj_lp :pointer)
(defctype :sndobj_buttbp :pointer)
(defctype :sndobj_buttbr :pointer)
(defctype :sndobj_butthp :pointer)
(defctype :sndobj_buttlp :pointer)
(defctype :sndobj_ap :pointer)
(defctype :sndobj_lopass :pointer)
(defctype :sndobj_hipass :pointer)
(defctype :sndobj_hilb :pointer)
(defctype :sndobj_syncgrain :pointer)
(defctype :sndobj_mixer :pointer)
(defctype :sndobj_pan :pointer)
(defctype :sndobj_gain :pointer)
(defctype :sndobj_interp :pointer)
(defctype :sndobj_phase :pointer)
(defctype :sndobj_ring :pointer)
(defctype :sndobj_unit :pointer)
(defctype :sndobj_lookup :pointer)
(defctype :sndobj_lookupi :pointer)
(defctype :sndobj_rand :pointer)
(defctype :sndobj_randh :pointer)
(defctype :sndobj_randi :pointer)
(defctype :sndobj_fft :pointer)
(defctype :sndobj_ifft :pointer)
(defctype :sndobj_pva :pointer)
(defctype :sndobj_pvs :pointer)
(defctype :sndobj_pvread :pointer)
(defctype :sndobj_ifgram :pointer)
(defctype :sndobj_sinanal :pointer)
(defctype :sndobj_sinsyn :pointer)
(defctype :sndobj_resyn :pointer)
(defctype :sndobj_adsyn :pointer)
(defctype :sndobj_ifadd :pointer)
(defctype :sndobj_specmult :pointer)
(defctype :sndobj_specinterp :pointer)
(defctype :sndobj_pvmask :pointer)
(defctype :sndobj_pvtransp :pointer)
(defctype :sndobj_pvmix :pointer)
(defctype :sndobj_pvblur :pointer)
(defctype :sndobj_pvfilter :pointer)
(defctype :sndobj_pvmorph :pointer)
(defctype :sndobj_specpolar :pointer)
(defctype :sndobj_specsplit :pointer)
(defctype :sndobj_specthresh :pointer)
(defctype :sndobj_specvoc :pointer)
(defctype :sndobj_speccart :pointer)
(defctype :sndobj_speccombine :pointer)
(defctype :sndobj_specin :pointer)
(defctype :sndobj_convol :pointer)
(defctype :sndobj_sndfio :pointer)
(defctype :sndobj_sndwave :pointer)
(defctype :sndobj_sndwavex :pointer)
(defctype :sndobj_sndpvocex :pointer)
(defctype :sndobj_sndsinio :pointer)
(defctype :sndobj_sndaiff :pointer)
(defctype :sndobj_sndbuffer :pointer)
(defctype :sndobj_sndcoreaudio :pointer)
(defctype :sndobj_harmtable :pointer)
(defctype :sndobj_usrharmtable :pointer)
(defctype :sndobj_trisegtable :pointer)
(defctype :sndobj_envtable :pointer)
(defctype :sndobj_sndtable :pointer)
(defctype :sndobj_plntable :pointer)
(defctype :sndobj_hammingtable :pointer)
(defctype :sndobj_notetable :pointer)
(defctype :sndobj_usrdeftable :pointer)
(defctype :sndobj_lopasstable :pointer)
(defctype :sndobj_pvenvtable :pointer)
(defctype :sndobj_specenvtable :pointer)
(defctype :sndobj_pvtable :pointer)
(defctype :sndobj_impulsetable :pointer)
(defctype :sa_busread :pointer)
(defctype :sa_buswrite :pointer)
(defctype :sa_floatsig :pointer)
(defctype :sa_acos :pointer)
(defctype :sa_asin :pointer)
(defctype :sa_Atan :pointer)
(defctype :sa_Atan2 :pointer)
(defctype :sa_Cos :pointer)
(defctype :sa_Sin :pointer)
(defctype :sa_Tan :pointer)
(defctype :sa_Acosh :pointer)
(defctype :sa_Asinh :pointer)
(defctype :sa_Atanh :pointer)
(defctype :sa_Cosh :pointer)
(defctype :sa_Sinh :pointer)
(defctype :sa_Tanh :pointer)
(defctype :sa_Exp :pointer)
(defctype :sa_Exp2 :pointer)
(defctype :sa_Expm1 :pointer)
(defctype :sa_Log :pointer)
(defctype :sa_Log10 :pointer)
(defctype :sa_Log2 :pointer)
(defctype :sa_Log1p :pointer)
(defctype :sa_Logb :pointer)
(defctype :sa_Fabs :pointer)
(defctype :sa_Cbrt :pointer)
(defctype :sa_Hypot :pointer)
(defctype :sa_Pow :pointer)
(defctype :sa_Sqrt :pointer)
(defctype :sa_Ceil :pointer)
(defctype :sa_Floor :pointer)
(defctype :sa_Fdim :pointer)
(defctype :sa_Fmax :pointer)
(defctype :sa_Fmin :pointer)
(defctype :sa_Multiply :pointer)
(defctype :sa_Divide :pointer)
(defctype :sa_Add :pointer)
(defctype :sa_Subtract :pointer)
(defctype :sa_GreaterThan :pointer)
(defctype :sa_GreaterThanEqual :pointer)
(defctype :sa_LessThan :pointer)
(defctype :sa_LessThanEqual :pointer)
(defctype :sa_And :pointer)
(defctype :sa_Or :pointer)
(defctype :sa_Not :pointer)
(defctype :sa_If :pointer)
(defctype :sa_PVOCEXRead :pointer)
(defctype :sa_Synth :pointer)
(defctype :sa_RTAudioStream :pointer)
(defctype :sa_RTOutput :pointer)


(defctype :float* :pointer)
(defctype :char* :pointer)
(defctype :double* :pointer)


(defparameter DSP_STOPPED 0)
(defparameter DSP_RUNNING 2)
(defparameter DSP_PAUSED 3)

(defcstruct msg_link
	(msg :pointer)
	(ID :int)
	(previous :pointer))

(defcfun ("_wrap_SndObj_IsProcessing" SndObj_IsProcessing) :boolean
  (self :sndobj))

(defcfun ("_wrap_SndObj_GetError" SndObj_GetError) :int
  (self :sndobj))

(defcfun ("_wrap_SndObj_SndObjEqualSndObj" SndObj_SndObjEqualSndObj) :sndobj
  (self :sndobj)
  (obj :sndobj))

(defcfun ("_wrap_SndObj_SndObjSumAssignSndObj" SndObj_SndObjSumAssignSndObj) :pointer
  (self :sndobj)
  (obj :pointer))

(defcfun ("_wrap_SndObj_SndObjSubtractAssignSndObj" SndObj_SndObjSubtractAssignSndObj) :pointer
  (self :sndobj)
  (obj :pointer))

(defcfun ("_wrap_SndObj_SndObjMultiplyAssignSndObj" SndObj_SndObjMultiplyAssignSndObj) :pointer
  (self :sndobj)
  (obj :pointer))

(defcfun ("_wrap_SndObj_SndObjSumAssignFloat" SndObj_SndObjSumAssignFloat) :pointer
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjSubtractAssignFloat" SndObj_SndObjSubtractAssignFloat) :pointer
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjMultiplyAssignFloat" SndObj_SndObjMultiplyAssignFloat) :pointer
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjSumSndObj" SndObj_SndObjSumSndObj) :sndobj
  (self :sndobj)
  (obj :pointer))

(defcfun ("_wrap_SndObj_SndObjSubtractSndObj" SndObj_SndObjSubtractSndObj) :sndobj
  (self :sndobj)
  (obj :pointer))

(defcfun ("_wrap_SndObj_SndObjMultiplySndObj" SndObj_SndObjMultiplySndObj) :sndobj
  (self :sndobj)
  (obj :pointer))

(defcfun ("_wrap_SndObj_SndObjSumFloat" SndObj_SndObjSumFloat) :sndobj
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjSubtractFloat" SndObj_SndObjSubtractFloat) :sndobj
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjMultiplyFloat" SndObj_SndObjMultiplyFloat) :sndobj
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjShiftLeftFloat" SndObj_SndObjShiftLeftFloat) :void
  (self :sndobj)
  (val :float))

(defcfun ("_wrap_SndObj_SndObjShiftLeftFloatVector" SndObj_SndObjShiftLeftFloatVector) :void
  (self :sndobj)
  (vector :float*))

(defcfun ("_wrap_SndObj_SndObjShiftRightSndIO" SndObj_SndObjShiftRightSndIO) :void
  (self :sndobj)
  (out :pointer))

(defcfun ("_wrap_SndObj_SndObjShiftLeftSndIO" SndObj_SndObjShiftLeftSndIO) :void
  (self :sndobj)
  (in :pointer))

(defcfun ("_wrap_SndObj_PushIn" SndObj_PushIn) :int
  (self :sndobj)
  (vector :float*)
  (size :int))

(defcfun ("_wrap_SndObj_PopOut" SndObj_PopOut) :int
  (self :sndobj)
  (vector :float*)
  (size :int))

(defcfun ("_wrap_SndObj_AddOut" SndObj_AddOut) :int
  (self :sndobj)
  (vector :float*)
  (size :int))

(defcfun ("_wrap_SndObj_GetMsgList" SndObj_GetMsgList) :void
  (self :sndobj)
  (list :pointer))

(defcfun ("_wrap_SndObj_Enable" SndObj_Enable) :void
  (self :sndobj))

(defcfun ("_wrap_SndObj_Disable" SndObj_Disable) :void
  (self :sndobj))

(defcfun ("_wrap_SndObj_Output" SndObj_Output) :float
  (self :sndobj)
  (pos :int))

(defcfun ("_wrap_SndObj_GetVectorSize" SndObj_GetVectorSize) :int
  (self :sndobj))

(defcfun ("_wrap_SndObj_SetVectorSize" SndObj_SetVectorSize) :void
  (self :sndobj)
  (vecsize :int))

(defcfun ("_wrap_SndObj_GetSr" SndObj_GetSr) :float
  (self :sndobj))

(defcfun ("_wrap_SndObj_SetSr" SndObj_SetSr) :void
  (self :sndobj)
  (sr :float))

(defcfun ("_wrap_SndObj_Set" SndObj_Set) :int
  (self :sndobj)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SndObj_Connect" SndObj_Connect) :int
  (self :sndobj)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SndObj_SetInput" SndObj_SetInput) :void
  (self :sndobj)
  (input :sndobj))

(defcfun ("_wrap_SndObj_GetInput" SndObj_GetInput) :sndobj
  (self :sndobj))

(defcfun ("_wrap_new_SndObj" new_SndObj) :sndobj
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_SndObj_empty" new_SndObj_empty) :sndobj)

(defcfun ("_wrap_delete_SndObj" delete_SndObj) :void
  (self :sndobj))

(defcfun ("_wrap_SndObj_ErrorMessage" SndObj_ErrorMessage) :string
  (self :sndobj))

(defcstruct _24Bit
	(s :pointer))

(defcfun ("_wrap_SndIO_m_sampsize_set" SndIO_m_sampsize_set) :void
  (self :sndobj_sndio)
  (m_sampsize :short))

(defcfun ("_wrap_SndIO_m_sampsize_get" SndIO_m_sampsize_get) :short
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_GetSr" SndIO_GetSr) :float
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_GetVectorSize" SndIO_GetVectorSize) :int
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_GetChannels" SndIO_GetChannels) :short
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_GetSize" SndIO_GetSize) :short
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_Output" SndIO_Output) :float
  (self :sndobj_sndio)
  (pos :int)
  (channel :int))

(defcfun ("_wrap_SndIO_SetOutput" SndIO_SetOutput) :short
  (self :sndobj_sndio)
  (channel :short)
  (input :sndobj))

(defcfun ("_wrap_new_SndIO" new_SndIO) :sndobj_sndio
  (channels :short)
  (bits :short)
  (inputlist :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndIO" delete_SndIO) :void
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_Read" SndIO_Read) :short
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_Write" SndIO_Write) :short
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_ErrorMessage" SndIO_ErrorMessage) :string
  (self :sndobj_sndio))

(defcfun ("_wrap_SndIO_Error" SndIO_Error) :int
  (self :sndobj_sndio))

(defcfun ("_wrap_Table_GetLen" Table_GetLen) :long
  (self :sndobj_table))

(defcfun ("_wrap_Table_GetTable" Table_GetTable) :pointer
  (self :sndobj_table))

(defcfun ("_wrap_Table_Lookup" Table_Lookup) :float
  (self :sndobj_table)
  (pos :int))

(defcfun ("_wrap_delete_Table" delete_Table) :void
  (self :sndobj_table))

(defcfun ("_wrap_Table_ErrorMessage" Table_ErrorMessage) :string
  (self :sndobj_table))

(defcfun ("_wrap_Table_MakeTable" Table_MakeTable) :short
  (self :sndobj_table))

(defcfun ("_wrap_Oscil_SetSr" Oscil_SetSr) :void
  (self :sndobj_oscil)
  (sr :float))

(defcfun ("_wrap_Oscil_m_factor_set" Oscil_m_factor_set) :void
  (self :sndobj_oscil)
  (m_factor :float))

(defcfun ("_wrap_Oscil_m_factor_get" Oscil_m_factor_get) :float
  (self :sndobj_oscil))

(defcfun ("_wrap_new_Oscil_empty" new_Oscil_empty) :sndobj_oscil)

(defcfun ("_wrap_new_Oscil" new_Oscil) :sndobj_oscil
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (inputfreq :sndobj)
  (inputamp :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Oscil" delete_Oscil) :void
  (self :sndobj_oscil))

(defcfun ("_wrap_Oscil_SetPhase" Oscil_SetPhase) :short
  (self :sndobj_oscil)
  (phase :float))

(defcfun ("_wrap_Oscil_SetTable" Oscil_SetTable) :void
  (self :sndobj_oscil)
  (table :sndobj_table))

(defcfun ("_wrap_Oscil_SetFreq" Oscil_SetFreq) :void
  (self :sndobj_oscil)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_Oscil_SetAmp" Oscil_SetAmp) :void
  (self :sndobj_oscil)
  (amp :float)
  (InAmpObj :sndobj))

(defcfun ("_wrap_Oscil_SetFreqSndObj" Oscil_SetFreqSndObj) :void
  (self :sndobj_oscil)
  (inputfr :sndobj))

(defcfun ("_wrap_Oscil_SetAmpSndObj" Oscil_SetAmpSndObj) :void
  (self :sndobj_oscil)
  (inputamp :sndobj))

(defcfun ("_wrap_Oscil_Connect" Oscil_Connect) :int
  (self :sndobj_oscil)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Oscil_Set" Oscil_Set) :int
  (self :sndobj_oscil)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Oscilt_empty" new_Oscilt_empty) :sndobj_oscilt)

(defcfun ("_wrap_new_Oscilt" new_Oscilt) :sndobj_oscilt
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (inputfreq :sndobj)
  (inputamp :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Oscilt" delete_Oscilt) :void
  (self :sndobj_oscilt))

(defcfun ("_wrap_new_Oscili_empty" new_Oscili_empty) :sndobj_oscili)

(defcfun ("_wrap_new_Oscili" new_Oscili) :sndobj_oscili
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (inputfreq :sndobj)
  (inputamp :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Oscili" delete_Oscili) :void
  (self :sndobj_oscili))

(defcfun ("_wrap_new_FastOsc_empty" new_FastOsc_empty) :sndobj_fastosc)

(defcfun ("_wrap_new_FastOsc" new_FastOsc) :sndobj_fastosc
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_FastOsc" delete_FastOsc) :void
  (self :sndobj_fastosc))

(defcfun ("_wrap_FastOsc_SetFreq" FastOsc_SetFreq) :void
  (self :sndobj_fastosc)
  (fr :float))

(defcfun ("_wrap_FastOsc_SetAmp" FastOsc_SetAmp) :void
  (self :sndobj_fastosc)
  (amp :float))

(defcfun ("_wrap_FastOsc_SetPhase" FastOsc_SetPhase) :void
  (self :sndobj_fastosc)
  (phase :float))

(defcfun ("_wrap_FastOsc_SetTable" FastOsc_SetTable) :void
  (self :sndobj_fastosc)
  (table :sndobj_table))

(defcfun ("_wrap_FastOsc_Set" FastOsc_Set) :int
  (self :sndobj_fastosc)
  (mess :string)
  (value :float))

(defcfun ("_wrap_FastOsc_Connect" FastOsc_Connect) :int
  (self :sndobj_fastosc)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_FastOsc_SetSr" FastOsc_SetSr) :void
  (self :sndobj_fastosc)
  (sr :float))

(defcfun ("_wrap_new_Osc_empty" new_Osc_empty) :sndobj_osc)

(defcfun ("_wrap_new_Osc" new_Osc) :sndobj_osc
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (inputfr :sndobj)
  (inputamp :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Osc" delete_Osc) :void
  (self :sndobj_osc))

(defcfun ("_wrap_Osc_SetFreq" Osc_SetFreq) :void
  (self :sndobj_osc)
  (inputfr :sndobj))

(defcfun ("_wrap_Osc_SetAmp" Osc_SetAmp) :void
  (self :sndobj_osc)
  (inputamp :sndobj))

(defcfun ("_wrap_Osc_Connect" Osc_Connect) :int
  (self :sndobj_osc)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Osci_empty" new_Osci_empty) :sndobj_osci)

(defcfun ("_wrap_new_Osci" new_Osci) :sndobj_osci
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (inputfr :sndobj)
  (inputamp :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Osci" delete_Osci) :void
  (self :sndobj_osci))

(defcfun ("_wrap_Osci_SetTable" Osci_SetTable) :void
  (self :sndobj_osci)
  (table :sndobj_table))

(defcfun ("_wrap_Osci_Connect" Osci_Connect) :int
  (self :sndobj_osci)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_SndIn_empty" new_SndIn_empty) :sndobj_sndin)

(defcfun ("_wrap_new_SndIn" new_SndIn) :sndobj_sndin
  (input :sndobj_sndio)
  (channel :short)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndIn" delete_SndIn) :void
  (self :sndobj_sndin))

(defcfun ("_wrap_SndIn_SetInput" SndIn_SetInput) :void
  (self :sndobj_sndin)
  (input :sndobj_sndio)
  (channel :short))

(defcfun ("_wrap_SndIn_Connect" SndIn_Connect) :int
  (self :sndobj_sndin)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SndIn_Set" SndIn_Set) :int
  (self :sndobj_sndin)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SndIn_ErrorMessage" SndIn_ErrorMessage) :string
  (self :sndobj_sndin))

(defcfun ("_wrap_SndRead_Outchannel" SndRead_Outchannel) :sndobj
  (self :sndobj_sndread)
  (channel :int))

(defcfun ("_wrap_new_SndRead_empty" new_SndRead_empty) :sndobj_sndread)

(defcfun ("_wrap_new_SndRead" new_SndRead) :sndobj_sndread
  (name :string)
  (pitch :float)
  (scale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndRead" delete_SndRead) :void
  (self :sndobj_sndread))

(defcfun ("_wrap_SndRead_SetInput" SndRead_SetInput) :void
  (self :sndobj_sndread)
  (name :string))

(defcfun ("_wrap_SndRead_SetScale" SndRead_SetScale) :void
  (self :sndobj_sndread)
  (scale :float))

(defcfun ("_wrap_SndRead_SetPitch" SndRead_SetPitch) :void
  (self :sndobj_sndread)
  (pitch :float))

(defcfun ("_wrap_SndRead_Set" SndRead_Set) :int
  (self :sndobj_sndread)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_ADSR_empty" new_ADSR_empty) :sndobj_adsr)

(defcfun ("_wrap_new_ADSR" new_ADSR) :sndobj_adsr
  (att :float)
  (maxamp :float)
  (dec :float)
  (sus :float)
  (rel :float)
  (dur :float)
  (InObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_ADSR" delete_ADSR) :void
  (self :sndobj_adsr))

(defcfun ("_wrap_ADSR_SetSr" ADSR_SetSr) :void
  (self :sndobj_adsr)
  (sr :float))

(defcfun ("_wrap_ADSR_SetMaxAmp" ADSR_SetMaxAmp) :void
  (self :sndobj_adsr)
  (maxamp :float))

(defcfun ("_wrap_ADSR_Sustain" ADSR_Sustain) :void
  (self :sndobj_adsr))

(defcfun ("_wrap_ADSR_Release" ADSR_Release) :void
  (self :sndobj_adsr))

(defcfun ("_wrap_ADSR_Restart" ADSR_Restart) :void
  (self :sndobj_adsr))

(defcfun ("_wrap_ADSR_SetADSR" ADSR_SetADSR) :void
  (self :sndobj_adsr)
  (att :float)
  (dec :float)
  (sus :float)
  (rel :float))

(defcfun ("_wrap_ADSR_SetDur" ADSR_SetDur) :void
  (self :sndobj_adsr)
  (dur :float))

(defcfun ("_wrap_ADSR_Set" ADSR_Set) :int
  (self :sndobj_adsr)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_IADSR_empty" new_IADSR_empty) :sndobj_iadsr)

(defcfun ("_wrap_new_IADSR" new_IADSR) :sndobj_iadsr
  (init :float)
  (att :float)
  (maxamp :float)
  (dec :float)
  (sus :float)
  (rel :float)
  (end :float)
  (dur :float)
  (InObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_IADSR" delete_IADSR) :void
  (self :sndobj_iadsr))

(defcfun ("_wrap_IADSR_SetInit" IADSR_SetInit) :void
  (self :sndobj_iadsr)
  (init :float))

(defcfun ("_wrap_IADSR_SetEnd" IADSR_SetEnd) :void
  (self :sndobj_iadsr)
  (end :float))

(defcfun ("_wrap_IADSR_Set" IADSR_Set) :int
  (self :sndobj_iadsr)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Buzz_empty" new_Buzz_empty) :sndobj_buzz)

(defcfun ("_wrap_new_Buzz" new_Buzz) :sndobj_buzz
  (fr :float)
  (amp :float)
  (harms :short)
  (InFrObj :sndobj)
  (InAmpObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Buzz" delete_Buzz) :void
  (self :sndobj_buzz))

(defcfun ("_wrap_Buzz_SetFreq" Buzz_SetFreq) :void
  (self :sndobj_buzz)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_Buzz_SetAmp" Buzz_SetAmp) :void
  (self :sndobj_buzz)
  (amp :float)
  (InAmpObj :sndobj))

(defcfun ("_wrap_Buzz_SetSr" Buzz_SetSr) :void
  (self :sndobj_buzz)
  (sr :float))

(defcfun ("_wrap_Buzz_SetHarm" Buzz_SetHarm) :void
  (self :sndobj_buzz)
  (harm :int))

(defcfun ("_wrap_Buzz_Set" Buzz_Set) :int
  (self :sndobj_buzz)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Buzz_Connect" Buzz_Connect) :int
  (self :sndobj_buzz)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Buzz_ErrorMessage" Buzz_ErrorMessage) :string
  (self :sndobj_buzz))

(defcfun ("_wrap_Balance_SetInput" Balance_SetInput) :void
  (self :sndobj_balance)
  (input1 :sndobj)
  (input2 :sndobj))

(defcfun ("_wrap_Balance_SetLPFreq" Balance_SetLPFreq) :void
  (self :sndobj_balance)
  (fr :float))

(defcfun ("_wrap_Balance_SetSr" Balance_SetSr) :void
  (self :sndobj_balance)
  (sr :float))

(defcfun ("_wrap_Balance_Set" Balance_Set) :int
  (self :sndobj_balance)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Balance_empty" new_Balance_empty) :sndobj_balance)

(defcfun ("_wrap_new_Balance" new_Balance) :sndobj_balance
  (input1 :sndobj)
  (input2 :sndobj)
  (fr :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Balance" delete_Balance) :void
  (self :sndobj_balance))

(defcfun ("_wrap_Balance_ErrorMessage" Balance_ErrorMessage) :string
  (self :sndobj_balance))

(defcfun ("_wrap_Balance_Connect" Balance_Connect) :int
  (self :sndobj_balance)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_DelayLine_empty" new_DelayLine_empty) :sndobj_delayLine)

(defcfun ("_wrap_new_DelayLine" new_DelayLine) :sndobj_delayLine
  (delaytime :float)
  (InObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_DelayLine" delete_DelayLine) :void
  (self :sndobj_delayLine))

(defcfun ("_wrap_DelayLine_Buffer" DelayLine_Buffer) :pointer
  (self :sndobj_delayLine))

(defcfun ("_wrap_DelayLine_GetWritePointerPos" DelayLine_GetWritePointerPos) :long
  (self :sndobj_delayLine))

(defcfun ("_wrap_DelayLine_GetDelayTime" DelayLine_GetDelayTime) :float
  (self :sndobj_delayLine))

(defcfun ("_wrap_DelayLine_SetSr" DelayLine_SetSr) :void
  (self :sndobj_delayLine)
  (sr :float))

(defcfun ("_wrap_DelayLine_Reset" DelayLine_Reset) :void
  (self :sndobj_delayLine))

(defcfun ("_wrap_DelayLine_SetDelayTime" DelayLine_SetDelayTime) :void
  (self :sndobj_delayLine)
  (delaytime :float))

(defcfun ("_wrap_DelayLine_Set" DelayLine_Set) :int
  (self :sndobj_delayLine)
  (mess :string)
  (value :float))

(defcfun ("_wrap_DelayLine_ErrorMessage" DelayLine_ErrorMessage) :string
  (self :sndobj_delayLine))

(defcfun ("_wrap_new_Tap_empty" new_Tap_empty) :sndobj_tap)

(defcfun ("_wrap_new_Tap" new_Tap) :sndobj_tap
  (delaytime :float)
  (DLine :sndobj_delayLine)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Tap" delete_Tap) :void
  (self :sndobj_tap))

(defcfun ("_wrap_Tap_SetDelayTime" Tap_SetDelayTime) :void
  (self :sndobj_tap)
  (delaytime :float))

(defcfun ("_wrap_Tap_SetDelayTap" Tap_SetDelayTap) :void
  (self :sndobj_tap)
  (DLine :sndobj_delayLine))

(defcfun ("_wrap_Tap_Set" Tap_Set) :int
  (self :sndobj_tap)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Tap_Connect" Tap_Connect) :int
  (self :sndobj_tap)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Tap_ErrorMessage" Tap_ErrorMessage) :string
  (self :sndobj_tap))

(defcfun ("_wrap_new_Tapi_empty" new_Tapi_empty) :sndobj_tapi)

(defcfun ("_wrap_new_Tapi" new_Tapi) :sndobj_tapi
  (delayinput :sndobj)
  (DLine :sndobj_delayLine)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Tapi" delete_Tapi) :void
  (self :sndobj_tapi))

(defcfun ("_wrap_Tapi_SetDelayInput" Tapi_SetDelayInput) :void
  (self :sndobj_tapi)
  (delayinput :sndobj))

(defcfun ("_wrap_Tapi_Connect" Tapi_Connect) :int
  (self :sndobj_tapi)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Comb_empty" new_Comb_empty) :sndobj_comb)

(defcfun ("_wrap_new_Comb" new_Comb) :sndobj_comb
  (gain :float)
  (delaytime :float)
  (InObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Comb" delete_Comb) :void
  (self :sndobj_comb))

(defcfun ("_wrap_Comb_SetGain" Comb_SetGain) :void
  (self :sndobj_comb)
  (gain :float))

(defcfun ("_wrap_Comb_Set" Comb_Set) :int
  (self :sndobj_comb)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Allpass_empty" new_Allpass_empty) :sndobj_allpass)

(defcfun ("_wrap_new_Allpass" new_Allpass) :sndobj_allpass
  (gain :float)
  (delaytime :float)
  (InObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Allpass" delete_Allpass) :void
  (self :sndobj_allpass))

(defcfun ("_wrap_new_StringFlt_empty" new_StringFlt_empty) :sndobj_stringflt)

(defcfun ("_wrap_new_StringFlt" new_StringFlt) :sndobj_stringflt
  (fr :float)
  (fdbgain :float)
  (inObj :sndobj)
  (InFrObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_StringFlt_decay" new_StringFlt_decay) :sndobj_stringflt
  (fr :float)
  (inObj :sndobj)
  (decay :float)
  (InFrObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_StringFlt" delete_StringFlt) :void
  (self :sndobj_stringflt))

(defcfun ("_wrap_StringFlt_SetSr" StringFlt_SetSr) :void
  (self :sndobj_stringflt)
  (sr :float))

(defcfun ("_wrap_StringFlt_SetDecay" StringFlt_SetDecay) :void
  (self :sndobj_stringflt)
  (decay :float))

(defcfun ("_wrap_StringFlt_SetFreq" StringFlt_SetFreq) :void
  (self :sndobj_stringflt)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_StringFlt_SetFdbgain" StringFlt_SetFdbgain) :void
  (self :sndobj_stringflt)
  (fdbgain :float))

(defcfun ("_wrap_StringFlt_Set" StringFlt_Set) :int
  (self :sndobj_stringflt)
  (mess :string)
  (value :float))

(defcfun ("_wrap_StringFlt_Connect" StringFlt_Connect) :int
  (self :sndobj_stringflt)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Pluck_empty" new_Pluck_empty) :sndobj_pluck)

(defcfun ("_wrap_new_Pluck" new_Pluck) :sndobj_pluck
  (fr :float)
  (amp :float)
  (fdbgain :float)
  (InFrObj :sndobj)
  (maxscale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Pluck_decay" new_Pluck_decay) :sndobj_pluck
  (fr :float)
  (amp :float)
  (InFrObj :sndobj)
  (decay :float)
  (maxscale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Pluck" delete_Pluck) :void
  (self :sndobj_pluck))

(defcfun ("_wrap_Pluck_RePluck" Pluck_RePluck) :void
  (self :sndobj_pluck))

(defcfun ("_wrap_Pluck_Set" Pluck_Set) :int
  (self :sndobj_pluck)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Pluck_SetAmp" Pluck_SetAmp) :void
  (self :sndobj_pluck)
  (amp :float)
  (maxscale :float))

(defcfun ("_wrap_new_VDelay_empty" new_VDelay_empty) :sndobj_vdelay)

(defcfun ("_wrap_new_VDelay" new_VDelay) :sndobj_vdelay
  (maxdelaytime :float)
  (fdbgain :float)
  (fwdgain :float)
  (dirgain :float)
  (InObj :sndobj)
  (InVdtime :sndobj)
  (InFdbgain :sndobj)
  (InFwdgain :sndobj)
  (InDirgain :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_VDelay_delaytime" new_VDelay_delaytime) :sndobj_vdelay
  (maxdelaytime :float)
  (delaytime :float)
  (fdbgain :float)
  (fwdgain :float)
  (dirgain :float)
  (InObj :sndobj)
  (InVdtime :sndobj)
  (InFdbgain :sndobj)
  (InFwdgain :sndobj)
  (InDirgain :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_VDelay" delete_VDelay) :void
  (self :sndobj_vdelay))

(defcfun ("_wrap_VDelay_Set" VDelay_Set) :int
  (self :sndobj_vdelay)
  (mess :string)
  (value :float))

(defcfun ("_wrap_VDelay_Connect" VDelay_Connect) :int
  (self :sndobj_vdelay)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_VDelay_SetMaxDelayTime" VDelay_SetMaxDelayTime) :void
  (self :sndobj_vdelay)
  (MaxDelaytime :float))

(defcfun ("_wrap_VDelay_SetDelayTime" VDelay_SetDelayTime) :void
  (self :sndobj_vdelay)
  (delaytime :float))

(defcfun ("_wrap_VDelay_SetVdtInput" VDelay_SetVdtInput) :void
  (self :sndobj_vdelay)
  (InVdtime :sndobj))

(defcfun ("_wrap_VDelay_SetFdbgain" VDelay_SetFdbgain) :void
  (self :sndobj_vdelay)
  (fdbgain :float)
  (InFdbgain :sndobj))

(defcfun ("_wrap_VDelay_SetFwdgain" VDelay_SetFwdgain) :void
  (self :sndobj_vdelay)
  (fwdgain :float)
  (InFwdgain :sndobj))

(defcfun ("_wrap_VDelay_SetDirgain" VDelay_SetDirgain) :void
  (self :sndobj_vdelay)
  (dirgain :float)
  (InDirgain :sndobj))

(defcfun ("_wrap_new_Pitch_empty" new_Pitch_empty) :sndobj_pitch)

(defcfun ("_wrap_new_Pitch" new_Pitch) :sndobj_pitch
  (delaytime :float)
  (InObj :sndobj)
  (pitch :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Pitch_semitones" new_Pitch_semitones) :sndobj_pitch
  (delaytime :float)
  (InObj :sndobj)
  (semitones :int)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Pitch" delete_Pitch) :void
  (self :sndobj_pitch))

(defcfun ("_wrap_Pitch_SetPitch" Pitch_SetPitch) :void
  (self :sndobj_pitch)
  (pitch :float))

(defcfun ("_wrap_Pitch_SetPitch_semitones" Pitch_SetPitch_semitones) :void
  (self :sndobj_pitch)
  (semitones :int))

(defcfun ("_wrap_Pitch_Set" Pitch_Set) :int
  (self :sndobj_pitch)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_SndLoop_empty" new_SndLoop_empty) :pointer)

(defcfun ("_wrap_new_SndLoop" new_SndLoop) :pointer
  (xfadetime :float)
  (looptime :float)
  (InObj :sndobj)
  (pitch :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndLoop" delete_SndLoop) :void
  (self :pointer))

(defcfun ("_wrap_SndLoop_SetXFade" SndLoop_SetXFade) :void
  (self :pointer)
  (xfadetime :float))

(defcfun ("_wrap_SndLoop_SetPitch" SndLoop_SetPitch) :void
  (self :pointer)
  (pitch :float))

(defcfun ("_wrap_SndLoop_ReSample" SndLoop_ReSample) :void
  (self :pointer))

(defcfun ("_wrap_SndLoop_Set" SndLoop_Set) :int
  (self :pointer)
  (mess :string)
  (value :float))

(defcfun ("_wrap_FIR_Connect" FIR_Connect) :int
  (self :pointer)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_FIR_Set" FIR_Set) :int
  (self :pointer)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Fir_empty" new_Fir_empty) :pointer)

(defcfun ("_wrap_new_FIR" new_FIR) :pointer
  (coeftable :sndobj_table)
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_FIR_impulse" new_FIR_impulse) :sndobj_fir
  (impulse :float*)
  (impulsesize :int)
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_FIR" delete_FIR) :void
  (self :sndobj_fir))

(defcfun ("_wrap_FIR_SetTable" FIR_SetTable) :void
  (self :sndobj_fir)
  (coeftable :sndobj_table))

(defcfun ("_wrap_FIR_SetImpulse" FIR_SetImpulse) :void
  (self :sndobj_fir)
  (impulse :float*)
  (impulsesize :int))

(defcfun ("_wrap_FIR_SetDelayTime" FIR_SetDelayTime) :void
  (self :sndobj_fir)
  (dtime :float))

(defcfun ("_wrap_new_Filter_empty" new_Filter_empty) :sndobj_filter)

(defcfun ("_wrap_new_Filter" new_Filter) :sndobj_filter
  (fr :float)
  (bw :float)
  (inObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Filter" delete_Filter) :void
  (self :sndobj_filter))

(defcfun ("_wrap_Filter_SetFreq" Filter_SetFreq) :void
  (self :sndobj_filter)
  (fr :float))

(defcfun ("_wrap_Filter_SetBW" Filter_SetBW) :void
  (self :sndobj_filter)
  (bw :float))

(defcfun ("_wrap_Filter_Set" Filter_Set) :int
  (self :sndobj_filter)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Filter_SetSr" Filter_SetSr) :void
  (self :sndobj_filter)
  (sr :float))

(defcfun ("_wrap_Filter_ErrorMessage" Filter_ErrorMessage) :string
  (self :sndobj_filter))

(defcfun ("_wrap_new_TpTz_empty" new_TpTz_empty) :sndobj_tptz)

(defcfun ("_wrap_new_TpTz" new_TpTz) :sndobj_tptz
  (a :double)
  (a1 :double)
  (a2 :double)
  (b1 :double)
  (b2 :double)
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_TpTz" delete_TpTz) :void
  (self :sndobj_tptz))

(defcfun ("_wrap_TpTz_SetParam" TpTz_SetParam) :void
  (self :sndobj_tptz)
  (a :double)
  (a1 :double)
  (a2 :double)
  (b1 :double)
  (b2 :double))

(defcfun ("_wrap_TpTz_Set" TpTz_Set) :int
  (self :sndobj_tptz)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Reson_empty" new_Reson_empty) :sndobj_reson)

(defcfun ("_wrap_new_Reson" new_Reson) :sndobj_reson
  (fr :float)
  (bw :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (inputbw :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_Reson_SetFreq" Reson_SetFreq) :void
  (self :sndobj_reson)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_Reson_SetBW" Reson_SetBW) :void
  (self :sndobj_reson)
  (bw :float)
  (InBWObj :sndobj))

(defcfun ("_wrap_delete_Reson" delete_Reson) :void
  (self :sndobj_reson))

(defcfun ("_wrap_Reson_Connect" Reson_Connect) :int
  (self :sndobj_reson)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Lp_empty" new_Lp_empty) :sndobj_lp)

(defcfun ("_wrap_new_Lp" new_Lp) :sndobj_lp
  (fr :float)
  (BW :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (inputBW :sndobj)
  (vecisize :int)
  (sr :float))

(defcfun ("_wrap_delete_Lp" delete_Lp) :void
  (self :sndobj_lp))

(defcfun ("_wrap_Lp_SetSr" Lp_SetSr) :void
  (self :sndobj_lp)
  (sr :float))

(defcfun ("_wrap_Lp_Set" Lp_Set) :int
  (self :sndobj_lp)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_ButtBP_empty" new_ButtBP_empty) :sndobj_buttbp)

(defcfun ("_wrap_new_ButtBP" new_ButtBP) :sndobj_buttbp
  (fr :float)
  (bw :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (inputbw :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_ButtBP" delete_ButtBP) :void
  (self :sndobj_buttbp))

(defcfun ("_wrap_ButtBP_Set" ButtBP_Set) :int
  (self :sndobj_buttbp)
  (mess :string)
  (value :float))

(defcfun ("_wrap_ButtBP_SetFreq" ButtBP_SetFreq) :void
  (self :sndobj_buttbp)
  (fr :float))

(defcfun ("_wrap_ButtBP_SetBW" ButtBP_SetBW) :void
  (self :sndobj_buttbp)
  (bw :float))

(defcfun ("_wrap_ButtBP_SetFreq_mod" ButtBP_SetFreq_mod) :void
  (self :sndobj_buttbp)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_ButtBP_SetBW_mod" ButtBP_SetBW_mod) :void
  (self :sndobj_buttbp)
  (bw :float)
  (InBWObj :sndobj))

(defcfun ("_wrap_ButtBP_SetSr" ButtBP_SetSr) :void
  (self :sndobj_buttbp)
  (sr :float))

(defcfun ("_wrap_ButtBP_Connect" ButtBP_Connect) :int
  (self :sndobj_buttbp)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_ButtBR_empty" new_ButtBR_empty) :sndobj_buttbr)

(defcfun ("_wrap_new_ButtBR" new_ButtBR) :sndobj_buttbr
  (fr :float)
  (bw :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (inputbw :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_ButtBR" delete_ButtBR) :void
  (self :sndobj_buttbr))

(defcfun ("_wrap_new_ButtHP_empty" new_ButtHP_empty) :sndobj_butthp)

(defcfun ("_wrap_new_ButtHP" new_ButtHP) :sndobj_butthp
  (fr :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_ButtHP" delete_ButtHP) :void
  (self :sndobj_butthp))

(defcfun ("_wrap_new_ButtLP_empty" new_ButtLP_empty) :sndobj_buttlp)

(defcfun ("_wrap_new_ButtLP" new_ButtLP) :sndobj_buttlp
  (fr :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_ButtLP" delete_ButtLP) :void
  (self :sndobj_buttlp))

(defcfun ("_wrap_new_Ap_empty" new_Ap_empty) :sndobj_ap)

(defcfun ("_wrap_new_Ap" new_Ap) :sndobj_ap
  (fr :float)
  (R :float)
  (inObj :sndobj)
  (inputfreq :sndobj)
  (inputR :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Ap" delete_Ap) :void
  (self :sndobj_ap))

(defcfun ("_wrap_Ap_SetFreq" Ap_SetFreq) :void
  (self :sndobj_ap)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_Ap_SetR" Ap_SetR) :void
  (self :sndobj_ap)
  (r :float)
  (InRObj :sndobj))

(defcfun ("_wrap_Ap_SetSr" Ap_SetSr) :void
  (self :sndobj_ap)
  (sr :float))

(defcfun ("_wrap_Ap_Set" Ap_Set) :int
  (self :sndobj_ap)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Ap_Connect" Ap_Connect) :int
  (self :sndobj_ap)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Ap_ErrorMessage" Ap_ErrorMessage) :string
  (self :sndobj_ap))

(defcfun ("_wrap_new_LoPass_empty" new_LoPass_empty) :sndobj_lopass)

(defcfun ("_wrap_new_LoPass" new_LoPass) :sndobj_lopass
  (freq :float)
  (inObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_LoPass" delete_LoPass) :void
  (self :sndobj_lopass))

(defcfun ("_wrap_LoPass_SetFreq" LoPass_SetFreq) :void
  (self :sndobj_lopass)
  (fr :float))

(defcfun ("_wrap_LoPass_SetSr" LoPass_SetSr) :void
  (self :sndobj_lopass)
  (sr :float))

(defcfun ("_wrap_LoPass_Set" LoPass_Set) :int
  (self :sndobj_lopass)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_HiPass_empty" new_HiPass_empty) :sndobj_hipass)

(defcfun ("_wrap_new_HiPass" new_HiPass) :sndobj_hipass
  (freq :float)
  (inObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_HiPass" delete_HiPass) :void
  (self :sndobj_hipass))

(defcfun ("_wrap_HiPass_SetFreq" HiPass_SetFreq) :void
  (self :sndobj_hipass)
  (fr :float))

(defcfun ("_wrap_HiPass_SetSr" HiPass_SetSr) :void
  (self :sndobj_hipass)
  (sr :float))

(defcfun ("_wrap_HiPass_Set" HiPass_Set) :int
  (self :sndobj_hipass)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Hilb_real_set" Hilb_real_set) :void
  (self :sndobj_hilb)
  (real :sndobj))

(defcfun ("_wrap_Hilb_real_get" Hilb_real_get) :sndobj
  (self :sndobj_hilb))

(defcfun ("_wrap_Hilb_imag_set" Hilb_imag_set) :void
  (self :sndobj_hilb)
  (imag :sndobj))

(defcfun ("_wrap_Hilb_imag_get" Hilb_imag_get) :sndobj
  (self :sndobj_hilb))

(defcfun ("_wrap_new_Hilb_empty" new_Hilb_empty) :sndobj_hilb)

(defcfun ("_wrap_new_Hilb" new_Hilb) :sndobj_hilb
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Hilb" delete_Hilb) :void
  (self :sndobj_hilb))

(defcfun ("_wrap_Hilb_ErrorMessage" Hilb_ErrorMessage) :string
  (self :sndobj_hilb))

(defcfun ("_wrap_new_SyncGrain_empty" new_SyncGrain_empty) :sndobj_syncgrain)

(defcfun ("_wrap_new_SyncGrain" new_SyncGrain) :sndobj_syncgrain
  (wavetable :sndobj_table)
  (envtable :sndobj_table)
  (fr :float)
  (amp :float)
  (pitch :float)
  (grsize :float)
  (prate :float)
  (inputfr :sndobj)
  (inputamp :sndobj)
  (inputpitch :sndobj)
  (inputgrsize :sndobj)
  (olaps :int)
  (vecisize :int)
  (sr :float))

(defcfun ("_wrap_delete_SyncGrain" delete_SyncGrain) :void
  (self :sndobj_syncgrain))

(defcfun ("_wrap_SyncGrain_Offset" SyncGrain_Offset) :void
  (self :sndobj_syncgrain)
  (pos :int))

(defcfun ("_wrap_SyncGrain_Offset_seconds" SyncGrain_Offset_seconds) :void
  (self :sndobj_syncgrain)
  (secs :float))

(defcfun ("_wrap_SyncGrain_SetWaveTable" SyncGrain_SetWaveTable) :void
  (self :sndobj_syncgrain)
  (wavetable :sndobj_table))

(defcfun ("_wrap_SyncGrain_SetEnvelopeTable" SyncGrain_SetEnvelopeTable) :void
  (self :sndobj_syncgrain)
  (envtable :sndobj_table))

(defcfun ("_wrap_SyncGrain_SetFreq" SyncGrain_SetFreq) :void
  (self :sndobj_syncgrain)
  (fr :float)
  (inputfr :sndobj))

(defcfun ("_wrap_SyncGrain_SetAmp" SyncGrain_SetAmp) :void
  (self :sndobj_syncgrain)
  (amp :float)
  (inputamp :sndobj))

(defcfun ("_wrap_SyncGrain_SetPitch" SyncGrain_SetPitch) :void
  (self :sndobj_syncgrain)
  (pitch :float)
  (inputpitch :sndobj))

(defcfun ("_wrap_SyncGrain_SetGrainSize" SyncGrain_SetGrainSize) :void
  (self :sndobj_syncgrain)
  (grsize :float)
  (inputgrsize :sndobj))

(defcfun ("_wrap_SyncGrain_SetPointerRate" SyncGrain_SetPointerRate) :void
  (self :sndobj_syncgrain)
  (prate :float))

(defcfun ("_wrap_SyncGrain_Set" SyncGrain_Set) :int
  (self :sndobj_syncgrain)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SyncGrain_Connect" SyncGrain_Connect) :int
  (self :sndobj_syncgrain)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SyncGrain_ErrorMessage" SyncGrain_ErrorMessage) :string
  (self :sndobj_syncgrain))


(defcstruct SndObjList
  (obj :sndobj)
  (next :pointer))

(defcfun ("_wrap_new_Mixer_empty" new_Mixer_empty) :sndobj_mixer)

(defcfun ("_wrap_new_Mixer" new_Mixer) :sndobj_mixer
  (ObjNo :int)
  (InObjs :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Mixer" delete_Mixer) :void
  (self :sndobj_mixer))

(defcfun ("_wrap_Mixer_GetObjNo" Mixer_GetObjNo) :int
  (self :sndobj_mixer))

(defcfun ("_wrap_Mixer_AddObj" Mixer_AddObj) :short
  (self :sndobj_mixer)
  (InObj :sndobj))

(defcfun ("_wrap_Mixer_DeleteObj" Mixer_DeleteObj) :short
  (self :sndobj_mixer)
  (InObj :sndobj))

(defcfun ("_wrap_Mixer_Connect" Mixer_Connect) :int
  (self :sndobj_mixer)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Mixer_ErrorMessage" Mixer_ErrorMessage) :string
  (self :sndobj_mixer))

(defcfun ("_wrap_Pan_left_set" Pan_left_set) :void
  (self :sndobj_pan)
  (left :sndobj))

(defcfun ("_wrap_Pan_left_get" Pan_left_get) :sndobj
  (self :sndobj_pan))

(defcfun ("_wrap_Pan_right_set" Pan_right_set) :void
  (self :sndobj_pan)
  (right :sndobj))

(defcfun ("_wrap_Pan_right_get" Pan_right_get) :sndobj
  (self :sndobj_pan))

(defcfun ("_wrap_new_Pan_empty" new_Pan_empty) :sndobj_pan)

(defcfun ("_wrap_new_Pan" new_Pan) :sndobj_pan
  (pan :float)
  (InObj :sndobj)
  (InPan :sndobj)
  (res :int)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Pan" delete_Pan) :void
  (self :sndobj_pan))

(defcfun ("_wrap_Pan_SetPan" Pan_SetPan) :void
  (self :sndobj_pan)
  (pan :float)
  (InPan :sndobj))

(defcfun ("_wrap_Pan_Set" Pan_Set) :int
  (self :sndobj_pan)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Pan_Connect" Pan_Connect) :int
  (self :sndobj_pan)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Pan_ErrorMessage" Pan_ErrorMessage) :string
  (self :sndobj_pan))

(defcfun ("_wrap_new_Gain_empty" new_Gain_empty) :sndobj_gain)

(defcfun ("_wrap_new_Gain" new_Gain) :sndobj_gain
  (gain :float)
  (InObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Gain" delete_Gain) :void
  (self :sndobj_gain))

(defcfun ("_wrap_Gain_Set" Gain_Set) :int
  (self :sndobj_gain)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Gain_SetGain" Gain_SetGain) :void
  (self :sndobj_gain)
  (gain :float))

(defcfun ("_wrap_Gain_SetGainM" Gain_SetGainM) :void
  (self :sndobj_gain)
  (gain_multiplier :float))

(defcfun ("_wrap_Gain_dBToAmp" Gain_dBToAmp) :float
  (self :sndobj_gain)
  (amp :float))

(defcfun ("_wrap_new_Interp_empty" new_Interp_empty) :sndobj_interp)

(defcfun ("_wrap_new_Interp" new_Interp) :sndobj_interp
  (initial :float)
  (final :float)
  (dur :float)
  (type :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Interp" delete_Interp) :void
  (self :sndobj_interp))

(defcfun ("_wrap_Interp_Set" Interp_Set) :int
  (self :sndobj_interp)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Interp_SetSr" Interp_SetSr) :void
  (self :sndobj_interp)
  (sr :float))

(defcfun ("_wrap_Interp_Restart" Interp_Restart) :void
  (self :sndobj_interp))

(defcfun ("_wrap_Interp_SetCurve" Interp_SetCurve) :void
  (self :sndobj_interp)
  (initial :float)
  (final :float)
  (m_typec :float))

(defcfun ("_wrap_Interp_SetDur" Interp_SetDur) :void
  (self :sndobj_interp)
  (dur :float))

(defcfun ("_wrap_new_Phase_empty" new_Phase_empty) :sndobj_phase)

(defcfun ("_wrap_new_Phase" new_Phase) :sndobj_phase
  (freq :float)
  (FreqInput :sndobj)
  (offset :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Phase" delete_Phase) :void
  (self :sndobj_phase))

(defcfun ("_wrap_Phase_SetFreq" Phase_SetFreq) :void
  (self :sndobj_phase)
  (freq :float)
  (FreqInput :sndobj))

(defcfun ("_wrap_Phase_SetPhase" Phase_SetPhase) :void
  (self :sndobj_phase)
  (offset :float))

(defcfun ("_wrap_Phase_Set" Phase_Set) :int
  (self :sndobj_phase)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Phase_Connect" Phase_Connect) :int
  (self :sndobj_phase)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Ring_empty" new_Ring_empty) :sndobj_ring)

(defcfun ("_wrap_new_Ring" new_Ring) :sndobj_ring
  (InObj1 :sndobj)
  (InObj2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Ring" delete_Ring) :void
  (self :sndobj_ring))

(defcfun ("_wrap_Ring_SetInput1" Ring_SetInput1) :void
  (self :sndobj_ring)
  (InObj :sndobj))

(defcfun ("_wrap_Ring_SetInput2" Ring_SetInput2) :void
  (self :sndobj_ring)
  (InObj :sndobj))

(defcfun ("_wrap_Ring_Connect" Ring_Connect) :int
  (self :sndobj_ring)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Unit_empty" new_Unit_empty) :sndobj_unit)

(defcfun ("_wrap_new_Unit" new_Unit) :sndobj_unit
  (m_amp :float)
  (mode :short)
  (step :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Unit" delete_Unit) :void
  (self :sndobj_unit))

(defcfun ("_wrap_Unit_SetAmp" Unit_SetAmp) :void
  (self :sndobj_unit)
  (amp :float))

(defcfun ("_wrap_Unit_SetStep" Unit_SetStep) :void
  (self :sndobj_unit)
  (step :float))

(defcfun ("_wrap_Unit_SetMode" Unit_SetMode) :void
  (self :sndobj_unit)
  (mode :short))

(defcfun ("_wrap_Unit_Set" Unit_Set) :int
  (self :sndobj_unit)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Lookup_empty" new_Lookup_empty) :sndobj_lookup)

(defcfun ("_wrap_new_Lookup" new_Lookup) :sndobj_lookup
  (table :sndobj_table)
  (offset :long)
  (InObj :sndobj)
  (mode :int)
  (normal :int)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_Lookup_SetMode" Lookup_SetMode) :void
  (self :sndobj_lookup)
  (mode :int)
  (normal :int))

(defcfun ("_wrap_delete_Lookup" delete_Lookup) :void
  (self :sndobj_lookup))

(defcfun ("_wrap_Lookup_Offset" Lookup_Offset) :void
  (self :sndobj_lookup)
  (offset :long))

(defcfun ("_wrap_Lookup_SetTable" Lookup_SetTable) :void
  (self :sndobj_lookup)
  (table :sndobj_table))

(defcfun ("_wrap_Lookup_Set" Lookup_Set) :int
  (self :sndobj_lookup)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Lookup_Connect" Lookup_Connect) :int
  (self :sndobj_lookup)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Lookupi_empty" new_Lookupi_empty) :sndobj_lookupi)

(defcfun ("_wrap_new_Lookupi" new_Lookupi) :sndobj_lookupi
  (table :sndobj_table)
  (offset :long)
  (InObj :sndobj)
  (mode :int)
  (normal :int)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Lookupi" delete_Lookupi) :void
  (self :sndobj_lookupi))

(defcfun ("_wrap_new_Rand_empty" new_Rand_empty) :sndobj_rand)

(defcfun ("_wrap_new_Rand" new_Rand) :sndobj_rand
  (amp :float)
  (InAmpObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Rand" delete_Rand) :void
  (self :sndobj_rand))

(defcfun ("_wrap_Rand_SetAmp" Rand_SetAmp) :void
  (self :sndobj_rand)
  (amp :float)
  (InAmpObj :sndobj))

(defcfun ("_wrap_Rand_Set" Rand_Set) :int
  (self :sndobj_rand)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Rand_Connect" Rand_Connect) :int
  (self :sndobj_rand)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_PhOscili_empty" new_PhOscili_empty) :sndobj_phoscili)

(defcfun ("_wrap_new_PhOscili" new_PhOscili) :sndobj_phoscili
  (table :sndobj_table)
  (fr :float)
  (amp :float)
  (inputfreq :sndobj)
  (inputamp :sndobj)
  (inputphase :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PhOscili" delete_PhOscili) :void
  (self :sndobj_phoscili))

(defcfun ("_wrap_PhOscili_Connect" PhOscili_Connect) :int
  (self :sndobj_phoscili)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_Randh_empty" new_Randh_empty) :sndobj_randh)

(defcfun ("_wrap_new_Randh" new_Randh) :sndobj_randh
  (fr :float)
  (amp :float)
  (InFrObj :sndobj)
  (InAmpObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Randh" delete_Randh) :void
  (self :sndobj_randh))

(defcfun ("_wrap_Randh_Connect" Randh_Connect) :int
  (self :sndobj_randh)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Randh_SetSr" Randh_SetSr) :void
  (self :sndobj_randh)
  (sr :float))

(defcfun ("_wrap_Randh_SetFreq" Randh_SetFreq) :void
  (self :sndobj_randh)
  (fr :float)
  (InFrObj :sndobj))

(defcfun ("_wrap_Randh_Set" Randh_Set) :int
  (self :sndobj_randh)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_Randi_empty" new_Randi_empty) :sndobj_randi)

(defcfun ("_wrap_new_Randi" new_Randi) :sndobj_randi
  (fr :float)
  (amp :float)
  (InFrObj :sndobj)
  (InAmpObj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Randi" delete_Randi) :void
  (self :sndobj_randi))

(defcfun ("_wrap_new_FFT_empty" new_FFT_empty) :sndobj_fft)

(defcfun ("_wrap_new_FFT" new_FFT) :sndobj_fft
  (window :sndobj_table)
  (input :sndobj)
  (scale :float)
  (fftsize :int)
  (hopsize :int)
  (m_sr :float))

(defcfun ("_wrap_delete_FFT" delete_FFT) :void
  (self :sndobj_fft))

(defcfun ("_wrap_FFT_GetFFTSize" FFT_GetFFTSize) :int
  (self :sndobj_fft))

(defcfun ("_wrap_FFT_GetHopSize" FFT_GetHopSize) :int
  (self :sndobj_fft))

(defcfun ("_wrap_FFT_SetWindow" FFT_SetWindow) :void
  (self :sndobj_fft)
  (window :sndobj_table))

(defcfun ("_wrap_FFT_Connect" FFT_Connect) :int
  (self :sndobj_fft)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_FFT_Set" FFT_Set) :int
  (self :sndobj_fft)
  (mess :string)
  (value :float))

(defcfun ("_wrap_FFT_SetScale" FFT_SetScale) :void
  (self :sndobj_fft)
  (scale :float))

(defcfun ("_wrap_FFT_SetFFTSize" FFT_SetFFTSize) :void
  (self :sndobj_fft)
  (fftsize :int))

(defcfun ("_wrap_FFT_SetHopSize" FFT_SetHopSize) :void
  (self :sndobj_fft)
  (hopsize :int))

(defcfun ("_wrap_new_IFFT_empty" new_IFFT_empty) :sndobj_ifft)

(defcfun ("_wrap_new_IFFT" new_IFFT) :sndobj_ifft
  (window :sndobj_table)
  (input :sndobj)
  (fftsize :int)
  (hopsize :int)
  (sr :float))

(defcfun ("_wrap_delete_IFFT" delete_IFFT) :void
  (self :sndobj_ifft))

(defcfun ("_wrap_IFFT_GetFFTSize" IFFT_GetFFTSize) :int
  (self :sndobj_ifft))

(defcfun ("_wrap_IFFT_GetHopSize" IFFT_GetHopSize) :int
  (self :sndobj_ifft))

(defcfun ("_wrap_IFFT_SetWindow" IFFT_SetWindow) :void
  (self :sndobj_ifft)
  (window :sndobj_table))

(defcfun ("_wrap_IFFT_Connect" IFFT_Connect) :int
  (self :sndobj_ifft)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_IFFT_Set" IFFT_Set) :int
  (self :sndobj_ifft)
  (mess :string)
  (value :float))

(defcfun ("_wrap_IFFT_SetFFTSize" IFFT_SetFFTSize) :void
  (self :sndobj_ifft)
  (fftsize :int))

(defcfun ("_wrap_IFFT_SetHopSize" IFFT_SetHopSize) :void
  (self :sndobj_ifft)
  (hopsize :int))

(defcfun ("_wrap_new_PVA_empty" new_PVA_empty) :sndobj_pva)

(defcfun ("_wrap_new_PVA" new_PVA) :sndobj_pva
  (window :sndobj_table)
  (input :sndobj)
  (scale :float)
  (fftsize :int)
  (hopsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVA" delete_PVA) :void
  (self :sndobj_pva))

(defcfun ("_wrap_PVA_Outphases" PVA_Outphases) :float
  (self :sndobj_pva)
  (pos :int))

(defcfun ("_wrap_PVA_Set" PVA_Set) :int
  (self :sndobj_pva)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVA_SetFFTSize" PVA_SetFFTSize) :void
  (self :sndobj_pva)
  (fftsize :int))

(defcfun ("_wrap_PVA_SetHopSize" PVA_SetHopSize) :void
  (self :sndobj_pva)
  (hopsize :int))

(defcfun ("_wrap_new_PVSempty" new_PVS_empty) :sndobj_pvs)

(defcfun ("_wrap_new_PVS" new_PVS) :sndobj_pvs
  (window :sndobj_table)
  (input :sndobj)
  (fftsize :int)
  (hopsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVS" delete_PVS) :void
  (self :sndobj_pvs))

(defcfun ("_wrap_PVS_Set" PVS_Set) :int
  (self :sndobj_pvs)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVS_SetFFTSize" PVS_SetFFTSize) :void
  (self :sndobj_pvs)
  (fftsize :int))

(defcfun ("_wrap_PVS_SetHopSize" PVS_SetHopSize) :void
  (self :sndobj_pvs)
  (hopsize :int))

(defcfun ("_wrap_PVRead_Outchannel" PVRead_Outchannel) :sndobj
  (self :sndobj_pvread)
  (channel :int))

(defcfun ("_wrap_PVRead_Set" PVRead_Set) :int
  (self :sndobj_pvread)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVRead_SetInput" PVRead_SetInput) :void
  (self :sndobj_pvread)
  (name :string))

(defcfun ("_wrap_PVRead_SetTimescale" PVRead_SetTimescale) :void
  (self :sndobj_pvread)
  (timescale :float))

(defcfun ("_wrap_new_PVRead_empty" new_PVRead_empty) :sndobj_pvread)

(defcfun ("_wrap_new_PVRead" new_PVRead) :sndobj_pvread
  (name :string)
  (timescale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVRead" delete_PVRead) :void
  (self :sndobj_pvread))

(defcfun ("_wrap_new_IFGram_empty" new_IFGram_empty) :sndobj_ifgram)

(defcfun ("_wrap_new_IFGram" new_IFGram) :sndobj_ifgram
  (window :sndobj_table)
  (input :sndobj)
  (scale :float)
  (fftsize :int)
  (hopsize :int)
  (sr :float))

(defcfun ("_wrap_delete_IFGram" delete_IFGram) :void
  (self :sndobj_ifgram))

(defcfun ("_wrap_IFGram_Set" IFGram_Set) :int
  (self :sndobj_ifgram)
  (mess :string)
  (value :float))

(defcfun ("_wrap_IFGram_Connect" IFGram_Connect) :int
  (self :sndobj_ifgram)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_IFGram_SetFFTSize" IFGram_SetFFTSize) :void
  (self :sndobj_ifgram)
  (fftsize :int))

(defcfun ("_wrap_new_SinAnal_empty" new_SinAnal_empty) :sndobj_sinanal)

(defcfun ("_wrap_new_SinAnal" new_SinAnal) :sndobj_sinanal
  (input :sndobj)
  (threshold :float)
  (maxtracks :int)
  (minpoints :int)
  (maxgap :int)
  (sr :float))

(defcfun ("_wrap_delete_SinAnal" delete_SinAnal) :void
  (self :sndobj_sinanal))

(defcfun ("_wrap_SinAnal_GetTrackID" SinAnal_GetTrackID) :int
  (self :sndobj_sinanal)
  (track :int))

(defcfun ("_wrap_SinAnal_GetTracks" SinAnal_GetTracks) :int
  (self :sndobj_sinanal))

(defcfun ("_wrap_SinAnal_Set" SinAnal_Set) :int
  (self :sndobj_sinanal)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SinAnal_Connect" SinAnal_Connect) :int
  (self :sndobj_sinanal)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SinAnal_SetThreshold" SinAnal_SetThreshold) :void
  (self :sndobj_sinanal)
  (threshold :float))

(defcfun ("_wrap_SinAnal_SetIFGram" SinAnal_SetIFGram) :void
  (self :sndobj_sinanal)
  (input :sndobj))

(defcfun ("_wrap_SinAnal_SetMaxTracks" SinAnal_SetMaxTracks) :void
  (self :sndobj_sinanal)
  (maxtracks :int))

(defcfun ("_wrap_new_SinSyn_empty" new_SinSyn_empty) :sndobj_sinsyn)

(defcfun ("_wrap_new_SinSyn" new_SinSyn) :sndobj_sinsyn
  (input :sndobj_sinanal)
  (maxtracks :int)
  (table :sndobj_table)
  (scale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SinSyn" delete_SinSyn) :void
  (self :sndobj_sinsyn))

(defcfun ("_wrap_SinSyn_SetTable" SinSyn_SetTable) :void
  (self :sndobj_sinsyn)
  (table :sndobj_table))

(defcfun ("_wrap_SinSyn_SetMaxTracks" SinSyn_SetMaxTracks) :void
  (self :sndobj_sinsyn)
  (maxtracks :int))

(defcfun ("_wrap_SinSyn_SetScale" SinSyn_SetScale) :void
  (self :sndobj_sinsyn)
  (scale :float))

(defcfun ("_wrap_SinSyn_Set" SinSyn_Set) :int
  (self :sndobj_sinsyn)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SinSyn_Connect" SinSyn_Connect) :int
  (self :sndobj_sinsyn)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_ReSyn_empty" new_ReSyn_empty) :sndobj_resyn)

(defcfun ("_wrap_new_ReSyn" new_ReSyn) :sndobj_resyn
  (input :sndobj_sinanal)
  (maxtracks :int)
  (table :sndobj_table)
  (pitch :float)
  (scale :float)
  (tscal :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_ReSyn_SetPitch" ReSyn_SetPitch) :void
  (self :sndobj_resyn)
  (pitch :float))

(defcfun ("_wrap_ReSyn_SetTimeScale" ReSyn_SetTimeScale) :void
  (self :sndobj_resyn)
  (scale :float))

(defcfun ("_wrap_ReSyn_Set" ReSyn_Set) :int
  (self :sndobj_resyn)
  (mess :string)
  (value :float))

(defcfun ("_wrap_delete_ReSyn" delete_ReSyn) :void
  (self :sndobj_resyn))

(defcfun ("_wrap_new_AdSyn_empty" new_AdSyn_empty) :sndobj_adsyn)

(defcfun ("_wrap_new_AdSyn" new_AdSyn) :sndobj_adsyn
  (input :sndobj_sinanal)
  (maxtracks :int)
  (table :sndobj_table)
  (pitch :float)
  (scale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_AdSyn" delete_AdSyn) :void
  (self :sndobj_adsyn))

(defcfun ("_wrap_new_IFAdd_empty" new_IFAdd_empty) :sndobj_ifadd)

(defcfun ("_wrap_new_IFAdd" new_IFAdd) :sndobj_ifadd
  (input :sndobj_ifgram)
  (bins :int)
  (table :sndobj_table)
  (pitch :float)
  (scale :float)
  (tscal :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_IFAdd" delete_IFAdd) :void
  (self :sndobj_ifadd))

(defcfun ("_wrap_new_SpecMult_empty" new_SpecMult_empty) :sndobj_specmult)

(defcfun ("_wrap_new_SpecMult" new_SpecMult) :sndobj_specmult
  (input1 :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_SpecMult_table" new_SpecMult_table) :sndobj_specmult
  (spectab :sndobj_table)
  (input1 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecMult" delete_SpecMult) :void
  (self :sndobj_specmult))

(defcfun ("_wrap_SpecMult_Connect" SpecMult_Connect) :int
  (self :sndobj_specmult)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SpecMult_SetInput2" SpecMult_SetInput2) :void
  (self :sndobj_specmult)
  (input2 :sndobj))

(defcfun ("_wrap_SpecMult_SetTable" SpecMult_SetTable) :void
  (self :sndobj_specmult)
  (spectab :sndobj_table))

(defcfun ("_wrap_new_SpecInterp_empty" new_SpecInterp_empty) :sndobj_specinterp)

(defcfun ("_wrap_new_SpecInterp" new_SpecInterp) :sndobj_specinterp
  (i_offset :float)
  (input1 :sndobj)
  (input2 :sndobj)
  (interpobj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecInterp" delete_SpecInterp) :void
  (self :sndobj_specinterp))

(defcfun ("_wrap_SpecInterp_Connect" SpecInterp_Connect) :int
  (self :sndobj_specinterp)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SpecInterp_Set" SpecInterp_Set) :int
  (self :sndobj_specinterp)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SpecInterp_SetInterp" SpecInterp_SetInterp) :void
  (self :sndobj_specinterp)
  (i_offset :float)
  (interpobj :sndobj))

(defcfun ("_wrap_new_PVMask_empty" new_PVMask_empty) :sndobj_pvmask)

(defcfun ("_wrap_new_PVMask" new_PVMask) :sndobj_pvmask
  (maskgain :float)
  (input :sndobj)
  (mask :sndobj)
  (inmaskgobj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_PVMask_table" new_PVMask_table) :sndobj_pvmask
  (maskgain :float)
  (masktable :sndobj_table)
  (input :sndobj)
  (inmaskgobj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVMask" delete_PVMask) :void
  (self :sndobj_pvmask))

(defcfun ("_wrap_PVMask_Connect" PVMask_Connect) :int
  (self :sndobj_pvmask)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_PVMask_Set" PVMask_Set) :int
  (self :sndobj_pvmask)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVMask_SetMaskInput" PVMask_SetMaskInput) :void
  (self :sndobj_pvmask)
  (mask :sndobj))

(defcfun ("_wrap_PVMask_SetMaskTable" PVMask_SetMaskTable) :void
  (self :sndobj_pvmask)
  (mask :sndobj_table))

(defcfun ("_wrap_PVMask_SetMaskGain" PVMask_SetMaskGain) :void
  (self :sndobj_pvmask)
  (maskgain :float)
  (inmaskg :sndobj))

(defcfun ("_wrap_PVTransp_Set" PVTransp_Set) :int
  (self :sndobj_pvtransp)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVTransp_Connect" PVTransp_Connect) :int
  (self :sndobj_pvtransp)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_PVTransp_SetPitch" PVTransp_SetPitch) :void
  (self :sndobj_pvtransp)
  (pitch :float)
  (inpitch :sndobj))

(defcfun ("_wrap_PVTransp_SetMode" PVTransp_SetMode) :void
  (self :sndobj_pvtransp)
  (mode :int))

(defcfun ("_wrap_new_PVTransp_empty" new_PVTransp_empty) :sndobj_pvtransp)

(defcfun ("_wrap_new_PVTransp" new_PVTransp) :sndobj_pvtransp
  (input :sndobj)
  (pitch :float)
  (mode :int)
  (inpitch :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVTransp" delete_PVTransp) :void
  (self :sndobj_pvtransp))

(defcfun ("_wrap_new_PVMix_empty" new_PVMix_empty) :sndobj_pvmix)

(defcfun ("_wrap_new_PVMix" new_PVMix) :sndobj_pvmix
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVMix" delete_PVMix) :void
  (self :sndobj_pvmix))

(defcfun ("_wrap_PVBlur_Set" PVBlur_Set) :int
  (self :sndobj_pvblur)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVBlur_SetBlurTime" PVBlur_SetBlurTime) :void
  (self :sndobj_pvblur)
  (time :float))

(defcfun ("_wrap_PVBlur_SetHopsize" PVBlur_SetHopsize) :void
  (self :sndobj_pvblur)
  (hopsize :int))

(defcfun ("_wrap_new_PVBlur_empty" new_PVBlur_empty) :sndobj_pvblur)

(defcfun ("_wrap_new_PVBlur" new_PVBlur) :sndobj_pvblur
  (input :sndobj)
  (blurtime :float)
  (hopsize :int)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVBlur" delete_PVBlur) :void
  (self :sndobj_pvblur))

(defcfun ("_wrap_new_PVFilter_empty" new_PVFilter_empty) :sndobj_pvfilter)

(defcfun ("_wrap_new_PVFilter" new_PVFilter) :sndobj_pvfilter
  (input :sndobj)
  (filspec :sndobj)
  (amnt :float)
  (amntobj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_PVFilter_table" new_PVFilter_table) :sndobj_pvfilter
  (filtertable :sndobj_table)
  (input :sndobj)
  (amnt :float)
  (amntobj :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVFilter" delete_PVFilter) :void
  (self :sndobj_pvfilter))

(defcfun ("_wrap_PVFilter_Connect" PVFilter_Connect) :int
  (self :sndobj_pvfilter)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_PVFilter_Set" PVFilter_Set) :int
  (self :sndobj_pvfilter)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVFilter_SetFilterInput" PVFilter_SetFilterInput) :void
  (self :sndobj_pvfilter)
  (filobj :sndobj))

(defcfun ("_wrap_PVFilter_SetFilterTable" PVFilter_SetFilterTable) :void
  (self :sndobj_pvfilter)
  (filtab :sndobj_table))

(defcfun ("_wrap_PVFilter_SetAmount" PVFilter_SetAmount) :void
  (self :sndobj_pvfilter)
  (amnt :float)
  (amntobj :sndobj))

(defcfun ("_wrap_new_PVMorph_empty" new_PVMorph_empty) :sndobj_pvmorph)

(defcfun ("_wrap_new_PVMorph" new_PVMorph) :sndobj_pvmorph
  (morphfr :float)
  (morpha :float)
  (input1 :sndobj)
  (input2 :sndobj)
  (inmorphfr :sndobj)
  (inmorpha :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVMorph" delete_PVMorph) :void
  (self :sndobj_pvmorph))

(defcfun ("_wrap_PVMorph_Connect" PVMorph_Connect) :int
  (self :sndobj_pvmorph)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_PVMorph_Set" PVMorph_Set) :int
  (self :sndobj_pvmorph)
  (mess :string)
  (value :float))

(defcfun ("_wrap_PVMorph_SetFreqMorph" PVMorph_SetFreqMorph) :void
  (self :sndobj_pvmorph)
  (morphfr :float)
  (inmorphfr :sndobj))

(defcfun ("_wrap_PVMorph_SetAmpMorph" PVMorph_SetAmpMorph) :void
  (self :sndobj_pvmorph)
  (morpha :float)
  (inmorpha :sndobj))

(defcfun ("_wrap_new_SpecPolar_empty" new_SpecPolar_empty) :sndobj_specpolar)

(defcfun ("_wrap_new_SpecPolar" new_SpecPolar) :sndobj_specpolar
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecPolar" delete_SpecPolar) :void
  (self :sndobj_specpolar))

(defcfun ("_wrap_SpecSplit_magnitude_set" SpecSplit_magnitude_set) :void
  (self :sndobj_specsplit)
  (magnitude :sndobj))

(defcfun ("_wrap_SpecSplit_magnitude_get" SpecSplit_magnitude_get) :sndobj
  (self :sndobj_specsplit))

(defcfun ("_wrap_SpecSplit_phase_set" SpecSplit_phase_set) :void
  (self :sndobj_specsplit)
  (phase :sndobj))

(defcfun ("_wrap_SpecSplit_phase_get" SpecSplit_phase_get) :sndobj
  (self :sndobj_specsplit))

(defcfun ("_wrap_new_SpecSplit_empty" new_SpecSplit_empty) :sndobj_specsplit)

(defcfun ("_wrap_new_SpecSplit" new_SpecSplit) :sndobj_specsplit
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecSplit" delete_SpecSplit) :void
  (self :sndobj_specsplit))

(defcfun ("_wrap_SpecThresh_SetThreshold" SpecThresh_SetThreshold) :void
  (self :sndobj_specthresh)
  (thresh :float))

(defcfun ("_wrap_SpecThresh_Set" SpecThresh_Set) :int
  (self :sndobj_specthresh)
  (mess :string)
  (value :float))

(defcfun ("_wrap_new_SpecThresh_empty" new_SpecThresh_empty) :sndobj_specthresh)

(defcfun ("_wrap_new_SpecThresh" new_SpecThresh) :sndobj_specthresh
  (threshold :float)
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecThresh" delete_SpecThresh) :void
  (self :sndobj_specthresh))

(defcfun ("_wrap_new_SpecVoc_empty" new_SpecVoc_empty) :sndobj_specvoc)

(defcfun ("_wrap_new_SpecVoc" new_SpecVoc) :sndobj_specvoc
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecVoc" delete_SpecVoc) :void
  (self :sndobj_specvoc))

(defcfun ("_wrap_new_SpecCart_empty" new_SpecCart_empty) :sndobj_speccart)

(defcfun ("_wrap_new_SpecCart" new_SpecCart) :sndobj_speccart
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecCart" delete_SpecCart) :void
  (self :sndobj_speccart))

(defcfun ("_wrap_new_SpecCombine_empty" new_SpecCombine_empty) :sndobj_speccombine)

(defcfun ("_wrap_new_SpecCombine" new_SpecCombine) :sndobj_speccombine
  (magin :sndobj)
  (phasin :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecCombine" delete_SpecCombine) :void
  (self :sndobj_speccombine))

(defcfun ("_wrap_SpecCombine_SetPhaseInput" SpecCombine_SetPhaseInput) :void
  (self :sndobj_speccombine)
  (phasin :sndobj))

(defcfun ("_wrap_SpecCombine_SetMagInput" SpecCombine_SetMagInput) :void
  (self :sndobj_speccombine)
  (magin :sndobj))

(defcfun ("_wrap_SpecCombine_Connect" SpecCombine_Connect) :int
  (self :sndobj_speccombine)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_new_SpecIn_empty" new_SpecIn_empty) :sndobj_specin)

(defcfun ("_wrap_new_SpecIn" new_SpecIn) :sndobj_specin
  (input :pointer)
  (channel :short)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SpecIn" delete_SpecIn) :void
  (self :sndobj_specin))

(defcfun ("_wrap_SpecIn_SetInput" SpecIn_SetInput) :void
  (self :sndobj_specin)
  (input :sndobj_sndio)
  (channel :short))

(defcfun ("_wrap_SpecIn_Connect" SpecIn_Connect) :int
  (self :sndobj_specin)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_SpecIn_Set" SpecIn_Set) :int
  (self :sndobj_specin)
  (mess :string)
  (value :float))

(defcfun ("_wrap_SpecIn_ErrorMessage" SpecIn_ErrorMessage) :string
  (self :sndobj_specin))

(defcfun ("_wrap_new_Convol_empty" new_Convol_empty) :sndobj_convol)

(defcfun ("_wrap_new_Convol" new_Convol) :sndobj_convol
  (impulse :sndobj_table)
  (input :sndobj)
  (scale :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Convol" delete_Convol) :void
  (self :sndobj_convol))

(defcfun ("_wrap_Convol_Connect" Convol_Connect) :int
  (self :sndobj_convol)
  (mess :string)
  (input :pointer))

(defcfun ("_wrap_Convol_Set" Convol_Set) :int
  (self :sndobj_convol)
  (mess :string)
  (value :float))

(defcfun ("_wrap_Convol_SetImpulse" Convol_SetImpulse) :void
  (self :sndobj_convol)
  (impulse :sndobj_table)
  (scale :float))

(defcfun ("_wrap_SndFIO_GetFile" SndFIO_GetFile) :pointer
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_GetMode" SndFIO_GetMode) :short
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_SetPos_float" SndFIO_SetPos_float) :void
  (self :sndobj_sndfio)
  (pos :float))

(defcfun ("_wrap_SndFIO_SetPos" SndFIO_SetPos) :void
  (self :sndobj_sndfio)
  (pos :long))

(defcfun ("_wrap_SndFIO_Eof" SndFIO_Eof) :int
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_GetDataFrames" SndFIO_GetDataFrames) :long
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_GetPos" SndFIO_GetPos) :float
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_GetStatus" SndFIO_GetStatus) :short
  (self :sndobj_sndfio))

(defcfun ("_wrap_new_SndFIO" new_SndFIO) :sndobj_sndfio
  (name :string)
  (mode :short)
  (channels :short)
  (bits :short)
  (inputlist :pointer)
  (spos :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndFIO" delete_SndFIO) :void
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_Read" SndFIO_Read) :short
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_Write" SndFIO_Write) :short
  (self :sndobj_sndfio))

(defcfun ("_wrap_SndFIO_ErrorMessage" SndFIO_ErrorMessage) :string
  (self :sndobj_sndfio))

(defcstruct wave_head
	(magic :long)
	(len0 :long)
	(magic1 :long)
	(magic2 :long)
	(len :long)
	(format :short)
	(nchns :short)
	(rate :long)
	(aver :long)
	(nBlockAlign :short)
	(size :short))

(defcstruct wave_data
	(magic3 :long)
	(datasize :long))

(defcfun ("_wrap_SndWave_GetHeader" SndWave_GetHeader) :pointer
  (self :sndobj_sndwave))

(defcfun ("_wrap_new_SndWave" new_SndWave) :sndobj_sndwave
  (name :string)
  (mode :short)
  (channels :short)
  (bits :short)
  (inputlist :pointer)
  (spos :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndWave" delete_SndWave) :void
  (self :sndobj_sndwave))

(defcfun ("_wrap_SndWave_Read" SndWave_Read) :short
  (self :sndobj_sndwave))

(defcfun ("_wrap_SndWave_Write" SndWave_Write) :short
  (self :sndobj_sndwave))

(defcfun ("_wrap_SndWave_IsWave" SndWave_IsWave) :boolean
  (self :sndobj_sndwave))

(defcfun ("_wrap_SndWave_ErrorMessage" SndWave_ErrorMessage) :string
  (self :sndobj_sndwave))

(defconstant WAVE_FORMAT_EXTENSIBLE #xFFFE)

(defconstant WAVE_FORMAT_PCM #x0001)

(defconstant WAVE_FORMAT_IEEE_FLOAT #x0003)

(defconstant SPEAKER_FRONT_LEFT #x1)

(defconstant SPEAKER_FRONT_RIGHT #x2)

(defconstant SPEAKER_FRONT_CENTER #x4)

(defconstant SPEAKER_LOW_FREQUENCY #x8)

(defconstant SPEAKER_BACK_LEFT #x10)

(defconstant SPEAKER_BACK_RIGHT #x20)

(defconstant SPEAKER_FRONT_LEFT_OF_CENTER #x40)

(defconstant SPEAKER_FRONT_RIGHT_OF_CENTER #x80)

(defconstant SPEAKER_BACK_CENTER #x100)

(defconstant SPEAKER_SIDE_LEFT #x200)

(defconstant SPEAKER_SIDE_RIGHT #x400)

(defconstant SPEAKER_TOP_CENTER #x800)

(defconstant SPEAKER_TOP_FRONT_LEFT #x1000)

(defconstant SPEAKER_TOP_FRONT_CENTER #x2000)

(defconstant SPEAKER_TOP_FRONT_RIGHT #x4000)

(defconstant SPEAKER_TOP_BACK_LEFT #x8000)

(defconstant SPEAKER_TOP_BACK_CENTER #x10000)

(defconstant SPEAKER_TOP_BACK_RIGHT #x20000)

(defconstant SPEAKER_RESERVED #x80000000)

(defcstruct GUID
	(Data1 :int)
	(Data2 :short)
	(Data3 :short)
	(Data4 :pointer))

(defcstruct wav_ex
	(wValidBitsPerSample :short)
	(dwChannelMask :int)
	(SubFormat :pointer))

(defcstruct WAVEFORMATEXTENSIBLE
	(waveformatex :pointer)
	(waveformat_ext :pointer))

(defcfun ("_wrap_new_SndWaveX" new_SndWaveX) :sndobj_sndwavex
  (name :string)
  (mode :short)
  (channels :short)
  (channelmask :int)
  (bits :short)
  (format :short)
  (inputlist :pointer)
  (spos :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndWaveX" delete_SndWaveX) :void
  (self :sndobj_sndwavex))

(defcfun ("_wrap_SndWaveX_GetHeader" SndWaveX_GetHeader) :void
  (self :sndobj_sndwavex)
  (pheader :pointer))

(defcfun ("_wrap_SndWaveX_GetChannelMask" SndWaveX_GetChannelMask) :int
  (self :sndobj_sndwavex))

(defcfun ("_wrap_SndWaveX_Read" SndWaveX_Read) :short
  (self :sndobj_sndwavex))

(defcfun ("_wrap_SndWaveX_Write" SndWaveX_Write) :short
  (self :sndobj_sndwavex))

(defcfun ("_wrap_SndWaveX_IsWaveExtensible" SndWaveX_IsWaveExtensible) :boolean
  (self :sndobj_sndwavex))

(defcstruct pvoc_data
	(wWordFormat :short)
	(wAnalFormat :short)
	(wSourceFormat :short)
	(wWindowType :short)
	(nAnalysisBins :int)
	(dwWinlen :int)
	(dwOverlap :int)
	(dwFrameAlign :int)
	(fAnalysisRate :float)
	(fWindowParam :float))

(defcstruct pvocex
	(dwVersion :int)
	(dwDataSize :int)
	(data :pointer))
#|
(defcenum pvoc_datatype
	:IEEE_FLOAT_T
	:IEEE_DOUBLE_T)

(defcenum pvoc_frametype
	(:PVOC_AMP_FREQ 0)
	:PVOC_AMP_PHASE
	:PVOC_COMPLEX)

(defcenum pvoc_windowtype
	(:DEFAULT 0)
	:HAMMING
	:HANNING
	:KAISER
	:RECTANGULAR
	:CUSTOM)
|#
(defcstruct WAVEFORMATPVOCEX
	(waveformatex :pointer)
	(waveformat_ext :pointer)
	(pvocformat_ext :pointer))

(defcfun ("_wrap_new_SndPVOCEX" new_SndPVOCEX) :sndobj_sndpvocex
  (name :string)
  (mode :short)
  (analformat :int)
  (windowtype :int)
  (channels :short)
  (channelmask :int)
  (bits :short)
  (format :int)
  (inputlist :pointer)
  (framepos :float)
  (hopsize :int)
  (fftsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndPVOCEX" delete_SndPVOCEX) :void
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_GetFFTSize" SndPVOCEX_GetFFTSize) :int
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_GetHopSize" SndPVOCEX_GetHopSize) :int
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_GetWindowType" SndPVOCEX_GetWindowType) :int
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_GetWindowLength" SndPVOCEX_GetWindowLength) :int
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_GetHeader" SndPVOCEX_GetHeader) :void
  (self :sndobj_sndpvocex)
  (pheader :pointer))

(defcfun ("_wrap_SndPVOCEX_SetTimePos" SndPVOCEX_SetTimePos) :void
  (self :sndobj_sndpvocex)
  (pos :float))

(defcfun ("_wrap_SndPVOCEX_Read" SndPVOCEX_Read) :short
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_Write" SndPVOCEX_Write) :short
  (self :sndobj_sndpvocex))

(defcfun ("_wrap_SndPVOCEX_IsPvocex" SndPVOCEX_IsPvocex) :boolean
  (self :sndobj_sndpvocex))

(defcstruct sinus_data
	(wWordFormat :short)
	(wHopsize :short)
	(wWindowType :short)
	(wMaxtracks :short)
	(dwWindowSize :int)
	(fThreshold :float)
	(fAnalysisRate :float))

(defcstruct sinusex
	(dwVersion :int)
	(data :pointer))

(defcstruct WAVEFORMATSINUSEX
	(waveformatex :pointer)
	(waveformat_ext :pointer)
	(sinusformat_ext :pointer))

(defcfun ("_wrap_new_SndSinIO" new_SndSinIO) :sndobj_sndsinio
  (name :string)
  (maxtracks :int)
  (threshold :float)
  (windowtype :int)
  (mode :short)
  (channels :short)
  (channelmask :int)
  (bits :short)
  (format :int)
  (inputlist :pointer)
  (framepos :float)
  (hopsize :int)
  (fftsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndSinIO" delete_SndSinIO) :void
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_Write" SndSinIO_Write) :short
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_Read" SndSinIO_Read) :short
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_GetTrackID" SndSinIO_GetTrackID) :int
  (self :sndobj_sndsinio)
  (track :int)
  (channel :int))

(defcfun ("_wrap_SndSinIO_GetTracks" SndSinIO_GetTracks) :int
  (self :sndobj_sndsinio)
  (channel :int))

(defcfun ("_wrap_SndSinIO_GetFFTSize" SndSinIO_GetFFTSize) :int
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_GetHopSize" SndSinIO_GetHopSize) :int
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_GetWindowType" SndSinIO_GetWindowType) :int
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_GetMaxTracks" SndSinIO_GetMaxTracks) :int
  (self :sndobj_sndsinio))

(defcfun ("_wrap_SndSinIO_GetHeader" SndSinIO_GetHeader) :void
  (self :sndobj_sndsinio)
  (pheader :pointer))

(defcfun ("_wrap_SndSinIO_SetTimePos" SndSinIO_SetTimePos) :void
  (self :sndobj_sndsinio)
  (pos :float))

(defcstruct CkHdr
	(ckID :unsigned-long)
	(ckSize :long))

(defcstruct FormHdr
	(ckHdr :pointer)
	(formType :unsigned-long))

(defcstruct CommChunk1
	(ckHdr :pointer)
	(numChannels :short))

(defcstruct CommChunk2
	(numSampleFrames :long)
	(sampleSize :short)
	(sampleRate :pointer))

(defcstruct Loop
	(playMode :short)
	(beginLoop :short)
	(endLoop :short))

(defcstruct InstrChunk
	(ckHdr :pointer)
	(baseNote :char)
	(detune :char)
	(lowNote :char)
	(highNote :char)
	(lowVelocity :char)
	(highVelocity :char)
	(gain :short)
	(sustainLoop :sndobj_loop)
	(releaseLoop :sndobj_loop))

(defcstruct SoundDataHdr
	(ckHdr :pointer)
	(offset :long)
	(blockSize :long))

(defcstruct aiff_head
	(ckID1 :unsigned-long)
	(ckSize1 :long)
	(formType :unsigned-long)
	(ckID2 :unsigned-long)
	(ckSize2 :long)
	(nchns :short)
	(numSampleFrames :long)
	(size :short)
	(rate :long))

(defcfun ("_wrap_SndAiff_IsAiff" SndAiff_IsAiff) :boolean
  (self :sndobj_sndaiff))

(defcfun ("_wrap_new_SndAiff" new_SndAiff) :sndobj_sndaiff
  (name :string)
  (mode :short)
  (channels :short)
  (bits :short)
  (inputlist :pointer)
  (spos :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndAiff" delete_SndAiff) :void
  (self :sndobj_sndaiff))

(defcfun ("_wrap_SndAiff_Read" SndAiff_Read) :short
  (self :sndobj_sndaiff))

(defcfun ("_wrap_SndAiff_Write" SndAiff_Write) :short
  (self :sndobj_sndaiff))

(defcfun ("_wrap_SndAiff_ErrorMessage" SndAiff_ErrorMessage) :string
  (self :sndobj_sndaiff))

(defcfun ("_wrap_new_SndBuffer" new_SndBuffer) :sndobj_sndbuffer
  (channels :short)
  (buffsize :int)
  (inputlist :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_SndBuffer" delete_SndBuffer) :void
  (self :sndobj_sndbuffer))

(defcfun ("_wrap_SndBuffer_Write" SndBuffer_Write) :short
  (self :sndobj_sndbuffer))

(defcfun ("_wrap_SndBuffer_Read" SndBuffer_Read) :short
  (self :sndobj_sndbuffer))

(defcfun ("_wrap_SndBuffer_ErrorMessage" SndBuffer_ErrorMessage) :string
  (self :sndobj_sndbuffer))

(defcfun ("_wrap_HarmTable_SetHarm" HarmTable_SetHarm) :void
  (self :sndobj_harmtable)
  (harm :int)
  (type :int))

(defcfun ("_wrap_HarmTable_ErrorMessage" HarmTable_ErrorMessage) :string
  (self :sndobj_harmtable))

(defcfun ("_wrap_HarmTable_MakeTable" HarmTable_MakeTable) :short
  (self :sndobj_harmtable))

(defcfun ("_wrap_new_HarmTable_empty" new_HarmTable_empty) :sndobj_harmtable)

(defcfun ("_wrap_HarmTable_SetPhase" HarmTable_SetPhase) :void
  (self :sndobj_harmtable)
  (phase :float))

(defcfun ("_wrap_new_HarmTable" new_HarmTable) :sndobj_harmtable
  (L :long)
  (harm :int)
  (type :int)
  (phase :float))

(defcfun ("_wrap_delete_HarmTable" delete_HarmTable) :void
  (self :sndobj_harmtable))

(defcfun ("_wrap_UsrHarmTable_SetHarm" UsrHarmTable_SetHarm) :void
  (self :sndobj_usrharmtable)
  (harm :int)
  (amps :float*))

(defcfun ("_wrap_UsrHarmTable_ErrorMessage" UsrHarmTable_ErrorMessage) :string
  (self :sndobj_usrharmtable))

(defcfun ("_wrap_UsrHarmTable_MakeTable" UsrHarmTable_MakeTable) :short
  (self :sndobj_usrharmtable))

(defcfun ("_wrap_new_UsrHarmTable_empty" new_UsrHarmTable_empty) :sndobj_usrharmtable)

(defcfun ("_wrap_new_UsrHarmTable" new_UsrHarmTable) :sndobj_usrharmtable
  (L :long)
  (harm :int)
  (amps :float*))

(defcfun ("_wrap_delete_UsrHarmTable" delete_UsrHarmTable) :void
  (self :sndobj_usrharmtable))

(defcfun ("_wrap_TrisegTable_SetCurve" TrisegTable_SetCurve) :void
  (self :sndobj_trisegtable)
  (init :float)
  (seg1 :float)
  (p1 :float)
  (seg2 :float)
  (p2 :float)
  (seg3 :float)
  (fin :float)
  (type :float))

(defcfun ("_wrap_TrisegTable_SetCurve_points" TrisegTable_SetCurve_points) :void
  (self :sndobj_trisegtable)
  (TSPoints :float*)
  (type :float))

(defcfun ("_wrap_TrisegTable_ErrorMessage" TrisegTable_ErrorMessage) :string
  (self :sndobj_trisegtable))

(defcfun ("_wrap_TrisegTable_MakeTable" TrisegTable_MakeTable) :short
  (self :sndobj_trisegtable))

(defcfun ("_wrap_new_TrisegTable_empty" new_TrisegTable_empty) :sndobj_trisegtable)

(defcfun ("_wrap_new_TrisegTable" new_TrisegTable) :sndobj_trisegtable
  (L :long)
  (init :float)
  (seg1 :float)
  (p1 :float)
  (seg2 :float)
  (p2 :float)
  (seg3 :float)
  (fin :float)
  (type :float))

(defcfun ("_wrap_new_TrisegTable_points" new_TrisegTable_points) :sndobj_trisegtable
  (L :long)
  (TSPoints :float*)
  (type :float))

(defcfun ("_wrap_delete_TrisegTable" delete_TrisegTable) :void
  (self :sndobj_trisegtable))

(defcfun ("_wrap_EnvTable_SetEnvelope" EnvTable_SetEnvelope) :void
  (self :sndobj_envtable)
  (segments :int)
  (start :float)
  (points :float*)
  (lengths :float*)
  (type :float))

(defcfun ("_wrap_EnvTable_ErrorMessage" EnvTable_ErrorMessage) :string
  (self :sndobj_envtable))

(defcfun ("_wrap_EnvTable_MakeTable" EnvTable_MakeTable) :short
  (self :sndobj_envtable))

(defcfun ("_wrap_new_EnvTable_empty" new_EnvTable_empty) :sndobj_envtable)

(defcfun ("_wrap_new_EnvTable" new_EnvTable) :sndobj_envtable
  (L :long)
  (segments :int)
  (start :float)
  (points :float*)
  (lengths :float*)
  (type :float))

(defcfun ("_wrap_delete_EnvTable" delete_EnvTable) :void
  (self :sndobj_envtable))

(defcfun ("_wrap_SndTable_SetInput" SndTable_SetInput) :void
  (self :sndobj_sndtable)
  (L :long)
  (input :sndobj_sndfio)
  (channel :short))

(defcfun ("_wrap_SndTable_ErrorMessage" SndTable_ErrorMessage) :string
  (self :sndobj_sndtable))

(defcfun ("_wrap_SndTable_MakeTable" SndTable_MakeTable) :short
  (self :sndobj_sndtable))

(defcfun ("_wrap_new_SndTable_empty" new_SndTable_empty) :sndobj_sndtable)

(defcfun ("_wrap_new_SndTable" new_SndTable) :sndobj_sndtable
  (L :long)
  (input :sndobj_sndfio)
  (channel :short))

(defcfun ("_wrap_delete_SndTable" delete_SndTable) :void
  (self :sndobj_sndtable))

(defcfun ("_wrap_PlnTable_SetPln" PlnTable_SetPln) :void
  (self :sndobj_plntable)
  (order :int)
  (coefs :double*)
  (range :float))

(defcfun ("_wrap_PlnTable_ErrorMessage" PlnTable_ErrorMessage) :string
  (self :sndobj_plntable))

(defcfun ("_wrap_PlnTable_MakeTable" PlnTable_MakeTable) :short
  (self :sndobj_plntable))

(defcfun ("_wrap_new_PlnTable_empty" new_PlnTable_empty) :sndobj_plntable)

(defcfun ("_wrap_new_PlnTable" new_PlnTable) :sndobj_plntable
  (L :long)
  (order :int)
  (coefs :double*)
  (range :float))

(defcfun ("_wrap_delete_PlnTable" delete_PlnTable) :void
  (self :sndobj_plntable))

(defcfun ("_wrap_HammingTable_SetParam" HammingTable_SetParam) :void
  (self :sndobj_hammingtable)
  (L :long)
  (alpha :float))

(defcfun ("_wrap_HammingTable_ErrorMessage" HammingTable_ErrorMessage) :string
  (self :sndobj_hammingtable))

(defcfun ("_wrap_HammingTable_MakeTable" HammingTable_MakeTable) :short
  (self :sndobj_hammingtable))

(defcfun ("_wrap_new_HammingTable_empty" new_HammingTable_empty) :sndobj_hammingtable)

(defcfun ("_wrap_new_HammingTable" new_HammingTable) :sndobj_hammingtable
  (L :long)
  (alpha :float))

(defcfun ("_wrap_delete_HammingTable" delete_HammingTable) :void
  (self :sndobj_hammingtable))

(defcfun ("_wrap_NoteTable_SetFreqInterval" NoteTable_SetFreqInterval) :void
  (self :sndobj_notetable)
  (lowerfreq :float)
  (upperfreq :float))

(defcfun ("_wrap_NoteTable_SetNoteInterval" NoteTable_SetNoteInterval) :void
  (self :sndobj_notetable)
  (lowernote :short)
  (uppernote :short))

(defcfun ("_wrap_new_NoteTable_empty" new_NoteTable_empty) :sndobj_notetable)

(defcfun ("_wrap_new_NoteTable" new_NoteTable) :sndobj_notetable
  (lowernote :short)
  (uppernote :short)
  (lowerfreq :float)
  (upperfreq :float))

(defcfun ("_wrap_delete_NoteTable" delete_NoteTable) :void
  (self :sndobj_notetable))

(defcfun ("_wrap_NoteTable_MakeTable" NoteTable_MakeTable) :short
  (self :sndobj_notetable))

(defcfun ("_wrap_NoteTable_ErrorMessage" NoteTable_ErrorMessage) :string
  (self :sndobj_notetable))

(defcfun ("_wrap_UsrDefTable_SetTable" UsrDefTable_SetTable) :void
  (self :sndobj_usrdeftable)
  (L :long)
  (values :float*))

(defcfun ("_wrap_UsrDefTable_ErrorMessage" UsrDefTable_ErrorMessage) :string
  (self :sndobj_usrdeftable))

(defcfun ("_wrap_UsrDefTable_MakeTable" UsrDefTable_MakeTable) :short
  (self :sndobj_usrdeftable))

(defcfun ("_wrap_new_UsrDefTable_empty" new_UsrDefTable_empty) :sndobj_usrdeftable)

(defcfun ("_wrap_new_UsrDefTable" new_UsrDefTable) :sndobj_usrdeftable
  (L :long)
  (values :float*))

(defcfun ("_wrap_delete_UsrDefTable" delete_UsrDefTable) :void
  (self :sndobj_usrdeftable))

(defcfun ("_wrap_LoPassTable_ErrorMessage" LoPassTable_ErrorMessage) :string
  (self :sndobj_lopasstable))

(defcfun ("_wrap_LoPassTable_MakeTable" LoPassTable_MakeTable) :short
  (self :sndobj_lopasstable))

(defcfun ("_wrap_LoPassTable_SetFreq" LoPassTable_SetFreq) :void
  (self :sndobj_lopasstable)
  (fr :float))

(defcfun ("_wrap_LoPassTable_SetSr" LoPassTable_SetSr) :void
  (self :sndobj_lopasstable)
  (sr :float))

(defcfun ("_wrap_new_LoPassTable" new_LoPassTable) :sndobj_lopasstable
  (impulsesize :int)
  (fr :float)
  (sr :float))

(defcfun ("_wrap_new_LoPassTable_empty" new_LoPassTable_empty) :sndobj_lopasstable)

(defcfun ("_wrap_delete_LoPassTable" delete_LoPassTable) :void
  (self :sndobj_lopasstable))

(defcfun ("_wrap_PVEnvTable_SetEnvelope" PVEnvTable_SetEnvelope) :void
  (self :sndobj_pvenvtable)
  (segments :int)
  (start :float)
  (points :float*)
  (lengths :float*)
  (type :float)
  (nyquistamp :float))

(defcfun ("_wrap_PVEnvTable_SetSr" PVEnvTable_SetSr) :void
  (self :sndobj_pvenvtable)
  (sr :float))

(defcfun ("_wrap_PVEnvTable_ErrorMessage" PVEnvTable_ErrorMessage) :string
  (self :sndobj_pvenvtable))

(defcfun ("_wrap_PVEnvTable_MakeTable" PVEnvTable_MakeTable) :short
  (self :sndobj_pvenvtable))

(defcfun ("_wrap_new_PVEnvTable_empty" new_PVEnvTable_empty) :sndobj_pvenvtable)

(defcfun ("_wrap_new_PVEnvTable" new_PVEnvTable) :sndobj_pvenvtable
  (L :long)
  (segments :int)
  (start :float)
  (points :float*)
  (lengths :float*)
  (type :float)
  (sr :float)
  (nyquistamp :float))

(defcfun ("_wrap_delete_PVEnvTable" delete_PVEnvTable) :void
  (self :sndobj_pvenvtable))

(defcfun ("_wrap_SpecEnvTable_MakeTable" SpecEnvTable_MakeTable) :short
  (self :sndobj_specenvtable))

(defcfun ("_wrap_new_SpecEnvTable_empty" new_SpecEnvTable_empty) :sndobj_specenvtable)

(defcfun ("_wrap_new_SpecEnvTable" new_SpecEnvTable) :sndobj_specenvtable
  (L :long)
  (segments :int)
  (start :float)
  (points :float*)
  (lengths :float*)
  (type :float)
  (nyquistamp :float))

(defcfun ("_wrap_delete_SpecEnvTable" delete_SpecEnvTable) :void
  (self :sndobj_specenvtable))

(defcfun ("_wrap_PVTable_SetTable" PVTable_SetTable) :void
  (self :sndobj_pvtable)
  (soundfile :sndobj_sndfio)
  (window :sndobj_table)
  (start :float)
  (end :float))

(defcfun ("_wrap_PVTable_MakeTable" PVTable_MakeTable) :short
  (self :sndobj_pvtable))

(defcfun ("_wrap_new_PVTable_empty" new_PVTable_empty) :sndobj_pvtable)

(defcfun ("_wrap_new_PVTable" new_PVTable) :sndobj_pvtable
  (L :int)
  (soundfile :sndobj_sndfio)
  (window :sndobj_table)
  (start :float)
  (end :float))

(defcfun ("_wrap_delete_PVTable" delete_PVTable) :void
  (self :sndobj_pvtable))

(defcfun ("_wrap_PVTable_ErrorMessage" PVTable_ErrorMessage) :string
  (self :sndobj_pvtable))

(defcfun ("_wrap_ImpulseTable_SetWindow" ImpulseTable_SetWindow) :void
  (self :sndobj_impulsetable)
  (window :sndobj_table))

(defcfun ("_wrap_ImpulseTable_MakeTable" ImpulseTable_MakeTable) :short
  (self :sndobj_impulsetable))

(defcfun ("_wrap_new_ImpulseTable_empty" new_ImpulseTable_empty) :sndobj_impulsetable)

(defcfun ("_wrap_new_ImpulseTable" new_ImpulseTable) :sndobj_impulsetable
  (L :long)
  (segments :int)
  (start :float)
  (points :float*)
  (lengths :float*)
  (type :float)
  (window :sndobj_table)
  (nyquistamp :float))

(defcfun ("_wrap_delete_ImpulseTable" delete_ImpulseTable) :void
  (self :sndobj_impulsetable))



(defcfun ("_wrap_new_Acos" new_Acos) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Acos" delete_Acos) :void
  (self :pointer))

(defcfun ("_wrap_new_Asin" new_Asin) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Asin" delete_Asin) :void
  (self :pointer))

(defcfun ("_wrap_new_Atan" new_Atan) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Atan" delete_Atan) :void
  (self :pointer))

(defcfun ("_wrap_new_Atan2" new_Atan2) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Atan2" delete_Atan2) :void
  (self :pointer))

(defcfun ("_wrap_new_Cos" new_Cos) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Cos" delete_Cos) :void
  (self :pointer))

(defcfun ("_wrap_new_Sin" new_Sin) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Sin" delete_Sin) :void
  (self :pointer))

(defcfun ("_wrap_new_Tan" new_Tan) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Tan" delete_Tan) :void
  (self :pointer))

(defcfun ("_wrap_new_Acosh" new_Acosh) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Acosh" delete_Acosh) :void
  (self :pointer))

(defcfun ("_wrap_new_Asinh" new_Asinh) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Asinh" delete_Asinh) :void
  (self :pointer))

(defcfun ("_wrap_new_Atanh" new_Atanh) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Atanh" delete_Atanh) :void
  (self :pointer))

(defcfun ("_wrap_new_Cosh" new_Cosh) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Cosh" delete_Cosh) :void
  (self :pointer))

(defcfun ("_wrap_new_Sinh" new_Sinh) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Sinh" delete_Sinh) :void
  (self :pointer))

(defcfun ("_wrap_new_Tanh" new_Tanh) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Tanh" delete_Tanh) :void
  (self :pointer))

(defcfun ("_wrap_new_Exp" new_Exp) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Exp" delete_Exp) :void
  (self :pointer))

(defcfun ("_wrap_new_Exp2" new_Exp2) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Exp2" delete_Exp2) :void
  (self :pointer))

(defcfun ("_wrap_new_Expm1" new_Expm1) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Expm1" delete_Expm1) :void
  (self :pointer))

(defcfun ("_wrap_new_Log" new_Log) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log" delete_Log) :void
  (self :pointer))

(defcfun ("_wrap_new_Log10" new_Log10) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log10" delete_Log10) :void
  (self :pointer))

(defcfun ("_wrap_new_Log2" new_Log2) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log2" delete_Log2) :void
  (self :pointer))

(defcfun ("_wrap_new_Log1p" new_Log1p) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log1p" delete_Log1p) :void
  (self :pointer))

(defcfun ("_wrap_new_Logb" new_Logb) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Logb" delete_Logb) :void
  (self :pointer))

(defcfun ("_wrap_new_Fabs" new_Fabs) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fabs" delete_Fabs) :void
  (self :pointer))

(defcfun ("_wrap_new_Cbrt" new_Cbrt) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Cbrt" delete_Cbrt) :void
  (self :pointer))

(defcfun ("_wrap_new_Hypot" new_Hypot) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Hypotf" new_Hypotf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Hypot" delete_Hypot) :void
  (self :pointer))

(defcfun ("_wrap_new_Pow" new_Pow) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Powf" new_Powf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Pow" delete_Pow) :void
  (self :pointer))

(defcfun ("_wrap_new_Sqrt" new_Sqrt) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Sqrt" delete_Sqrt) :void
  (self :pointer))

(defcfun ("_wrap_new_Ceil" new_Ceil) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Ceil" delete_Ceil) :void
  (self :pointer))

(defcfun ("_wrap_new_Floor" new_Floor) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Floor" delete_Floor) :void
  (self :pointer))

(defcfun ("_wrap_new_Fdim" new_Fdim) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fdim" delete_Fdim) :void
  (self :pointer))

(defcfun ("_wrap_new_Fmax" new_Fmax) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Fmaxf" new_Fmaxf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fmax" delete_Fmax) :void
  (self :pointer))

(defcfun ("_wrap_new_Fmin" new_Fmin) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Fminf" new_Fminf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fmin" delete_Fmin) :void
  (self :pointer))

(defcfun ("_wrap_new_Multiply" new_Multiply) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Multiplyf" new_Multiplyf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Multiply" delete_Multiply) :void
  (self :pointer))

(defcfun ("_wrap_new_Divide" new_Divide) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Dividef" new_Dividef) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Divide" delete_Divide) :void
  (self :pointer))

(defcfun ("_wrap_new_Add" new_Add) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Addf" new_Addf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Add" delete_Add) :void
  (self :pointer))

(defcfun ("_wrap_new_Subtract" new_Subtract) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Subtractf" new_Subtractf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Subtract" delete_Subtract) :void
  (self :pointer))

(defcfun ("_wrap_new_GreaterThan" new_GreaterThan) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_GreaterThanf" new_GreaterThanf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_GreaterThan" delete_GreaterThan) :void
  (self :pointer))

(defcfun ("_wrap_new_GreaterThanEqual" new_GreaterThanEqual) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_GreaterThanEqualf" new_GreaterThanEqualf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_GreaterThanEqual" delete_GreaterThanEqual) :void
  (self :pointer))

(defcfun ("_wrap_new_LessThan" new_LessThan) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_LessThanf" new_LessThanf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_LessThan" delete_LessThan) :void
  (self :pointer))

(defcfun ("_wrap_new_LessThanEqual" new_LessThanEqual) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_LessThanEqualf" new_LessThanEqualf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_LessThanEqual" delete_LessThanEqual) :void
  (self :pointer))

(defcfun ("_wrap_new_And" new_And) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Andf" new_Andf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_And" delete_And) :void
  (self :pointer))

(defcfun ("_wrap_new_Or" new_Or) :pointer
  (input :pointer)
  (input2 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Orf" new_Orf) :pointer
  (input :pointer)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Or" delete_Or) :void
  (self :pointer))

(defcfun ("_wrap_new_Not" new_Not) :pointer
  (input :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Not" delete_Not) :void
  (self :pointer))

(defcfun ("_wrap_new_If" new_If) :pointer
  (input :pointer)
  (input2 :pointer)
  (input3 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Iffs" new_Iffs) :pointer
  (input :pointer)
  (input2 :float)
  (input3 :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Ifsf" new_Ifsf) :pointer
  (input :pointer)
  (input2 :pointer)
  (input3 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Ifff" new_Ifff) :pointer
  (input :pointer)
  (input2 :float)
  (input3 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_If" delete_If) :void
  (self :pointer))

(defcfun ("_wrap_SndObj_DoProcess" SndObj_DoProcess) :short
  (self :sndobj))

(defcfun ("_wrap_Oscil_DoProcess" Oscil_DoProcess) :short
  (self :sndobj_oscil))

(defcfun ("_wrap_Oscili_DoProcess" Oscili_DoProcess) :short
  (self :sndobj_oscili))

(defcfun ("_wrap_Oscilt_DoProcess" Oscilt_DoProcess) :short
  (self :sndobj_oscilt))

(defcfun ("_wrap_FastOsc_DoProcess" FastOsc_DoProcess) :short
  (self :sndobj_fastosc))

(defcfun ("_wrap_Osc_DoProcess" Osc_DoProcess) :short
  (self :sndobj_osc))

(defcfun ("_wrap_Osci_DoProcess" Osci_DoProcess) :short
  (self :sndobj_osci))

(defcfun ("_wrap_SndIn_DoProcess" SndIn_DoProcess) :short
  (self :sndobj_sndin))


(defcfun ("_wrap_SndRead_DoProcess" SndRead_DoProcess) :short
  (self :sndobj_sndread))

(defcfun ("_wrap_ADSR_DoProcess" ADSR_DoProcess) :short
  (self :sndobj_adsr))

(defcfun ("_wrap_IADSR_DoProcess" IADSR_DoProcess) :short
  (self :sndobj_iadsr))

(defcfun ("_wrap_Buzz_DoProcess" Buzz_DoProcess) :short
  (self :sndobj_buzz))

(defcfun ("_wrap_Balance_DoProcess" Balance_DoProcess) :short
  (self :sndobj_balance))

(defcfun ("_wrap_DelayLine_DoProcess" DelayLine_DoProcess) :short
  (self :sndobj_delayLine))

(defcfun ("_wrap_Tap_DoProcess" Tap_DoProcess) :short
  (self :sndobj_tap))

(defcfun ("_wrap_Tapi_DoProcess" Tapi_DoProcess) :short
  (self :sndobj_tapi))

(defcfun ("_wrap_Comb_DoProcess" Comb_DoProcess) :short
  (self :sndobj_comb))

(defcfun ("_wrap_Allpass_DoProcess" Allpass_DoProcess) :short
  (self :sndobj_allpass))

(defcfun ("_wrap_StringFlt_DoProcess" StringFlt_DoProcess) :short
  (self :sndobj_stringflt))

(defcfun ("_wrap_Pluck_DoProcess" Pluck_DoProcess) :short
  (self :sndobj_pluck))

(defcfun ("_wrap_VDelay_DoProcess" VDelay_DoProcess) :short
  (self :sndobj_vdelay))

(defcfun ("_wrap_Pitch_DoProcess" Pitch_DoProcess) :short
  (self :sndobj_pitch))

(defcfun ("_wrap_SndLoop_DoProcess" SndLoop_DoProcess) :short
  (self :pointer))

(defcfun ("_wrap_FIR_DoProcess" FIR_DoProcess) :short
  (self :sndobj_fir))

(defcfun ("_wrap_Filter_DoProcess" Filter_DoProcess) :short
  (self :sndobj_filter))

(defcfun ("_wrap_TpTz_DoProcess" TpTz_DoProcess) :short
  (self :sndobj_tptz))

(defcfun ("_wrap_Reson_DoProcess" Reson_DoProcess) :short
  (self :sndobj_reson))

(defcfun ("_wrap_Lp_DoProcess" Lp_DoProcess) :short
  (self :sndobj_lp))

(defcfun ("_wrap_ButtBP_DoProcess" ButtBP_DoProcess) :short
  (self :sndobj_buttbp))

(defcfun ("_wrap_ButtBR_DoProcess" ButtBR_DoProcess) :short
  (self :sndobj_buttbr))

(defcfun ("_wrap_ButtHP_DoProcess" ButtHP_DoProcess) :short
  (self :sndobj_butthp))

(defcfun ("_wrap_ButtLP_DoProcess" ButtLP_DoProcess) :short
  (self :sndobj_buttlp))




(defcfun ("_wrap_Ap_DoProcess" Ap_DoProcess) :short
  (self :sndobj_ap))

(defcfun ("_wrap_LoPass_DoProcess" LoPass_DoProcess) :short
  (self :sndobj_lopass))

(defcfun ("_wrap_HiPass_DoProcess" HiPass_DoProcess) :short
  (self :sndobj_hipass))

(defcfun ("_wrap_Hilb_DoProcess" Hilb_DoProcess) :short
  (self :sndobj_hilb))

(defcfun ("_wrap_SyncGrain_DoProcess" SyncGrain_DoProcess) :short
  (self :sndobj_syncgrain))

(defcfun ("_wrap_Mixer_DoProcess" Mixer_DoProcess) :short
  (self :sndobj_mixer))

(defcfun ("_wrap_Pan_DoProcess" Pan_DoProcess) :short
  (self :sndobj_pan))

(defcfun ("_wrap_Gain_DoProcess" Gain_DoProcess) :short
  (self :sndobj_gain))

(defcfun ("_wrap_Interp_DoProcess" Interp_DoProcess) :short
  (self :sndobj_interp))

(defcfun ("_wrap_Phase_DoProcess" Phase_DoProcess) :short
  (self :sndobj_phase))

(defcfun ("_wrap_Ring_DoProcess" Ring_DoProcess) :short
  (self :sndobj_ring))

(defcfun ("_wrap_Unit_DoProcess" Unit_DoProcess) :short
  (self :sndobj_unit))

(defcfun ("_wrap_Lookup_DoProcess" Lookup_DoProcess) :short
  (self :sndobj_lookup))

(defcfun ("_wrap_Lookupi_DoProcess" Lookupi_DoProcess) :short
  (self :sndobj_lookupi))

(defcfun ("_wrap_Rand_DoProcess" Rand_DoProcess) :short
  (self :sndobj_rand))

(defcfun ("_wrap_PhOscili_DoProcess" PhOscili_DoProcess) :short
  (self :sndobj_phoscili))

(defcfun ("_wrap_Randh_DoProcess" Randh_DoProcess) :short
  (self :sndobj_randh))

(defcfun ("_wrap_Randi_DoProcess" Randi_DoProcess) :short
  (self :sndobj_randi))

(defcfun ("_wrap_FFT_DoProcess" FFT_DoProcess) :short
  (self :sndobj_fft))

(defcfun ("_wrap_IFFT_DoProcess" IFFT_DoProcess) :short
  (self :sndobj_ifft))

(defcfun ("_wrap_PVA_DoProcess" PVA_DoProcess) :short
  (self :sndobj_pva))

(defcfun ("_wrap_PVS_DoProcess" PVS_DoProcess) :short
  (self :sndobj_pvs))

(defcfun ("_wrap_PVRead_DoProcess" PVRead_DoProcess) :short
  (self :sndobj_pvread))

(defcfun ("_wrap_IFGram_DoProcess" IFGram_DoProcess) :short
  (self :sndobj_ifgram))

(defcfun ("_wrap_SinAnal_DoProcess" SinAnal_DoProcess) :short
  (self :sndobj_sinanal))

(defcfun ("_wrap_SinSyn_DoProcess" SinSyn_DoProcess) :short
  (self :sndobj_sinsyn))

(defcfun ("_wrap_ReSyn_DoProcess" ReSyn_DoProcess) :short
  (self :sndobj_resyn))

(defcfun ("_wrap_AdSyn_DoProcess" AdSyn_DoProcess) :short
  (self :sndobj_adsyn))

(defcfun ("_wrap_IFAdd_DoProcess" IFAdd_DoProcess) :short
  (self :sndobj_ifadd))

(defcfun ("_wrap_SpecMult_DoProcess" SpecMult_DoProcess) :short
  (self :sndobj_specmult))

(defcfun ("_wrap_SpecInterp_DoProcess" SpecInterp_DoProcess) :short
  (self :sndobj_specinterp))

(defcfun ("_wrap_PVMask_DoProcess" PVMask_DoProcess) :short
  (self :sndobj_pvmask))

(defcfun ("_wrap_PVTransp_DoProcess" PVTransp_DoProcess) :short
  (self :sndobj_pvtransp))

(defcfun ("_wrap_PVMix_DoProcess" PVMix_DoProcess) :short
  (self :sndobj_pvmix))

(defcfun ("_wrap_PVBlur_DoProcess" PVBlur_DoProcess) :short
  (self :sndobj_pvblur))

(defcfun ("_wrap_PVFilter_DoProcess" PVFilter_DoProcess) :short
  (self :sndobj_pvfilter))

(defcfun ("_wrap_PVMorph_DoProcess" PVMorph_DoProcess) :short
  (self :sndobj_pvmorph))

(defcfun ("_wrap_SpecPolar_DoProcess" SpecPolar_DoProcess) :short
  (self :sndobj_specpolar))

(defcfun ("_wrap_SpecSplit_DoProcess" SpecSplit_DoProcess) :short
  (self :sndobj_specsplit))

(defcfun ("_wrap_SpecThresh_DoProcess" SpecThresh_DoProcess) :short
  (self :sndobj_specthresh))

(defcfun ("_wrap_SpecVoc_DoProcess" SpecVoc_DoProcess) :short
  (self :sndobj_specvoc))

(defcfun ("_wrap_SpecCart_DoProcess" SpecCart_DoProcess) :short
  (self :sndobj_speccart))

(defcfun ("_wrap_SpecCombine_DoProcess" SpecCombine_DoProcess) :short
  (self :sndobj_speccombine))

(defcfun ("_wrap_SpecIn_DoProcess" SpecIn_DoProcess) :short
  (self :sndobj_specin))

(defcfun ("_wrap_Convol_DoProcess" Convol_DoProcess) :short
  (self :sndobj_convol))





(defcfun ("_wrap_new_Acos" new_Acos) :sa_acos
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Acos" delete_Acos) :void
  (self :sa_acos))

(defcfun ("_wrap_Acos_DoProcess" Acos_DoProcess) :short
  (self :sa_acos))

(defcfun ("_wrap_new_Asin" new_Asin) :sa_asin
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Asin" delete_Asin) :void
  (self :sa_asin))

(defcfun ("_wrap_Asin_DoProcess" Asin_DoProcess) :short
  (self :sa_asin))

(defcfun ("_wrap_new_Atan" new_Atan) :sa_Atan
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Atan" delete_Atan) :void
  (self :sa_Atan))

(defcfun ("_wrap_Atan_DoProcess" Atan_DoProcess) :short
  (self :sa_Atan))

(defcfun ("_wrap_new_Atan2" new_Atan2) :sa_Atan2
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Atan2" delete_Atan2) :void
  (self :sa_Atan2))

(defcfun ("_wrap_Atan2_DoProcess" Atan2_DoProcess) :short
  (self :sa_Atan2))

(defcfun ("_wrap_new_Cos" new_Cos) :sa_Cos
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Cos" delete_Cos) :void
  (self :sa_Cos))

(defcfun ("_wrap_Cos_DoProcess" Cos_DoProcess) :short
  (self :sa_Cos))

(defcfun ("_wrap_new_Sin" new_Sin) :sa_Sin
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Sin" delete_Sin) :void
  (self :sa_Sin))

(defcfun ("_wrap_Sin_DoProcess" Sin_DoProcess) :short
  (self :sa_Sin))

(defcfun ("_wrap_new_Tan" new_Tan) :sa_Tan
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Tan" delete_Tan) :void
  (self :sa_Tan))

(defcfun ("_wrap_Tan_DoProcess" Tan_DoProcess) :short
  (self :sa_Tan))

(defcfun ("_wrap_new_Acosh" new_Acosh) :sa_Acosh
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Acosh" delete_Acosh) :void
  (self :sa_Acosh))

(defcfun ("_wrap_Acosh_DoProcess" Acosh_DoProcess) :short
  (self :sa_Acosh))

(defcfun ("_wrap_new_Asinh" new_Asinh) :sa_Asinh
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Asinh" delete_Asinh) :void
  (self :sa_Asinh))

(defcfun ("_wrap_Asinh_DoProcess" Asinh_DoProcess) :short
  (self :sa_Asinh))

(defcfun ("_wrap_new_Atanh" new_Atanh) :sa_Atanh
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Atanh" delete_Atanh) :void
  (self :sa_Atanh))

(defcfun ("_wrap_Atanh_DoProcess" Atanh_DoProcess) :short
  (self :sa_Atanh))

(defcfun ("_wrap_new_Cosh" new_Cosh) :sa_Cosh
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Cosh" delete_Cosh) :void
  (self :sa_Cosh))

(defcfun ("_wrap_Cosh_DoProcess" Cosh_DoProcess) :short
  (self :sa_Cosh))

(defcfun ("_wrap_new_Sinh" new_Sinh) :sa_Sinh
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Sinh" delete_Sinh) :void
  (self :sa_Sinh))

(defcfun ("_wrap_Sinh_DoProcess" Sinh_DoProcess) :short
  (self :sa_Sinh))

(defcfun ("_wrap_new_Tanh" new_Tanh) :sa_Tanh
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Tanh" delete_Tanh) :void
  (self :sa_Tanh))

(defcfun ("_wrap_Tanh_DoProcess" Tanh_DoProcess) :short
  (self :sa_Tanh))

(defcfun ("_wrap_new_Exp" new_Exp) :sa_Exp
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Exp" delete_Exp) :void
  (self :sa_Exp))

(defcfun ("_wrap_Exp_DoProcess" Exp_DoProcess) :short
  (self :sa_Exp))

(defcfun ("_wrap_new_Exp2" new_Exp2) :sa_Exp2
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Exp2" delete_Exp2) :void
  (self :sa_Exp2))

(defcfun ("_wrap_Exp2_DoProcess" Exp2_DoProcess) :short
  (self :sa_Exp2))

(defcfun ("_wrap_new_Expm1" new_Expm1) :sa_Expm1
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Expm1" delete_Expm1) :void
  (self :sa_Expm1))

(defcfun ("_wrap_Expm1_DoProcess" Expm1_DoProcess) :short
  (self :sa_Expm1))

(defcfun ("_wrap_new_Log" new_Log) :sa_Log
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log" delete_Log) :void
  (self :sa_Log))

(defcfun ("_wrap_Log_DoProcess" Log_DoProcess) :short
  (self :sa_Log))

(defcfun ("_wrap_new_Log10" new_Log10) :sa_Log10
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log10" delete_Log10) :void
  (self :sa_Log10))

(defcfun ("_wrap_Log10_DoProcess" Log10_DoProcess) :short
  (self :sa_Log10))

(defcfun ("_wrap_new_Log2" new_Log2) :sa_Log2
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log2" delete_Log2) :void
  (self :sa_Log2))

(defcfun ("_wrap_Log2_DoProcess" Log2_DoProcess) :short
  (self :sa_Log2))

(defcfun ("_wrap_new_Log1p" new_Log1p) :sa_Log1p
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Log1p" delete_Log1p) :void
  (self :sa_Log1p))

(defcfun ("_wrap_Log1p_DoProcess" Log1p_DoProcess) :short
  (self :sa_Log1p))

(defcfun ("_wrap_new_Logb" new_Logb) :sa_Logb
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Logb" delete_Logb) :void
  (self :sa_Logb))

(defcfun ("_wrap_Logb_DoProcess" Logb_DoProcess) :short
  (self :sa_Logb))

(defcfun ("_wrap_new_Fabs" new_Fabs) :sa_Fabs
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fabs" delete_Fabs) :void
  (self :sa_Fabs))

(defcfun ("_wrap_Fabs_DoProcess" Fabs_DoProcess) :short
  (self :sa_Fabs))

(defcfun ("_wrap_new_Cbrt" new_Cbrt) :sa_Cbrt
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Cbrt" delete_Cbrt) :void
  (self :sa_Cbrt))

(defcfun ("_wrap_Cbrt_DoProcess" Cbrt_DoProcess) :short
  (self :sa_Cbrt))

(defcfun ("_wrap_new_Hypot" new_Hypot) :sa_Hypot
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Hypotf" new_Hypotf) :sa_Hypot
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Hypot" delete_Hypot) :void
  (self :sa_Hypot))

(defcfun ("_wrap_Hypot_DoProcess" Hypot_DoProcess) :short
  (self :sa_Hypot))

(defcfun ("_wrap_new_Pow" new_Pow) :sa_Pow
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Powf" new_Powf) :sa_Pow
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Pow" delete_Pow) :void
  (self :sa_Pow))

(defcfun ("_wrap_Pow_DoProcess" Pow_DoProcess) :short
  (self :sa_Pow))

(defcfun ("_wrap_new_Sqrt" new_Sqrt) :sa_Sqrt
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Sqrt" delete_Sqrt) :void
  (self :sa_Sqrt))

(defcfun ("_wrap_Sqrt_DoProcess" Sqrt_DoProcess) :short
  (self :sa_Sqrt))

(defcfun ("_wrap_new_Ceil" new_Ceil) :sa_Ceil
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Ceil" delete_Ceil) :void
  (self :sa_Ceil))

(defcfun ("_wrap_Ceil_DoProcess" Ceil_DoProcess) :short
  (self :sa_Ceil))

(defcfun ("_wrap_new_Floor" new_Floor) :sa_Floor
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Floor" delete_Floor) :void
  (self :sa_Floor))

(defcfun ("_wrap_Floor_DoProcess" Floor_DoProcess) :short
  (self :sa_Floor))

(defcfun ("_wrap_new_Fdim" new_Fdim) :sa_Fdim
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fdim" delete_Fdim) :void
  (self :sa_Fdim))

(defcfun ("_wrap_Fdim_DoProcess" Fdim_DoProcess) :short
  (self :sa_Fdim))

(defcfun ("_wrap_new_Fmax" new_Fmax) :sa_Fmax
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Fmaxf" new_Fmaxf) :sa_Fmax
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fmax" delete_Fmax) :void
  (self :sa_Fmax))

(defcfun ("_wrap_Fmax_DoProcess" Fmax_DoProcess) :short
  (self :sa_Fmax))

(defcfun ("_wrap_new_Fmin" new_Fmin) :sa_Fmin
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Fminf" new_Fminf) :sa_Fmin
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Fmin" delete_Fmin) :void
  (self :sa_Fmin))

(defcfun ("_wrap_Fmin_DoProcess" Fmin_DoProcess) :short
  (self :sa_Fmin))

(defcfun ("_wrap_new_Multiply" new_Multiply) :sa_Multiply
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Multiplyf" new_Multiplyf) :sa_Multiply
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Multiply" delete_Multiply) :void
  (self :sa_Multiply))

(defcfun ("_wrap_Multiply_DoProcess" Multiply_DoProcess) :short
  (self :sa_Multiply))

(defcfun ("_wrap_new_Divide" new_Divide) :sa_Divide
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Dividef" new_Dividef) :sa_Divide
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Divide" delete_Divide) :void
  (self :sa_Divide))

(defcfun ("_wrap_Divide_DoProcess" Divide_DoProcess) :short
  (self :sa_Divide))

(defcfun ("_wrap_new_Add" new_Add) :sa_Add
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Addf" new_Addf) :sa_Add
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Add" delete_Add) :void
  (self :sa_Add))

(defcfun ("_wrap_Add_DoProcess" Add_DoProcess) :short
  (self :sa_Add))

(defcfun ("_wrap_new_Subtract" new_Subtract) :sa_Subtract
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Subtractf" new_Subtractf) :sa_Subtract
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Subtract" delete_Subtract) :void
  (self :sa_Subtract))

(defcfun ("_wrap_Subtract_DoProcess" Subtract_DoProcess) :short
  (self :sa_Subtract))

(defcfun ("_wrap_new_GreaterThan" new_GreaterThan) :sa_GreaterThan
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_GreaterThanf" new_GreaterThanf) :sa_GreaterThan
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_GreaterThan" delete_GreaterThan) :void
  (self :sa_GreaterThan))

(defcfun ("_wrap_GreaterThan_DoProcess" GreaterThan_DoProcess) :short
  (self :sa_GreaterThan))

(defcfun ("_wrap_new_GreaterThanEqual" new_GreaterThanEqual) :sa_GreaterThanEqual
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_GreaterThanEqualf" new_GreaterThanEqualf) :sa_GreaterThanEqual
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_GreaterThanEqual" delete_GreaterThanEqual) :void
  (self :sa_GreaterThanEqual))

(defcfun ("_wrap_GreaterThanEqual_DoProcess" GreaterThanEqual_DoProcess) :short
  (self :sa_GreaterThanEqual))

(defcfun ("_wrap_new_LessThan" new_LessThan) :sa_LessThan
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_LessThanf" new_LessThanf) :sa_LessThan
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_LessThan" delete_LessThan) :void
  (self :sa_LessThan))

(defcfun ("_wrap_LessThan_DoProcess" LessThan_DoProcess) :short
  (self :sa_LessThan))

(defcfun ("_wrap_new_LessThanEqual" new_LessThanEqual) :sa_LessThanEqual
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_LessThanEqualf" new_LessThanEqualf) :sa_LessThanEqual
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_LessThanEqual" delete_LessThanEqual) :void
  (self :sa_LessThanEqual))

(defcfun ("_wrap_LessThanEqual_DoProcess" LessThanEqual_DoProcess) :short
  (self :sa_LessThanEqual))

(defcfun ("_wrap_new_And" new_And) :sa_And
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Andf" new_Andf) :sa_And
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_And" delete_And) :void
  (self :sa_And))

(defcfun ("_wrap_And_DoProcess" And_DoProcess) :short
  (self :sa_And))

(defcfun ("_wrap_new_Or" new_Or) :sa_Or
  (input :sndobj)
  (input2 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Orf" new_Orf) :sa_Or
  (input :sndobj)
  (input2 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Or" delete_Or) :void
  (self :sa_Or))

(defcfun ("_wrap_Or_DoProcess" Or_DoProcess) :short
  (self :sa_Or))

(defcfun ("_wrap_new_Not" new_Not) :sa_Not
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Not" delete_Not) :void
  (self :sa_Not))

(defcfun ("_wrap_Not_DoProcess" Not_DoProcess) :short
  (self :sa_Not))

(defcfun ("_wrap_new_If" new_If) :sa_If
  (input :sndobj)
  (input2 :sndobj)
  (input3 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Iffs" new_Iffs) :sa_If
  (input :sndobj)
  (input2 :float)
  (input3 :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Ifsf" new_Ifsf) :sa_If
  (input :sndobj)
  (input2 :sndobj)
  (input3 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_new_Ifff" new_Ifff) :sa_If
  (input :sndobj)
  (input2 :float)
  (input3 :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_If" delete_If) :void
  (self :sa_If))

(defcfun ("_wrap_If_DoProcess" If_DoProcess) :short
  (self :sa_If))

(defcfun ("_wrap_new_RTOutput_empty" new_RTOutput_empty) :sa_RTOutput)

(defcfun ("_wrap_new_RTOutput" new_RTOutput) :sa_RTOutput
  (chan :int)
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_RTOutput" delete_RTOutput) :void
  (self :sa_RTOutput))

(defcfun ("_wrap_RTOutput_SetChan" RTOutput_SetChan) :void
  (self :sa_RTOutput)
  (chan :int))

(defcfun ("_wrap_RTOutput_DoProcess" RTOutput_DoProcess) :short
  (self :sa_RTOutput))

(defcfun ("_wrap_new_BusWrite_empty" new_BusWrite_empty) :sa_buswrite)

(defcfun ("_wrap_new_BusWrite" new_BusWrite) :sa_buswrite
  (bus :int)
  (input :sndobj)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_BusWrite" delete_BusWrite) :void
  (self :sa_buswrite))

(defcfun ("_wrap_BusWrite_SetBus" BusWrite_SetBus) :void
  (self :sa_buswrite)
  (bus :int))

(defcfun ("_wrap_BusWrite_DoProcess" BusWrite_DoProcess) :short
  (self :sa_buswrite))

(defcfun ("_wrap_new_BusRead_empty" new_BusRead_empty) :sa_busread)

(defcfun ("_wrap_new_BusRead" new_BusRead) :sa_busread
  (bus :int)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_BusRead" delete_BusRead) :void
  (self :sa_busread))

(defcfun ("_wrap_BusRead_SetBus" BusRead_SetBus) :void
  (self :sa_busread)
  (bus :int))

(defcfun ("_wrap_BusRead_DoProcess" BusRead_DoProcess) :short
  (self :sa_busread))

(defcfun ("_wrap_PVOCEXRead_Outchannel" PVOCEXRead_Outchannel) :sndobj
  (self :sa_PVOCEXRead)
  (channel :int))

(defcfun ("_wrap_new_PVOCEXRead_empty" new_PVOCEXRead_empty) :sa_PVOCEXRead)

(defcfun ("_wrap_new_PVOCEXRead" new_PVOCEXRead) :sa_PVOCEXRead
  (name :string)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_PVOCEXRead" delete_PVOCEXRead) :void
  (self :sa_PVOCEXRead))

(defcfun ("_wrap_PVOCEXRead_SetInput" PVOCEXRead_SetInput) :void
  (self :sa_PVOCEXRead)
  (name :string))

(defcfun ("_wrap_PVOCEXRead_DoProcess" PVOCEXRead_DoProcess) :short
  (self :sa_PVOCEXRead))

(defcfun ("_wrap_new_FloatSig_empty" new_FloatSig_empty) :sa_floatsig)

(defcfun ("_wrap_new_FloatSig" new_FloatSig) :sa_floatsig
  (val :float)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_FloatSig" delete_FloatSig) :void
  (self :sa_floatsig))

(defcfun ("_wrap_FloatSig_SetVal" FloatSig_SetVal) :void
  (self :sa_floatsig)
  (val :float))

(defcfun ("_wrap_FloatSig_DoProcess" FloatSig_DoProcess) :short
  (self :sa_floatsig))

(defcfun ("_wrap_RTRunAudio" RTRunAudio) :pointer
  (as :pointer))

(defcfun ("_wrap_paCallback" paCallback) :int
  (input :pointer)
  (output :pointer)
  (frameCount :unsigned-long)
  (timeInfo :pointer)
  (statusFlags :pointer)
  (userData :pointer))


(defcfun ("_wrap_new_Synth_empty" new_Synth_empty) :sa_Synth)

(defcfun ("_wrap_new_Synth" new_Synth) :sa_Synth
  (ObjNo :int)
  (InObjs :pointer)
  (duration :long)
  (synthFreeFunc :pointer)
  (vecsize :int)
  (sr :float))

(defcfun ("_wrap_delete_Synth" delete_Synth) :void
  (self :sa_Synth))

(defcfun ("_wrap_Synth_SetDuration" Synth_SetDuration) :void
  (self :sa_Synth)
  (duration :long))

(defcfun ("_wrap_Synth_GetDuration" Synth_GetDuration) :long
  (self :sa_Synth))

(defcfun ("_wrap_Synth_GetObjNo" Synth_GetObjNo) :int
  (self :sa_Synth))

(defcfun ("_wrap_Synth_DoProcess" Synth_DoProcess) :short
  (self :sa_Synth))

(defcfun ("_wrap_Synth_ErrorMessage" Synth_ErrorMessage) :string
  (self :sa_Synth))

(defcfun ("_wrap_Synth_Add" Synth_Add) :int
  (self :sa_Synth))

(defcfun ("_wrap_Synth_Free" Synth_Free) :int
  (self :sa_Synth))

(defcfun ("_wrap_Synth_Next" Synth_Next) :sa_Synth
  (self :sa_Synth))

(defcfun ("_wrap_Synth_Previous" Synth_Previous) :sa_Synth
  (self :sa_Synth))

(defcfun ("_wrap_RTAudioStream_Instance" RTAudioStream_Instance) :sa_RTAudioStream
  (sampleRate :float)
  (inputChannels :int)
  (outputChannels :int)
  (busses :int)
  (bufferSize :int))

(defcfun ("_wrap_RTAudioStream_Start" RTAudioStream_Start) :int
  (self :sa_RTAudioStream)
  (priority :int))

(defcfun ("_wrap_RTAudioStream_Stop" RTAudioStream_Stop) :int
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_GetSampleRate" RTAudioStream_GetSampleRate) :float
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_GetVectorSize" RTAudioStream_GetVectorSize) :int
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_GetPAStream" RTAudioStream_GetPAStream) :pointer
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_sampleRate_set" RTAudioStream_m_sampleRate_set) :void
  (self :sa_RTAudioStream)
  (m_sampleRate :float))

(defcfun ("_wrap_RTAudioStream_m_sampleRate_get" RTAudioStream_m_sampleRate_get) :float
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_inputChannels_set" RTAudioStream_m_inputChannels_set) :void
  (self :sa_RTAudioStream)
  (m_inputChannels :int))

(defcfun ("_wrap_RTAudioStream_m_inputChannels_get" RTAudioStream_m_inputChannels_get) :int
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_outputChannels_set" RTAudioStream_m_outputChannels_set) :void
  (self :sa_RTAudioStream)
  (m_outputChannels :int))

(defcfun ("_wrap_RTAudioStream_m_outputChannels_get" RTAudioStream_m_outputChannels_get) :int
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_busses_set" RTAudioStream_m_busses_set) :void
  (self :sa_RTAudioStream)
  (m_busses :int))

(defcfun ("_wrap_RTAudioStream_m_busses_get" RTAudioStream_m_busses_get) :int
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_bufferSize_set" RTAudioStream_m_bufferSize_set) :void
  (self :sa_RTAudioStream)
  (m_bufferSize :int))

(defcfun ("_wrap_RTAudioStream_m_bufferSize_get" RTAudioStream_m_bufferSize_get) :int
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_synthHead_set" RTAudioStream_m_synthHead_set) :void
  (self :sa_RTAudioStream)
  (m_synthHead :sa_Synth))

(defcfun ("_wrap_RTAudioStream_m_synthHead_get" RTAudioStream_m_synthHead_get) :sa_Synth
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_outputs_set" RTAudioStream_m_outputs_set) :void
  (self :sa_RTAudioStream)
  (m_outputs :pointer))

(defcfun ("_wrap_RTAudioStream_m_outputs_get" RTAudioStream_m_outputs_get) :pointer
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_inputs_set" RTAudioStream_m_inputs_set) :void
  (self :sa_RTAudioStream)
  (m_inputs :pointer))

(defcfun ("_wrap_RTAudioStream_m_inputs_get" RTAudioStream_m_inputs_get) :pointer
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_inbusses_set" RTAudioStream_m_inbusses_set) :void
  (self :sa_RTAudioStream)
  (m_inbusses :pointer))

(defcfun ("_wrap_RTAudioStream_m_inbusses_get" RTAudioStream_m_inbusses_get) :pointer
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_outbusses_set" RTAudioStream_m_outbusses_set) :void
  (self :sa_RTAudioStream)
  (m_outbusses :pointer))

(defcfun ("_wrap_RTAudioStream_m_outbusses_get" RTAudioStream_m_outbusses_get) :pointer
  (self :sa_RTAudioStream))

(defcfun ("_wrap_RTAudioStream_m_status_set" RTAudioStream_m_status_set) :void
  (self :sa_RTAudioStream)
  (m_status :int))

(defcfun ("_wrap_RTAudioStream_m_status_get" RTAudioStream_m_status_get) :int
  (self :sa_RTAudioStream))


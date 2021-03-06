;;; ****************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; ****************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2006/04/30 15:26:50 $

;;;
;;; Portmidi FFI for Gauche Scheme using c-wrapper
;;;

(select-module portmidi)

;; load Portmidi library
(define libportmidi 
  (if (equal? (car (sys-uname)) "Darwin")
      "/usr/local/lib/libportmidi.dylib"
      "/usr/local/lib/libportmidi.so"))

(if (not (file-exists? libportmidi))
    (errorf "PortMidi library ~S does not exit. Fix pathname in portmidi.scm" libportmidi)) 

(c-load-library libportmidi)
 
;; genwrapper gauche-portmidi /usr/local/src/portmidi/pm_common/portmidi.h  /usr/local/src/portmidi/porttime/porttime.h  /usr/local/lib/libportmidi.dylib > gauche-portmidi.scm

;; generated by c-wrapper

(define __GXX_ABI_VERSION 102)
(define __SCHAR_MAX__ 127)
(define __SHRT_MAX__ 32767)
(define __INT_MAX__ 2147483647)
(define __LONG_MAX__ 2147483647)
(define __LONG_LONG_MAX__ 9223372036854775807)
(define __WCHAR_MAX__ 2147483647)
(define __CHAR_BIT__ 8)
(define __FLT_EVAL_METHOD__ 0)
(define __FLT_RADIX__ 2)
(define __FLT_MANT_DIG__ 24)
(define __FLT_DIG__ 6)
(define __FLT_MIN_EXP__ (- 125))
(define __FLT_MIN_10_EXP__ (- 37))
(define __FLT_MAX_EXP__ 128)
(define __FLT_MAX_10_EXP__ 38)
(define __FLT_MAX__ 3.40282347e38)
(define __FLT_MIN__ 1.17549435e-38)
(define __FLT_EPSILON__ 1.1920929e-7)
(define __FLT_DENORM_MIN__ 1.40129846e-45)
(define __DBL_MANT_DIG__ 53)
(define __DBL_DIG__ 15)
(define __DBL_MIN_EXP__ (- 1021))
(define __DBL_MIN_10_EXP__ (- 307))
(define __DBL_MAX_EXP__ 1024)
(define __DBL_MAX_10_EXP__ 308)
(define __DBL_MAX__ 1.7976931348623157e308)
(define __DBL_MIN__ 2.2250738585072014e-308)
(define __DBL_EPSILON__ 2.220446049250313e-16)
(define __DBL_DENORM_MIN__ 5.0e-324)
(define __LDBL_MANT_DIG__ 53)
(define __LDBL_DIG__ 15)
(define __LDBL_MIN_EXP__ (- 1021))
(define __LDBL_MIN_10_EXP__ (- 307))
(define __LDBL_MAX_EXP__ 1024)
(define __LDBL_MAX_10_EXP__ 308)
(define __DECIMAL_DIG__ 17)
(define __VERSION__ "3.3 20030304 (Apple Computer, Inc. build 1666)")
(define __STDC_HOSTED__ 1)
(define __NO_INLINE__ 1)
(define __FINITE_MATH_ONLY__ 0)
(define _ARCH_PPC 1)
(define __BIG_ENDIAN__ 1)
(define _BIG_ENDIAN 1)
(define __ppc__ 1)
(define __POWERPC__ 1)
(define __NATURAL_ALIGNMENT__ 1)
(define __MACH__ 1)
(define __APPLE__ 1)
(define __GNUC__ 3)
(define __GNUC_MINOR__ 3)
(define __GNUC_PATCHLEVEL__ 0)
(define __APPLE_CC__ 1666)
(define __DYNAMIC__ 1)
(define ptNoError 0)
(define ptHostError (- 10000))
(define ptAlreadyStarted (+ (- 10000) 1))
(define ptAlreadyStopped (+ (+ (- 10000) 1) 1))
(define ptInsufficientMemory (+ (+ (+ (- 10000) 1) 1) 1))
(define <PtError> <c-int>)
(define <PtTimestamp> <c-long>)
(define <PtCallback> (make-c-func-ptr <c-void> (list <PtTimestamp> (ptr <c-void>))))
(define Pt_Start (make-c-func 'Pt_Start <PtError> (list <c-int> (ptr <PtCallback>) (ptr <c-void>))))
(define Pt_Stop (make-c-func 'Pt_Stop <PtError> (list)))
(define Pt_Started (make-c-func 'Pt_Started <c-int> (list)))
(define Pt_Time (make-c-func 'Pt_Time <PtTimestamp> (list)))
(define FALSE 0)
(define TRUE 1)
(define PM_DEFAULT_SYSEX_BUFFER_SIZE 1024)
(define pmNoError 0)
(define pmHostError (- 10000))
(define pmInvalidDeviceId (+ (- 10000) 1))
(define pmInsufficientMemory (+ (+ (- 10000) 1) 1))
(define pmBufferTooSmall (+ (+ (+ (- 10000) 1) 1) 1))
(define pmBufferOverflow (+ (+ (+ (+ (- 10000) 1) 1) 1) 1))
(define pmBadPtr (+ (+ (+ (+ (+ (- 10000) 1) 1) 1) 1) 1))
(define pmBadData (+ (+ (+ (+ (+ (+ (- 10000) 1) 1) 1) 1) 1) 1))
(define pmInternalError (+ (+ (+ (+ (+ (+ (+ (- 10000) 1) 1) 1) 1) 1) 1) 1))
(define pmBufferMaxSize (+ (+ (+ (+ (+ (+ (+ (+ (- 10000) 1) 1) 1) 1) 1) 1) 1) 1))
(define <PmError> <c-int>)
(define Pm_Initialize (make-c-func 'Pm_Initialize <PmError> (list)))
(define Pm_Terminate (make-c-func 'Pm_Terminate <PmError> (list)))
(define <PortMidiStream> <c-void>)
(define Pm_HasHostError (make-c-func 'Pm_HasHostError <c-int> (list (ptr <PortMidiStream>))))
(define Pm_GetErrorText (make-c-func 'Pm_GetErrorText (ptr <c-char>) (list <PmError>)))
(define Pm_GetHostErrorText (make-c-func 'Pm_GetHostErrorText <c-void> (list (ptr <c-char>) <c-uint>)))
(define HDRLENGTH 50)
(define PM_HOST_ERROR_MSG_LEN 256)
(define <PmDeviceID> <c-int>)
(define pmNoDevice (- 1))
(define <c-struct:G376> (alloc-c-struct 'G376))
(init-c-struct! <c-struct:G376> `((structVersion unquote <c-int>) (interf unquote (ptr <c-char>)) (name unquote (ptr <c-char>)) (input unquote <c-int>) (output unquote <c-int>) (opened unquote <c-int>)))
(define <PmDeviceInfo> <c-struct:G376>)
(define Pm_CountDevices (make-c-func 'Pm_CountDevices <c-int> (list)))
(define Pm_GetDefaultInputDeviceID (make-c-func 'Pm_GetDefaultInputDeviceID <PmDeviceID> (list)))
(define Pm_GetDefaultOutputDeviceID (make-c-func 'Pm_GetDefaultOutputDeviceID <PmDeviceID> (list)))
(define <PmTimestamp> <c-long>)
(define <PmTimeProcPtr> (make-c-func-ptr <PmTimestamp> (list (ptr <c-void>))))
(define Pm_GetDeviceInfo (make-c-func 'Pm_GetDeviceInfo (ptr <PmDeviceInfo>) (list <PmDeviceID>)))
(define Pm_OpenInput (make-c-func 'Pm_OpenInput <PmError> (list (ptr (ptr <PortMidiStream>)) <PmDeviceID> (ptr <c-void>) <c-long> <PmTimeProcPtr> (ptr <c-void>))))
(define Pm_OpenOutput (make-c-func 'Pm_OpenOutput <PmError> (list (ptr (ptr <PortMidiStream>)) <PmDeviceID> (ptr <c-void>) <c-long> <PmTimeProcPtr> (ptr <c-void>) <c-long>)))
(define PM_FILT_ACTIVE 1)
(define PM_FILT_SYSEX 2)
(define PM_FILT_CLOCK 4)
(define PM_FILT_PLAY 8)
(define PM_FILT_F9 16)
(define PM_FILT_TICK PM_FILT_F9)
(define PM_FILT_FD 32)
(define PM_FILT_UNDEFINED (logior PM_FILT_F9 PM_FILT_FD))
(define PM_FILT_RESET 64)
(define PM_FILT_REALTIME (logior (logior (logior (logior (logior PM_FILT_ACTIVE PM_FILT_SYSEX) PM_FILT_CLOCK) PM_FILT_PLAY) PM_FILT_UNDEFINED) PM_FILT_RESET))
(define PM_FILT_NOTE 128)
(define PM_FILT_CHANNEL_AFTERTOUCH 256)
(define PM_FILT_POLY_AFTERTOUCH 512)
(define PM_FILT_AFTERTOUCH (logior PM_FILT_CHANNEL_AFTERTOUCH PM_FILT_POLY_AFTERTOUCH))
(define PM_FILT_PROGRAM 1024)
(define PM_FILT_CONTROL 2048)
(define PM_FILT_PITCHBEND 4096)
(define PM_FILT_MTC 8192)
(define PM_FILT_SONG_POSITION 16384)
(define PM_FILT_SONG_SELECT 32768)
(define PM_FILT_TUNE 65536)
(define PM_FILT_SYSTEMCOMMON (logior (logior (logior PM_FILT_MTC PM_FILT_SONG_POSITION) PM_FILT_SONG_SELECT) PM_FILT_TUNE))
(define Pm_SetFilter (make-c-func 'Pm_SetFilter <PmError> (list (ptr <PortMidiStream>) <c-long>)))
(define Pm_SetChannelMask (make-c-func 'Pm_SetChannelMask <PmError> (list (ptr <PortMidiStream>) <c-int>)))
(define Pm_Abort (make-c-func 'Pm_Abort <PmError> (list (ptr <PortMidiStream>))))
(define Pm_Close (make-c-func 'Pm_Close <PmError> (list (ptr <PortMidiStream>))))
(define <PmMessage> <c-long>)
(define <c-struct:G377> (alloc-c-struct 'G377))
(init-c-struct! <c-struct:G377> `((message unquote <PmMessage>) (timestamp unquote <PmTimestamp>)))
(define <PmEvent> <c-struct:G377>)
(define Pm_Read (make-c-func 'Pm_Read <PmError> (list (ptr <PortMidiStream>) (ptr <PmEvent>) <c-long>)))
(define Pm_Poll (make-c-func 'Pm_Poll <PmError> (list (ptr <PortMidiStream>))))
(define Pm_Write (make-c-func 'Pm_Write <PmError> (list (ptr <PortMidiStream>) (ptr <PmEvent>) <c-long>)))
(define Pm_WriteShort (make-c-func 'Pm_WriteShort <PmError> (list (ptr <PortMidiStream>) <PmTimestamp> <c-long>)))
(define Pm_WriteSysEx (make-c-func 'Pm_WriteSysEx <PmError> (list (ptr <PortMidiStream>) <PmTimestamp> (ptr <c-uchar>))))

;; end generated

(define pm:filt-active PM_FILT_ACTIVE) 
(define pm:filt-sysex PM_FILT_SYSEX)
(define pm:filt-clock PM_FILT_CLOCK) 
(define pm:filt-play PM_FILT_PLAY) 
(define pm:filt-f9 PM_FILT_F9) 
(define pm:filt-fd PM_FILT_FD) 
(define pm:filt-reset PM_FILT_RESET) 
(define pm:filt-note PM_FILT_NOTE) 
(define pm:filt-channel-aftertouch PM_FILT_CHANNEL_AFTERTOUCH) 
(define pm:filt-poly-aftertouch PM_FILT_POLY_AFTERTOUCH) 
(define pm:filt-program PM_FILT_PROGRAM) 
(define pm:filt-control PM_FILT_CONTROL) 
(define pm:filt-pitchbend PM_FILT_PITCHBEND) 
(define pm:filt-mtc PM_FILT_MTC) 
(define pm:filt-song-position PM_FILT_SONG_POSITION) 
(define pm:filt-song-select PM_FILT_SONG_SELECT) 
(define pm:filt-tune PM_FILT_TUNE) 
(define pm:filt-tick PM_FILT_TICK)
(define pm:filt-undefined PM_FILT_UNDEFINED)
(define pm:filt-realtime PM_FILT_REALTIME)
(define pm:filt-aftertouch PM_FILT_AFTERTOUCH)
(define pm:filt-systemcommon PM_FILT_SYSTEMCOMMON)
(define *portmidi* #f) ; #t if loaded

(define host-error-text (make-string 256 #\*))

(define-macro (with-pm-error form)
  (let ((v (gensym)))
    `(let ((,v ,form))
       (if (not (= ,v pmNoError))
	   (if (= ,v pmHostError)
	       (begin ;; WARNING: will this work??
		 (Pm_GetHostErrorText host-error
				      (string-length host-error-text))
		 (errorf "Host error is: ~a" host-error))
	       (error (x->string (pm-get-error-text ,v))))
           ,v))))

(define (pm:PortMidi )
  ;; initializer, call before using lib
  (or *portmidi*
      (begin (Pm_Initialize)
             (set! *portmidi* #t))))

(define (pm:Message status data1 data2)
  ;; portmidi messages are just unsigneds
  (logior (logand (ash data2 16) #xFF0000)
          (logand (ash data1 08) #xFF00)
          (logand status #xFF)))

(define (pm:Message.status m)
  (logand m #xFF))

(define (pm:Message.data1 m)
  (logand (ash m -08) #xFF))

(define (pm:Message.data2 m)	
  (logand (ash m -16) #xFF))

;;; accessors 

(define (pm:DeviceInfo.interf ptr)
  (x->string (ref (deref ptr) 'interf)))

(define (pm:DeviceInfo.name ptr)
  (x->string (ref (deref ptr) 'name)))

(define (pm:DeviceInfo.input ptr)
  (if (= (ref (deref ptr) 'input) 0)
      #f
      #t))

(define (pm:DeviceInfo.output ptr)
  (if (= (ref (deref ptr) 'output) 0)
      #f
      #t))

(define (pm:DeviceInfo.opened ptr)
  (if (= (ref (deref ptr) 'opened) 0)
      #f
      #t))

;; event accessors

(define (pm:Event.message e . v)
  ;; WARNING: e is struct, not pointer to struct
  (if (null? v)
      (ref e 'message) ; (deref e)
      (begin (set! (ref e 'message) (car v))
	     (car v))))
    
(define (pm:Event.timestamp e . v)
  ;; WARNING: e is struct, not pointer
  (if (null? v)
      (ref e 'timestamp) ; (deref e)
      (begin (set! (ref e 'timestamp) (car v))
	     (car v))))

;;; functions

(define (pm:Initialize )
  (with-pm-error (Pm_Initialize)))

(define (pm:Terminate )
  (with-pm-error (Pm_Terminate)))


(define (pm:HasHostError pms) 
  (Pm_HasHostError pms))

(define (pm:GetErrorText err) 
  (x->string (Pm_GetErrorText err)))

; how do i do this?
;(progn
;  (defalien "pm-GetHostErrorText" void (a c-string) (b unsigned-int))
;  (defun GetHostErrorText () 
;    (pm-GetHostErrorText 256)))

(define (pm:CountDevices )
  (pm:PortMidi)
  (Pm_CountDevices ))

(define (pm:GetDefaultInputDeviceID )
  (let ((id (Pm_GetDefaultInputDeviceID  )))
    (if (= id pmNoDevice) #f id)))

(define (pm:GetDefaultOutputDeviceID )
  (let ((id (Pm_GetDefaultOutputDeviceID )))
    (if (= id pmNoDevice) #f id)))

(define (pm:OpenInput dev siz)
  ;; timer must be running before opening
  (unless (pt:Started) (pt:Start))
  (let ((*pmin* (make (ptr <PortMidiStream>))))
    (let ((err (Pm_OpenInput (ptr *pmin*)
			     dev
			     (make-null-ptr)
			     siz
			     (make-null-ptr)
			     (make-null-ptr))))
        (if (= err pmNoError)
           *pmin*
	   (error (pm:GetErrorText err))))))

; (load "/Lisp/portmidi/portmidi.scm")
; (select-module portmidi)
; (pm:PortMidi)
; (pt:Start)
; (define zzz (pm:OpenOutput 5 128 0))
; (define on (pm:Message #b10010000 60 64))
; (pm:WriteShort zzz (+ (pt:Time) 100) on)

(define (pm:OpenOutput dev siz lat)
  ;; timer must be running before opening
  (unless (pt:Started) (pt:Start))
  (let ((*pmout* (make (ptr <PortMidiStream>))))
    (let ((err (Pm_OpenOutput (ptr *pmout*)
			      dev
			      (make-null-ptr)
			      siz
			      (make-null-ptr)
			      (make-null-ptr) 
			      lat)))
      (if (= err pmNoError)
          *pmout*
          (error (pm:GetErrorText err))))))

(define (pm:SetFilter a filts) 
  (with-pm-error (Pm_SetFilter a filts)))

(define (pm:SetChannelMask pms msk)
  (with-pm-error (Pm_SetChannelMask pms msk)))

(define (pm:Abort pms)
  (with-pm-error (Pm_Abort pms)))

(define (pm:Close pms)
  (with-pm-error (Pm_Close pms)))

(define (pm:Read pms buf len) 
  (let ((res (Pm_Read pms buf len)))
    (if (< res 0)
        (error (pm:GetErrorText res))
        res)))

(define (pm:Poll pms)
  (let ((res (Pm_Poll pms)))
    (cond ((= res 0) #f)
          ((= res 1) #t)
          (else 
	   (error (pm:GetErrorText res))))))

(define (pm:Write pms buf len)
  (with-pm-error (Pm_Write pms buf len)))

(define (pm:WriteShort pms tim msg)
  (with-pm-error (Pm_WriteShort pms tim msg)))

(define (pm:WriteSysex pms tim str)
  (with-pm-error (Pm_WriteSysEx pms tim str)))

;; added api

(define (pm:EventBufferFree buf)
  (values))

(define (pm:EventBufferNew len)
  (make (make-c-array <PmEvent> len)))

(define (pm:EventBufferElt buf i)
  (c-array-ref buf i))

(define (pm:EventBufferSet buf ind tim msg)
  (let ((e (pm:EventBufferElt buf ind)))
    ;; WARNING: e is struct, not pointer to struct
    (set! (ref e 'timestamp) tim)
    (set! (ref e 'message) msg)
    (values)))

(define (pm:EventBufferMap fn buf end)
  (do ((i 0 (+ i 1))
       (e #f))       
      ((not (< i end)) 
       (values))
    ;; WARNING: e is struct, not pointer to struct
    (set! e (pm:EventBufferElt buf i))
    ( fn (pm:Event.message e) (pm:Event.timestamp e))))

(define (pm:GetDeviceInfo . id)
  (let ((getone
	 (lambda (id)
           (let ((d (Pm_GetDeviceInfo id)))
             (list ':id id
                   ':name (pm:DeviceInfo.name d)
                   ':type (if (pm:DeviceInfo.input d) ':input ':output)
                   ':open (pm:DeviceInfo.opened d))))))
    ;; make sure lib is initialized before checking devices
    (pm:PortMidi)
    (if (null? id)
	(let ((head (list #f)))
	  (do ((z (pm:CountDevices ))
	       (i 0 (+ i 1))
	       (tail head (cdr tail)))
	      ((= i z) (cdr head))
	    (set-cdr! tail (list (getone i)))))
	(getone (car id)))))

;;; porttime.h

(define (pt:Started )
  (let ((res (Pt_Started)))
    (if (= res FALSE) #f #t)))

(define (pt:Start )
  ;; NB: This has to be called before opening output or input.
  ;; it seems that if its called 2x we get an error.
  (unless (pt:Started)
    (with-pm-error (Pt_Start 1 (make-null-ptr) (make-null-ptr))))
  (values))

(define (pt:Stop )
  (when (pt:Started)
    (with-pm-error (Pt_Stop)))
  (values))

(define (pt:Time )
  (Pt_Time))

;(load "portmidi-recv.scm")

(provide "portmidi")

;;; eof


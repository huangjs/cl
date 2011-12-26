;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.9 $
;;; $Date: 2006/05/01 14:46:06 $


(define-module portmidi
    (use c-wrapper)
    (export pm:filt-active pm:filt-sysex pm:filt-clock pm:filt-play pm:filt-f9 pm:filt-fd pm:filt-reset pm:filt-note pm:filt-channel-aftertouch pm:filt-poly-aftertouch pm:filt-program pm:filt-control pm:filt-pitchbend pm:filt-mtc pm:filt-song-position pm:filt-song-select pm:filt-tune pm:filt-tick pm:filt-undefined pm:filt-realtime pm:filt-aftertouch pm:filt-systemcommon pm:PortMidi pm:Message pm:Message.status pm:Message.data1 pm:Message.data2 pm:DeviceInfo.interf pm:DeviceInfo.name pm:DeviceInfo.input pm:DeviceInfo.output pm:DeviceInfo.opened pm:Event.message pm:Event.timestamp pm:Initialize pm:Terminate pm:HasHostError pm:GetErrorText pm:CountDevices pm:GetDefaultInputDeviceID pm:GetDefaultOutputDeviceID pm:OpenInput pm:OpenOutput pm:SetFilter pm:SetChannelMask pm:Abort pm:Close pm:Read pm:Poll pm:Write pm:WriteShort pm:WriteSysex pm:EventBufferFree pm:EventBufferNew pm:EventBufferElt pm:EventBufferSet pm:EventBufferMap pm:GetDeviceInfo pt:Started pt:Start pt:Stop pt:Time)
    )

;(select-module portmidi)

(load "gauche-portmidi.scm")

(let ((lib (string-append
	    (sys-dirname (sys-realpath (port-name (current-load-port))))
	    "/"
	    (if (equal? (car (sys-uname)) "Darwin")
		"libgauchepmrecv.dylib"
		"libgauchepmrecv.so"))))
  ;; optional loading if lib exists.
  (if (file-exists? lib)
      (begin (load "gauche-recv.scm")
	     (load "cm-recv.scm"))))

;;; EOF

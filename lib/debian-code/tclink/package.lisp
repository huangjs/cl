;;; -*- Mode: Lisp -*-
;;; Package definition for CL-TCLINK
;;; Copyright 2002 Matthew Danish <mrd@debian.org>
;;; Distributed under the terms of the LLGPL.
;;; See LICENSE file for more details.

(defpackage #:ORG.MAPCAR.TCLINK
  (:use #:COMMON-LISP #:UFFI #:SPLIT-SEQUENCE)
  (:nicknames #:TCLINK #:CL-TCLINK)
  (:export #:PARAMETER-MAX-LENGTH
	   #:PARAMETER-TOO-LONG
	   #:WHICH-PARAMETER
	   #:PARAMETER-VALUE
	   
	   #:CREATE
	   #:DESTROY
	   #:PUSH-PARAMETER
	   #:SEND
	   #:GET-RESPONSE
	   #:GET-ENTIRE-RESPONSE
	   #:GET-VERSION
	   #:WITH-HANDLE
	   #:SEND-WITH-PARAMETERS))

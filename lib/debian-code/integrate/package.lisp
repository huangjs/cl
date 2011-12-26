;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: integrate -*-

(in-package #:cl-user)

(defpackage #:integrate
  (:use #:common-lisp)
  (:export
   #:*step-size*
   #:*tolerance*
   #:*integrator*
   #:*target-t*
   #:*log*
   #:*stop-function*
   #:precision-loss
   #:integrate

   #:integrated-1-function
   #:euler
   #:make-adaptive-integrator
   #:runge-kutta
   ))

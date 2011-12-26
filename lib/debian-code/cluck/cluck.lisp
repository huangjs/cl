;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cluck.asd
;;;; Purpose:       Common Lisp uControler Clock Calculator
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  March 2007
;;;;
;;;; $Id: cluck.lisp 11571 2007-03-09 14:53:51Z kevin $
;;;;
;;;; Copyright (c) 2007 Kevin M. Rosenberg
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. Neither the name of the author nor the names of the contributors
;;;;    may be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.
;;;; *************************************************************************

(defpackage #:cluck
  (:use #:cl)
  (:export
   #:show-timers
   #:show-8-bit-timers
   #:show-16-bit-timers
   #:show-32-bit-timers
   #:ms-clocks
   #:avr-uart-divisors
   #:pic-uart-divisors
   #:zero-error-uart-clocks))

(in-package #:cluck)

(defvar *f-cpu* 16000000)

(defvar *8-bit-prescalars* '(1 8 64 256))
(defvar *10-bit-prescalars* '(1 8 64 256 1024))

(defvar *base-error-zero-baud-clk* (* 9 25 8192)
"Base multiple for multi-megahertz clock frequencies to have
0% error at common UART baud rates. Value of this base is 1.8432 million.
Common multiples of this are 2 (3.6864Mhz), 4 (7.3728Mhz), 8 (14745600),
and 10 (18.432MHz)")

(defun show-timers (f-cpu prescalers width)
  (let ((max-count (expt 2 width)))
    (format t "~&Prescalar MaxRate     MinUS     MinRate        MaxMS~%")
    (dolist (prescale prescalers)
      (let ((base (/ f-cpu prescale)))
        (format t "~4D ~12,1F ~9,3F ~10,4F ~13,3F~%"
                prescale
                (coerce base 'float)
                (coerce (/ 1000000 base) 'float)
                (coerce (/ base max-count) 'float)
                (coerce (/ 1000 (/ base max-count)) 'float))))))

(defun show-8-bit-timers (&optional (f-cpu *f-cpu*))
  (show-timers f-cpu *10-bit-prescalars* 8))

(defun show-16-bit-timers (&optional (f-cpu *f-cpu*))
  (show-timers f-cpu *10-bit-prescalars* 16))

(defun show-32-bit-timers (&optional (f-cpu *f-cpu*))
  "Show max/min periods for 32-bit timers. For 16-bit PIC
controllers, 32-bit timers use 8-bit prescalers"
  (show-timers f-cpu *8-bit-prescalars* 32))

(defun ms-timer-width (ms f-cpu prescalars width)
  "Returns the prescalar and compare count for both 8 and 16 bit timers."
  (labels ((nearest-count (prescale)
             (let ((count (round (* ms (/ f-cpu 1000 prescale))))
                   (max-count (expt 2 width)))
               (cond
                 ((< count 1)
                   1)
                 ((<= count max-count)
                   count)
                 ((> max-count)
                   max-count))))
           (clk-ms (prescale count)
             (unless (zerop count)
               (/ count (/ f-cpu 1000 prescale))))
           (percent-error (actual desired)
             (* 100 (- (/ actual desired) 1))))

    (dolist (prescale prescalars)
      (let* ((count (nearest-count prescale))
             (clk-ms (clk-ms prescale count))
             (err (percent-error clk-ms ms))
             (fmt-err (if (> err 1000)
                        "   >1000%"
                        (format nil "~8,3F%" err))))
        (format t "~2D  ~4D  ~5D ~10,4F ~A~%"
                width prescale count clk-ms fmt-err)))))

(defun ms-timer (ms &optional (f-cpu *f-cpu*))
  "Returns the prescalar and compare count for both 8 and 16 bit timers."
  (dolist (width '(8 16))
    (ms-timer-width ms f-cpu *10-bit-prescalars* width)))

(defparameter *baud-rates* '(300 600 1200 2400 4800 9600 14400 19200 28800
                             38400 56000 57600 76800 115200 128000 250000
                             256000 500000))

(defun avr-uart-divisors (&optional (f-cpu *f-cpu*) (view-below-percent nil))
  "Displays the divisor UBRR and error percent for various baud
rates for F_CPU. UBBR is limited to 12 bits."
  (dolist (baud *baud-rates*)
    (let* ((ubrr (min 4096
                      (max 0 (round (- (/ f-cpu 16 baud) 1)))))
           (ubrr2 (min 4096
                       (max 0 (round (- (/ f-cpu 8 baud) 1)))))
           (actual-baud (/ f-cpu 16 (1+ ubrr)))
           (actual-baud2 (/ f-cpu 8 (1+ ubrr2)))
           (err (* 100 (- (/ actual-baud baud) 1)))
           (err2 (* 100 (- (/ actual-baud2 baud) 1))))
      (when (or (not view-below-percent)
                (or (< (abs err) view-below-percent)
                    (< (abs err2) view-below-percent)))
        (format t "~6D ~4D ~5,1F% ~4D ~5,1F%~%"
                baud ubrr err ubrr2 err2)))))

(defun pic-uart-divisors (&optional (fcy *f-cpu*) (view-below-percent nil))
  "Displays the divisor BRG and error percent for various baud
rates for Fcy. BRG is limited to 16 bits."
  (dolist (baud *baud-rates*)
    (let* ((brg (min 65536
                     (max 0 (round (- (/ fcy 16 baud) 1)))))
           (actual-baud (/ fcy 16 (1+ brg)))
           (err (* 100 (- (/ actual-baud baud) 1))))
      (when (or (not view-below-percent)
                (< (abs err) view-below-percent))
        (format t "~6D ~4D ~5,1F%~%" baud brg err)))))

(defun zero-error-uart-clocks ()
  (dolist (mult '(1 2 4 6 8 10 12))
    (format t "~&~8,4F MHz~%" (* mult  *base-error-zero-baud-clk* 1e-6))))


(in-package cm)

;;;
;;; Consing and timing tests. eval tests two times since first
;;; invocation may have to build methods etc.

(define *duty* (* 8 15 60 10)) ; equals 8 voices for 15 minutes at 10 notes/sec

(cond-expand
 (openmcl
  (defun do-gc () (funcall (find-symbol "GC" :ccl)))
  (defmacro time-it (form) `(time ,form)))
 (sbcl
  (defun do-gc () (funcall (find-symbol "GC" :sb-ext)))
  (defmacro time-it (form) `(time ,form)))
 (gauche
  (define (do-gc) (gc))
  (define-macro (time-it form)
    `(let ((tot (get-keyword :total-bytes (gc-stat))))
       (time ,form)
       (format #t "; bytes allocated: ~d~%"
               (- (get-keyword :total-bytes (gc-stat))
                  tot))))))

(define-macro (time-form reps . forms)
  (let ((a (gensym))
	(b (gensym)))
    `(begin
       (do-gc)
       (time-it (do ((,a 0 (+ ,a 1))
		     (,b ,reps))
		    ((= ,a ,b) #f)
		  ,@forms)))))


;;;
;;; empty process calling
;;;

;; base test empty process
(define foo0 (process for i below *duty* do #f))

(cond-expand 
 (common-lisp (time-form *duty* (funcall foo0)))
 (else (time-form *duty* ( foo0))))

#||
Dual PPC 1.8GH OSX 10.3.9

OPENMCL:
took 9 milliseconds (0.009 seconds) to run.
Of that, 9 milliseconds (0.009 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode
SBCL:
Evaluation took:
  0.012 seconds of real time
  0.01 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   0.369
; user   0.350
; sys    0.000
; bytes allocated: 13827600

||#

;;;
;;; Midis and Floats
;;;

(define foo1 (process for i below *duty*
		      do (new midi :time (* i 1.0))))

(cond-expand 
 (common-lisp (time-form *duty* (funcall foo1)))
 (else (time-form *duty* ( foo1))))


#||
OPENMCL:
took 996 milliseconds (0.996 seconds) to run.
Of that, 967 milliseconds (0.967 seconds) were spent in user mode
         13 milliseconds (0.013 seconds) were spent in system mode
         16 milliseconds (0.016 seconds) were spent executing other OS processes.
2 milliseconds (0.002 seconds) was spent in GC.
 5,184,000 bytes of memory allocated.

SBCL:
Evaluation took:
  6.779 seconds of real time
  5.51 seconds of user run time
  0.76 seconds of system run time
  0 page faults and
  155,445,264 bytes consed.

Gauche:
; real   1.047
; user   1.070
; sys    0.010
; bytes allocated: 33411616

||#

;;;
;;; reuse 1 midi, floating point time operation
;;;

(define foo2 (process with o = (new midi :time 0)
		      for i below *duty*
		      do (sv o :time (* i 1.0))))

(cond-expand
 (common-lisp (time-form *duty* (funcall foo2)))
 (else (time-form *duty* ( foo2))))

#||
SBCL:
Evaluation took:
  0.029 seconds of real time
  0.03 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  573,440 bytes consed.
||#

;;;
;;; reuse 1 midi, integer time operation
;;;

(define foo3 (process with o = (new midi :time 0)
		      for i below *duty*
		      do (sv o :time (* i 1000))))

(cond-expand
 (common-lisp (time-form *duty* (funcall foo3)))
 (else (time-form *duty* ( foo3))))

#||
SBCL:
Evaluation took:
  0.019 seconds of real time
  0.02 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.
||#

;;;
;;; midi messages and ints
;;;

(define foo4 (process for i below *duty*
		      do
		      (make-note-on  0 0 (* i 1000))
		      (make-note-off 0 0 0)))
(cond-expand
 (common-lisp (time-form *duty* (funcall foo4)))
 (else (time-form *duty* ( foo4))))

#||
OPENMCL:
took 50 milliseconds (0.050 seconds) to run.
Of that, 50 milliseconds (0.050 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode

SBCL:
Evaluation took:
  0.041 seconds of real time
  0.03 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   0.746
; user   0.710
; sys    0.000
; bytes allocated: 16131600

||#

;;;
;;; calling write-event to nothing
;;;

(time-form *duty* (output (make-note-on 0 1 2) :to #f))

#||
OPENMCL:
took 76 milliseconds (0.076 seconds) to run.
Of that, 76 milliseconds (0.076 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode

SBCL:
Evaluation took:
  0.052 seconds of real time
  0.05 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   0.775
; user   0.770
; sys    0.000
; bytes allocated: 7491504

||#

;;;
;;; Write-event to midi file
;;;

(begin (define mf (open-io "/tmp/test.mid" :output))
       (sv mf :scaler 1) ; default 480.0 conses...
       (define mn (new midi :keynum 72 :amplitude 64 :duration 1))
       ;;(define mn (new midi-program-change :channel 0 :program 64))
       (initialize-io mf)
       (define m 0)
       (time-form *duty* (output mn :to mf :at (incf m)))
       (output mn :to mf :at 0)
       (deinitialize-io mf)
       (close-io mf))

#||
OPENMCL:
took 1,875 milliseconds (1.875 seconds) to run.
Of that, 1,880 milliseconds (1.880 seconds) were spent in user mode
         20 milliseconds (0.020 seconds) were spent in system mode

SBCL:
Evaluation took:
  0.573 seconds of real time
  0.54 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; user   6.560
; sys    0.040
; bytes allocated: 93315144

||#

;;;
;;;  Events.  CONSING is due to floating point midi file scaler
;;;

(define foo1 (process with m = (new midi :duration 1)
		      for i below *duty*
		      do (sv m :time (now))
		      output m wait 1))

(time-form 1 (events foo1 "/tmp/test.mid" :play #f))

#||
OPENMCL: took 2,491 milliseconds (2.491 seconds) to run.
Of that, 2,350 milliseconds (2.350 seconds) were spent in user mode
         20 milliseconds (0.020 seconds) were spent in system mode
         121 milliseconds (0.121 seconds) were spent executing other OS processes.
1 milliseconds (0.001 seconds) was spent in GC.
 1,157,240 bytes of memory allocated.

SBCL:
Evaluation took:
  0.65 seconds of real time
  0.64 seconds of user run time
  0.01 seconds of system run time
  0 page faults and
  589,824 bytes consed.

Gauche:
; user   8.040
; sys    0.030
; bytes allocated: 120406144

||#

;;;
;;; Integer random. setq is to stop sbcl from optmizing out form.
;;; SBCL: integer random is slower and consier than float random.

(define aaa 0)
(time-form *duty* (set! aaa (random 100)))

#||
OPENMCL:
took 15 milliseconds (0.015 seconds) to run.
Of that, 20 milliseconds (0.020 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode

SBCL:
  0.091 seconds of real time
  0.08 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  2,796,760 bytes consed.

Gauche:
; user   0.130
; sys    0.000
; bytes allocated: 1155520

||#

;;;
;;; Float random
;;;
(define aaa 0)
(time-form *duty* (set! aaa (random 100.0)))

#||
OPENMCL:
took 204 milliseconds (0.204 seconds) to run.
Of that, 170 milliseconds (0.170 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode
         34 milliseconds (0.034 seconds) were spent executing other OS processes.
 576,000 bytes of memory allocated.

SBCL:
  0.014 seconds of real time
  0.0 seconds of user run time
  0.01 seconds of system run time
  0 page faults and
  573,440 bytes consed.

Gauche:
; real   0.153
; user   0.150
; sys    0.000
; bytes allocated: 2307520

||#

;;;
;;; Cycle pattern
;;;

(define pat (new cycle :of (list 1 2 3 4 5)))
(time-form *duty*  (next pat))

#||
OPENMCL:
took 339 milliseconds (0.339 seconds) to run.
Of that, 290 milliseconds (0.290 seconds) were spent in user mode
         10 milliseconds (0.010 seconds) were spent in system mode
         39 milliseconds (0.039 seconds) were spent executing other OS processes.

SBCL:
Evaluation took:
  0.084 seconds of real time
  0.11 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   3.184
; user   3.160
; sys    0.000
; bytes allocated: 71427504

||#

;;;
;;; Heap pattern
;;;

(define pat (new heap :of (list 1 2 3 4 5)))
(time-form *duty* (next pat))

#||
OPENMCL:
took 393 milliseconds (0.393 seconds) to run.
Of that, 360 milliseconds (0.360 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode
         33 milliseconds (0.033 seconds) were spent executing other OS processes.

SBCL:
Evaluation took:
  0.202 seconds of real time
  0.17 seconds of user run time
  0.03 seconds of system run time
  0 page faults and
  2,804,984 bytes consed.

Gauche:
; real   4.149
; user   4.020
; sys    0.020
; bytes allocated: 97808440
||#

(define pat (new weighting :of (list 1 2 3 4 5)))
(time-form *duty* (next pat))

#||
OPENMCL:
took 650 milliseconds (0.650 seconds) to run.
Of that, 600 milliseconds (0.600 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode
         50 milliseconds (0.050 seconds) were spent executing other OS processes.
 576,000 bytes of memory allocated.

SBCL:
Evaluation took:
  0.198 seconds of real time
  0.16 seconds of user run time
  0.01 seconds of system run time
  0 page faults and
  569,344 bytes consed.

Gauche:
; real   5.965
; user   6.030
; sys    0.010
; bytes allocated: 169578040

||#


;;;
;;; Keynum
;;;

(time-form *duty* (keynum 'c4))

#||
OPENCML:
took 293 milliseconds (0.293 seconds) to run.
Of that, 300 milliseconds (0.300 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode

SBCL:
  0.123 seconds of real time
  0.11 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   0.540
; user   0.530
; sys    0.000
; bytes allocated: 10371520

||#

;;;
;;; Floating point keynum
;;;

(time-form *duty* () (keynum 660 :hz))

#||
OPENMCL:
took 681 milliseconds (0.681 seconds) to run.
Of that, 640 milliseconds (0.640 seconds) were spent in user mode
         30 milliseconds (0.030 seconds) were spent in system mode
         11 milliseconds (0.011 seconds) were spent executing other OS processes.
3 milliseconds (0.003 seconds) was spent in GC.
 6,912,000 bytes of memory allocated.

SBCL:
  0.574 seconds of real time
  0.42 seconds of user run time
  0.05 seconds of system run time
  0 page faults and
  21,884,920 bytes consed.

Gauche:
; real   1.787
; user   1.760
; sys    0.000
; bytes allocated: 44355520

||#

;;;
;;; Note
;;;

(time-form *duty* (note 60))

#||
OPENMCL:
took 309 milliseconds (0.309 seconds) to run.
Of that, 310 milliseconds (0.310 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode

SBCL:
  0.129 seconds of real time
  0.12 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   0.570
; user   0.540
; sys    0.000
; bytes allocated: 10371504

||#

;;;
;;; RTS
;;;

(use-system :rts)

;;;
;;; Basic FFI call
;;;

(time-form *duty* (rts:scheduler-state?))

#||
OPENMCL:
took 29 milliseconds (0.029 seconds) to run.
Of that, 40 milliseconds (0.040 seconds) were spent in user mode
         10 milliseconds (0.010 seconds) were spent in system mode

SBCL:
Evaluation took:
  0.019 seconds of real time
  0.01 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   6.920
; user   6.960
; sys    0.040
; bytes allocated: 194691520

||#

;;;
;;; Accessing floating point time
;;;

(rts )
(time-form *duty* (rts:scheduler-time))
(rts-stop)

#||
OPENMCL:
took 41 milliseconds (0.041 seconds) to run.
Of that, 40 milliseconds (0.040 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode
         1 milliseconds (0.001 seconds) were spent executing other OS processes.
 576,000 bytes of memory allocated.

SBCL:
Evaluation took:
  0.032 seconds of real time
  0.02 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  569,344 bytes consed.

Gauche:
; real   6.654
; user   6.580
; sys    0.020
; bytes allocated: 190083520

||#

;;;
;;; Integer time
;;;

(rts :time-format :msec)
(time-form *duty* (rts:scheduler-time))
(rts-stop)

#||
OPENMCL:
took 153 milliseconds (0.153 seconds) to run.
Of that, 120 milliseconds (0.120 seconds) were spent in user mode
         0 milliseconds (0.000 seconds) were spent in system mode
         33 milliseconds (0.033 seconds) were spent executing other OS processes.

SBCL:
Evaluation took:
  0.049 seconds of real time
  0.04 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

Gauche:
; real   6.845
; user   6.850
; sys    0.000
; bytes allocated: 194691520

||#

COMPARISON OF RTS TOOLS (SBCL/Darwin)


(time-form *duty* (between 1 1000))
SBCL
Evaluation took:
  0.1 seconds of real time
  0.07 seconds of user run time
  0.02 seconds of system run time
  0 page faults and
  2,809,144 bytes consed.

(time-form *duty* (rts:between 1 1000))
SBCL:
Evaluation took:
  0.028 seconds of real time
  0.02 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

(time-form *duty* (rhythm 1/4))
SBCL:
Evaluation took:
  0.16 seconds of real time
  0.12 seconds of user run time
  0.04 seconds of system run time
  0 page faults and
  4,603,904 bytes consed.

(time-form *duty* (rts:rhythm 480))
SBCL:
Evaluation took:
  0.022 seconds of real time
  0.02 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

(time-form *duty* (interpl .33 '(0 0 1 100)))
SBCL:
Evaluation took:
  0.09 seconds of real time
  0.08 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  1,720,320 bytes consed.

(time-form *duty* (rts:interpl 33 '(0 0 100 100)))
SBCL:
Evaluation took:
  0.03 seconds of real time
  0.03 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.

(time-form *duty* (shuffle '(0 1 2 3 4 5)))
SBCL:
Evaluation took:
  0.719 seconds of real time
  0.52 seconds of user run time
  0.13 seconds of system run time
  0 page faults and
  20,331,344 bytes consed.


(time-form *duty* (rts:shuffle '(0 1 2 3 4 5)))
SBCL:
Evaluation took:
  0.292 seconds of real time
  0.29 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.






















(time-form *duty* () (rhythm 3/16))

(time-form *duty* () (rhythm 1 60 1))


(defun foo (a &optional (b 1) (c 2))
  (+ a b c))
  
(time-form *duty* () (foo 1))
(time-form *duty* () (foo 1 2 3))

(defun bar (a &key (b 1) (c 2))
  (+ a b c))

(time-form *duty* () (bar 1))
(time-form *duty* () (bar 1 :c 2 :b 3))


#||
OPENMCL:

SBCL:

Gauche:

||#
#||
OPENMCL:

SBCL:

Gauche:

||#

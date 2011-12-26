;;; **********************************************************************
;;; Copyright (C) 2005 Todd Ingalls, Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2006/06/01 23:21:25 $

(define-class* <osc-stream> (<event-stream>)
  ((remote-port :init-value #f :init-keyword :remote-port)
   (remote-host :init-value #f :init-keyword :remote-host)
   (local-port :init-value #f :init-keyword :local-port)
   ;; data list: (<thread> #'stopper )
   (receive-data :init-value (list #f #f)
     :accessor rt-stream-receive-data)
   (receive-mode :init-value :message :init-keyword :receive-mode
            :accessor rt-stream-receive-mode)
   (latency :init-value 0.0 :init-keyword :latency)
   (buffer-size :init-value 512 :init-keyword :buffer-size)
   (socket :init-value #f))
  :name 'osc-stream
  :metaclass <io-class>)

(define-method* (open-io (obj <osc-stream>) dir . args)
  dir
  (let ((rh #f) (rp #f) (lp #f))
    (if (pair? args)
        (begin
          (when (> (length args) 0)
            (set! rh (list-ref args 0)))
          (when (> (length args) 1)
            (set! rp (list-ref args 1)))
          (when (> (length args) 2)
            (set! lp (list-ref args 2)))))
    (unless (io-open obj)
      (slot-set! obj 'socket
                 (make-udp-socket
                  (if rh rh (slot-ref obj 'remote-host))
                  (if rp rp (slot-ref obj 'remote-port))
                  (if lp lp (slot-ref obj 'local-port))))
      (slot-set! obj 'open #t))
    (set! *out* obj)))

(define (osc-open . args)
  (apply open-io "osc.udp" t args))

(define (osc-open? .args)
  (with-args (&optional (osc (find-object "osc.udp")))
    (if osc
        (let ((io (io-open osc)))
          (if io 
              osc
	    #f)))))


(define-method* (close-io (obj <osc-stream>) . mode)
  mode
  (when (io-open obj)
    (udp-socket-close (slot-ref obj 'socket))
    ;(udp-socket-shutdown (slot-ref obj 'socket) 2)
    (slot-set! obj 'open #f))
  (set! *out* #f))

(define (osc-close .args)
  (with-args ((&optional (osc (find-object "osc.udp"))))
    (if (osc-open? osc)
        ;;;;change!!!!!!!!!!!
        (cond ((receiver? osc)
               (error "osc-close: Can't close osc because a receiver is currently running."))
              (t (close-io osc ':force) osc))
      osc)))

;;; send-msg
;;; (send-msg <list> <osc-stream>)
;;; (send-msg '("foo" 1 2 3 4 5) *osc*)


(define-method* (send-msg message (io <osc-stream>))
  (multiple-value-bind (mess len)
    (format-osc message)
  (send-osc mess io len)))

;;;send-bundle
;;; (send-bundle <time> <list> <osc-stream>)
;;; (send-bundle 1.5 '("foo" 1 2 3 4 'a) *osc*)
;;; can also nest messages
;;; (send-bundle 1.5 '(("foo" 1 2 3 4) ("baz" 1 2 3 4 5 'b)))


(define-method* (send-bundle offset message (io <osc-stream>))
  (let ((arr (make-byte-vector "#bundle"))
        (mess-len 0))
    (set! arr (u8vector-append arr (make-osc-timetag offset io)))
    ;;this should be smarter
    (if (list? (list-ref message 0))
        (begin
          (dolist (bundle-mess message)
            (multiple-value-bind (mess len)
              (format-osc bundle-mess)
              (set! arr
                    (u8vector-append arr
                                     (make-byte-vector len)
                                     mess))
              (set! mess-len (+ mess-len len))))
          (set! mess-len (+ mess-len 20))
          (send-osc arr io mess-len))
      (multiple-value-bind (mess len)
        (format-osc message)
        (set! arr
              (u8vector-append arr
                               (make-byte-vector len)
                               mess))
        (set! mess-len (+ len 8 8 4))
        (send-osc arr io mess-len)))))



(define (make-osc-timetag offset out)
  (let* ((now (current-time))
         (offset-time (seconds->time offset))
         (target-time (seconds->time (+ (time->seconds now) (time->seconds offset-time) (slot-ref out 'latency))))
         (vec #f))
    (set! vec (make-byte-vector (+ 2208988800 (slot-ref target-time 'second))))
    (u8vector-append vec (make-byte-vector (inexact->exact (* (modf (time->seconds target-time)) #xffffffff))))))


(define (u8vector->double vec)
  (let ((dv (uvector-alias <f64vector> vec)))
    (f64vector-ref dv 0)))

(define *bundle-header-bytes* #u8(35 98 117 110 100 108 101 0))

(define (osc-bundle? vec)
  (let ((res #t))
    (if (u8vector? vec)
        (dotimes (i 8)
          (when (not (= (u8vector-ref vec i) (u8vector-ref *bundle-header-bytes* i)))
            (set! res #f)))
      (set! res #f))
    res))
          
(define (osc-parse-timestamp vec)
  (let ((ts #f))
    (setf ts (u8vector->uint (u8vector-subseq arr 0 4)))
    (setf ts (+ ts
             (exact->inexact
              (/ (u8vector->uint (u8vector-subseq arr 4 8))
                 4294967295))))))

;;(find-index (lambda (x) (= x 0)) *bundle-header-bytes*)

(define (osc-parse-contents arr)
  (let ((lst (list))
	(mess #f)
	(pos #f)
	(sym-vector #f)
	(sym #f)
	(sym-len 0)
	(first-token-len #f)
	(type-list #f)
	(type-list-len #f))
    (set! first-token-len (find-index (lambda (x) (= x 0)) arr))
    (if first-token-len
	(begin
	  (set! lst (append! lst (list (string->symbol
                                        (string-upcase (u8vector->string (u8vector-subseq arr 0 first-token-len)))))))
	  (set! mess (u8vector-subseq arr (find-index (lambda (x) (= x 44))  arr)))
	  (set! type-list (loop for i across (subseq mess 0 (position 0 mess))
				collect (code-char i)))
	  (set! type-list-len (length type-list))
	  (set! mess (u8vector-subseq mess (+ type-list-len (- 4 (mod type-list-len 4)))))
	  (set! pos 0)
	  (dolist (j type-list)
            (cond ((eq? j #\i)
                   (set! lst (append! lst (list (u8vector->int (u8vector-subseq mess pos (+ pos 4))))))
                   (set! pos (+ pos 4)))
                  ((eq? j #\f)
                   (set! lst (append! lst (list (u8vector->float (u8vector-subseq mess pos (+ pos 4))))))
                   (set! pos (+ pos 4)))
                  ((eq? j #\d)
                   (set! lst (append! lst (list (u8vector->double (u8vector-subseq mess pos (+ pos 8))))))
                   (set! pos (+ pos 8)))
                  ((eq? j #\s)
                   (set! sym-vector (u8vector-subseq mess pos))
                   (set! sym (u8vector->string sym-vector))
                   (set! lst (append! lst (list (string-trim-right sym #\newline) #\/)))
                   (set! sym-len (string-length sym))
                   (if (= 0 (mod sym-len 4))
                       (set! pos (+ pos sym-len))
                     (set! pos (+ (+ sym-len (- 4 (mod sym-len 4))) pos)))))))
      (set! lst (append! lst (list (string->symbol (u8vector->string arr))))))
    lst))

(define (osc-parse-message vec)
  (osc-parse-contents vec))

(define (osc-parse-bundle arr)
  (let ((msg-len #f)
        (pos 0)
        (arr-len (u8vector-length arr))
        (bundle (list)))
    (do ()
        ((>= pos arr-len))
      (set! msg-len (u8vector->int (u8vector-subseq arr pos 4)))
      (set! pos (+ pos 4))
      (setf bundle (append! bundle (osc-parse-contents (u8vector-subseq arr pos (+ pos msg-len)))))
      (set! pos (+ pos msg-len)))
    bundle))

(define (osc-vector->osc-message arr)
  (let ((timestamp #f)
	(msg #f))
    (if (osc-bundle? arr)
	(begin 
	  (set! timestamp (osc-parse-timestamp (u8vector-subseq arr 8 16)))
	  (set! msg (osc-parse-bundle (u8vector-subseq arr 16))))
      (set! msg (osc-parse-contents arr)))
    (list msg timestamp)))

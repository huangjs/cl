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
;;; $Revision: 1.13 $
;;; $Date: 2005/10/03 05:54:03 $


(define-class* <sc-stream> (<osc-stream>)
  ((remote-port :init-value 57110)
   (remote-host :init-value "127.0.0.1")
   (local-port :init-value 57100)
   (latency :init-value 0.05 :init-keyword :latency
            :accessor rt-stream-latency)
   (receive-mode :accessor rt-stream-receive-mode :init-value :object
                 :init-keyword :receive-mode)
   (notify :init-value #f))
  :name 'sc-stream
  :metaclass <io-class>
  :file-types '("sc.udp" ))

;; open-io and close-io inherited
;; from osc-stream

(define-method* (write-event (obj <scsynth>) (io <sc-stream>) time)
  time
  (let* ((node-set-list #f)
         (synthname (symbol->string (class-name (class-of obj))))
         (slots (instance-slots obj))
         (inits (list #F))
         (msg (cons* "/s_new"
                     (string-downcase synthname)
                     (slot-ref obj 'node)
                     (slot-ref obj 'add-action)
                     (slot-ref obj 'target)
                     (do ((tail slots (cdr tail))
                          (args inits))
                         ((null? tail)
                          (cdr inits))
                       (unless (member (car tail)
                                       '(node add-action 
                                              target time))
                         (cond ((list? (slot-ref obj (car tail)))
                                (begin
                                  (set! node-set-list (append! node-set-list (list (string-downcase (symbol->string (car tail))))))
                                  (let ((mess-list (slot-ref obj (car tail))))
                                    (set! node-set-list (append! node-set-list (list (length mess-list))))
                                    (set! node-set-list (append! node-set-list mess-list)))))
                               ((equal? (find-class* 'sc-env) (class-of (slot-ref obj (car tail))))
                                (set! node-set-list (append! node-set-list (list (string-downcase (symbol->string (car tail))))))
                                (let ((mess-list (sc-env->list (slot-ref obj (car tail)))))
                                  (set! node-set-list (append! node-set-list (list (length mess-list))))
                                  (set! node-set-list (append! node-set-list mess-list))))
                               (else
                                (set-cdr! args 
                                          (list
                                           (string-downcase
                                            (symbol->string (car tail)))
                                           (slot-ref obj (car tail))))
                                (set! args (cddr args)))))))))
    (if node-set-list
        (send-bundle 0 (list msg (append! (list "/n_setn" (slot-ref obj 'node)) node-set-list)) io)
      (send-msg msg io))))

(define-method* (write-event (obj <load-synthdef>) (io <sc-stream>) time)
  time
  (let ((msg (load-synthdef (slot-ref obj 'path))))
    (send-msg msg io)))

(define-method* (write-event (obj <load-synthdef-dir>) (io <sc-stream>) time)
  time
  (let ((msg (load-synthdef-dir (slot-ref obj 'path))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-free>) (io <sc-stream>) time)
  time
  (let ((msg (node-free (slot-ref obj 'node))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-run>) (io <sc-stream>) time)
  time
  (let ((msg (node-run (slot-ref obj 'node) (slot-ref obj 'flag))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-set>) (io <sc-stream>) time)
  time
  (let ((msg (node-set (slot-ref obj 'node) (slot-ref obj 'controls-values))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-setn>) (io <sc-stream>) time)
  time
  (let ((msg (node-setn (slot-ref obj 'node) (slot-ref obj 'controls-values))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-fill>) (io <sc-stream>) time)
  time
  (let ((msg (node-fill (slot-ref obj 'node) 
                        (slot-ref obj 'control) 
                        (slot-ref obj 'num-controls)
                        (slot-ref obj 'value))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-map>) (io <sc-stream>) time)
  time
  (let ((msg (node-map (slot-ref obj 'node) (slot-ref obj 'controls-buses))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-mapn>) (io <sc-stream>) time)
  time
  (let ((msg (node-mapn (slot-ref obj 'node)
                        (slot-ref obj 'control)
                        (slot-ref obj 'value)
                        (slot-ref obj 'num-controls))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-before>) (io <sc-stream>) time)
  time
  (let ((msg (node-before (slot-ref obj 'node) (slot-ref obj 'before))))
    (send-msg msg io)))

(define-method* (write-event (obj <node-after>) (io <sc-stream>) time)
  time
  (let ((msg (node-after (slot-ref obj 'node) (slot-ref obj 'after))))
    (send-msg msg io)))


(defobject node-query (sc-cmd) 
  ((node :initform #f))
  (:parameters node)
  (:event-streams))

(define-method* (write-event (obj <node-query>) (io <sc-stream>) time)
  time
  (let ((node (slot-value obj 'node))
        (msg (list "/n_query")))
    (if (pair? node)
        (set! msg (append! msg node))
      (set! msg (append! msg (list node))))
    (send-msg msg io)))

(defobject synth-get (sc-cmd) 
  ((node :initform #f)
   (controls :initform #f))
  (:parameters node controls)
  (:event-streams))

(define-method* (write-event (obj <synth-get>) (io <sc-stream>) time)
  time
  (let ((controls (slot-value obj 'controls))
        (msg (list "/s_get" (slot-value obj 'node))))
    (if (pair? controls)
        (set! msg (append! msg controls))
      (set! msg (append! msg (list controls))))
    (send-msg msg io)))

(defobject synth-getn (sc-cmd) 
  ((node :initform #f)
   (controls :initform #f)
   (num-controls :initform #f))
  (:parameters node controls num-controls)
  (:event-streams))

(define-method* (write-event (obj <synth-getn>) (io <sc-stream>) time)
  time
  (let* ((controls (slot-value obj 'controls))
         (controls-len (length controls))
         (num-controls (slot-value obj 'num-controls))
         (msg (list "/s_getn" (slot-value obj 'node))))
    (if (pair? controls)
        (do ((i 0 (+ i 1)))
            ((> i controls-len))
          (set! msg (append! msg (list (list-ref controls i) (list-ref num-controls i)))))
      (set! msg (append! msg (list controls num-controls))))
    (send-msg msg io)))

(define-method* (write-event (obj <group-new>) (io <sc-stream>) time)
  time
  (let ((msg (group-new (slot-ref obj 'id) (slot-ref obj 'add-action) (slot-ref obj 'target))))
    (if (> time 0)
        (send-bundle time msg io)
      (send-msg msg io))))

(define-method* (write-event (obj <group-head>) (io <sc-stream>) time)
  time
  (let ((msg (group-head (slot-ref obj 'group) (slot-ref obj 'node))))
    (send-msg msg io)))

(define-method* (write-event (obj <group-tail>) (io <sc-stream>) time)
  time
  (let ((msg (group-tail (slot-ref obj 'group) (slot-ref obj 'node))))
    (send-msg msg io)))

(define-method* (write-event (obj <group-free-all>) (io <sc-stream>) time)
  time
  (let ((msg (group-free-all (slot-ref obj 'group))))
    (send-msg msg io)))

(define-method* (write-event (obj <group-deep-free>) (io <sc-stream>) time)
  time
  (let ((msg (group-deep-free (slot-ref obj 'group))))
    (send-msg msg io)))

(define-method* (write-event (obj <ugen-command>) (io <sc-stream>) time)
  time
  (let ((msg (ugen-command (slot-ref obj 'node)
                           (slot-ref obj 'ugen-index) 
                           (slot-ref obj 'command-name)
                           (slot-ref obj 'args))))
      (send-msg msg io)))

(define-method* (write-event (obj <buffer-alloc>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-alloc (slot-ref obj 'bufnum) 
                           (slot-ref obj 'frames)
                           (slot-ref obj 'channels))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-alloc-read>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-alloc-read (slot-ref obj 'bufnum)
                                (slot-ref obj 'file) (slot-ref obj 'start-frame)
                                (slot-ref obj 'frames))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-read>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-read (slot-ref obj 'bufnum) (slot-ref obj 'file)
                          (slot-ref obj 'start-frame) (slot-ref obj 'frames)
                          (slot-ref obj 'buffer-start-frame)
                          (slot-ref obj 'leave-open?))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-write>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-write (slot-ref obj 'bufnum) (slot-ref obj 'file)
                           (slot-ref obj 'header) (slot-ref obj 'sample-format)
                           (slot-ref obj 'frames) (slot-ref obj 'start-frame)
                           (slot-ref obj 'leave-open?))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-free>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-free (slot-ref obj 'bufnum))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-zero>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-zero (slot-ref obj 'bufnum))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-set>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-set (slot-ref obj 'bufnum)
                         (slot-ref obj 'samples-values))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-setn>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-setn (slot-ref obj 'bufnum)
                          (slot-ref obj 'samples-values))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-fill>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-fill (slot-ref obj 'bufnum)
                          (slot-ref obj 'start-sample) (slot-ref obj 'num-samples)
                          (slot-ref obj 'value))))
    (send-msg msg io)))

(define-method* (write-event (obj <buffer-close>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-close (slot-ref obj 'bufnum))))
    (send-msg msg io)))

(defobject buffer-query (sc-cmd) 
  ((bufnums :initform #f))
  (:parameters bufnums)
  (:event-streams))

(define-method* (write-event (obj <buffer-query>) (io <sc-stream>) time)
  time
  (let ((bufnums (slot-value obj 'bufnums))
        (msg (list "/b_query")))
    (if (pair? bufnums)
        (set! msg (append! msg bufnums))
      (set! msg (append! msg (list bufnums))))
    (send-msg msg io)))

(defobject buffer-get (sc-cmd) 
  ((bufnum :initform #f)
   (samples :initform #f))
  (:parameters bufnum samples)
  (:event-streams))

(define-method* (write-event (obj <buffer-get>) (io <sc-stream>) time)
  time
  (let ((samples (slot-value obj 'samples))
        (msg (list "/b_get" (slot-value obj 'bufnum))))
    (if (pair? samples)
        (set! msg (append! msg (list samples)))
      (set! msg (append! msg (list samples))))
    (send-msg msg io)))

(defobject buffer-getn (sc-cmd) 
  ((bufnum :initform #f)
   (samples :initform #f)
   (num-samples :initform #f))
  (:parameters bufnum samples num-samples)
  (:event-streams))

(define-method* (write-event (obj <buffer-getn>) (io <sc-stream>) time)
  time
  (let* ((bufnum (slot-value obj 'bufnum))
         (samples (slot-value obj 'samples))
         (samples-len #f)
         (num-samples (slot-value obj 'num-samples))
         (msg (list "/b_getn" bufnum)))
    (if (pair? samples)
        (begin
          (set! samples-len (length samples))
          (do ((i 0 (+ i 1)))
              ((> i samples-len))
            (set! msg (append! msg (list (list-ref samples i) (list-ref num-samples i))))))
      (set! msg (append! msg (list samples num-samples))))
    (send-msg msg io)))


(define-method* (write-event (obj <buffer-gen>) (io <sc-stream>) time)
  time
  (let ((msg (buffer-gen (slot-ref obj 'bufnum) (slot-ref obj 'command)
                         (slot-ref obj 'flags) (slot-ref obj 'args))))
    (send-msg msg io)))

(define-method* (write-event (obj <control-set>) (io <sc-stream>) time)
  time
  (let ((msg (control-set (slot-ref obj 'bus) (slot-ref obj 'value))))
    (send-msg msg io)))

(define-method* (write-event (obj <control-setn>) (io <sc-stream>) time)
  time
  (let ((msg (control-setn (slot-ref obj 'bus)
                           (slot-ref obj 'value))))
    (send-msg msg io)))

(define-method* (write-event (obj <control-fill>) (io <sc-stream>) time)
  time
  (let ((msg (control-fill (slot-ref obj 'bus) (slot-ref obj 'num-buses)
                           (slot-ref obj 'value))))
    (send-msg msg io)))

(defobject control-get (sc-cmd) 
  ((bus :initform #f))
  (:parameters bus)
  (:event-streams))

(define-method* (write-event (obj <control-get>) (io <sc-stream>) time)
  time
  (let ((controls (slot-value obj 'bus))
        (msg (list "/c_get")))
    (if (pair? controls)
        (set! msg (append! msg controls))
      (set! msg (append! msg (list controls))))
    (send-msg msg io)))

(defobject control-getn (sc-cmd) 
  ((bus :initform #f)
   (num-buses :initform #f))
  (:parameters bus num-buses)
  (:event-streams))

(define-method* (write-event (obj <control-getn>) (io <sc-stream>) time)
  time
  (let* ((buses (slot-value obj 'bus))
         (buses-len (length buses))
         (num-buses (slot-value obj 'num-buses))
         (msg (list "/c_getn")))
    (if (pair? buses)
        (do ((i 0 (+ i 1)))
            ((> i buses-len))
          (set! msg (append! msg (list (list-ref buses i) (list-ref num-buses i)))))
      (set! msg (append! msg (list buses num-buses))))
    (send-msg msg io)))

(define-method* (write-event (obj <sc-buffer>) (io <sc-stream>) time)
  (cond ((slot-ref obj 'with-file)
         (write-event (make <buffer-alloc-read> :bufnum (slot-ref obj 'bufnum) :file (slot-ref obj 'with-file) :frames (slot-ref obj 'frames)
                            :start-frame (slot-ref obj 'starting-at)) io time))
        ((pair? (slot-ref obj 'with-values))
         (write-event (make <buffer-alloc> :bufnum (slot-ref obj 'bufnum) :frames (slot-ref obj 'frames) :channels 1) io time)
         (write-event (make <buffer-setn> :bufnum (slot-ref obj 'bufnum)
                            :samples-values (list (slot-ref obj 'starting-at) (slot-ref obj 'with-values))) io time))
        ((number? (slot-ref obj 'with-values))
         (write-event (make <buffer-alloc> :bufnum (slot-ref obj 'bufnum) :frames (slot-ref obj 'frames) :channels 1) io time)
         (write-event (make <buffer-fill> :bufnum (slot-ref obj 'bufnum) :num-samples (- (slot-ref obj 'frames) (slot-ref obj 'starting-at))
                            :value (slot-ref obj 'with-values)) io time))
        ((procedure? (slot-ref obj 'with-values))
         (let ((vals '()))
           (dotimes (i (- (slot-ref obj 'frames) (slot-ref obj 'starting-at)))
             (set! vals (append! vals (list (apply (slot-ref obj 'with-values) '())))))
           (write-event (make <buffer-alloc> :bufnum (slot-ref obj 'bufnum) :frames (slot-ref obj 'frames) :channels 1) io time)
           (write-event (make <buffer-setn> :bufnum (slot-ref obj 'bufnum) :samples-values (list (slot-ref obj 'starting-at) vals)) io time)))
        ((slot-ref obj 'with-gen)
         (write-event (make <buffer-alloc> :bufnum (slot-ref obj 'bufnum) :frames (slot-ref obj 'frames) :channels 1) io time)
         (write-event (make <buffer-gen> :bufnum (slot-ref obj 'bufnum)
                            :flags (if (slot-ref obj 'flags)
                                       (slot-ref obj 'flags)
                                     :wavetable)
                            :command (car (slot-ref obj 'with-gen))
                            :args (car (cdr (slot-ref obj 'with-gen)))) io time))))

(define (sc-quit . args)
  (with-args (args &optional out)
    (let ((msg '("/quit")))
      (if out
        (send-msg msg out)
        (if *out*
            (send-msg msg *out*))))))

(define (sc-dumposc bool . args)
  (with-args (args &optional out)
    (let ((msg (list "/dumpOSC" (if bool 1 0))))
      (if out
          (send-msg msg out)
        (if *out*
            (send-msg msg *out*))))))

(define (sc-status . args)
  (with-args (args &optional out)
    (let ((msg (list "/status")))
      (if out
          (send-msg msg out)
        (if *out*
            (send-msg msg *out*))))))

(define (sc-clearsched . args)
  (with-args (args &optional out)
    (let ((msg '("/clearSched")))
      (if out
          (send-msg msg out)
        (if *out*
            (send-msg msg *out*))))))

(define (sc-open . args)
  (set! *out* (apply (function open-io) "sc.udp" #t args)))

(define (sc-open?)
  (let ((sc (find-object "sc.udp")))
    (and sc (io-open sc) sc)))

(define (sc-close)
  (if (sc-open?)
      (begin
        (sc-clearsched)
        (close-io (find-object "sc.udp")))
    #f))

(define (sc-flush . args)
  (with-args (args &optional out)
    (if out
        (begin
          (sc-clearsched out)
          (write-event (make <group-free-all> :group 0) out 0))
      (begin
        (sc-clearsched (sc-open?))
        (write-event (make <group-free-all> :group 0) (sc-open?) 0)))))

  
(define (sc-notify bool . args)
  (with-args (args &optional out)
    (let ((msg (list "/notify" (if bool 1 0))))
      (if out (send-msg msg out) (if *out* (send-msg msg *out*))))))


(define-method* (reply-set-slots (obj <top>) lst)
  lst
  obj
  #f)

(eval-when (:compile-toplevel :load-toplevel :execute)
           (define-class* <sc-reply> () 
             ()
             :name 'sc-reply))

(define *reply-objects* '(/done done-reply  /fail fail-reply /status.reply status-reply
                                status.reply status-reply
                             /synced synced-reply /s_set synth-get-reply
                             /s_setn synth-getn-reply /b_set buffer-get-reply
                             /b_setn buffer-getn-reply /b_info buffer-info-reply
                             /c_set control-get-reply /c_setn control-getn-reply
                             /n_go node-go-reply /n_end node-end-reply
                             /n_off node-off-reply /n_on node-on-reply
                             /n_move node-move-reply /n_info node-info-reply
                             /tr trigger-reply))


(define-class* <done-reply> (<sc-reply>)
  ((cmd-name :init-value #f :accessor done-cmd-name))
  :name 'done-reply)

(define-method* (reply-set-slots (obj <done-reply>) lst)
  (slot-set! obj 'cmd-name (pop lst)))



(define-class* <fail-reply> (<sc-reply>)
  ((cmd-name :init-value #f :accessor fail-cmd-name)
   (error :init-value #f :accessor fail-error))
  :name 'fail-reply)

(define-method* (reply-set-slots (obj <fail-reply>) lst)
  (slot-set! obj 'cmd-name (pop lst))
  (slot-set! obj 'error (pop lst)))

(define-class* <status-reply> (<sc-reply>)
  ((num-ugens :init-value #f :accessor status-num-ugens)
   (num-synths :init-value #f :accessor status-num-synths)
   (num-groups :init-value #f :accessor status-num-groups)
   (num-loaded-synths :init-value #f :accessor status-num-loaded-synths)
   (avg-cpu :init-value #f :accessor status-avg-cpu)
   (peak-cpu :init-value #f :accessor status-peak-cpu)
   (sample-rate :init-value #f :accessor status-sample-reate)
   (actual-sample-rate :init-value #f :accessor status-actual-sample-rate))
  :name 'status-reply)

(define-method* (reply-set-slots (obj <status-reply>) lst)
  (pop lst)
  (slot-set! obj 'num-ugens (pop lst))
  (slot-set! obj 'num-synths (pop lst))
  (slot-set! obj 'num-groups (pop lst))
  (slot-set! obj 'num-loaded-synths (pop lst))
  (slot-set! obj 'avg-cpu (pop lst))
  (slot-set! obj 'peak-cpu (pop lst))
  (slot-set! obj 'sample-rate (pop lst))
  (slot-set! obj 'actual-sample-rate (pop lst)))

(define-class* <synced-reply> (<sc-reply>)
  ((id :init-value #f :accessor synced-id))
  :name 'synced-reply)

(define-method* (reply-set-slots (obj <synced-reply>) lst)
  (slot-set! obj 'id (pop lst)))


(define-class* <synth-get-reply> (<sc-reply>)
  ((node :init-value #f :accessor synth-get-node)
   (controls-values :init-value #f :accessor synth-get-controls-values))
  :name 'synth-get-reply)

(define-method* (reply-set-slots (obj <synth-get-reply>) lst)
  (let ((cv (list)))
    (slot-set! obj 'node (pop lst))
    (dolist (i lst)
      (cond ((string? i)
             (set! cv (append! cv (list (string->keyword i)))))
            ((symbol? i)
             (set! cv (append! cv (list (symbol->keyword i)))))
            (#t
             (set! cv (append! cv (list i))))))
    (slot-set! obj 'controls-values cv)))


(define-class* <synth-getn-reply> (<sc-reply>)
  ((node :init-value #f :accessor synth-getn-node)
   (controls-values :init-value #f :accessor synth-getn-controls-values))
  :name 'synth-getn-reply)

(define-method* (reply-set-slots (obj <synth-getn-reply>) lst)
  (let ((cv (list)))
    (slot-set! obj 'node (pop lst))
    (do ((c #f) (n 0) (v #f))
        ((not (pair? lst)))
      (set! v (list))
      (set! c (pop lst))
      (cond ((string? c)
             (set! cv (append! cv (list (string->keyword c)))))
            ((symbol? c)
             (set! cv (append! cv (list (symbol->keyword c)))))
            (#t
             (set! cv (append! cv (list c)))))
      (set! n (pop lst))
      (dotimes (j n)
        (set! v (append! v (list (pop lst)))))
      (set! cv (append! cv (list v))))
    (slot-set! obj 'controls-values cv)))


(define-class* <buffer-get-reply> (<sc-reply>)
  ((bufnum :init-value #f :accessor buffer-get-bufnum)
   (samples-values :init-value #f :accessor buffer-get-samples-values))
  :name 'buffer-get-reply)

(define-method* (reply-set-slots (obj <buffer-get-reply>) lst)
  (slot-set! obj 'bufnum (pop lst))
  (slot-set! obj 'samples-values lst))


(define-class* <buffer-getn-reply> (<sc-reply>)
  ((bufnum :init-value '() :accessor buffer-getn-bufnum)
   (samples-values :init-value '() :accessor buffer-getn-samples-values))
  :name 'buffer-getn-reply)

(define-method* (reply-set-slots (obj <buffer-getn-reply>) lst)
  (let ((cv (list)))
    (slot-set! obj 'bufnum (pop lst))
    (do ((n 0) (v #f))
        ((not (pair? lst)))
      (set! v (list))
      (set! cv (append! cv (list (pop lst))))
      (set! n (pop lst))
      (dotimes (j n)
        (set! v (append! v (list (pop lst)))))
      (set! cv (append cv (list v))))
    (slot-set! obj 'samples-values cv)))


(define-class* <buffer-query-reply> (<sc-reply>)
  ((bufnum :init-value '() :accessor buffer-query-bufnum)
   (num-frames :init-value '() :accessor buffer-query-num-frames)
   (num-chans :init-value '() :accessor buffer-query-num-chanes)
   (sample-rate :init-value '() :accessor buffer-query-sample-rate))
  :name 'buffer-query-reply)

(define-method* (reply-set-slots (obj <buffer-query-reply>) lst)
  (let ((len (length lst)))
    (if (= len 4)
        (begin
          (slot-set! obj 'bufnum (pop lst))
          (slot-set! obj 'num-frames (pop lst))
          (slot-set! obj 'num-chans (pop lst))
          (slot-set! obj 'sample-rate (pop lst)))
      (do () ((not (pair? lst)))
        (slot-set! obj 'bufnum (append! (slot-ref obj 'bufnum) (list (pop lst))))
        (slot-set! obj 'num-frames (append! (slot-ref obj 'num-frames) (list (pop lst))))
        (slot-set! obj 'num-chans (append! (slot-ref obj 'num-chans) (list (pop lst))))
        (slot-set! obj 'sample-rate (append! (slot-ref obj 'sample-rate) (list (pop lst))))))))


(define-class* <control-get-reply> (<sc-reply>)
  ((bus :init-value '() :accessor control-get-bus)
   (value :init-value '() :accessor control-get-value))
  :name 'control-get-reply)

(define-method* (reply-set-slots (obj <control-get-reply>) lst)
  (if (= (length lst) 2)
      (begin
        (slot-set! obj 'bus (pop lst))
        (slot-set! obj 'value (pop lst)))
    (do () ((not (pair? lst)))
      (slot-set! obj 'bus (append! (slot-ref obj 'bus) (list (pop lst))))
      (slot-set! obj 'value (append! (slot-ref obj 'value) (list (pop lst)))))))


(define-class* <control-getn-reply> (<sc-reply>)
  ((bus :init-value '() :accessor control-getn-reply)
   (value :init-value '() :accessor control-getn-value))
  :name 'control-getn-reply)

(define-method* (reply-set-slots (obj <control-getn-reply>) lst)
  (let ((b #f) (n #f)
        (v (list)))
    (set! b (pop lst))
    (set! n (pop lst))
    (dotimes (i n)
      (set! v (append! v (list (pop lst)))))
    (if (> (length lst) n)
        (begin
          (slot-set! obj 'bus (append! (slot-ref obj 'bus) (list b)))
          (slot-set! obj 'value (append! (slot-ref obj 'value) (list v)))
          (do () ((not (pair? lst)))
            (slot-set! obj 'bus (append! (slot-ref obj 'bus) (pop lst)))
            (set! v (list))
            (set! n (pop lst))
            (dotimes (i n)
              (set! v (append! v (list (pop lst)))))
            (slot-set! obj 'value (append! (slot-ref obj 'value) (list v)))))
      (begin
        (slot-set! obj 'bus b)
        (slot-set! obj 'value v)))))

(define-class* <node-go-reply> (<sc-reply>)
  ((node :init-value #f :accessor node-go-node)
   (parent-group :init-value #f :accessor node-go-parent-node)
   (previous-node :init-value #f :accessor node-go-previous-node)
   (next-node :init-value #f :accessor node-go-next-node)
   (type :init-value #f :accessor node-go-type)
   (head-node :init-value #f :accessor node-go-head-node)
   (tail-node :init-value #f :accessor node-go-tail-node))
  :name 'node-go-reply)

(define-method* (reply-set-slots (obj <node-go-reply>) lst)
  (let ((n #f))
    (slot-set! obj 'node (pop lst))
    (slot-set! obj 'parent-group (pop lst))
    (slot-set! obj 'previous-node (pop lst))
    (slot-set! obj 'next-node (pop lst))
    (set! n (pop lst))
    (slot-set! obj 'type (if (= n 1) 'group 'synth))
    (if (= n 1)
        (begin
          (slot-set! obj 'head-node (pop lst))
          (slot-set! obj 'tail-node (pop lst))))))


(define-class* <node-end-reply> (<sc-reply>)
  ((node :init-value #f :accessor node-end-node)
   (parent-group :init-value #f :accessor node-end-parent-group)
   (previous-node :init-value #f :accessor node-end-previous-node)
   (next-node :init-value #f :accessor node-end-next-node)
   (type :init-value #f :accessor node-end-type) 
   (head-node :init-value #f :accessor node-end-head-node)
   (tail-node :init-value #f :accessor node-end-tail-node))
  :name 'node-end-reply)

(define-method* (reply-set-slots (obj <node-end-reply>) lst)
  (let ((n #f))
    (slot-set! obj 'node (pop lst))
    (slot-set! obj 'parent-group (pop lst))
    (slot-set! obj 'previous-node (pop lst))
    (slot-set! obj 'next-node (pop lst))
    (set! n (pop lst))
    (slot-set! obj 'type (if (= n 1) 'group 'synth))
    (if (= n 1)
        (begin
          (slot-set! obj 'head-node (pop lst))
          (slot-set! obj 'tail-node (pop lst))))))


(define-class* <node-off-reply> (<sc-reply>)
  ((node :init-value #f :accessor node-off-node)
   (parent-group :init-value #f :accessor node-off-parent-group)
   (previous-node :init-value #f :accessor node-off-previous-node)
   (next-node :init-value #f :accessor node-off-next-node)
   (type :init-value #f :accessor node-off-type)
   (head-node :init-value #f :accessor node-off-head-node) 
   (tail-node :init-value #f :accessor node-off-tail-node))
  :name 'node-off-reply)

(define-method* (reply-set-slots (obj <node-off-reply>) lst)
  (let ((n #f))
    (slot-set! obj 'node (pop lst))
    (slot-set! obj 'parent-group (pop lst))
    (slot-set! obj 'previous-node (pop lst))
    (slot-set! obj 'next-node (pop lst))
    (set! n (pop lst))
    (slot-set! obj 'type (if (= n 1) 'group 'synth))
    (if (= n 1)
        (begin
          (slot-set! obj 'head-node (pop lst))
          (slot-set! obj 'tail-node (pop lst))))))


(define-class* <node-on-reply> (<sc-reply>)
  ((node :init-value #f :accessor node-on-node)
   (parent-group :init-value #f :accessor node-on-parent-group)
   (previous-node :init-value #f :accessor node-on-previous-node)
   (next-node :init-value #f :accessor node-on-next-node)
   (type :init-value #f :accessor node-on-type)
   (head-node :init-value #f :accessor node-on-head-node)
   (tail-node :init-value #f :accessor node-on-tail-node))
  :name 'node-on-reply)

(define-method* (reply-set-slots (obj <node-on-reply>) lst)
  (let ((n #f))
    (slot-set! obj 'node (pop lst))
    (slot-set! obj 'parent-group (pop lst))
    (slot-set! obj 'previous-node (pop lst))
    (slot-set! obj 'next-node (pop lst))
    (set! n (pop lst))
    (slot-set! obj 'type (if (= n 1) 'group 'synth))
    (if (= n 1)
        (begin
          (slot-set! obj 'head-node (pop lst))
          (slot-set! obj 'tail-node (pop lst))))))


(define-class* <node-move-reply> (<sc-reply>)
  ((node :init-value #f :accessor node-move-node)
   (parent-group :init-value #f :accessor node-move-parent-group)
   (previous-node :init-value #f :accessor node-move-previous-node)
   (next-node :init-value #f :accessor node-move-next-node)
   (type :init-value #f :accessor node-move-type)
   (head-node :init-value #f :accessor node-head-node)
   (tail-node :init-value #f :accessor node-tail-node))
  :name 'node-move-reply)

(define-method* (reply-set-slots (obj <node-move-reply>) lst)
  (let ((n #f))
    (slot-set! obj 'node (pop lst))
    (slot-set! obj 'parent-group (pop lst))
    (slot-set! obj 'previous-node (pop lst))
    (slot-set! obj 'next-node (pop lst))
    (set! n (pop lst))
    (slot-set! obj 'type (if (= n 1) 'group 'synth))
    (if (= n 1)
        (begin
          (slot-set! obj 'head-node (pop lst))
          (slot-set! obj 'tail-node (pop lst))))))

(define-class* <node-info-reply> (<sc-reply>)
  ((node :init-value #f :accessor node-info-node)
   (parent-group :init-value #f :accessor node-info-parent-group)
   (previous-node :init-value #f :accessor node-info-previous-node)
   (next-node :init-value #f :accessor node-info-next-node)
   (type :init-value #f :accessor node-info-type)
   (head-node :init-value #f :accessor node-info-head-node)
   (tail-node :init-value #f :accessor node-info-tail-node))
  :name 'node-info-reply)

(define-method* (reply-set-slots (obj <node-info-reply>) lst)
  (let ((n #f))
    (slot-set! obj 'node (pop lst))
    (slot-set! obj 'parent-group (pop lst))
    (slot-set! obj 'previous-node (pop lst))
    (slot-set! obj 'next-node (pop lst))
    (set! n (pop lst))
    (slot-set! obj 'type (if (= n 1) 'group 'synth))
    (if (= n 1)
        (begin
          (slot-set! obj 'head-node (pop lst))
          (slot-set! obj 'tail-node (pop lst))))))


(define-class* <trigger-reply> (<sc-reply>)
  ((node :init-value #f :accessor trigger-node)
   (id :init-value #f :accessor trigger-id)
   (value :init-value #f :accessor trigger-value))
  :name 'trigger-reply)

(define-method* (reply-set-slots (obj <trigger-reply>) lst)
  (slot-set! obj 'node (pop lst))
  (slot-set! obj 'id (pop lst))
  (slot-set! obj 'value (pop lst)))


  
(define-method* (receive (str <sc-stream>) . args)
  (let* ((n 0)
        (in #f)
        (hook (if (pair? args) (pop args) #f))
        (mode (if (pair? args) (pop args) #f))
        (rm (if (not mode) #t (eq? mode ':message)))
        (fn #f)
        (res #f))
    (if (io-open str)
        (set! in str))
    (if in
        (begin
          (set! n (udp-socket-recv (slot-ref in 'socket) (slot-ref str 'buffer-size)))
          (if n
              (begin
                (if hook
                    (begin
                      (set! fn (lambda (mm)
                                 (hook (osc-vector->osc-message mm))))
                      (if rm (funcall fn n)
                        (hook n))
                      (set! res #t))
                  (begin
                    (if rm
                        (set! res (osc-vector->osc-message n))
                      (set! res n))))))
          res)
      (err "osc stream ~s not open" str))))

(define-method* (init-receiver (str <sc-stream>) type)
  type
  (unless (io-open str)
    (open-io str nil)))

(define-method* (deinit-receiver (str <sc-stream>) type)
  type
  (let ((data (rt-stream-receive-data str)))
    (when (io-open str)
      (close-io str))
    (list-set! data 0 #f)
    (list-set! data 1 #f)))

(define-method* (set-receive-mode! (str <sc-stream>) mode)
  (unless (member mode '(:message :raw :object))
    (err "receive: ~s is not a sc receive mode." mode))
  (slot-set! str 'receive-mode mode))

(define (osc-message->sc-object lst)
  (let* ((obj? (list-prop *reply-objects* (pop (car lst))))
	 (obj (if obj? (make obj?) nil)))
    (if obj
	(reply-set-slots obj (car lst)))
    obj))

(define-method* (stream-receiver hook (str <sc-stream>) type)
  ;; hook is 2arg lambda or nil, type is :threaded or :periodic
  (let* ((data (rt-stream-receive-data str)) ; (<thread> <stop> )
         (mode (rt-stream-receive-mode str))
         (stop #f)) 
    ;; can receive either message or raw buffer
    (unless (member mode '(:message :raw :object))
      (err "receive: ~s is not a osc receive mode." mode))
    ;; the receiving thread's do loop terminates as soon as the stop
    ;; flag is #t. to stop we call the cached "stopper" closure that
    ;; sets the var to #t.
    (cond ((not (procedure? hook))
           (err "Receive: hook is not a function: ~s" hook))
          ((first data)
           (err "Can't set input hook: another hook is running!"))
          (else
           ;; ready to go
           (let* ((in str)
                  (rm (or (eq? mode ':message) (eq? mode ':object)))
                  (th #f) ; thread
                  (st #f) ; thread stopper
                  (fn #f) ; mapper
                  (ra (rt-stream-receive-rate str))
                  )
             (case mode
               ((:message)
                (set! fn (lambda (mm)
                        ( hook (osc-vector->osc-message mm)))))
               ((:object)
                (set! fn (lambda (mm)
                           ( hook (osc-message->sc-object (osc-vector->osc-message mm) ))))))
             (case type
               ((:threaded )
                (set! th
                      (make-thread
                       (lambda ()
                         (do ((n #f))
                             (stop  
                              #f)
                           (set! n (udp-socket-recv (slot-ref in 'socket) (slot-ref str 'buffer-size)))
                           (if n
                               (if rm
                                   (funcall fn n)
                                 ( hook n))
                             ;; only sleep if no message??
                             (thread-sleep! ra))))))
                (set! st (lambda () (set! stop #t))))
               ((:periodic )
                (set! th
                      (lambda () 
                        (let ((n 0))
                          (set! n (udp-socket-recv (slot-ref in 'socket) (slot-ref str 'buffer-size)))
                          (if n
                              (if rm
                                  (funcall fn n)
                                ( hook n))))))
                (set! st (lambda ()
                           (remove-periodic-task! :receive))))
               )
             ;; cache the stuff
             (list-set! data 0 th)
             (list-set! data 1 st)
             th)))))



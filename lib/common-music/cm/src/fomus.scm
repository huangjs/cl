;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube
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
;;; $Revision: 1.20 $
;;; $Date: 2007/03/25 16:08:04 $

;;;
;;; Interface to David Psenicka's Fomus notation package.
;;;

(in-package :cm)

(define-class* <fomus-file> (<event-file>)
  ((parts :init-keyword :parts :init-value '()
          :accessor fomus-file-parts)
   (global :init-keyword :global :init-value '()
           :accessor fomus-file-global)
   (view :init-keyword :view :init-value #t :accessor fomus-file-view)
   (play :init-keyword :play :init-value #f :accessor fomus-file-play)
   (tempo :init-keyword :tempo :init-value 60 :accessor fomus-file-tempo)
   )
  :name 'fomus-file
  :metaclass <io-class>
  :file-types '("*.fms" "*.xml" "*.ly")) 
         
(define-method* (object-time (obj <event-base>))
  (event-off obj))

(define-method* (open-io (io <fomus-file>) dir . args)
  args
  (when (eq? dir ':output)
    (let ((parts (fomus-file-parts io))
          (globs (fomus-file-global io)))
      ;; check for singles
      (if (not (pair? parts))
          (set! (fomus-file-parts io) 
                (if (null? parts) (list) (list parts))))
      (if (not (pair? globs))
          (set! (fomus-file-global io) 
                (if (null? globs) (list) (list globs))))
      ;; flush existing data from parts
      (for-each (lambda (p)
                  (set! (part-events p) (list)))
                (fomus-file-parts io))
      (set! (io-open io) #t)))
  io)
    
(define-method* (close-io (io <fomus-file>) . mode)
  (let ((err? (and (not (null? mode))
                   (eq? (car mode) ':error))))
    (set! (io-open io) #f)
    (unless err?
      (let* ((args (event-stream-args io))
             (bend (list-prop args ':output)))
        (unless bend
          (let* ((file (file-output-filename io))
                 (type (filename-type file)))
            (cond ((equal? type "ly")
                   (set! bend (list ':lilypond :filename file
				    :view (fomus-file-view io))))
                  ((equal? type "xml")
		   (set! bend (list ':musicxml :filename file
				    :view (fomus-file-view io))))
                  (else
		   (set! bend (list ':fomus :filename file))))
	    (when (fomus-file-play io)
              (set! bend (list bend
			       (list ':midi :play #t
				     :filename 
				     (make-pathname :type "mid" :defaults file)
					  :tempo (fomus-file-tempo io)
					  ))))
            (set! args (cons* ':output bend args))))
        (apply (function fomus)
               :parts (fomus-file-parts io)
               :global (fomus-file-global io)
               args)))))

(define (fomus-file-part stream id)
  (do ((tail (fomus-file-parts stream) (cdr tail))
       (part #f))
      ((or (null? tail) part) 
       (when (not part)
         (set! part (make-part :partid id :instr (if (keyword? id) id #f)))
         (set! (fomus-file-parts stream)
               (cons part (fomus-file-parts stream))))
       part)
    (if (eq? id (obj-partid (car tail)))
        (set! part (car tail)))))

(define-method* (write-event (obj <event-base>) (fil <fomus-file>) scoretime)
  (let ((part (fomus-file-part fil (obj-partid obj))))
    ;; use score time not local time.
    (set! (event-off obj) scoretime)
    (set! (part-events part)
          (cons obj (part-events part)))
    obj))

(define-method* (write-event (obj <midi>) (fil <fomus-file>) scoretime)
  (let* ((myid (midi-channel obj))
         (part (fomus-file-part fil myid))
         (marks '()))
    (set! (part-events part)
          (cons (make-note :partid myid
                           :off scoretime
                           :note (midi-keynum obj)
                           :dur (midi-duration obj)
                           :marks marks)
                (part-events part)))))

(define (partid->part pid)
  (loop for p in *parts* thereis (eq? pid (obj-partid p))))

;; allow fomus parts to schedule their notes.
(define-method* (schedule-object (obj <part>) start sched)
  (let ((mystart (+ start 0)))
    (enqueue *qentry-seq* (cons obj (part-events obj))
	     mystart
	     mystart
	     sched)))

(define-method* (write-event (obj <note>) (fil <midi-file>) scoretime)
  (let* ((myid (obj-partid obj))
         (part (partid->part myid))
         (opts (if (not part) (list) (part-opts part)))
         (chan #f)
         (ampl 64))
    opts
;    ;; figure this out later...
;    (do ((tail opts (cddr tail)))
;        ((null? tail) #f)
;      (case (car tail)
;        (( :midi-chan ) (set! chan (cadr tail)))
;        (( :midi-vel ) (set! ampl (cadr tail)))
;        (( :midi-min-stacatto-dur ) #f)
;        (( :midi-stacatto-durratio ) #f)
;        (( :midi-trill-dur ) #f)
;        (( :midi-trill-vel ) #f)
;        (( :midi-min-grace-dur ) #f)
;        (( :midi-max-grace-dur ) #f)
;        (( :midi-grace-durratio ) #f)        
;        (( :midi-accent-velratio ) #f)
;        (( :midi-marcato-vel ) #f)
;        (( :midi-slur-overlap-dur ) #f)
;        (( :midi-tenuto-dur ) #f)
;        (( :midi-fermata-durratio ) #f)
;        (( :midi-breath-dur ) #f)
;        (( :midi-harmonic-prog ) #f)
;        (( :midi-pizz-prog ) #f)
;        (else #f)))
    ;; use id as default chan if its an integer 
    (unless chan (set! chan (if (integer? myid) myid 0)))
    ;; add dynamic if not same as last note.

    ;; at some point this should write the message rather
    ;; that call write-event with a midi note...
    ;; do the output
    (write-event (make <midi> :time (event-off obj)
                       :amplitude ampl
                       :keynum (event-note obj)
                       :duration (event-dur obj)
                       :channel chan)
                 fil scoretime)))

(define-method* (import-events (file <fomus-file>) . args)
  (with-args (args &key (seq #t))
    (let ((fil (file-output-filename file)))
      (cond ((or (not seq)
		 (is-a? seq <seq>))
	     #f)
	    ((eq? seq #t)
	     (set! seq (make <seq>
			     :name (format #f "~a-notes" (filename-name fil)))))
	    (else
	     (err "import-events: ~S is not a boolean or seq." seq)))
      (multiple-value-bind (parts notes globs sets)
	  (fomus-file fil)
	(set! (fomus-file-parts file) parts)
	(set! (fomus-file-global file) globs)
	(set! (event-stream-args file) sets)
	(if seq
	    (begin (set! (container-subobjects seq) notes)
		   seq)
	    notes)))))


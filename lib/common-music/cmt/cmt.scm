(in-package :cm)

(define *cmt-default-input* #f)
(define *cmt-default-output* #f)

(define-class* <cmt-stream> (<rt-stream>)
  ((file :init-value #f :init-keyword :file :accessor cmt-stream-file)
   (version :init-value 0 :accessor event-file-version
            :init-keyword :version)
   (versioning :init-value #f :init-keyword :versioning
               :accessor event-file-versioning)
   (elt-type :init-value :char :accessor file-elt-type
             :init-keyword :elt-type))
  :name 'cmt-stream
  :metaclass <io-class>)

(define-method* (open-io (obj <cmt-stream>) dir . args)
  args
  (let ((file #f)
        (name #f))
    (if (eq? dir :output)
        (cond ((event-stream-stream obj)
               (set! file (event-stream-stream obj)))
              (else 
               ;; mayby increment file version. done before writeing so
               ;; output name is constant until next time opened for
               ;; output
               (bump-version obj)
               (set! name (file-output-filename obj))
               ;; in scheme opening existing file is undefined...
               (if (file-exists? name) (delete-file name))
               (set! file (open-file name dir (file-elt-type obj)))))
      (if (eq? dir :input)
          (if (event-stream-stream obj)
              (set! file (event-stream-stream obj))
            (set! file (open-file (object-name obj) 
                                  dir (file-elt-type obj))))
        (err "Direction not :input or :output: ~s" dir)))
    (set! (io-direction obj) dir)
    (set! (io-open obj) file)
    (initialize-io obj)
    obj))
  
(define-method* (close-io (io <cmt-stream>) . mode)
  mode  ; gag 'unused var' warnings from cltl compilers
  (when (io-open io)
    (unless (event-stream-stream io)
      (close-file (io-open io) (io-direction io)))
    (set! (io-open io) #f))
  io)

(define-method* (write-message message (io <cmt-stream>))
  (let ((fd (io-open io)))
    (format fd message)
    (values)))

(define-method* (initialize-io (io <cmt-stream>))
  (when (eq? (io-direction io) ':output)
    (write-message (format #f "file opened at ~s ~%" (now)) io)))

(eval-when (:compile-toplevel :load-toplevel :execute)
           (define-class* <cmt-message> (<event>) 
             ()
             :name 'cmt-message))

(defobject cmt-on (cmt-message)
  ((args :initform #f))
   (:parameters args)
   (:event-streams))

(define-method* (write-event (obj <cmt-on>) (io <cmt-stream>) time)
  (let ((mess (format #f "cmt-on ~s ~s~%" (now) (slot-ref obj 'args))))
    (write-message  mess io)))

(defobject cmt-off (cmt-message)
  ((args :initform #f))
   (:parameters args)
   (:event-streams))

(define-method* (write-event (obj <cmt-off>) (io <cmt-stream>) time)
  (let ((mess (format #f "cmt-off ~s ~s" (now) (slot-ref obj 'args))))
    (write-message mess io)))

(defobject cmt (cmt-message)
  ((duration :initform 1.0)
   (args :initform #f))
   (:parameters duration args)
   (:event-streams))

(define-method* (write-event (obj <cmt>) (io <cmt-stream>) time)
  (new cmt-on :args (slot-ref obj 'args))
  (enqueue *qentry-message* (new cmt-off :args (slot-ref obj 'args)))
  (values))



(define (cmt-string->cmt-object str)
  (let* ((mess (string-split str #\ ))
        (obj #f)
        (id (pop mess)))
    (when (equal? id "cmt-on")
      (set! obj (new cmt-on :time (string->number (pop mess)) :args mess)))
    (when (equal? id "cmt-off")
      (set! obj (new cmt-on :time (string->number (pop mess)) :args mess)))
    obj))

(define (cmt-readline fd)
  (let ((mess (read-line fd)))
    (if (eof-object? mess)
        'eof
      (cmt-string->cmt-object mess))))

(define-method* (import-events (file <cmt-stream>) . args)
  args
  (with-open-io (obj file :input)
    (do ((str (io-open obj))
         (seq (list ))
         (mess #f)
         (end #f)
         (len 0))
        ((not (not end))
         (make <seq> :subobjects (reverse! (cddr seq))))
      (set! mess (cmt-readline str))
      (if (equal? 'eof mess)
          (set! end #t)
        (if mess
            (push  mess seq))))))



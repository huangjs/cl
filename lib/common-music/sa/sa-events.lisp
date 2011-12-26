(in-package :cm)

(progn (defclass sa-stream (rt-stream)
         ((sample-rate :initform 44100 :initarg :sample-rate)
	  (input-channels :initform 0 :initarg :input-channels)
	  (output-channels :initform 2 :initarg :output-channels)
	  (vector-size :initform 1024 :initarg :vector-size)
	  (busses :initform 0 :initarg :busses)
	  (object-pointer :initform nil))
         #+metaclasses
         (:metaclass io-class))
       (defparameter <sa-stream> (find-class 'sa-stream))
       (finalize-class <sa-stream>)
       (setf (io-class-file-types <sa-stream>) '("sa.dac"))
       (values))

(defmethod open-io ((obj sa-stream) dir &rest args)
  dir args
  (unless (io-open obj)
    (progn
      (setf (slot-value obj 'object-pointer)
	    (sa::RTAudioStream_Instance
	     (float (slot-value obj 'sample-rate))
	     (slot-value obj 'input-channels)
	     (slot-value obj 'output-channels)
	     (slot-value obj 'busses)
	     (slot-value obj 'vector-size)))
      (setf sa::*vector-size* (slot-value obj 'vector-size))
      (sa::RTAudioStream_Start (slot-value obj 'object-pointer) 80)
      (setf (slot-value obj 'open) t)))
  (setf *out* obj))

(defun sa-open (&rest args)
  (apply #'open-io "sa.dac" t args))

(defun sa-open? (&optional (sa (find-object "sa.dac")))
  (if sa
      (let ((io (io-open sa)))
	(if io 
	    sa
	    nil))))

(defmethod close-io ((obj sa-stream) &rest mode)
  mode
  (when (io-open obj)
    (sa::RTAudioStream_Stop (slot-value obj 'object-pointer))
    (setf (slot-value obj 'open) nil))
  (setf *out* nil))


(defmethod write-event ((obj sa::synth) (io sa-stream) time)
  time
  (if io
      (if (io-open io)
	  (progn
	    (sa::Synth_Add (slot-value obj 'sa::object-pointer))
	    t)
	  nil)
      nil))



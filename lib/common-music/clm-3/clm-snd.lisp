;;; new (26-May-00) communication path between Snd and CLM

(in-package :clm)

(defvar clm-x-inited nil)

(defun init-x ()
  (when (not clm-x-inited)
    (clm-init-x *clm-date*)
    (setf clm-x-inited t)))

(defun start-snd ()
  (clm-start-snd)
  (init-x))

(defun send-snd (cmd)
  (init-x)
  (let ((val (clm-send-snd cmd)))
    (when (= val -1)
      (start-snd)
      (sleep 1)
      (clm-send-snd cmd))))

(defun receive-snd (&optional (timeout 60))
  (init-x)
  (do ((i 0 (1+ i)))
      ((>= i timeout) nil)
    (let ((val (clm-receive-snd)))
      ;;(format t "l: ~A " val) (force-output)
      (if (and val (stringp val) (> (length val) 0))
	  (return-from receive-snd val))
      (sleep 1))))

(defun eval-snd (&optional with-eval)
  (let ((str (receive-snd)))
    (if str
	(if with-eval
	    (eval (read-from-string str))
	  (read-from-string str)))))

(defun send-snd-rsvp (str)
  (send-snd
   (concatenate 'string	"(set! (window-property \"CLM_VERSION\" \"CLM_COMMAND\") " str ")")))

(defun send-and-receive-snd (str &optional with-eval)
  (send-snd-rsvp str)
  (eval-snd with-eval))




;;; snd-sound: CLM asks Snd to send CLM the current state of file saved in a temp file (CLM should delete it)

(defvar tempfile-ctr 0)
(defvar snd-tempfiles nil)

(defun snd-sound (&optional file)
  (let ((tempfile (format nil "temp-~D.snd" tempfile-ctr)))
    (incf tempfile-ctr)
    (send-snd-rsvp
     (if file
	 (format nil "(save-sound-as ~S (find-sound ~S))" tempfile file)
       (format nil "(save-sound-as ~S)" tempfile)))
    (receive-snd)
    (pushnew tempfile snd-tempfiles :test #'string=)
    tempfile))


(defun snd-region (&optional (reg 0))
  (let ((tempfile (format nil "temp-~D.snd" tempfile-ctr)))
    (incf tempfile-ctr)
    (send-snd-rsvp
     (format nil "(save-region ~D ~S)" reg tempfile))
    (receive-snd)
    (pushnew tempfile snd-tempfiles :test #'string=)
    tempfile))


;;; snd-cleanup cleans up temp files created by imbedded snd-clm-samples calls
(defmacro snd-cleanup (&body body)
  `(let ((old-temps snd-tempfiles))
     (setf snd-tempfiles nil)
     (let ((val ,@body))
       (loop for file in snd-tempfiles do (if (and file (probe-file file)) (delete-file file)))
       (setf snd-tempfiles old-temps)
       val)))

;;; snd-edit-sound: CLM sends Snd the current (new) state of file
(defun snd-edit-sound (newname &optional oldname)
  (if oldname
      (send-snd (format nil "(set-samples 0 (mus-sound-frames ~S) ~S (find-sound ~S))" newname newname oldname))
    (send-snd (format nil "(set-samples 0 (mus-sound-frames ~S) ~S)" newname newname))))

;;; snd-edit: edit sound with temp file cleanup
(defmacro snd-edit ((&optional nfile) &body body)
  `(let ((newname (snd-cleanup ,@body)))
     (snd-edit-sound newname ,nfile)
     newname))

(defmacro to-snd ((&rest args) &body body)
  `(send-snd (format nil "(open-sound ~S)" (with-sound ,args ,@body))))

(defun snd-display (filename data len srate chans)
  (clm-initialize-links)
  (array->file filename data len srate chans)
  (send-snd (format nil "(update-sound (find-sound ~S))" (expand-filename->string filename))))

;;; (display data) sends the data array (double-float elements) to snd, starting snd if necessary

(defvar display-files nil)

(defmacro display (data &key name (srate 1) (channels 1))
  ;; need wrapper to get symbol name
  `(let ((filename (or ,name (concatenate 'string (symbol-name ',data) ".data"))))
     (pushnew filename display-files :test #'string=)
     (clm::snd-display filename ,data (length ,data) ,srate ,channels)))

;;; run-time display?

(defmacro undisplay (data &key name)
  `(let ((filename (or ,name (concatenate 'string (symbol-name ',data) ".data"))))
     (clm::snd-close (expand-filename->string filename))
     (setf display-files (delete filename display-files :test #'string=))
     (if (probe-file filename) (delete-file filename))))

;;; now to communicate envelopes between clm and snd:

(defmacro clm-envelope (e)
  `(send-snd (format nil "(defvar ~(~A~) '~A)" ',e ,e)))

(defmacro snd-envelope (e)
  `(send-and-receive-snd (format nil "~(~A~)" ',e)))


;;; ---- run-time send-snd
;;;
;;; handled as a form of clm-print (does this mean the instrument needs a call on XOpenDisplay?

(def-clm-fun 'send-snd #'(lambda (var x) (package-op '<send-snd> var x :clm-boolean nil t)))

(defmacro <send-snd> (result &optional fstr &rest args)
  #-(or (not (or cmu sbcl excl)) windoze x86-64)
  (let ((lst (gethash (second result) vars)))
    (if (and lst (varinfo-refd lst))
	(format *c-file* "  ~A = 0;~%" (lc (second result))))
    (format *c-file* "  {char *buf; buf = (char *)calloc(512,sizeof(char));~%   sprintf(buf,~A);~%   clm_send_snd(buf); free(buf);}~%"
	    (apply #'lisp-to-C-format nil fstr args)))
  nil)


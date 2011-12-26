
;; very simple server started on port

(and (find-package "MAXIMA") (push :maxima *features*))

#+maxima
(in-package "MAXIMA")



(defun user::setup ( port &optional (host "localhost"))
  (let* ((sock (open-socket host port)))
    (setq me sock)
   #+gcl (setq si::*sigpipe-action* 'si::bye)
    (setq *socket-connection* sock)
    (setq *standard-input* sock)
    (setq *standard-output* sock)
    (setq *error-output* sock)
    (setq *terminal-io* sock)
    (format t "pid=~a~%"        (getpid))
    (force-output sock)
    (setq *debug-io* sock)
    (values)
    ))

;;; from CLOCC: <http://clocc.sourceforge.net>
(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (socket-connect port host :element-type
                                 (if bin '(unsigned-byte 8) 'character))

    #+cmu (sys:make-fd-stream (ext:connect-to-inet-socket host port)
                              :input t :output t :element-type
                              (if bin '(unsigned-byte 8) 'character))
    #+gcl (si::socket port :host host)
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    #-(or allegro clisp cmu gcl lispworks)
    (error 'not-implemented :proc (list 'open-socket host port bin))))



#+maxima
(progn
(setq $in_netmath t)
(setq $show_openplot nil))


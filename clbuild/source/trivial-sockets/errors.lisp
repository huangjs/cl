(in-package :trivial-sockets)

;; you're using a part of the interface that the implementation doesn't do
(define-condition unsupported (error) 
  ((feature :initarg :feature :reader unsupported-feature)))

;; all-purpose error: host not found, host not responding,
;; no service on that port, etc
(define-condition socket-error (error)
  ((nested-error :initarg :nested-error :reader socket-nested-error)))


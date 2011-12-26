(in-package :cl-user)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init #P"/huang/cl/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+quicklisp
(ql:use-only-quicklisp-systems)

;; (compile
;;  (defun load-and-compile-if-necessary (source-file output-file)
;;    (when (not (probe-file output-file))
;;      (compile-file source-file :output-file output-file))
;;    (load output-file)))

;; #+allegro (require :asdf)
;; #+sbcl (require :asdf)
;; #+lispworks (assert (find-package :asdf))
;; ;; (load-and-compile-if-necessary
;; ;; 	     (merge-pathnames "cl/deps/asdf.lisp" (user-homedir-pathname))
;; ;; 	     #+LISPWORKS-64BIT (merge-pathnames "cl/deps/asdf.lw.64ufasl" (user-homedir-pathname))
;; ;; 	     #+LISPWORKS-32BIT (merge-pathnames "cl/deps/asdf.lw.ufasl" (user-homedir-pathname)))
;; #+ccl (require :asdf)

(load (merge-pathnames "cl/deps/asdf-config.lisp" (user-homedir-pathname)))

(defun asdf (package)
  (asdf:oos 'asdf:compile-op package)
  (asdf:oos 'asdf:load-op package))

;; (asdf :swank)

;;; display control
(setf *print-length* 100
      *print-lines* 80
      *print-array* t
      *print-pretty* t)

;;; my libs
(ql:quickload :alexandria)
(ql:quickload :iterate)
;; (load-clocc-system :screamer)
(setf iterate::*always-declare-variables* t)

;; (progn
;;   (asdf :cl-interpol)
;;   (asdf :cl-fad)
;;   (asdf :parse-number)
;;   (asdf :split-sequence)
;;   (asdf :mw-equiv)
;;   (asdf :net-telent-date)
;;   (asdf :closer-mop)
;;   (asdf :meta)
;;   (asdf :alexandria)

;;   (swank:swank-require :swank-arglists)

;;   ;;; FIXME: conflict with SBCL
;; ;;;   (asdf :hjs-lib) 
;; ;;;   (asdf :hjs-utils)

;;   (asdf :cffi)
;;   (asdf :babel)
;;   (asdf :cl-ppcre)
;;   (asdf :cl-unification)
;;   (asdf :portable-threads)
;;   (asdf :metabang-bind)
;;   (asdf :metacopy)
;;   (asdf :osicat)
  
;;   (asdf :trivial-backtrace)
;;   (asdf :trivial-features)
;;   (asdf :trivial-garbage)
;;   (asdf :trivial-gray-streams)
;;   (asdf :trivial-shell)
;;   (asdf :trivial-utf-8)
;;   )

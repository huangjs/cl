;;; random.lisp --- Random number generator.

;;; Author: R. Scott McIntire

;;; History:
;;  Version 1.0: Aug 2003

(defpackage :rsm.random
  (:use :common-lisp)
  (:documentation
   "This package provides a high quality random number generator.

Export Summary:

b-rand: Produces a uniform binary random number. That is its values are 0 or 1.
u-rand: Produces a uniform random number (double-float) on the interval [0,1).
i-rand: Produce a random number on the interval 
        [0, min(2^32, most-positive-fixnum)).
init  : Initialize the random number generator.
set-rand: Set the seed of the random number generator.(takes one arg, a fixnum).
")
  (:export 
   #:b-rand
   #:init
   #:i-rand
   #:u-rand
   ))

(in-package rsm.random)

;;; To use the foreign function interface with the rule-30 random 
;;; number generator. Add the following.
;;; Note: Since the ff's have been added after the in-package statement,
;;; they will be local to the package.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (uffi:def-function ("urand" u-rand) () 
    :returning :double
    :module "rsm-random")
  (uffi:def-function ("brand" b-rand) () 
    :returning :int
    :module "rsm-random")
  (uffi:def-function ("random" ii-rand) () 
    :returning :int
    :module "rsm-random")
  (uffi:def-function ("rand_init" init)
      ((x :int))
    :returning :void
    :module "rsm-random")
  (init 123451))


(defun i-rand ()
  "Return a uniform random number on the interval 
[0, min(2^32, most-positive-fixnum))."
  (mod (ii-rand) most-positive-fixnum))


(defun init/current-time ()
  "Initialize the random number generator using the current time."
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (logand (+ (get-universal-time) 
	     (get-internal-real-time))
	  #xFFFFFFFF))


(defun init/file-path (path-name)
  "Initialize the random number generator using a seed from a file."
  (with-open-file (str path-name :direction :input)
    (let ((seed (parse-integer (read str) :junk-allowed t)))
      (if seed
          (init (logand (abs seed) #xFFFFFFFF))
        (error "init/file-path: Bad seed from path ~s~%" path-name))
      (init seed))))

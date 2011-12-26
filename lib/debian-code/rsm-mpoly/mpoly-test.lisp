;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.mpoly.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mpoly-test.lisp
;;;; Purpose:       Regression testing for multivariate polynomial arithmetic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: mpoly-test.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.mpoly.test
  (:use #:cl #:ptester)
  (:documentation
   "Provides a test harness for multivariate polynomial arithmetic.")
  )


(in-package rsm.mpoly.test)


(defparameter *foo* (rsm.mpoly:poly (2 (1 3)) (10 (2 1)) (4 (1 1))))
(defparameter *goo* (rsm.mpoly:poly (3 (1 1)) (-5 (1 2)) (-1 (2 1))))
(defparameter *boo* (rsm.mpoly:poly (2 (1 1)) (5 (2 1)) (3 (1 0)) (11 (0 0))))
(defparameter *doo* (rsm.mpoly:poly (3.2 (1 1)) (#c(3 4) (1 2)) (-1 (2 1))))


(defun run-mpoly-tests ()

  (with-tests (:name "MULTIVARIATE POLYNOMIAL TESTS")
    
    (test '(2 . #(1 3))
          (rsm.mpoly:term 2 (1 3))
          :test #'equalp)
    
    (test (rsm.mpoly:poly (4 (2 1)) (#c(3 4) (1 2)) (5.2 (1 1)) 
                    (3 (1 0)) (11 (0 0)))
          (rsm.mpoly:+ *boo* *doo*)
          :test #'equalp)
    
    (test (rsm.mpoly:poly (9 (2 1)) (2 (1 3)) (-5 (1 2)) (7 (1 1)))
          (rsm.mpoly:+ *foo* *goo*)
          :test #'equalp)
    
    (test (rsm.mpoly:poly (2 (1 3)) (10 (2 1)) (4 (1 1)) (10 (0 0)))
          (rsm.mpoly:+ *foo* 10)
          :test #'equalp)
    
    (test (rsm.mpoly:poly (2 (1 3)) (10 (2 1)) (4 (1 1)) (1 (0 0)))
          (rsm.mpoly:+ *foo* 1)
          :test #'equalp)

    (test (rsm.mpoly:poly (2 (1 1)) (5 (2 1)) (3 (1 0)) (32 (0 0)))
          (rsm.mpoly:+ *boo* 21)
          :test #'equalp)

    (test (rsm.mpoly:poly (-10 (4 2)) (-2 (3 4)) (-50 (3 3)) (26 (3 2))
                    (-10 (2 5)) (6 (2 4)) (-20 (2 3)) (12 (2 2)))
          (rsm.mpoly:* *foo* *goo*)
          :test #'equalp)

    (test (rsm.mpoly:poly (-60 (4 2)) (-12 (3 4)) (-300 (3 3)) (156 (3 2))
                    (-60 (2 5)) (36 (2 4)) (-120 (2 3)) (72 (2 2)))
          (rsm.mpoly:* *foo* 2 *goo* 3)
          :test #'equalp)
    
    (test (rsm.mpoly:poly (6 (2 2)) (15 (3 2)) (9 (2 1)) (33 (1 1))
                    (-10 (2 3)) (-25 (3 3)) (-15 (2 2)) (-55 (1 2))
                    (-2 (3 2)) (-5 (4 2)) (-3 (3 1)) (-11 (2 1)))
          (rsm.mpoly:* *goo* *boo*)
          :test #'equalp)
    
    (test (rsm.mpoly:poly (1000 (6 3)) (600 (5 5)) (1200 (5 3)) (120 (4 7))
                    (480 (4 5)) (480 (4 3)) (8 (3 9)) (48 (3 7))
                    (96 (3 5)) (64 (3 3)))
          (rsm.mpoly:^ *foo* 3)
          :test #'equalp)

    ;; Takes several seconds.
    (unwind-protect
        (progn
          ;; Set the modulus for the next test.
          (rsm.mpoly:set-modulus 17)
          (test (rsm.mpoly:poly 
                 (4 (16 16)) (13 (16 14)) (8 (16 12)) (1 (16 10))
                 (15 (16 8)) (4 (16 6)) (9 (16 4)) (16 (16 2))
                 (3 (15 16)) (10 (15 2)) (9 (14 16)) (15 (14 4))
                 (9 (14 2)) (10 (13 16)) (14 (13 6)) (16 (13 4))
                 (15 (13 2)) (13 (12 16)) (4 (12 8)) (15 (12 6))
                 (11 (12 4)) (9 (12 2)) (5 (11 16)) (6 (11 10))
                 (9 (11 8)) (2 (11 6)) (4 (11 4)) (4 (11 2))
                 (15 (10 16)) (9 (10 12)) (6 (10 10)) (13 (10 8))
                 (12 (10 6)) (1 (10 4)) (11 (10 2)) (11 (9 16))
                 (5 (9 14)) (2 (9 12)) (12 (9 10)) (6 (9 8))
                 (12 (9 6)) (11 (9 4)) (13 (9 2)) (15 (8 16))
                 (1 (8 14)) (7 (8 12)) (11 (8 10)) (2 (8 8)) (10 (8 6))
                 (10 (8 4)) (13 (8 2)) (4 (7 16)) (5 (7 14))
                 (12 (7 12)) (2 (7 10)) (4 (7 8)) (11 (7 6))
                 (7 (7 4)) (2 (7 2)) (11 (6 16)) (16 (6 14)) 
                 (5 (6 12)) (12 (6 10)) (3 (6 8)) (1 (6 6)) (7 (6 4)) 
                 (12 (6 2)) (6 (5 16)) (13 (5 14)) (16 (5 12)) (15 (5 10)) 
                 (2 (5 8)) (5 (5 6)) (5 (5 4)) (10 (5 2)) (12 (4 16)) 
                 (12 (4 14)) (11 (4 12)) (14 (4 10)) (5 (4 8))
                 (14 (4 6)) (13 (4 4)) (5 (4 2)) (3 (3 16)) (10 (3 14))
                 (3 (3 12)) (7 (3 10)) (2 (3 8)) (2 (3 6)) (4 (3 4))
                 (16 (3 2)) (7 (2 16)) (9 (2 14)) (11 (2 12)) (7 (2 10))
                 (9 (2 8)) (12 (2 6)) (2 (2 4)) (10 (2 2)) (9 (1 16))
                 (1 (1 14)) (1 (1 12)) (9 (1 10)) (11 (1 8)) (5 (1 6))
                 (4 (1 4)) (15 (1 2)))
                (rsm.mpoly:^ *foo* 2000)
                :test #'equalp))
      ;; Unset the modulus for the next test.
      (rsm.mpoly:set-modulus nil))

    )
  t
  )


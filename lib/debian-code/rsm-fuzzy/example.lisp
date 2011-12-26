;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          example.lisp
;;;; Purpose:       Fuzzy Examples.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: example.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

;;; Must load the package rsm.fuzzy.

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(deffuzzy my-fuzzy
    :adjs
  ((small ((1 0) (3 0.25) (4 0.75) (6 1) (8 0)))
   (large ((1 0) (2 0.30) (4 0.80) (5 1) (7 0)))
   (blue ((1 0) (1.5 0.4) (3 0.5) (5 1) (7 0)))
   (green ((1 0) (2 0.1) (4 1) (6.5 0)))
   (dry  ((1 0) (2.5 0.50) (3.5 0.75) (4.5 1) (6.5 0)))
   (wet ((1 0) (3 0.50) (4 0.75) (5 1) (6.5 0))))
  :adj-groups 
  ((size (small large))
   (color (blue green))
   (humidity (dry wet)))
  :vars
  ((x 1.0 :adj-group size)
   (y 2.0 :adj-group color)
   (z 3.0 
      :adj-group humidity
      :rules ((r1 (and (x is somewhat small) 
                       (y is very blue)    ) dry) 
              (r2 (and (x is not large) 
                       (y is very green)   ) wet)))))


(deffuzzy my-fuzzy2
    :adjs
  ((small  (tri 1 3 5))
   (medium (tri 3 5 6))
   (large  (tri 5 7 8))
   (blue   (tri 1 3 5))
   (green  (trap 2 4 5 7))
   (dry    (trap 1 3.5 5.5 7))
   (wet    (trap 2.5 5.5 7 8)))
  :adj-groups 
  ((size (small medium large))
   (color (blue green))
   (humidity (dry wet)))
  :vars
  ((x 3.0 :adj-group size)
   (y 4.0 :adj-group color)
   (z 3.0 
      :adj-group humidity
      :rules ((r1 (and 
                   (or
                    (x is somewhat small) 
                    (x is medium))
                   (y is very blue)        ) dry)
              (r2 (and (x is not large) 
                       (y is very green)   ) wet)))))

;;;; EXAMPLE:
#|
:cl example
;; (fuz:set-fuzzy-system my-fuzzy)
(rsm.fuzzy:print-adjs)
(rsm.fuzzy:print-vars)
(rsm.fuzzy:fire z)
(rsm.fuzzy:set-var-val z 4.0)
|#


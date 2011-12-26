;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package Definition For Genetic programming.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

;;; Package for genetic programming.
(defpackage rsm.gen-prog
  (:use #:cl)
  (:documentation
   "A package for doing genetic programming.

Export Summary:  

make-gen-params: Make a structure that has genetic programming parameters.

make-func-from-pairs: Makes a function from a vector of pairs.

make-initial-population: Make the initial population.

form-next-generation: Form the next generation.

get-program-cache: Get the program cache of best k programs.

get-best-programs: Get the very best programs.

examine-population: Examine the current generation.

")  
  (:export 
   #:make-gen-params                    ; Make a structure that has 
                                        ;  genetic programming parameters.
   #:make-func-from-pairs               ; Makes a function from a vector
                                        ; of pairs.
   #:make-initial-population            ; Make the initial population.
   #:form-next-generation               ; Form the next generation.
   #:get-program-cache                  ; Get the program cache of best k 
                                        ;  programs.
   #:get-best-programs                  ; Get the very best programs.
   #:examine-population                 ; Examine the current generation.
   ))


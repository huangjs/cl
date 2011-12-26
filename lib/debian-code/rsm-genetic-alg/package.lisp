;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for Genetic Algorithms.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.genetic-alg
  (:use #:cl)
  (:documentation
   "A gene pool is a list of genes. 
 Each gene is made up of characters from a vector of letters. 
 Succeeding generations are formed by choosing pairs of genes randomly;
 splicing them randomly; and then possibly mutating them. 
 Although the choice of pairs is random, it is not uniformly random; 
 pairs which are more \"fit\" are selected more frequently.
 Fitness is determined by a fitness function that is supplied by the user.
 The value of any fitness function should be >= the value *base-fitness-value*
 defined below.
 The two primary macros/functions used are <defgenetic> and
 <solve-all-genetic-problems>. The first (defgenetic) defines genetic problems 
 for genetic programming while the second solves them.

REQUIRES: package rsm.cache


Export Summary:

genetic: The name of the structure that stores
         the genetic information necessary to 
         run a simulation.
   
    ACCESS FUNCTIONS TO STRUCTURE GENETIC (named below as G).
   
g-name            : G name (string).
g-mutation-rate   : G mutation rate (fixnum).
g-fitness-function: G fitness function(compiled function).
g-alphabet        : G alphabet (vector).
g-pool            : G initial gene pool (list of genes).
   
         BEGIN PRIMARY EXPORTS

defgenetic: Macro to define a genetic problem.
   
solve-all-genetic-problems: Solve all registered problems.
solve-genetic-problems    : Solve genetic problems in a name list.
solve-genetic-problem     : Solve a named genetic problem.
   
clear-genetic-problems    : Clear the registry of genetic problems.
   
display-solutions         : Display the solutions.
display-solution          : Display a solution.

         END PRIMARY EXPORTS
   
ga-sim: Simulate the evolution of a gene pool.

")
  (:export 
   
                                        ; DATA STRUCTURE GENETIC.
   
   #:genetic                            ; The name of the structure that stores
                                        ; the genetic information necessary to 
                                        ; run a simulation.
   
                                        ; ACCESS FUNCTIONS TO STRUCTURE GENETIC.
   
   #:g-name                             ; G name (string).
   #:g-mutation-rate                    ; G mutation rate (fixnum).
   #:g-fitness-function                 ; G fitness function(compiled function).
   #:g-alphabet                         ; G alphabet (vector).
   #:g-pool                             ; G initial gene pool (list of genes).
   
                                        ; BEGIN PRIMARY EXPORTS

   #:defgenetic                         ; Define a genetic problem.
      
   #:solve-all-genetic-problems         ; Solve all registered problems.
   #:solve-genetic-problems             ; Solve genetic problems in a name list.
   #:solve-genetic-problem              ; Solve a named genetic problem.
   
   #:clear-genetic-problems             ; Clear the registry of genetic 
                                        ; problems.
   
   #:display-solutions                  ; Display the solutions.
   #:display-solution                   ; Display a solution.

                                        ; END PRIMARY EXPORTS
   
   #:ga-sim))                           ; Simulate the evolution of a gene pool.

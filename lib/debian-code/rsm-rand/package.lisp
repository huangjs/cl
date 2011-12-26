;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition For Discrete Random Number Generators.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.3 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.rand
  (:use #:cl)
  (:documentation
   "This package provides a discrete random number generator.

Export Summary:

make-standard-rangen: Make a new random number generator based on a list of 
                      value-density pairs.
               Example: (rsm.rand:make-standard-randgen 
                          '((1 0.2) (2 0.25) (3 0.25) (4 0.3)))
               makes a random generator object that has values 1,2,3,4 with
               corresponding probability density values 0.2, 0.25, 0.25, 0.3.
clone        : Clone a random generator object.
next-rand    : Get a random number (or numbers) from the a random number object.
rand-val-dens: Set the value-density pairs for the random number object.
rand-dist    : Get the distribution function (array) from a random 
               generator object.
bin-rand     : Form an ASCII display that describes the distribution of 
               the random number object.")
  (:export 
   #:abstract-randgen
   #:standard-randgen
   #:clone
   #:make-standard-randgen
   #:next-rand
   #:rand-val-dens
   #:rand-dist
   #:bin-rand))

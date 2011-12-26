;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package Definition For Fuzzy Logic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)


(defpackage rsm.fuzzy
  (:use #:cl)
  (:documentation
   "DESCRIPTION: A package to define and compute with fuzzy systems.   
   
Fuzzy logic allows one to compute numerically with 
objects that are specified and related in a somewhat 
sloppy linguistic way. The implementation here uses four notions: 
A fuzzy adjective; a fuzzy adjective group; a fuzzy variable; 
and a fuzzy rule.

Fuzzy adjective: A named function (defined here as a piecewise 
                 linear function).

Fuzzy adjective group: A named group of adjectives.

Fuzzy variable : A named object with a numeric value and, optionally, 
                 a list of rules that determine it's value from other 
                 fuzzy variables.

Fuzzy rule     : A named object that relates one or more fuzzy variables 
                 (in a linguistic way) with a fuzzy variable associated 
                 with this rule.

In addition, there is a fuzzy class (and associated deffuzzy macro) that 
defines a named collection of fuzzy adjectives and fuzzy variables.
NOTE: If a fuzzy variable is specified in deffuzzy with fuzzy rules, then
      any fuzzy variables referred in the rules must have been defined
      earlier in the macro.

The form deffuzzy is:

(deffuzzy <name>
    :adjs
  ((<adj1> ((<begin> <val>) ... (<end> <val>)))
    ...)
  :adj-groups 
  ((<adj-group1> (<name1> ...))
    ...)
  :vars
  ((<var-without-rules> <val> :adj-group <adj-group>)
   (<var-with-rules> <val>
      :adj-group <adj-group>
      :rules ((<rule1> <rule-logic> <adj>)
                ...)))

Adjectives may also be described using the key words tri and trap.
This allow the user to describe common fuzzy adjectives whose graphs
have the shape of triangles or trapezoids.

For example the form, (tri 1 3 5), is used to describe a fuzzy adjective
that is zero when \"x\" is below 1 or greater than 5. The function in 
between 1 and 5 is triangular shaped starting at 0 when \"x\" is 1; 
rising to a maximum of 1 when \"x\" is 3; and falling back to 0 when \"x\" is 5.

The trap key word is similar.
For example: the form, (trap 2 4 5 7), is used to describe a fuzzy adjective
that is zero when \"x\" is below 2 or greater than 7. The graph of the 
function in between 2 and 7 is a trapezoid starting at 0 when \"x\" is 2; 
rising to a maximum of 1 when \"x\" is 4; remaining at 1 until 5; and 
falling back to 0 when \"x\" is 7.


 HOW TO COMPUTE A FUZZY VARIABLE'S NUMERIC VALUE FROM ITS RULES:
 Here we sketch the idea. To start, suppose that
 a fuzzy variable z depends on fuzzy variables x and y by way of 
 the following rules:
     IF (x IS small) AND (y IS VERY blue) THEN z IS dry
     IF (x IS large) OR (y IS SOMEWHAT green) THEN z IS wet 
     IF (x IS medium) AND (y IS NOT yellow) THEN z IS moist 

   Then to calculate the value of the fuzzy var z which depends on 
   fuzzy variables x and y through the rules above, do:
     Fire all the rules governing z and then get the numeric value of z by 
     defuzzification (fuzzy centroid method).
     This means perform these two steps:
      1). Fire one rule (say the first one above - the z is dry rule). 
           This is done as follows:
           Get the numeric measures of \"how small x is\" and 
           \"how very blue y is\"; name them x_small, y_blue respectively.
           Next, we get a numeric result of the ANDing of the 
           \"x phrase\" with the \"y phrase\" by taking the minimum of 
           x_small and y_blue. Now compute the area and the moment of the 
           fuzzy adjective function, dry, cut by the 
           horizontal line = Min(x_small, y_blue). 
           (NOTE: if we had ORed the phrases the corresponding numeric 
           function would  be Max.) 
      2). Do the same for the next rule and then set the numeric value 
          of z to the fuzzy centroid - which is found by the formula 
          (sum of all moments) / (sum of all areas).


 FUZZY CONTROLLER IDEA
 To use this package for a fuzzy controller, do the following:
 1). Write down the dynamics for a system with a control. 
     (Something like: x-dot = f(t,x,y,controller), 
     y-dot = g(t,x,y,controller) )
 2). Write the fuzzy rules governing the controller based on the 
      state variables. 
        (IF (x IS small) AND (y IS small) THEN controller IS medium)
 3). At each \"time step\" fire the rules for the controller 
     (fire-rules-return-value controller) - getting a value 
     for the new controller. Then, set the new value of the controller.



Export Summary:

deffuzzy                       : Create a fuzzy system.
make-fuzzy-system              : Make a fuzzy system (function).
clear-fuzzy-systems            : Clear out the fuzzy systems.
  
make-adj make-var make-rule    : Constructors
   
find-adj find-var find-rule    : Inspectors   
print-adjs print-vars          : Inspectors
find-fuzzy-system              : Inspector
   
get-var-val                    : Getter
set-var-val                    : Setter
set-fuzzy-system               : Setter
   
fire-rules-get-value           : Determine fuzzy var's value 
                               :  using its fuzzy rules.
fire))                         : Convenience macro for 
                               :  fire-rules-get-value.
")

  (:export  
   
   #:deffuzzy                           ; Create a fuzzy system.
   #:make-fuzzy-system                  ; Make a fuzzy system (function).
   #:clear-fuzzy-systems                ; Clear out the fuzzy systems.
   
   #:make-adj #:make-var #:make-rule    ; Constructors
   
   #:find-adj #:find-var #:find-rule    ; Inspectors   
   #:print-adjs #:print-vars            ; Inspectors
   #:find-fuzzy-system                  ; Inspector
   
   #:get-var-val                        ; Getter
   #:set-var-val                        ; Setter
   #:set-fuzzy-system                   ; Setter
   
   #:fire-rules-get-value               ; Determine fuzzy var's value 
                                        ;  using its fuzzy rules.
   #:fire))                             ; Convenience macro for 
                                        ;  fire-rules-get-value.


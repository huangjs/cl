;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for String Utilities.
;;;; Programmer:    R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.3 2003/09/10 22:19:26 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.string
  (:use #:cl)
  (:documentation
   "This package provides string manipulation functions.

Export Summary:

contains: Returns all strings in a list that contain any strings in a list of 
          string fragments. 
does-not-contain: Returns all strings in a list that DO NOT contain 
                  any strings in a list of string fragments.
file->string:   Reads a file returning a string of its contents.
file->number-list: Returns a list of numbers read from a text file.
file->number-table: Returns a list of a list of numbers after reading a file.
file->string-list: Returns a list of strings read from a text file.
file->string-table: Returns a list of a list of strings after reading a file.
fluff-string: Return a new string that wraps a character
              around every character in the supplied string.
join: Concatenate the strings in a list returning a new string.
list->file: Write a list out to a file.
number-list->file: Write a list of numbers (one per line) to a file.
split: Split a string into a list of strings.
string->file: Writes a string to a file.
string->number: Convert a string representing a number to a number.")
  (:export 
   #:contains
   #:does-not-contain
   #:file->number-table
   #:file->number-list
   #:file->string
   #:file->string-list
   #:file->string-table
   #:fluff-string
   #:join
   #:list->file
   #:number-list->file
   #:split
   #:string->file
   #:string->number))

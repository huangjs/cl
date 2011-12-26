;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.asd
;;;; Purpose:       ASDF definition file for CSV
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jan 2003
;;;;
;;;; *************************************************************************

(defpackage #:csv-system (:use #:asdf #:cl))
(in-package #:csv-system)

(defsystem :csv
  :name "cl-csv"
  :author "Frangois-Reni Rideau Dang-Vu Bbn"
  :version "1.8"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Bugroff"
  :description "A small text utility to import CSV files"
  :components
  ((:file "csv-src")))



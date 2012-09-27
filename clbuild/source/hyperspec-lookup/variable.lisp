;;;; $Id: variable.lisp,v 1.1.1.1 2003-11-13 19:12:22 eenge Exp $
;;;; $Source: /project/hyperspec-lookup/cvsroot/hyperspec-lookup/variable.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :hs)

(defparameter *hyperspec-table* (make-hash-table :test #'equalp)
  "Table of symbol -> partial-URL mappings for the Common Lisp
Hyperspec.")
(defparameter *hyperspec-map-file*
  #p"/usr/share/doc/hyperspec/Data/Map_Sym.txt"
  "Pathname for file that maps symbol to partial-URLs for the Common
Lisp Hyperspec.")

(defparameter *hyperspec-root*
  "http://www.lispworks.com/reference/HyperSpec/"
  "The root URL for the Hyperspec onto which the partial-URLs in the
*hyperspec-table* will be concatenated.")

(defparameter *mop-table* (make-hash-table :test #'equalp)
  "Table of symbol -> partial-URL mappings for the Common Lisp Object
System MetoObject Protocol.")
  
(defparameter *mop-map-file*
  #p"/home/eenge/dev/net-nittin-hyperspec/Mop_Sym.txt"
  "Pathname for file that maps symbol to partial-URLs for the Common
Lisp Object System MetoObject Protocol.")

(defparameter *mop-root* "http://www.alu.org/mop/"
  "The root URL for the Common Lisp Object System MetaObject Protocol
onto which the partial-URLs in the *amop-table* will be
concatenated.")

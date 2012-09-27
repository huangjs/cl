;;;; $Id: package.lisp,v 1.1.1.1 2003-11-13 19:12:22 eenge Exp $
;;;; $Source: /project/hyperspec-lookup/cvsroot/hyperspec-lookup/package.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :hyperspec
      (:use :cl)
    (:nicknames :hs)
    (:export :lookup
             :hyperspec-lookup
             :mop-lookup
             :read-sym-file
             :populate-table
             :*hyperspec-root*
             :*hyperspec-map-file*
             :*mop-root*
             :*mop-map-file*)))

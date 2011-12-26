(defpackage #:ubf.a
  (:use #:cl)
  (:export #:tagged-object #:binary-data #:read-message #:read-token #:reset-ubf-state #:write-message))

(defpackage #:ubf.a-constant)		; constants ('') are interned here
(defpackage #:ubf.a-type)    		; type tags (``) are interned here

;;  arch-tag: "02317cdd-e9e3-11d7-ad53-000c76244c24"

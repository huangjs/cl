;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC -*-

#|

DESC: apispec/api-python.lisp - the code for generating python APIs
Copyright (c) 2000 - Karl Trygve Kalleberg and Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-apispec)

(defun pythonify-word (word &optional class-name)
  (let* ((wlen (length word))
	 (new-word (make-string wlen :initial-element #\Space))
	 (loc 0)
	 (caps-next nil))
    
    (declare (type fixnum loc wlen))

    (setq wlen (- wlen 1))
    
    (loop for i of-type fixnum from 0 to wlen
	  for c of-type character = (schar word i)
	  do
	  (cond ((eq c #\-)
		 (setq caps-next t))
		(t
		 (if (or caps-next (and class-name (eq i 0)))
		     (setf (schar new-word loc) (char-upcase c))
		   (setf (schar new-word loc) c))
		 (setq caps-next nil)
		 (incf loc))))
    (string-right-trim '(#\Space) new-word)))

;;(trace pythonify-word)
		 	   

(defun py-get-constant-name (const)
  (format nil "GENERATED_CONSTANT_~:@(~a~)" 
	  (pythonify-word (if (typep const 'apispec-constant)
			    (car (apispec-constant.name const))
			  const))))

(defun py-get-class-name (api-name obj)
  (declare (ignore api-name))
;;  (string-downcase (car (apispec-class.name obj))))
  (pythonify-word (car (apispec-class.name obj)) t))

;;(trace py-get-class-name)

(defmethod output-code ((obj apispec-toplevel) (lang api-lang-python) stream (domain (eql :domain)))
  (let ((api-name (car (apispec-toplevel.name obj))))
    (format stream "# This file is autogenerated by the SDS API Generator.~2%")

;    (format stream "package sds.api.~a;~2%" (string-downcase api-name))

    (format stream "import sds.base~2%")
	    
    (format stream "class ~a:~2%" (pythonify-word api-name t))

    (format stream " def printXMLHeader(self, os, tool): ~%")
    (format stream "  self.printXMLHeader(os, tool, \"~a\")~2%" api-name)

    (let ((constants (apispec-toplevel.constants obj)))
      (dolist (c constants)
	(let ((value (car (apispec-constant.val c)))
	      (name (car (apispec-constant.name c))))
	  (format stream " ~a = \"~a\"~%"
		  (py-get-constant-name c)
		  (if value value name)))))
    (format stream "~3%")

    ))

(defmethod output-code ((obj apispec-toplevel) (lang api-lang-python) stream (exports (eql :exports)))
  (let ((api-name (car (apispec-toplevel.name obj)))
	(classes (apispec-toplevel.classes obj)))
    
    (format stream "# This file is autogenerated by the SDS API Generator.~2%")

    (dolist (c classes) 
      (let ((cl-name (py-get-class-name api-name c)))
	(format stream "from ~a import *~2%" cl-name)))

    (format stream "from factory import Factory~2%")

    ;; this one is an evil hack to allow people to stuff in
    ;; a common.py
    (format stream "try:~%  from common import *~%except ImportError:~%  pass~2%")

    
    ))

(defmethod output-code ((obj apispec-toplevel) (lang api-lang-python) stream (factory (eql :factory)))
  
  (let ((api-name (car (apispec-toplevel.name obj))))

    (format stream "# This file is autogenerated by the SDS API Generator.~2%")

;    (format stream "package sds.api.~a;~2%" (string-downcase api-name))

    (format stream "import sds.base~%")
    (format stream "import string~%")
    (format stream "import types~%")
    (format stream "from ~a import ~a~%" (string-downcase api-name) (pythonify-word api-name t))
    
    (dolist (c (apispec-toplevel.classes obj))
      (let ((sm-name (py-get-class-name api-name c)))
	(format stream "import ~a~%" sm-name)))

    (terpri stream)
    
    (format stream "class Factory (sds.base.Factory): ~2%") ;; api-name
    (format stream " def create(self, theClass):~%")
    (format stream "~%")
    (format stream "  self.ptr = None;~%")
    (format stream "  classname = string.lower(theClass);~2%")
      
    (format stream "  if 0:~%")
    (format stream "    pass~%")
    (dolist (c (apispec-toplevel.classes obj))
      (let ((elmname (car (apispec-class.elmname c)))
	    (name (car (apispec-class.name c)))
	    (sm-name (py-get-class-name api-name c)))
	(format stream "  elif string.find(classname, string.lower(~a.~a)) == 0:~%"
		api-name
		(py-get-constant-name (if elmname elmname name)))
	(format stream "   self.ptr = ~a.~a()~%"
		sm-name sm-name)))
      
    (format stream "  else:~%")
    (format stream "   DEBUG_PUT(\"Unknown class '\" + theClass + \"'\")~%")
    (format stream "  return self.ptr~%")
    (format stream "~2%")

    ))

(defmethod output-code ((obj apispec-class) (lang api-lang-python) stream api-name)

  (let ((cl-name (py-get-class-name api-name obj))
	(elmname (car (apispec-class.elmname obj)))
	(name (car (apispec-class.name obj)))
	(base-class "sds.base.Parseable"))

    (format stream "# This file is autogenerated by the SDS API Generator.~2%")

;    (format stream "package sds.api.~a;~2%" (string-downcase api-name))

    (format stream "import sds.base~%")
    (format stream "from sds.base import AttrType~%")
    (format stream "from sds.base import SubElemType~%")
    (format stream "from ~a import ~a~%" (string-downcase api-name) (pythonify-word api-name t))

    (terpri stream)
    
    (format stream "class ~a (~a): ~%" (pythonify-word cl-name t) base-class)
;    (format stream "~%")

    ;;; very important that all attributes will be written before all subelements!!!
    ;;; this should either be enforced better, or be fixed in sds.base.XML_Parseable.get/setContent()

#||    
    (loop for v in (apispec-class.vars obj)
	for iter upfrom 0 by 1 do
	  (format stream " _~a = ~a;~%" 
		  (pythonify-word (car (apispec-var.name v)))
		  iter))
||#

    (format stream "~%")
    
    (format stream " def __init__(self): ~%")
    (format stream "  ~a.__init__(self)~%" base-class)
    
#||
    (let ((attrlen (length (apispec-class.attrs obj))))
      (if (> attrlen 0)
	  (let ((content 
		 (loop for v in (apispec-class.attrs obj) 
		   collect (strcat "\"" (car (apispec-attr.name v)) "\"" ":None"))))
	    (format stream "  self.attrInfo = {~{ ~a,~}} # ~a~%" 
		    content attrlen)
	  )))

    (let ((subelemlen (length (apispec-class.subelems obj))))
      (if (> subelemlen 0)
	  (let ((content 
		 (loop for v in (apispec-class.subelems obj) 
		   collect (strcat "\"" (car (apispec-subelem.name v)) "\":None"))))
	    (format stream "  self.subelemInfo = {~{ ~a,~}} # ~a~%" 
		    content subelemlen)
	  )))
||#
    (format stream "  # self.attrInfo = {}~%")
    (format stream "  # self.subelemInfo = {}~%")

    (let ((childrenlen (length (apispec-class.vars obj))))
      (if (> childrenlen 0)
	  (let ((content (loop for v in (apispec-class.vars obj) 
			   collect (strcat "\"" (car (apispec-var.name v)) "\":" 
					   (if (or
						(string-equal (car (apispec-var.type v))
						    "ptrlist")
						(string-equal (car (apispec-var.type v))
						    "stringlist"))
					       "[]"
					     "None")))))
	    (format stream "  self.children = {~{ ~a,~}} # ~a~%" content childrenlen)
	 ;; (format stream "  self.children = [] # ~a~%" childrenlen)
	  )))

    (format stream "~%")
    
    (let ((attrs (apispec-class.attrs obj)))
      (when attrs
	(loop for a in attrs for iter upfrom 0 by 1 do
	      (format stream "  self.attrInfo[\"~a\"] = sds.base.AttrInfo(\"~a\", AttrType.ATTR_~:@(~a~), \"~a\")~%"
		      (car (apispec-attr.name a))
		      (car (apispec-attr.name a))
		      (car (apispec-attr.type a))
		      (pythonify-word (car (apispec-attr.var  a)))
		      ))))

    (let ((attrs (apispec-class.subelems obj)))
      (when attrs
	(loop for a in attrs for iter upfrom 0 by 1 do
	      (format stream "  self.subelemInfo[\"~a\"] = sds.base.SubElemInfo(~a.~a, SubElemType.SUBELEM_~:@(~a~), \"~a\")~%"
		      (car (apispec-subelem.name a))
		      api-name
		      (py-get-constant-name (car (apispec-subelem.name a)))
		      (car (apispec-subelem.type a))
		      (pythonify-word (car (apispec-subelem.var  a)))
		      ))))

    (format stream "~2%")


    (format stream " def copy(self): ~%")
    (format stream "  return ~a(self);~%" cl-name)
    (format stream "~2%")

    (format stream " def getElementName(self): ~%")
    (format stream "  return ~a.~a;~%"
	    api-name
	    (py-get-constant-name (if elmname elmname name)))
    (format stream "~2%")

;;    (format stream " def __init__(self, c): ~%" cl-name cl-name)
;;    (format stream "  parseable.__init__(c)~%")
;;    (format stream "~2%")
    ))

  
(defmethod output-code ((obj apispec-toplevel) (lang api-lang-python) stream dir-name)

  (declare (ignore stream))
  ;; (format t "Generating to ~a~%" some-name)
  
  (let ((api-name (car (apispec-toplevel.name obj)))
	(classes (apispec-toplevel.classes obj)))
    
    (make-sure-dirs-exist (ensure-dir-name dir-name))

    ;; first elements
    (dolist (c classes)
      (let ((cl-name (py-get-class-name api-name c)))
        (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/" cl-name ".py")))
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	  (output-code c lang str api-name))))
    
    ;; then factory
    (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/" "factory.py")))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (output-code obj lang str :factory))

    ;; then domain
    (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/" (string-downcase api-name) ".py")))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (output-code obj lang str :domain))

    ;; then module exports
    (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/__init__.py")))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (output-code obj lang str :exports))
    
  ))

#||
(defmethod output-code ((obj apispec-class) (lang api-lang-python) stream api-name)

  (let ((cl-name (py-get-class-name api-name obj)))

    ))
||#

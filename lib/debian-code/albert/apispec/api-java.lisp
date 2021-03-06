;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC -*-

#|

DESC: apispec/api-java.lisp - the code for generating java APIs
Copyright (c) 1999-2000 - Karl Trygve Kalleberg and Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-apispec)

(defun javaify-word (word &optional class-name)
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

;;(trace javaify-word)
		 	   

(defun jv-get-constant-name (const)
  (format nil "GENERATED_CONSTANT_~:@(~a~)" 
	  (javaify-word (if (typep const 'apispec-constant)
			    (car (apispec-constant.name const))
			  const))))

(defun jv-get-class-name (api-name obj)
  (strcat api-name "_"  (javaify-word (car (apispec-class.name obj)) t)))
	  

(defun get-actual-type (key)
  (cond
   ((string-equal key "string") "final int")
   ((string-equal key "stringlist") "final int")
   ((string-equal key "ptr") "final int")
   ((string-equal key "ptrlist") "final int")
   (t (warn "Unable to understand the type ~a" key))))

(defmethod output-code ((obj apispec-toplevel) (lang api-lang-java) stream (domain (eql :domain)))
  (let ((api-name (car (apispec-toplevel.name obj))))
    (format stream "/// This file is autogenerated by the SDS API Generator.~2%")

    (format stream "package sds.api.~a;~2%" (string-downcase api-name))

    (format stream "import sds.base.*;~2%")
	    
    (format stream "public class ~a {~2%" api-name)

    (let ((constants (apispec-toplevel.constants obj)))
      (dolist (c constants)
	(let ((value (car (apispec-constant.val c)))
	      (name (car (apispec-constant.name c))))
	  (format stream "   public final static String ~a = new String(\"~a\");~%"
		  (jv-get-constant-name c)
		  (if value value name)))))
    (format stream "}~3%")

    ))

(defmethod output-code ((obj apispec-toplevel) (lang api-lang-java) stream (factory (eql :factory)))
  
  (let ((api-name (car (apispec-toplevel.name obj))))

    (format stream "/// This file is autogenerated by the SDS API Generator.~2%")

    (format stream "package sds.api.~a;~2%" (string-downcase api-name))

    (format stream "import sds.base.*;~2%")

    (format stream "public class ~a_Factory extends XML_Factory {~2%" api-name)
    (format stream "   public XML_Parseable create(String theClass) {~%")
    (format stream "~%")
    (format stream "      XML_Parseable ptr = null;~2%")
      
    (format stream "      if(false) ;~%")
    (dolist (c (apispec-toplevel.classes obj))
      (let ((elmname (car (apispec-class.elmname c)))
	    (name (car (apispec-class.name c))))
       (format stream "      else if (theClass.equalsIgnoreCase(~a.~a)) ptr = new ~a();~%"
	       api-name
	       (jv-get-constant-name (if elmname elmname name))
	       (jv-get-class-name api-name c))))
      
      (format stream "      else {~%")
      (format stream "           DEBUG.PUT(\"Unknown class '\" + theClass + \"'\");~%")
      (format stream "      }~2%")
      (format stream "      return ptr;~%")
      (format stream "   }~2%")

    (format stream "}~2%")

    ))

(defmethod output-code ((obj apispec-class) (lang api-lang-java) stream api-name)

  (let ((cl-name (jv-get-class-name api-name obj))
	(elmname (car (apispec-class.elmname obj)))
	(name (car (apispec-class.name obj))))

    (format stream "/// This file is autogenerated by the SDS API Generator.~2%")

    (format stream "package sds.api.~a;~2%" (string-downcase api-name))

    (format stream "import sds.base.*;~2%")

    (format stream "public class ~a extends XML_Parseable {~%" cl-name)
    (format stream "~%")

    ;;; very important that all attributes will be written before all subelements!!!
    ;;; this should either be enforced better, or be fixed in sds.base.XML_Parseable.get/setContent()
    
    (loop for v in (apispec-class.vars obj)
	  for iter upfrom 0 by 1 do
      (format stream "   public final static int ~a = ~a;~%" 
	      (javaify-word (car (apispec-var.name v)))
	      iter))

    (format stream "~%")
    
    (format stream "   public ~a() {~2%" cl-name)

    (let ((attrlen (length (apispec-class.attrs obj))))
      (if (> attrlen 0)
	  (format stream "      attrInfo = new XML_AttrInfo[~a];~%" attrlen)))

    (let ((subelemlen (length (apispec-class.subelems obj))))
      (if (> subelemlen 0)
	  (format stream "      subelemInfo = new XML_SubElemInfo[~a];~%" subelemlen)))


    (let ((childrenlen (length (apispec-class.vars obj))))
      (if (> childrenlen 0)
	  (format stream "      children = new Object[~a];~%" childrenlen)))

    (format stream "~%")
    
    (let ((attrs (apispec-class.attrs obj)))
      (when attrs
	(loop for a in attrs for iter upfrom 0 by 1 do
	  (format stream "      attrInfo[~a] = new XML_AttrInfo(\"~a\", new XML_AttrType(XML_AttrType.ATTR_~:@(~a~)), ~a);~%"
		  iter
		  (car (apispec-attr.name a))
		  (car (apispec-attr.type a))
		  (javaify-word (car (apispec-attr.var  a)))
		  ))))

    
    (let ((attrs (apispec-class.subelems obj)))
      (when attrs
	(loop for a in attrs for iter upfrom 0 by 1 do
	  (format stream "      subelemInfo[~a] = new XML_SubElemInfo(~a.~a, new XML_SubElemType(XML_SubElemType.SUBELEM_~:@(~a~)), ~a);~%"
		  iter
		  api-name
		  (jv-get-constant-name (car (apispec-subelem.name a)))
		  (car (apispec-subelem.type a))
		  (javaify-word (car (apispec-subelem.var  a)))
		  ))))

    (format stream "   }~2%")


    (format stream "   public XML_Parseable copy() {~%")
    (format stream "      return new ~a(this);~%" cl-name)
    (format stream "   }~2%")

    (format stream "   public String getElementName() {~%") ;; cl-name
    (format stream "      return ~a.~a;~%"
	    api-name
	    (jv-get-constant-name (if elmname elmname name)))
    (format stream "   }~2%")

    (format stream "   protected ~a(~a c) {~2%" cl-name cl-name)
    (format stream "      super(c);~%")
    (format stream "   }~%")

    (format stream "}~2%")
    ))

  
(defmethod output-code ((obj apispec-toplevel) (lang api-lang-java) stream dir-name)

  (declare (ignore stream))
;;  (format t "Generating to ~a~%" some-name)
  
  (let ((api-name (car (apispec-toplevel.name obj)))
	(classes (apispec-toplevel.classes obj)))
    
    (make-sure-dirs-exist (ensure-dir-name dir-name))
    
    (dolist (c classes)
      (let ((cl-name (jv-get-class-name api-name c)))
        (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/" cl-name ".java")))
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	  (output-code c lang str api-name))))
    
    ;; then factory
    (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/" api-name "_Factory.java")))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (output-code obj lang str :factory))

    ;; then domain
    (with-open-file (str (merge-pathnames (pathname (strcat dir-name "/" api-name ".java")))
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (output-code obj lang str :domain))
    
  ))

#||
(defmethod output-code ((obj apispec-class) (lang API-Lang-Java) stream api-name)

  (let ((cl-name (jv-get-class-name api-name obj)))

    ))
||#

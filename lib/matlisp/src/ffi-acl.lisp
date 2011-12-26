;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :fortran-ffi-accessors; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved. 
;;; 
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;; 
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Adapted from ffi-cmu.lisp by Tunc Simsek, Univ. of California, Berkeley
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: ffi-acl.lisp,v 1.4 2001/07/26 15:44:54 rtoy Exp $
;;;
;;; $Log: ffi-acl.lisp,v $
;;; Revision 1.4  2001/07/26 15:44:54  rtoy
;;; Moved the Fortran name mangling stuff to its own file.  Some common
;;; things from ffi-acl and ffi-cmu also moved there.
;;;
;;; Revision 1.3  2001/02/21 19:44:07  simsek
;;; o Added the :long keyword (equivalent to :integer)
;;; o Fixed the way strings are passed to the routines
;;;   Now using STRING-TO-NATIVE.
;;;
;;; Revision 1.2  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.1  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Allegro CL Alien function interface to FORTRAN (BLAS/LAPACK)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "FORTRAN-FFI-ACCESSORS")

(defun parse-doc-&-parameters (body &optional header footer)
  (if (stringp (first body))
      (values `(,(%cat% header (first body) footer)) (rest body))
    (values (if (or header footer)
		(%cat% header "" footer)
	      nil)
	    body)))

;; If TYPE is some kind of array, return non-NIL to indicate that we
;; need to cast this as an array type for the alien function
;; interface.
(defun cast-as-array-p (type)
  (or (listp type)
      (eq type :complex-single-float)
      (eq type :complex-double-float)))

;; Convert the Fortran TYPE to the underlying alien type.
(defun get-read-in-type (type)
  (flet ((convert (type)
	   ;; Fortran wants, essentially, the complex number to look
	   ;; like a 2-element array consisting of the real and
	   ;; imaginary parts.
	   (ecase type
	     (:integer '(:int fixnum))
	     (:long '(:long fixnum))
	     ((:single-float :complex-single-float) '(:float single-float))
	     ((:double-float :complex-double-float) '(:double double-float)))))
    
    ;;; Hmm! too many hacks here.
    ;;; Can cast fixnum to int but
    ;;; when declared (simple-array fixnum (*)) doesn't
    ;;; like a (simple-array (unsigned-byte 32) (*)) coming in,
    ;;; eg. from gesv callind dgesv
    ;;; I don't think CMU CL had a problem with that !!!
    (if (cast-as-array-p type)
	`(:foreign-address 
	  (simple-array ,(let ((ttype (second 
				       (convert 
					(if (listp type)
					    (second type)
					  type)))))
			   (case ttype
			     (fixnum '(unsigned-byte 32))
			     (t ttype)))
			 (*)))
      (convert type))))

;; Convert the Fortran return value to the corresponding alien type.
(defun get-read-out-type (type)
  (ecase type
    (:void :void)
    (:integer :int)
    (:long :long)
    (:single-float :float)
    (:double-float :double)))


;; Convert the Fortran style parameter into the corresponding alien
;; style parameter.

;; Return non-NIL if STYLE is designates some type of output variable.
(defun get-read-out-style (style)
  (member style '(:in-out :out :output :input-output :input-or-output
		  :workspace-output :workspace-or-output)))

;; Parse the parameter list of the Fortran routine and return a new
;; list appropriate for use in defining the alien function.
(defun parse-fortran-parameters (body)

  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)
    (declare (ignore doc))

    (let* ((aux-pars nil)
	   (new-pars
	    (mapcar #'(lambda (par)
			(destructuring-bind (name type &optional (style :input))
			    par
			    (declare (ignore style))
			  (case type
			    (:string
			     ;; Hmm! I'm really using the
			     ;; fact that BLAS/LAPACK always
			     ;; use CHARACTER*1 for strings.
			     ;; That is, they use the first
			     ;; letter of a string to get
			     ;; its meaning.  (see note _char_trick_ below)

			     ;; On windows we're using the CLAPACK
			     ;; verisons of the BLAS/LAPACK libraries
			     ;; The strings there are declared as char*
			     ;; since we're using fortran calling
			     ;; conventions, the following char
			     ;; will be passed correctly as char*;
			     ;; I'm assuming that :foreign-address
			     ;; also works and don't know the reason
			     ;; why there are 2 cases here.  Will
			     ;; check when at first opportunity.
			     
			     #-:unix `(,name :char character)
			     #+:unix
			     `(,name :foreign-address)
			    )
			     (t
			     `(,name ,@(get-read-in-type type))))))
		    pars)))
      `(;; don't want documentation for direct interface, not useful
	;; ,@doc 
	,@new-pars ,@aux-pars))))

;; Create a form specifying a simple Lisp function that calls the
;; underlying Fortran routine of the same name.
(defun def-fortran-interface (name
			      return-type 
			      body)
  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)

    ;; Hmm, this passes over pars many, many times.  Should we
    ;; rearrange it so that we pass over pars just once and collect
    ;; the various pieces at the same time?
    (let* (
	   (return-value `(,(gensym "RETURN-VAL-")))
	   ;; Names of all the args
	   (args (remove 'hidden-complex-return-variable (mapcar #'first pars)))
	   ;; The actual name of the underlying Fortran routine
	   (ffi-fn (make-fortran-ffi-name name))
	   ;; Names of all the ffi args
	   (ffi-args (mapcar #'(lambda (p)
				 (if (eq (second p) :string)
				     ;; note _char_tric_ (see above)
				     #-:unix `(char ,(first p) 0)
				     #+:unix
				     `(excl::string-to-native ,(first p))
				   (first p)))
				 pars))
	   ;; Extra arguments for string handling (the length
	   ;; of the string), if needed.
	   (aux-ffi-args (mapcar #'(lambda (p)
				     `(length (the string ,(first p))))
				 (remove-if-not #'(lambda (p)
						    (eq (second p) :string))
						pars)))
	   ;; The return variable(s)
	   (rvs (mapcar #'first
			(remove-if-not #'(lambda (p)
					   (get-read-out-style (third p)))
				       pars)))
	   ;; The definition of the Lisp interface function we want.
	   (defun-body
	       `(
		 (let ((,@return-value 
			(,ffi-fn ,@ffi-args)))
			;;; looks like we don't need these extra args
		        ;;; which give the length of a string
		        ;;; (,ffi-fn ,@ffi-args ,@aux-ffi-args)))
		   
		     (declare (ignore ,@(and (eq return-type :void) return-value)))
		     (values ,@(and (not (eq return-type :void))  return-value)
			     ,@(mapcar #'(lambda (s)
					   (if (eq s 'hidden-complex-return-variable)
					       `(complex (aref ,s 0) (aref ,s 1))
					       ;; see *** below
					       ;; 'hidden-complex-return-variable
					       s)) rvs))))))
      (declare (ignore aux-ffi-args))

      (if (find 'hidden-complex-return-variable (mapcar #'first pars))
	  `(
	    ;; Too hard to debug if inlined.
	    ;;(declaim (inline ,name))
	    (defun ,name ,args
	      ,@doc

	      ;;; *** not sure if Allegro has specialized complex numbers
	      ;;; I'll play it safe for now.
	      ;;; (let ((hidden-complex-return-variable #c(0d0 0d0)))
	      ;;;   (declare (type (complex double-float) hidden-complex-return-variable))
	      ;;;   ,@defun-body)
	      (let ((hidden-complex-return-variable (make-array 2 :element-type 'double-float
								    :initial-contents '(0.0d0 0.0d0))))
		,@defun-body)

	      ))
	  `(
	    ;; Too hard to debug if inlined.
	    ;;(declaim (inline ,name))
	    (defun ,name ,args
	      ,@doc
	      ,@defun-body))))))


(defmacro def-fortran-routine (name return-type &rest body)
  "def-fortran-routine name return-type {(arg-name arg-type {style})}*

This macro performs two related actions.  First, it defines an alien
interface to the Fortran routine.  Then it also defines a Lisp
function with the same name that calls the Fortran function
appropriately.

The name of the Fortran routine is NAME, which is a symbol.  This is
also the name of Lisp function corresponding to the Fortran function.

The remaining forms specify the individual arguments that are passed
to the routine. ARG-NAME is a symbol that names the argument,
primarily for documentation.  ARG-TYPE is the Fortran type of the
argument (see below). STYLE specifies whether the argument is an input
or an output of the routine (see below).  The default for STYLE is
:INPUT.

RETURN-TYPE
  :VOID                    A Fortran subroutine (no values returned)
  :INTEGER                 Returns an INTEGER*4 value
  :SINGLE-FLOAT            Returns a REAL*4 value
  :DOUBLE-FLOAT            Returns a REAL*8 value
  :COMPLEX-SINGLE-FLOAT    Returns a COMPLEX*8 value
  :COMPLEX-DOUBLE-FLOAT    Returns a COMPLEX*16 value

ARG-TYPE
  :INTEGER                 INTEGER*4
  :SINGLE-FLOAT            REAL*4
  :DOUBLE-FLOAT            DOUBLE PRECISION (REAL*8)
  :COMPLEX-SINGLE-FLOAT    COMPLEX*8
  :COMPLEX-DOUBLE-FLOAT    COMPLEX*16 
  (* X)                    An array of type X, where X is one of the above
                           types.
  :STRING                  CHARACTER*(*)

STYLE
  When ARG-TYPE is a simple scalar (including complex) STYLE means:
    :INPUT            Value will be used but not modified.
		      Similar to the :COPY style of DEF-ALIEN-ROUTINE.
    :OUTPUT           Input value not used (but some value must be given),
		      a value is returned via the Lisp command VALUES from
		      the lisp function NAME. Similar to the :IN-OUT style of
		      DEF-ALIEN-ROUTINE.
    :INPUT-OUTPUT     Input value may be used, a value
		      is returned via the lisp command VALUES from the
		      lisp function NAME.
		      Similar to the :IN-OUT style of DEF-ALIEN-ROUTINE.

  When ARG-TYPE is an array or string STYLE means:
    :INPUT            Array entries are used but not modified.
    :OUTPUT           Array entries need not be initialized on input,
		      but will be *modified*.  In addition, the array
		      will be returned via the Lisp command VALUES
		      from the lisp function NAME.

    :INPUT-OUTPUT     Like :OUTPUT but initial values on entry may be used.

The keyword :WORKSPACE is a nickname for :INPUT.  The keywords
:INPUT-OR-OUTPUT,:WORKSPACE-OUTPUT, :WORKSPACE-OR-OUTPUT are nicknames
for :OUTPUT.

"
  (let ((fortran-name (make-fortran-name `,name))
	(lisp-name  (make-fortran-ffi-name `,name))
	(hack-return-type `,return-type)
	(hack-body `(,@body)))

    (multiple-value-bind (doc pars)
	(parse-doc-&-parameters `(,@body))

      (when (or (eq hack-return-type :complex-single-float)
		(eq hack-return-type :complex-double-float))
	;; The return type is complex.  Since this is a "structure",
	;; Fortran inserts a "hidden" first parameter before all
	;; others.  This is used to store the resulting complex
	;; number.  Then there is no "return" value, so set the return
	;; type to :void.
	;;
	;; Warning: There is inadvertent variable capture here.  The
	;; user better not call this routine with a variable namded
	;; HIDDEN-COMPLEX-RETURN-VARIABLE!  We should probably gensym
	;; this.
	(setq hack-body `(,@doc
			  (hidden-complex-return-variable ,hack-return-type :out)
			  ,@pars))
	(setq hack-return-type :void)))
			  
    `(eval-when (load eval compile)
       (progn
	 ;(declaim (inline ,lisp-name))
	 (def-foreign-call (,lisp-name ,fortran-name) 
			  (,@(parse-fortran-parameters hack-body))
			  :returning ,(get-read-out-type hack-return-type)
			  :convention :fortran
			  )
	 ,@(def-fortran-interface name hack-return-type hack-body)))))


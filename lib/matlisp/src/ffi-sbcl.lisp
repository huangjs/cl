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
;;; Originally written by Raymond Toy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: ffi-sbcl.lisp,v 1.2 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: ffi-sbcl.lisp,v $
;;; Revision 1.2  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.7  2002/07/26 21:38:02  rtoy
;;; Fix a bug in generating the Fortran inteface when a complex number is
;;; returned.  Use an array instead of a complex number for the result and
;;; create a complex from the array elements for the function value.
;;;
;;; Revision 1.6  2001/07/26 15:44:54  rtoy
;;; Moved the Fortran name mangling stuff to its own file.  Some common
;;; things from ffi-acl and ffi-cmu also moved there.
;;;
;;; Revision 1.5  2001/02/26 22:54:23  rtoy
;;; It appears to be ok to inline the def-alien-routine and
;;; vector-data-addresses.  The copy! bug isn't tickled.
;;;
;;; Revision 1.4  2001/02/21 19:40:52  simsek
;;; o Added the :long keyword (equivalent to :long)
;;;
;;; Revision 1.3  2000/10/04 01:11:19  simsek
;;; o Removed inlines (see comments in code)
;;;
;;; Revision 1.2  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.1  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.13  2000/06/19 22:21:45  rtoy
;;; Define packages elsewhere.
;;;
;;; Revision 1.12  2000/05/08 15:28:20  rtoy
;;; Removed the variable capture of hidden-complex-return-value by
;;; gensym'ing a new var.
;;;
;;; Revision 1.11  2000/05/05 18:56:51  rtoy
;;; o Try to add comments to routines and stuff.
;;; o Some minor simplification of the code
;;; o Clean up def-fortran-interface.  (Use only one form for the basic
;;;   function.)  Remove one source of macro variable capture.  Still have
;;;   one with hidden-complex-return-variable, though.
;;; o Cleaned up the comments and doc-string for def-fortran-routine.
;;; o incf-sap: don't need to special case n = 1 because CMUCL is smart
;;;   enough to fold the multiplication.
;;; o Add matlisp-specialized-array type.
;;; o Use vector-sap if possible.
;;; o Remove obsolete with-vector-data-addresses
;;;
;;; Revision 1.10  2000/05/02 14:32:13  rtoy
;;; Convert CR/LF to standard Unix LF.
;;;
;;; Revision 1.9  2000/05/02 13:48:34  rtoy
;;; Turn off invalid trap when calling out to Fortran routines.  Needed
;;; to fix a problem with SVD stopping with an invalid exception.
;;;
;;; Revision 1.8  2000/04/14 00:04:55  simsek
;;; o Added INCF-SAP so that the size of a
;;;   double, single, etc .. need not be known in any
;;;   other lisp files.
;;; o In future revisions, these machine dependent sizes should be determined
;;;   by configure (e.g. configure can check whether a Fortran DOUBLE PRECISION
;;;   is a C double and check the size of C double and so on.
;;;
;;; Revision 1.7  2000/28/01 17:44:46  simsek
;;; o Using SYSTEM::WITHOUT-GCING instead of GC-ON and GC-OFF
;;;   in WITH-VECTOR-DATA-ADDRESSES. 
;;;
;;; Revision 1.6  2000/20/01 09:18:24  simsek
;;; o Added DEFPACKAGE.
;;; o Reworked DEF-FORTRAN-INTERFACE.  The CMUCL FFI always returns a value 
;;;   for a function even if it is void (in this case NIL).  Also, hacked
;;;   the STYLE specifier to accept :OUTPUT, :INPUT, :INPUT-OUTPUT, 
;;;   :INPUT-OR-OUTPUT, :WORKSPACE, :WORKSPACE-OUTPUT, :WORKSPACE-OR-OUTPUT
;;;   to establish some form of semantics for interfacing Lisp (a functional
;;;   language) to the BLAS/LAPACK Fortran routines (a pass-by-reference structure).
;;; o Added CAT, SCAT, PARSE-DOC-&-PARAMETERS, CAST-AS-ARRAY-P, GET-READ-IN-TYPE,
;;;   GET-READ-OUT-TYPE, GET-READ-IN-STYLE and GET-READ-OUT-STYLE.
;;; o Renames MAKE-LISP-NAME to MAKE-FORTRAN-FFI-NAME and renamed
;;;   HANDLE-FORTRAN-PARAMETERS to PARSE-FORTRAN-PARAMETERS.
;;;
;;; Revision 1.5  1999/08/05 15:00:49  toy
;;; Add support for simple arrays of any dimension.
;;;
;;; Revision 1.4  1999/08/04 22:12:46  toy
;;; o Fixup a compiler warning in def-fortran-interface.
;;; o Fix the doc string in vector-data-address.
;;; o Change vector-data-address so we always check for the type.  If we
;;;   screw this up, we can really hose the whole lisp, so make sure the
;;;   argument has the right type!
;;;
;;; Revision 1.3  1999/08/04 13:36:19  toy
;;; o  Fix up some comment errors.
;;; o  VECTOR-DATA-ADDRESS returns an SAP instead of an integer now.  Make
;;;    WITH-VECTOR-DATA-ADDRESSES work with this.
;;;
;;; Revision 1.2  1999/08/02 21:47:22  toy
;;; o  Handle the style parameter.
;;; o  We want all return values, in case some are set by the Fortran
;;;    routine.
;;;
;;; Revision 1.1  1999/08/02 17:15:04  toy
;;; Initial revision
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alien function interface to FORTRAN (BLAS/LAPACK)
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
	     (:integer 'sb-c::int)
	     (:long 'sb-c::int)
	     ((:single-float :complex-single-float) 'sb-c::single-float)
	     ((:double-float :complex-double-float) 'sb-c::double-float))))
    
    (if (cast-as-array-p type)
	`(* ,(convert (if (listp type)
			  (second type)
			type)))
      (convert type))))

;; Convert the Fortran return value to the corresponding alien type.
(defun get-read-out-type (type)
  (ecase type
    (:void 'void)
    (:integer 'sb-c::int)
    (:long 'sb-c::int)
    (:single-float 'sb-c::single-float)
    (:double-float 'sb-c::double-float)))


;; Convert the Fortran style parameter into the corresponding alien
;; style parameter.
(defun get-read-in-style (style type)
  (if (or (cast-as-array-p type)
	  (eq type :string))
      :in
      (ecase style
	((nil :input :workspace) :copy)
	((:output :input-output :input-or-output
		  :workspace-output :workspace-or-output) :in-out)
	((:out :in :copy :in-out) style))))

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
			  (case type
			    (:string
			     (pushnew `(,(scat "LEN-" name) sb-c::int :copy) aux-pars)
			     `(,name sb-c::c-string ,(get-read-in-style style type)))
			    (t
			     `(,name ,(get-read-in-type type) ,(get-read-in-style style type))))))
		    pars)))
      `(;; don't want documentation for direct interface, not useful
	;; ,@doc 
	,@new-pars ,@aux-pars))))

;; Create a form specifying a simple Lisp function that calls the
;; underlying Fortran routine of the same name.
(defun def-fortran-interface (name return-type body hidden-var-name)
  (multiple-value-bind (doc pars)
      (parse-doc-&-parameters body)

    ;; Hmm, this passes over pars many, many times.  Should we
    ;; rearrange it so that we pass over pars just once and collect
    ;; the various pieces at the same time?
    (let* (
	   (return-value `(,(gensym "RETURN-VAL-")))
	   ;; Names of all the args
	   (args (remove hidden-var-name (mapcar #'first pars)))
	   ;; A list of pairs suitable for use with
	   ;; with-vector-data-addresses
	   (saps (mapcar #'(lambda (p)
			     `(,(scat "ADDR-" (first p)) ,(first p)))
			 (remove-if-not #'(lambda (p)
					    (cast-as-array-p (second p)))
					pars)))
	   ;; The actual name of the underlying Fortran routine
	   (ffi-fn (make-fortran-ffi-name name))
	   ;; The FFI return variables
	   (ffi-rvs (mapcar #'(lambda (p)
				(scat "NEW-" (first p)))
			    (remove-if-not #'(lambda (p)
					       (and (not (cast-as-array-p (second p)))
						    (not (eq (second p) :string))
						    (get-read-out-style (third p))))
					   pars)))
	   ;; The FFI arguments
	   (ffi-args (mapcar #'(lambda (p)
				 (if (cast-as-array-p (second p))
				     (scat "ADDR-" (first p))
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
	   (rvs (mapcar #'(lambda (p)
			    (if (or (cast-as-array-p (second p))
				    (eq (second p) :string))
				(first p)
				(scat "NEW-" (first p))))
			(remove-if-not #'(lambda (p)
					   (get-read-out-style (third p)))
				       pars)))
	   ;; The definition of the Lisp interface function we want.
	   (defun-body
	       `(
		 ;; Too hard to debug if inlined.
		 ;;(declaim (inline ,name))
		 (with-vector-data-addresses (,@saps)
		   (multiple-value-bind (,@return-value
					 ,@ffi-rvs)
		       (,ffi-fn ,@ffi-args ,@aux-ffi-args)
		   
		     (declare (ignore ,@(and (eq return-type :void) return-value)))
		     (values ,@(and (not (eq return-type :void))  return-value)
			     ,@(mapcar #'(lambda (s)
					   (if (eq s hidden-var-name)
					       hidden-var-name
					       s)) rvs)))))))
				      

      (if (find hidden-var-name (mapcar #'first pars))
	  `(
	    ;; Too hard to debug if inlined.
	    ;;(declaim (inline ,name))
	    ;;
	    ;; This used to create a complex number for
	    ;; hidden-var-name, but seemed to be causing some problems
	    ;; for CMUCL.  Therefore, we now create a 2-element array
	    ;; to hold the result (which is ok by Fortran rules), can
	    ;; create the complex result from the contents of the
	    ;; array.
	    ;;
	    ;; The problem seems to be that CMUCL was creating a
	    ;; "static" storage area for hidden-var-name.  Since CMUCL
	    ;; doesn't seem to know that it was being modified in the
	    ;; body, it returns the address of the static storage. So,
	    ;; for example, (/ (dot a b) (dot c d)) would have the
	    ;; results of the first dot overwritten by the second,
	    ;; making the division always return #c(1.0 0.0).  This is
	    ;; the theory---I'm not 100% sure it's right, but this
	    ;; change fixes the bug.
	    (let ((,hidden-var-name (make-array 2 :element-type 'double-float)))
	      (defun ,name ,args
		,@doc
		,@defun-body
		(complex (aref ,hidden-var-name 0) (aref ,hidden-var-name 1)))))
	  `(
	    ;; Too hard to debug if inlined.
	    ;;(declaim (inline ,name))
	    (defun ,name ,args
	      ,@doc
	      ,@defun-body))))))


;;
;; DEF-FORTRAN-ROUTINE
;;
;; A macro similar to DEF-ALIEN-ROUTINE but specialized to the Fortran
;; BLAS/LAPACK libraries.
;;
;; An external Fortran routine definition form (DEF-FORTRAN-ROUTINE
;; MY-FUN ...) creates two functions:
;;
;;   1. a raw FFI (foreign function interface),
;;   2. an easier to use lisp interface to the raw interface.
;;
;; The documentation  given here relates in the most part to the
;; simplified lisp interface.
;;
;; Example:
;; ========
;; libblas.a contains the fortran subroutine DCOPY(N,X,INCX,Y,INCY)
;; which copies the vector Y of N double-float's to the vector X.
;; The function name in libblas.a is \"dcopy_\" (by Fortran convention).
;;
;; (DEF-FORTRAN-ROUTINE DCOPY :void 
;;   (N :integer :input)
;;   (X (* :double-float) :output)
;;   (INCX :integer :input)
;;   (Y (* :double-float) :input)
;;   (INCY :integer :input))
;;
;; will expand into:
;;
;; (DEF-ALIEN-ROUTINE ("dcopy_" FORTRAN-DCOPY) void
;;    (N :int :copy)
;;    (X (* double-float))
;;    ...
;;
;; and
;; 
;; (DEFUN DCOPY (N,X,INCX,Y,INCY)
;;    ...
;;
;; In turn, the lisp function DCOPY calls FORTRAN-DCOPY which calls
;; the Fortran function "dcopy_" in libblas.a.
;;
;; There is a nasty hack for complex return values.  This is how
;; Solaris f77 handles functions that return complex numbers.  In
;; essence, an extra parameter is inserted before all others and this
;; extra parameter is used to store the complex result.
;;
;; Here is an example
;;
;; (DEF-FORTRAN-ROUTINE ZDOTC :complex-double-float
;;   (N :integer :input)
;;   (X (* :complex-double-float) :input)
;;   (INCX :integer :input)
;;   (Y (* :complex-double-float) :input)
;;   (INCY :integer :input))
;;
;;  will expand into:
;;
;; (DEF-ALIEN-ROUTINE ("zdotc_" FORTRAN-ZDOTC) void
;;    (hidden-complex-return-variable (* double-float) :in)
;;    (n :int :copy)
;;    (zx (* double-float) :in)
;;    (incx :int :copy)
;;    (zy (* double-float) :in)
;;    (incy :int :copy))
;;
;;  and:
;;
;;  (DEFUN ZDOTC (N ZX INCX ZY INCY)
;;    (let ((hidden-complex-return-variable
;;           (make-array 2 :element-type 'double-float)))
;;      (with-vector-data-addresses
;;       ((addr-hidden-complex-return-variable hidden-complex-return-variable)
;;        (addr-zx zx) 
;;        (addr-zy zy))
;;       (multiple-value-bind
;;           (return-value)
;;           (fortran-zdotc addr-hidden-complex-return-variable n addr-zx incx
;;            addr-zy incy)
;;         (declare (ignore return-value))
;;         (values (complex (aref hidden-complex-return-variable 0)
;;                          (aref hidden-complex-return-variable 1)))))))))
;;
;;
;; Arguments:
;; ==========
;;
;;
;; NAME    Name of the lisp interface function that will be created.
;;         The name of the raw FFI will be derived from NAME via
;;         the function MAKE-FFI-NAME.  The name of foreign function
;;         (presumable a Fortran Function in an external library) 
;;         will be derived from NAME via MAKE-FORTRAN-NAME.
;;
;;         See MAKE-FFI-NAME, MAKE-FORTRAN-NAME.
;;
;; RETURN-TYPE
;;         The type of data that will be returned by the external
;;         (presumably Fortran) function.
;;       
;;             (MEMBER RETURN-TYPE '(:VOID :INTEGER :SINGLE-FLOAT :DOUBLE-FLOAT
;;                                   :COMPLEX-SINGLE-FLOAT :COMPLEX-DOUBLE-FLOAT))
;;
;;
;;         See GET-READ-OUT-TYPE.
;;
;; BODY    A list of parameter forms.  A parameter form is:
;;
;;                  (VARIABLE TYPE &optional (STYLE :INPUT))
;;
;;         The VARIABLE is the name of a parameter accepted by the
;;         external (presumably Fortran) routine.  TYPE is the type of
;;         VARIABLE.  The recognized TYPE's are:
;;
;;                TYPE                    Corresponds to Fortran Declaration
;;                ----                    ----------------------------------
;;                :STRING                  CHARACTER*(*)
;;                :INTEGER                 INTEGER
;;                :SINGLE-FLOAT            REAL
;;                :DOUBLE-FLOAT            DOUBLE PRECISION
;;                :COMPLEX-SINGLE-FLOAT    COMPLEX
;;                :COMPLEX-DOUBLE-FLOAT    COMPLEX*16
;;                 (* X)                   An array of type X.
;;
;;               (MEMBER X '(:INTEGER :SINGLE-FLOAT :DOUBLE-FLOAT
;;                           :COMPLEX-SINGLE-FLOAT :COMPLEX-DOUBLE-FLOAT)
;;
;;
;;         The STYLE (default :INPUT) defines how VARIABLE is treated.
;;         This is by far the most difficult quantity to learn.  To
;;         begin with:
;;
;;
;;                (OR (MEMBER STYLE '(:INPUT :OUTPUT :INPUT-OUTPUT))
;;                    (MEMBER STYLE '(:IN :COPY :IN-OUT :OUT)))
;;
;;            TYPE        STYLE             Description
;;            ----        -----             -----------
;;              X          :INPUT            Value will be used but not modified.
;;                                           Similar to the :COPY style of DEF-ALIEN-ROUTINE.
;;                         :OUTPUT           Input value not used (but some value must be given),
;;                                           a value is returned via the Lisp
;;                                           command VALUES from the lisp function NAME.
;;                                           Similar to the :IN-OUT style of DEF-ALIEN-ROUTINE.
;;                         :INPUT-OUTPUT     Input value may be used, a  value
;;                                           is returned via the lisp command VALUES from the
;;                                           lisp function NAME.
;;                                           Similar to the :IN-OUT style of DEF-ALIEN-ROUTINE.
;;
;;           ** Note:  In all 3 cases above the input VARIABLE will not be destroyed
;;                     or modified directly, a copy is taken and a pointer of that
;;                     copy is passed to the (presumably Fortran) external routine.
;;
;;
;;           (OR (* X)     :INPUT           Array entries are used but not modified.
;;               :STRING)  :OUTPUT          Array entries need not be initialized on input,
;;                                          but will be *modified*.  In addition, the array
;;                                          will be returned via the Lisp command VALUES
;;                                          from the lisp function NAME.
;;
;;                         :INPUT-OUTPUT    Like :OUTPUT but initial values on entry may be used.
;;              
;;         The keyword :WORKSPACE is a nickname for :INPUT.  The
;;         keywords :INPUT-OR-OUTPUT, :WORKSPACE-OUTPUT,
;;         :WORKSPACE-OR-OUTPUT are nicknames for :OUTPUT.
;;
;;         This is complicated.  Suggestions are encouraged to
;;         interface a *functional language* to a *pass-by-reference
;;         language*.
;;
;; Further Notes:
;; ===============
;;
;; Fortran calling sequence says everything is pass-by-reference.
;; Essentially, every parameter is actually a pointer to the
;; parameter.  In CMUCL, we take this to mean :in-out or :copy
;; parameter type, but we could have actually used a pointer.  I'm not
;; sure what is the right way to do this.
;;
;; Some Fortran routines use Fortran character strings in the
;; parameter list.  The definition here is suitable for Solaris
;; where the Fortran character string is converted to a C-style null
;; terminated string, AND an extra hidden parameter that is appended
;; to the parameter list to hold the length of the string.
;;
;; If your Fortran does this differently, you'll have to change this
;; definition accordingly!
;;
;;
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
	(hack-body `(,@body))
	(hidden-var-name nil))

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
	(setq hidden-var-name (gensym "HIDDEN-COMPLEX-RETURN-"))
	(setq hack-body `(,@doc
			  (,hidden-var-name ,hack-return-type :out)
			  ,@pars))
	(setq hack-return-type :void)))
			  
    `(eval-when (load eval compile)
       (progn

	 ;; Removing 'inlines'
	 ;; It seems that CMUCL has a problem
	 ;; with inlines of FFI's when a
	 ;; lisp image is saved.  Until
	 ;; the matter is clarified we
	 ;; leave out 'inline's
         
	 ;(declaim (inline ,lisp-name))   ;sbcl 0.8.5 has problems with inlining
	 (define-alien-routine (,fortran-name ,lisp-name) ,(get-read-out-type hack-return-type)
	   ,@(parse-fortran-parameters hack-body))
	 ,@(def-fortran-interface name hack-return-type hack-body hidden-var-name)))))


;; Increment an SAP by N, assuming SAP has type TYPE.  Thus, if TYPE
;; is double-float, and N is 2, we really want to increment the sap by
;; 16 since a double-float has length 8.
(defmacro incf-sap (type sap &optional (n 1 n-p))
  (ecase type
    (:double-float `(setf ,sap (sb-sys:sap+ ,sap (* ,n 8))))
    (:single-float `(setf ,sap (sb-sys:sap+ ,sap (* ,n 8))))
    (:complex-double-float  `(setf ,sap (sb-sys:sap+ ,sap (* ,n 16))))
    (:complex-single-float  `(setf ,sap (sb-sys:sap+ ,sap (* ,n 8))))))

;; These are the specialized arrays that matlisp understands how to
;; deal with. (complex double-float) and (complex single-float) aren't
;; really arrays, but CMUCL basically stores the real and imaginary
;; parts in consecutive memory locations, just like (simple-array
;; double-float (*)), so we can handle them too.
;;
;; Although CMUCL may support other specialized vectors, it's not
;; likely that any foreign function would actually understand the
;; data.  In particular, there are unsigned-bytes of length 4, 2, and
;; 1, which are packed all into words.  There is also fixnum
;; (signed-byte 30), but foreign functions probably wouldn't know the
;; format of fixnums.
;;
;; We also don't currently support arrays with elements that are 8 or
;; 16 bits long.  (But we could.  Just need more code.  Might be
;; useful to support Fortran INTEGER*1 and INTEGER*2 types.)

(deftype matlisp-specialized-array ()
  `(or (complex double-float)
       (complex single-float)
       (simple-array (complex double-float) *)
       (simple-array (complex single-float) *)
       (simple-array double-float *)
       (simple-array single-float *)
       (simple-array (signed-byte 32) *)
       (simple-array (signed-byte 16) *)
       (simple-array (signed-byte  8) *)
       (simple-array (unsigned-byte 32) *)
       (simple-array (unsigned-byte 16) *)
       (simple-array (unsigned-byte  8) *)))

;; Removing 'inlines'
;; It seems that CMUCL has a problem
;; with inlines of FFI's when a
;; lisp image is saved.  Until
;; the matter is clarified we
;; leave out 'inline's
(declaim (inline vector-data-address))
(defun vector-data-address (vec)
  "Return the physical address of where the actual data of the object
VEC is stored.

  VEC - must be a either a (complex double-float), (complex single-float)
        or a specialized array type in CMU Lisp.  This currently means
        VEC is a simple-array of one dimension of one of the following types:

                  (complex double-float)
                  (complex single-float)
                  double-float
                  single-float
                  (signed-byte 32)
                  (signed-byte 16)
                  (signed-byte  8)
                  (unsigned-byte 32)
                  (unsigned-byte 16)
                  (unsigned-byte  8)

Returns
  1   - system area pointer to the actual data"
  (locally
      (declare (optimize (speed 1) (safety 3)))
    ;; It's quite important that the arrays have the write type.
    ;; Otherwise, we will probably get the address of the data wrong,
    ;; and then foreign function could be scribbling over who knows
    ;; where!
    ;;
    (check-type vec matlisp-specialized-array))
  (locally
      (declare (type matlisp-specialized-array vec)
	       (optimize (speed 3) (safety 0) (space 0)))

      ;; For complex double-floats, memory is laid out like
      ;;   
      ;;   byte offset    Value
      ;;        0         type code
      ;;        4         unused filler
      ;;        8         real part
      ;;       16         imaginary part
      ;;
      ;; For complex single-floats, memory is laid out like
      ;;   
      ;;   byte offset    Value
      ;;        0         type code
      ;;        4         real part
      ;;        8         imaginary part
      ;;
      ;; For multidimensional simple-arrays, we have this:
      ;;
      ;;   byte offset    Value
      ;;        0         type code and header length (gives array rank too)
      ;;        4         fill pointer (fixnum)
      ;;        8         fill-pointer-p (T or NIL)
      ;;       12         available elements (fixnum)
      ;;       16         address of data vector object
      ;;       20         displacement (fixnum, usually 0)
      ;;       24         displacedp (T or NIL)
      ;;       28         range of first index (fixnum)
      ;;       32         range of second index (fixnum)
      ;;       ...        ...
      ;;
      ;; For us, the important part is at offset 16, the address of
      ;; the data vector object.  This should be a pointer to some
      ;; type of specialized simple-array.  Thus, the first case above
      ;; can be used to get the actual data.
      (if (typep vec '(simple-array * (*)))
	  (sb-sys:vector-sap vec)
	  (let ((base-address
		 (the (unsigned-byte 32) 
		   (logandc1 7 (sb-kernel:get-lisp-obj-address vec))))) 
	    (declare (type (unsigned-byte 32) base-address))
	    (sb-sys:int-sap
	     (etypecase vec
	       ((complex double-float)
		(the (unsigned-byte 32) (+ 8 base-address)))
	       ((complex single-float)
		(the (unsigned-byte 32) (+ 4 base-address)))
	       ((simple-array * *)
		;; A multidimensional simple-array
		(let ((data-vector
		       (logandc1 7 (sb-sys:sap-ref-32
				    (sb-sys:int-sap (+ base-address 16))
				    0))))
		  (the (unsigned-byte 32) (+ data-vector 8))))))))))

;;; Hmm, according to the Solaris f77 manpage, Fortran assumes certain
;;; floating point modes.  It says arithmetic is non-stop and
;;; underflows are gradual.  We assume that means all traps are off,
;;; including Invalid.  This is important: SVD can cause an Invalid
;;; exception.  However, with Invalid disabled, SVD will complete and
;;; return the expected results.
;;;
;;; So we save the current mode, set the mode for Fortran, run the
;;; body, and finally reset the mode back to the original.

(defmacro with-fortran-float-modes (&body body)
  "Execute the body with the IEEE FP modes appropriately set for Fortran"
  `(sb-int:with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero :invalid)
    ,@body))


(defmacro with-vector-data-addresses (vlist &body body)
  "WITH-VECTOR-DATA-ADDRESSES (var-list &body body)

 Execute the body with the variables in VAR-LIST appropriately bound.
 VAR-LIST should be a list of pairs.  The first element is the address
 of the desired object; the second element is the variable whose address
 we want.

 Garbage collection is also disabled while executing the body."
  ;; We wrap everything inside a WITHOUT-GCING form to inhibit garbage
  ;; collection to avoid complications that may arise during a
  ;; collection while in a fortran call.
  ;;
  ;; This might not really be necessary, but it's not clear if the
  ;; alien object will have the right value if GC occurs after getting
  ;; the alien object but before the alien function is called.  Let's
  ;; be safe rather than sorry.
  `(with-fortran-float-modes
    (sb-sys::without-gcing
	   (let (,@(mapcar #'(lambda (pair)
			       `(,(first pair)
				 (vector-data-address ,(second pair))))
			   vlist))
	     ,@body))))

(defmacro with-gensyms (symlist &body body)
  `(let ,(mapcar #'(lambda (sym)
		      `(,sym (gensym ,(symbol-name sym))))
		  symlist)
    ,@body))

#| Obsolete code as of Revision 1.6. simsek

;;;; Naming conventions.  Given the Fortran routine name, the lisp
;;;; name has "FORTRAN-" prepended to it.  Thus, dswap becomes
;;;; FORTRAN-DSWAP.

;;;; Implementation notes:
;;;;
;;;; Fortran calling sequence says everything is
;;;; pass-by-reference. Essentially, every parameter is actually a
;;;; pointer to the parameter.  In CMUCL, we take this to mean :in-out
;;;; parameter type, but we could have actually used a pointer.  I'm
;;;; not sure what is the right way to do this.
;;;;
;;;; Some Fortran routines use Fortran character strings in the
;;;; parameter list.  The definition here is suitable for Solaris
;;;; where the Fortran character string is converted to a C-style null
;;;; terminated string, AND an extra hidden parameter that is appended
;;;; to the parameter list to hold the length of the string.
;;;;
;;;; If your Fortran does this differently, you'll have to change this
;;;; definition accordingly!


(eval-when (load compile eval)
(defun make-fortran-name (name)
  ;; Given the Fortran routine name NAME, this returns the real
  ;; underlying name.  This depends on the compiler conventions being
  ;; used.  Some Fortran compilers take the Fortran name NAME and
  ;; produce "name_" as the real routine name.  Others will prepend
  ;; the underscore.  Yet others might convert the name to all upper
  ;; case.
  (concatenate 'string (string-downcase (symbol-name name)) "_"))

(defun make-lisp-name (name)
  (concatenate 'string "FORTRAN-" (symbol-name name)))

(defun handle-fortran-parameters (parlist)
  (flet ((convert-fortran-type (type)
	   (second (assoc type '((:integer sb-c::int)
				 (:single-float sb-c::single-float)
				 (:double-float sb-c::double-float)
				 (:complex-single-float sb-c::single-float)
				 (:complex-double-float sb-c::double-float))))))
    (multiple-value-bind (doc-string pars)
	(if (stringp (first parlist))
	    (values (list (concatenate 'string "Fortran FFI:
"
				       (first parlist))) (rest parlist))
	    (values nil parlist))
      (let* ((string-len-pars '())
	     (new-parlist
	      (mapcar #'(lambda (par)
			  (destructuring-bind (var type &optional style)
			      par
			    ;; Note: We use :copy instead of :in-out
			    ;; for scalar variables.  That means the
			    ;; Fortran routine is not going to modify
			    ;; the scalar so we don't need to return a
			    ;; new value.  If this is wrong, we need
			    ;; to change this to :in-out, but that can
			    ;; cause additional consing for the
			    ;; returned value.
			    (cond ((eq type :integer)
				   `(,var int ,(or style :copy)))
				  ((eq type :double-float)
				   `(,var double-float ,(or style :copy)))
				  ((eq type :single-float)
				   `(,var single-float ,(or style :copy)))
				  ((eq type :complex-double-float)
				   ;; Fortran wants, essentially, the
				   ;; complex number to look like a
				   ;; 2-element array consisting of
				   ;; the real and imaginary parts.
				   `(,var (* double-float)))
				  ((eq type :complex-single-float)
				   `(,var (* single-float)))
				  ((eq type :string)
				   ;; C strings
				   (pushnew `(,(intern (concatenate 'string
								    "LEN-"
								    (symbol-name var)))
					      int)
					    string-len-pars)
				   `(,var c-string))
				  ((listp type)
				   `(,var (* ,(convert-fortran-type (second type)))))
				  (t
				   (error "Unknown Fortran type")))))
		      pars)))
	`(,@doc-string ,@new-parlist ,@string-len-pars)))))
)

;; A slightly simplified interface to def-alien-routine that creates
;; two functions: the raw interface, and a slightly easier to use lisp
;; interface to the raw interface.

;; TODO: If I were smarter, I would be able to make this routine
;; generate all of the 4 possible versions of the routine: single,
;; double, complex, double complex.  Someday.  Easy to do in
;; principle: the first letter of the name is S, D, C, or Z.  All
;; floating-point parameter types get changed to :single-float,
;; :double-float, :complex-single-float, and :complex-double-float.

(defmacro def-fortran-routine (name return-type &rest stuff)
  "def-fortran-routine name return-type {(arg-name arg-type [style])}*

Define a foreign interface function to a Fortran routine with the
specified NAME.  Two functions are created.  One is named
FORTRAN-<NAME> which denotes the actual bare interface to the Fortran
function.  The other is named NAME, which is a lisp interface to the
underlying bare interface.  This function properly handles lisp arrays
and scalar types when calling the underlying bare interface.

RETURN-TYPE is the return type of the routine.  VOID may be used to
specify a functionwith no result.

The arguments of the function are specified as follows: ARG-NAME is a
symbol denoting the argument, primarily for documentation.  ARG-TYPE
is the type of the argument.  STYLE is specifies how the argument is
passed.

The ARG-TYPES recognized are:
     :string                      Fortran CHARACTER*(*)
     :integer                     Fortran INTEGER
     :single-float                Fortran REAL
     :double-float                Fortran DOUBLE PRECISION
     :complex-single-float        Fortran COMPLEX
     :complex-double-float        Fortran COMPLEX*16
     (* X)                        An array of type X, where X should either be
                                  :integer, :single-float, :double-float,
                                  :complex-single-float, or :complex-double-float

STYLE, if given, should be one of the following:

     :IN-OUT                      The scalar variable is modified by the routine.
                                  Otherwise, the variable is an input only.

Arrays do not need a STYLE option and any STYLE is ignored.

See ALIEN:DEF-ALIEN-ROUTINE for more information.
"
  (let ((fortran-name (make-fortran-name name))
	(lisp-name (intern (make-lisp-name name))))
  `(progn
    (declaim (inline ,lisp-name))
    (def-alien-routine (,fortran-name ,lisp-name) ,return-type
      ,@(handle-fortran-parameters stuff))
    ,@(def-fortran-interface name stuff))))

;; Create a simple lisp interface to the Fortran function by passing
;; array addresses to the bare Fortran function.
(defun def-fortran-interface (name stuff)
  (multiple-value-bind (doc-string pars)
      (if (stringp (first stuff))
	  (values (list (first stuff)) (rest stuff))
	  (values nil stuff))
    (let* ((string-len-pars '())
	   (lisp-name (intern (make-lisp-name name)))
	   (name-alist
	    (mapcar #'(lambda (n)
			(destructuring-bind (var var-type &optional style)
			    n
			  (declare (ignore style))
			  (cond ((or (listp var-type)
				     (eq var-type :complex-double-float)
				     (eq var-type :complex-single-float))
				 `(,var ,(gensym (symbol-name var)) ,var-type))
				(t
				 (when (eq var-type :string)
				   (pushnew `(length (the string ,var)) string-len-pars))
				 `(,var)))))
		    pars)))

      `((declaim (inline ,name))
	(defun ,name ,(mapcar #'first name-alist)
	  ,@doc-string
	  (with-vector-data-addresses
	      ,(mapcar #'(lambda (x)
			   (list (second x)
				 (first x)))
		       (remove-if #'(lambda (x)
				      (null (rest x)))
				  name-alist))
	    (,lisp-name ,@(mapcar #'(lambda (x)
				      (or (second x) (first x)))
				  name-alist)
			,@string-len-pars)))))))

|#


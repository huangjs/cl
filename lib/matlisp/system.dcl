;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10 -*-
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
;;; $Id: system.dcl,v 1.26 2005/08/19 16:55:35 rtoy Exp $
;;;
;;; $Log: system.dcl,v $
;;; Revision 1.26  2005/08/19 16:55:35  rtoy
;;; Make sure (most of) the source-pathnames have a trailing semicolon.
;;; Fixes an issue with Allegro 7.
;;;
;;; Revision 1.25  2004/05/24 16:38:40  rtoy
;;; Remove unused code.
;;;
;;; Revision 1.24  2004/05/20 21:41:54  rtoy
;;; DEFLOGICALPATH is in the MATLISP-START package now.
;;;
;;; Revision 1.23  2004/03/17 03:22:13  simsek
;;; Adding support for DFFTPACK and TOMS on windows.
;;;
;;; Revision 1.22  2003/12/07 15:03:44  rtoy
;;; Add support for SBCL.  I did not test if SBCL works, but CMUCL still
;;; works.
;;;
;;; From Robbie Sedgewick on matlisp-users, 2003-11-13.
;;;
;;; Revision 1.21  2003/06/01 15:21:59  rtoy
;;; Add conditions.lisp to dependencies.
;;;
;;; Revision 1.20  2002/09/30 18:28:52  simsek
;;; o Added changes by N.Neuss for getrs functions
;;;
;;; Revision 1.19  2002/01/08 00:33:40  rtoy
;;; Add defsystem definition for the MINPACK package.
;;;
;;; Revision 1.18  2001/10/25 21:52:29  rtoy
;;; Add geqr for QR support.
;;;
;;; Revision 1.17  2001/07/26 15:48:14  rtoy
;;; Added f77-mangling.lisp.
;;;
;;; Revision 1.16  2001/07/21 17:56:04  simsek
;;; Gnuplot not currently suported for Allegro
;;;
;;; Revision 1.15  2001/07/10 16:16:58  rtoy
;;; Add gnuplot
;;;
;;; Revision 1.14  2001/05/01 13:12:12  rtoy
;;; macros.l now contains I1MACH, R1MACH, D1MACH so don't need these
;;; versions anymore.
;;;
;;; Revision 1.13  2001/04/28 13:32:52  rtoy
;;; Added interface to TOMS 715 (SPECFUN).
;;;
;;; Revision 1.12  2001/04/26 21:54:48  rtoy
;;; o Try to get the correct source-path for all modules/files so that it
;;;   works on Allegro and CMUCL.
;;; o Added zeroin function.
;;;
;;; Revision 1.11  2001/04/25 17:49:37  rtoy
;;; o Add the cpoly package.
;;; o Arrance the module structure to match the directory structure more
;;;   closely.
;;;
;;; Revision 1.10  2001/02/26 22:44:26  rtoy
;;; The source-pathname's for quadpack were messed up.
;;;
;;; Revision 1.9  2001/02/26 19:54:55  rtoy
;;; Forgot to add quadpack.lisp to the matlisp system definition;
;;; rearrange module structure so quadpack is a complete module unto
;;; itself.  (Except for dependency on f2cl macros.)
;;;
;;; Revision 1.8  2001/02/23 18:02:03  rtoy
;;; Add stuff needed to build quadpack as a part of matlisp.
;;;
;;; Revision 1.7  2001/02/22 08:10:35  simsek
;;; o Added support for CMUCL 18c and Allegro 6.0
;;;
;;; Revision 1.6  2000/10/04 01:19:18  simsek
;;; o Moved version related code to package.lisp
;;;
;;; Revision 1.5  2000/07/11 02:45:15  simsek
;;; o Changed version from 1.0a to 1.0b
;;;
;;; Revision 1.4  2000/07/11 02:04:50  simsek
;;; o Added support for Allegro CL
;;; o Moved configuration code to config.lisp
;;;
;;; Revision 1.3  2000/05/05 21:57:33  simsek
;;; o Removed ysmm from matlisp-lapack-wrappers
;;;    we're not doing symmetric matrices yet
;;;
;;; Revision 1.2  2000/05/05 21:34:03  simsek
;;; o Updated defsystem form to include dfftpack stuff
;;;
;;; Revision 1.1  2000/04/13 20:43:15  simsek
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "COMMON-LISP-USER")

(matlisp-start::deflogicalpath "matlisp")

(require "MAKE" (namestring 
		 (translate-logical-pathname 
		  "matlisp:defsystem.lisp")))

(mk::defsystem matlisp-packages
      :source-pathname "matlisp:"
      :source-extension "lisp"
      :components
      ((:file "packages")))

(mk::defsystem lazy-loader
      :source-pathname "matlisp:lib;"
      :source-extension "lisp"
      :binary-pathname "matlisp:bin;"
      :depends-on ("matlisp-packages")
      :components
      ((:file "lazy-loader"
	      ;; you need the load-only here,
	      ;; otherwise, Allegro tries to
	      ;; load the DLL (SO)'s twice
	      ;; and fails.
	)))

(mk::defsystem matlisp
      :source-pathname "matlisp:"
      :source-extension "lisp"
      :binary-pathname "matlisp:bin;"
      :depends-on ("lazy-loader"
                   "matlisp-packages")
      :components
      ((:module "foreign-interface"
	:source-pathname "matlisp:src;"
	:source-extension "lisp"
	:binary-pathname ""
	:components ("f77-mangling"
		     #+:cmu "ffi-cmu"
		     #+:sbcl "ffi-sbcl"
		     #+:allegro "ffi-acl"
		     ))
       (:module "foreign-functions"
	:source-pathname "matlisp:src;"
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface")
	:components ("blas"
		     "lapack"
		     "dfftpack"
		     #+nil "ranlib"))
       (:module "matlisp-essentials"
	:source-pathname "matlisp:src;"
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface" 
		     "foreign-functions")
	:components ("conditions"
		     "matrix"
		     "ref"
		     "print"
		     "copy"))

       (:module "matlisp-blas-wrappers"
	:source-pathname "matlisp:src;"
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface" 
		     "foreign-functions"
		     "matlisp-essentials")
	:components ("axpy"
		     "scal"
		     "swap"
		     "gemm"))

       (:module "matlisp-lapack-wrappers"
	:source-pathname "matlisp:src;"
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface" 
		     "foreign-functions"
		     "matlisp-essentials")
	:components ("gesv"
		     "geev"
		     "getrf"
		     "getrs"))

       (:module "matlisp-functions"
        :source-pathname "matlisp:src;"
	:source-extension "lisp"
	:binary-pathname ""
	:depends-on ("foreign-interface"
		     "foreign-functions"
		     "matlisp-essentials"
		     "matlisp-blas-wrappers"
		     "matlisp-lapack-wrappers")
	:components ("compat"
		     "help"
		     "diag"
		     "special"
		     "reader"
		     "trans"
		     "realimag"
		     "reshape"
		     "join"
		     "svd"
		     "sum"
		     "norm"
		     "dot"
		     "trace"
		     "seq"
		     "vec"
		     "map"
		     "mplus"
		     "mminus"
		     "mtimes"
		     "mdivide"
		     "msqrt"
		     "fft"
		     "geqr"))
       (:module "special-functions"
		:source-pathname "matlisp:src;"
		:binary-pathname ""
		:depends-on ("matlisp-functions")
		:components
		((:file "specfun")))
       ;; Various add-on packages for matlisp
       ;; This is just the f2cl macros we need, not all of f2cl.
       (:module "f2cl-macros"
		:source-pathname "matlisp:lib-src;"
		:source-extension "l"
		:binary-pathname ""
		:components
		((:file "macros")))
       ;; This is Quadpack, converted from the Fortran
       ;; implementation to Lisp via f2cl.
       (:module "quadpack-functions"
		:source-pathname ""
		:binary-pathname ""
		:depends-on ("f2cl-macros")
		:components
		((:module "quadpack-interface"
			  :source-pathname "matlisp:src;"
			  :binary-pathname ""
			  :components
			  ((:file "quadpack")))
		 (:module "quadpack-lib"
			  :source-pathname "matlisp:lib-src;quadpack;"
			  :binary-pathname ""
			  :package "QUADPACK"
			  :components
			  (
			   #+nil
			   (:module mach-par
				    :source-pathname ""
				    :source-extension "lisp"
				    :binary-pathname ""
				    :components
				    ((:file "d1mach")
				     (:file "i1mach")))
			   (:module src
				    :source-pathname ""
				    ;; :depends-on ("mach-par")
				    :binary-pathname ""
				    :components
				    (
				     ;; Support
				     (:file "dqwgtf")
				     (:file "dqcheb")
				     (:file "dqk15w")
				     (:file "dqwgts")
				     (:file "dqwgtc")
				     (:file "dgtsl")
				     (:file "xerror")
	       
				     ;; Core integration routines
				     (:file "dqk15")
				     (:file "dqk31")
				     (:file "dqk41")
				     (:file "dqk51")
				     (:file "dqk61")
				     (:file "dqk21")
				     (:file "dqk15i")
				     (:file "dqelg")
				     (:file "dqpsrt")
				     (:file "dqc25s"
					    :depends-on ("dqcheb" "dqk15w"))
				     (:file "dqmomo")
				     (:file "dqc25c"
					    :depends-on ("dqcheb"
							 "dqk15w"))
				     (:file "dqc25f"
					    :depends-on ("dgtsl"
							 "dqcheb"
							 "dqk15w"
							 "dqwgtf"))
				     ;; Basic integrators
				     (:file "dqage"
					    :depends-on ("dqk15"
							 "dqk31"
							 "dqk41"
							 "dqk51"
							 "dqk61"
							 "dqk21"
							 "dqpsrt"))
				     (:file "dqagie"
					    :depends-on ("dqelg"
							 "dqk15i"
							 "dqpsrt"))
				     (:file "dqagpe"
					    :depends-on ("dqelg"
							 "dqpsrt"
							 "dqk21"
							 ))
				     (:file "dqagse"
					    :depends-on ("dqk21"
							 "dqelg"
							 "dqpsrt"))
				     (:file "dqawfe"
					    :depends-on ("dqagie"
							 "dqawoe"
							 "dqelg"))
				     (:file "dqawoe"
					    :depends-on ("dqc25f"
							 "dqpsrt"
							 "dqelg"))
				     (:file "dqawse"
					    :depends-on ("dqc25s"
							 "dqmomo"
							 "dqpsrt"))
				     (:file "dqawce"
					    :depends-on ("dqc25c"
							 "dqpsrt"))
				     ;; Simplified interface routines
				     (:file "dqng"
					    :depends-on ("xerror"))
				     (:file "dqag"
					    :depends-on ("dqage"
							 "xerror"))
				     (:file "dqags"
					    :depends-on ("dqagse"
							 "xerror"))
				     (:file "dqagi"
					    :depends-on ("dqagie"
							 "xerror"))
				     (:file "dqawf"
					    :depends-on ("dqawfe"
							 "xerror"))
				     (:file "dqawo"
					    :depends-on ("dqawoe"
							 "xerror"))
				     (:file "dqaws"
					    :depends-on ("dqawse"
							 "xerror"))
				     (:file "dqawc"
					    :depends-on ("dqawce"
							 "xerror"))))))))
       (:module "minpack-functions"
		:source-pathname ""
		:binary-pathname ""
		:depends-on ("f2cl-macros")
		:components
		((:module "minpack-lib"
			  :source-pathname "matlisp:lib-src;minpack;"
			  :binary-pathname ""
			  :package "MINPACK"
			  :components
			  ((:file "dpmpar")
			   (:file "enorm")
			   (:file "fdjac2")
			   (:file "qrsolv")
			   (:file "lmpar")
			   (:file "qrfac")
			   (:file "lmdif")
			   (:file "lmdif1")
			   (:file "lmder")
			   (:file "lmder1")
			   (:file "dogleg")
			   (:file "qform")
			   (:file "r1mpyq")
			   (:file "r1updt")
			   (:file "hybrj" :depends-on ("dogleg" "qform" "r1mpyq" "r1updt"))
			   (:file "hybrj1" :depends-on ("hybrj"))
			   ))))
       (:module "lib-src"
		:binary-pathname ""
		:components
		(#+nil
		 (:file "d1mach"
			:package "MATLISP-LIB")
		 (:module "cpoly"
			  :source-extension "lisp"
			  :binary-pathname ""
			  :components
			  ((:file "cpoly")
			   (:file "zeroin"
				  :package "MATLISP-LIB")))
		 #+(or :cmu :sbcl)
		 (:module "gnuplot"
			  :source-extension "lisp"
			  :binary-pathname ""
			  :components
			  ((:file "gnuplot")))))))


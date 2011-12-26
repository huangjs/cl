;;; -*- syntax: common-lisp; package: clm; base: 10; mode: lisp -*-
;;;
;;; definstrument, various instrument debugging functions
;;;
;;; used to be in sound.lisp, but split out to make debugging the new versions simpler.
;;; Definstrument was straightforward until version 3 of cmus -- in its new incarnation
;;; it has to know how to write/compile/load c modules on each system.  And as of
;;; 12-Feb-97 it has to be able to live with any combination of lisp/machine output
;;; on the same directory (i.e. user here at ccrma starts clm, which automatically chooses
;;; whichever lisp/machine combination happens to work there, all such machines
;;; mounting the user's directories, so when he loads his instruments, clm has to
;;; find the right versions).

(in-package :clm)

(defvar *clm-snd-in-progress* nil)
(defvar *current-ins-args* nil)

#-clisp
(defmacro set-instrument-properties (name &optional file print-function)
  `(progn
     (setf (get ,name :ins-vars) ',variable-load-list)
     (setf (get ,name :ins-args) ',clm::*current-ins-args*)
     (setf (get ,name :c-proc) ,(symbol-name clm::*c-proc*))
     (setf (get ,name :print-function) ,print-function)
     (setf (get ,name :c-file-name) ,file)))

#+clisp
(defmacro set-instrument-properties (name &optional file print-function library)
  `(progn
     (setf (get ,name :ins-vars) ',variable-load-list)
     (setf (get ,name :ins-args) ',clm::*current-ins-args*)
     (setf (get ,name :c-proc) ,(symbol-name clm::*c-proc*))
     (setf (get ,name :print-function) ,print-function)
     (setf (get ,name :library) ,library)
     (setf (get ,name :c-file-name) ,file)))

(defun clm-datai (&optional name) (get (or name *clm-ins*) :datai))
(defun clm-datar (&optional name) (get (or name *clm-ins*) :datar))

(defun ins-var (var-name &optional (ins-name *clm-ins*))
  (let ((insvars (get ins-name :ins-vars)))
    (if insvars
	(let ((var (find (symbol-name var-name) insvars :test #'string-equal :key #'first)))
	  (when var
	    (gen-unload (run-type->class (aref (clm-datai ins-name) (second var)))
			(second var)
			(third var)
			(clm-datai ins-name)
			(clm-datar ins-name)))))))

(defun describe-instrument (&optional ins-name varname (stream t))
  (let* ((insname (or ins-name *clm-ins*))
	 (vars (get insname :ins-vars)))
    (if vars
	(let ((print-function (get insname :print-function)))
	  (if print-function
	      (funcall print-function vars varname stream)
	    (let* ((datai (clm-datai insname))
		   (datar (and datai (clm-datar insname))))
	      (if (and datai datar)
		  (progn
		    (format stream "~A:~%" (or *clm-ins* insname))
		    (loop for var in vars do
		      (format stream "  ~A: ~A~%" (first var)
			      (if (eq (fourth var) :integer)
				  (aref datai (second var))
				(if (eq (fourth var) :real)
				    (aref datar (third var))
				  (if (= (aref datai (second var)) +string+)
				      (format nil "~S" (gen-unload "1" (second var) (third var) datai datar))
				    (gen-unload (run-type->class (aref datai (second var))) (second var) (third var) datai datar))))))))))))))

(defun di (&optional name) (describe-instrument name))

#+clisp (ffi:default-foreign-language :stdc)

#+openmcl
(defun fixup-gensyms-and-write (lst &key stream)
  ;; (defun hi () (let ((#:G123 0)) #:G123)) is an error in Lisp! so before writing the
  ;; ins-code to the intermediate file in the *ins-file-loading* case, we need to
  ;; strip out the #:'s and replace them with something unlikely such as "clm_".
  ;; The keyword "stream" is used to mimic lisp's write function which we're replacing.
  (let ((code (write-to-string lst))
	(looking nil))
    (loop for char across code do
      (if (char= char #\#)
	  (setf looking t)
	(if looking
	    (progn
	      (if (char= char #\:)
		  (write-string "clm_" stream)
		(progn
		  (write-char #\# stream) ;# but not #:
		  (write-char char stream)))
	      (setf looking nil))
	  (write-char char stream))))))

(defun noopfun (x y z)
  (declare (ignore y z))
  x)

#+cltl2
(defun without-extension (name)
  (let ((ext (pathname-type name)))
    (subseq name 0 (- (length name) (1+ (length ext))))))

(defvar *c-flags* nil)
(setf *c-flags*
  (concatenate 'string
	       #-(or clisp (and openmcl (not linuxppc-target))) " -c"
	       " -g"
	       #-windoze " -O2" ;; this avoids a bizarre segfault in expsrc/Linux/ACL???
                " -I."   ;; [mus-config.h] clm.h cmus.h sndlib.h
 	       " -I"     ;;   needs also the actual source dir in case user is compiling elsewhere (mus-config.h also now)
 	       *clm-source-directory*
	       " -I"     ;; and mus-config.h...
	       *clm-binary-directory*
	       #+(and sgi (not acl-50)) " -Olimit 3000"
	       #+(and sgi acl-50) " -Olimit 3000 -n32 -w"
	       #+windoze " -O1 -DMUS_WINDOZE"
	       #+(and windoze excl) " -DHAVE_ACL"
	       #+(and mac-osx (not openmcl)) " -framework CoreAudio"
	       #+(and openmcl (not linuxppc-target)) " -no-cpp-precomp"
	       #+(and excl macosx) " -dynamic -no-cpp-precomp"
	       #+(and excl linux debug) " -Wall"
	       #+(or clisp netbsd x86-64) " -fPIC"
	       #+(and excl freebsd) " -fPIC -DPIC"
	       ))

(defvar *libclm-pathname* (format nil "~Alibclm.~A" *clm-binary-directory* *so-ext*))

(defvar *ins-file-loading* nil)
#+(or clisp cmu) (defvar so-ctr 0)

;;; *definstrument-hook* can be set to a function that returns a
;;; form to include in the definstrument expansion. If the hook
;;; is already set at macroexpansion time then its result will
;;; be expanded and compiled with the instrument definition.
;;; Otherwise, if the hook is set at load time then its result
;;; will be evaluated when the ins is loaded. Otherwise the hook
;;; is nil and it has no effect.

(defvar *definstrument-hook* nil)

(defmacro definstrument (ins-name (&rest args) &body body &environment env)
  (let* ((*header-info* nil)
	 (*c-file-name* nil)
	 (*c-compiler-options* *c-flags*)
	 (*c-print-function* nil)
	 (*with-reflection* nil)
	 (name
	  (if (listp ins-name)
	      (apply
	       #'(lambda
		   (nam &key c-file c-include-file
			(c-options *c-flags*)
			print-function
			&allow-other-keys)
		   (prog1 nam
		     (setf *c-file-name* c-file)
		     (setf *header-info* c-include-file)
		     (setf *c-compiler-options* c-options)
		     (setf *c-print-function* print-function)))
	       ins-name)
	    ins-name))
	 (silly-name (gentemp (string name)))
	 (dependent-file nil)
	 (antecedent-file nil)
         ;; if the hook exists at macroexpansion time then
         ;; include its form in the compilation. otherwise
         ;; include a form that checks at load time.
         (hookform (if *definstrument-hook*
                     (funcall *definstrument-hook* name args)
                     `(eval-when (load eval)
                        (if *definstrument-hook*
                          (eval (funcall *definstrument-hook*
                                         ',name ',args))))))
         )

    #+clisp (let ((lib (get name :library))) ; if edit + recompile, need to unload old .so 
	      (when lib
		(ffi:close-foreign-library lib)
		(setf (get name :library) nil)))

    (setf *current-ins-args* args)
      (let* ((lsp-name (concatenate 'string "clm_" (string-downcase (lisp->c-name (symbol-name name)))))
	     ;; since *ins-file-loading* doesnt recompute cfile each time name must be reused.
	     (c-ff (if clm::*ins-file-loading* (intern lsp-name) (gentemp lsp-name)))
	     #+(or cmu sbcl excl clisp) (c-ff-name (symbol-name c-ff))
	     #+(or cmu sbcl) (c-ff-cmu (gentemp lsp-name))
	     (ins-file-name (or #+(and excl cltl2) (truename (or excl:*source-pathname* *load-pathname*))
				#+(and excl (not cltl2)) (truename excl:*source-pathname*)
				#+(or clisp cmu sbcl openmcl) (or *load-pathname* *compile-file-truename*) ;this is the CLtL2 name
				(error "oops -- I can't find ~A's lisp source file!" name)))
	     (c-file-name (or *c-file-name*
			      ;; try to find compile-time input file name so that the subsequent .c and .o files
			      ;; are written to the same directory.
			      (filename->string
			       (merge-pathnames
				(concatenate 'string
					     "clm_"
					     (lisp->c-name #-openmcl (symbol-name name) #+openmcl (string-downcase name))
					     ;; changed 27-Feb-03 because instrument name with ">" or presumably "&" can confuse Linux
					     ".c")
				ins-file-name))))
	     (l-file-name
	      (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2)) #-windoze ".o" #+windoze ".obj")
	      )
	     #+(or excl sbcl openmcl) (so-file-name (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2))  "." *so-ext*))
	     #+(or clisp cmu) (so-file-name (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2)) (format nil "_~D" so-ctr) "." *so-ext*))
	     #+cmu (cmucl-file-name
		    (let ((input-file (filename->string (pathname-name (or *load-pathname* *compile-file-truename*)))))
		      (filename->string
		       (merge-pathnames
			(concatenate 'string input-file ".cmucl")
			(or *load-pathname* *compile-file-truename*)))))
	     (ins-code-file (if clm::*ins-file-loading* (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2))  ".icl")))
	     ;; ^ this is for openmcl only, I think
	     )
	#+(or clisp cmu) (setf so-ctr (1+ so-ctr))
	;; multi-instrument files don't work in cmucl -- this is a cmucl bug.
	#+cmu
	(with-open-file (fil cmucl-file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
			(format fil "(load-foreign ~S)~%" so-file-name)
			;; how to get the compiler's output filename?
			(format fil "(load ~S)~%"
				(concatenate 'string
					     (or *clm-ins-directory* (directory-namestring so-file-name))
					     (filename->string
					      (pathname-name (or *load-pathname* *compile-file-truename*)))
					     "."
					     *clm-fasl-name*)))
	(let ((ins-code nil))
          ;; create C file unless *ins-file-loading* is true and there is
          ;; already a c file and its write date is not later than the lisp file
          (if clm::*ins-file-loading*
	      (setf dependent-file c-file-name
		    antecedent-file #+openmcl *load-pathname* #-openmcl nil); *load-pathname* is not defined in older lisps, giving dumb warnings
            (setf dependent-file #-(or clisp openmcl) l-file-name #+(or clisp openmcl) so-file-name
                  antecedent-file c-file-name))
          (if (and clm::*ins-file-loading*
		   (probe-file dependent-file)
		   (probe-file antecedent-file)
		   (>= (file-write-date (truename dependent-file))
		       (file-write-date (truename antecedent-file))))
	      (with-open-file (fil ins-code-file :direction :input)
			      (setf ins-code (read fil)))
	    (unwind-protect
		(progn
		  (setf *c-file* (open c-file-name :direction :output :if-exists :supersede :if-does-not-exist :create))
		  (princ (format nil "; Writing ~S~%" c-file-name))
		  
		  (format *c-file* "/* translate ~A in ~A to C~% *   written ~A by CLM of ~A~% */~%~%"
			  (string-downcase name)
			  ins-file-name
			  (timestring)
			  *clm-date*)
		  (format *c-file* "#include <mus-config.h>~%~
                                    #include <stdio.h>~%~
                                    #include <stdlib.h>~%~
                                    #include <stdarg.h>~%~
                                    #include <math.h>~%")
		  (format *c-file* "#include <signal.h>~%")
		  #+sgi (format *c-file* "#include <sys/fpu.h>~%")
		  (format *c-file* "#include <cmus.h>~%")

		  (if *header-info* (format *c-file* "#include ~S~%" *header-info*))
		  (format *c-file* "~%")
		  (format *c-file* "static sig_atomic_t got_sigint = 0; /* catch C-C if hung */~%")
		  (format *c-file* "static void sig_err(int sig) {got_sigint = sig;}~%")
		  (format *c-file* "~%")

		  (setf *c-proc* c-ff)
		  (setf *lisp-proc* ins-name)
		  (setf ins-code (walk-form `(locally ,@body) env 'noopfun))
		  ;; the extra lambda is needed by walk-form

		  )			;unwind-protect progn
	      (close *c-file*)
	      #+sbcl (setf *c-file* nil)
	      ))		;unwind-protect cleanup
	  `(progn
             ,hookform
             (eval-when (compile load eval)
	       (when (or (not (probe-file ,dependent-file))
			 (not (probe-file ,antecedent-file))
 			 (> (file-write-date (truename ,antecedent-file)) (file-write-date (truename ,dependent-file)))
			 (and clm::*ins-file-loading*
			      (or (not (probe-file ,l-file-name))
				  (not (probe-file ,c-file-name))
				  (> (file-write-date (truename ,c-file-name)) (file-write-date (truename ,l-file-name))))))

		 ;;; ---------------------------------------- COMPILE ----------------------------------------
		 
		 (princ (format nil "; Compiling ~S~%" ,c-file-name))

		 ;;; ---------------- EXCL-WINDOWS
		 #+(and excl windoze)
		 (clm::run-in-shell ,*clm-compiler-name*
				    (format nil " ~A ~A -Fo~A" ,c-file-name ,*c-compiler-options* ,l-file-name))

		 ;;; ---------------- EXCL-OSX
		 #+(and excl macosx)
		 (progn
		   (clm::run-in-shell ,*clm-compiler-name*
				      (format nil " ~A ~A -c -o ~A~%" ,c-file-name ,*c-compiler-options* ,l-file-name))
		   (format t "~A ~A ~A -o ~A~%" ,*clm-compiler-name* ,c-file-name ,*c-compiler-options* ,l-file-name))

		 ;;; ---------------- OPENMCL
		 #+(and openmcl (not linuxppc-target))
		 (clm::run-in-shell "cc" (format nil " -bundle ~A -o ~A ~A -lclm -L~A"
						 ,c-file-name ,so-file-name ,*c-compiler-options* *clm-binary-directory*))

		 ;;; ---------------- CLISP
		 ;;; clisp inserts unrequested cr's!! can't use format or run-in-shell here
		 #+clisp 
		 (ext::shell (concatenate 'string ,*clm-compiler-name* " " ,c-file-name " " ,*c-compiler-options* 
					  " -shared -o " ,so-file-name " -L" *clm-binary-directory* " " *libclm-pathname* (string #\Newline)))

		 ;;; ---------------- NOT OPENMCL/MCL/EXCL-WINDOWS/OSX/CLISP
		 #-(or clisp (and excl (or windoze macosx)) (and openmcl (not linuxppc-target)))
		 (clm::run-in-shell ,*clm-compiler-name*
				    (format nil " ~A ~A -o ~A~%" ,c-file-name ,*c-compiler-options* ,l-file-name))



		 ;;; ---------------------------------------- LOAD ----------------------------------------
		 
		 #+(and (or excl cmu sbcl (and openmcl linuxppc-target)))
		 (princ (format nil "; Creating shared object file ~S~%" ,so-file-name))

		 ;;; ---------------- SGI (all)
		 #+(and (or excl cmu sbcl) sgi)
		 (clm::run-in-shell "ld" (format nil "-shared -all ~A -o ~A ~A -L~A -lclm -lm -lc~%"
						 ,so-file-name ,l-file-name *clm-binary-directory*))
		 ;; what about -laudio?

		 ;;; ---------------- SUN (not acl 7)
		 #+(and (or excl cmu sbcl) (not acl-70) sun)
		 (clm::run-in-shell "ld" (format nil "-G -o ~A ~A -L~A -lclm -lm -lc~%"
						 ,so-file-name ,l-file-name *clm-binary-directory*))

		 ;;; ---------------- SUN (acl 7)
		 #+(and acl-70 sun)
		 (clm::run-in-shell "ld" (format nil "-G -o ~A ~A -L~A ~A -lm -lc~%"
                                                ,so-file-name ,l-file-name *clm-binary-directory* *libclm-pathname*))

		;;; ---------------- LINUX (all except clisp)
		 #+(and (or excl cmu sbcl) linux (not freebsd))
		 (clm::run-in-shell "ld" (format nil "-shared -whole-archive -o ~A ~A ~A ~A~%"
						 ,so-file-name ,l-file-name *libclm-pathname*
						 "-lm"))

		 ;;; ---------------- OSX (acl 7)
		 #+(and acl-70 macosx)
		 (clm::run-in-shell "ld"
		    (format nil "-bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o ~A ~A -lm -lc -lcc_dynamic -framework CoreAudio "
			    ,so-file-name ,l-file-name))

		 ;;; ---------------- OSX CMUCL/SBCL
		 #+(and mac-osx (or cmu sbcl))
		 (clm::run-in-shell "gcc"
		   (format nil "-o ~A ~A ~A -dynamiclib" ,so-file-name ,l-file-name *libclm-pathname*))

		 ;;; ---------------- PPC (openmcl)
		 #+(and openmcl linuxppc-target) 
		 (clm::run-in-shell "ld" (format nil "-shared -whole-archive -o ~A ~A ~A~%"
						 ,so-file-name ,l-file-name *libclm-pathname*))

		 ;;; ---------------- FREEBSD
 		 #+(and cmu freebsd)
 		 (clm::run-in-shell "ld" (format nil "-r -L/usr/lib -o ~A ~A -L~A -lm~%"
 		 ,so-file-name ,l-file-name *clm-binary-directory*))
		 
		 ;; this puts the absolute location of libclm.so in the ins .so file -- another way to solve
		 ;; this problem would be to ask the user to add the clm directory to his LD_LIBRARY_PATH
		 ;; environment variable: (.cshrc): setenv LD_LIBRARY_PATH /usr/lib:/lib:/user/b/bil/linux/clm

		 #+(and excl freebsd)
		 (clm::run-in-shell "ld" (format nil "-Bshareable -Bdynamic -o ~A ~A ~A ~A~%"
						 ,so-file-name ,l-file-name *libclm-pathname*
						 "-lm"))
		 

		 ;;; ---------------- WINDOWS
		 #+(and excl windoze)
		 (clm::run-in-shell "cl" (concatenate 'string " -D_MT -MD -nologo -LD -Zi -W3 -Fe"
						      ,so-file-name " "
						      ,l-file-name " "
						      *clm-binary-directory* "libclm.lib"
						      ))

		 ))

	     #+excl (without-warnings (load ,so-file-name))
	     #+openmcl (ccl:open-shared-library ,so-file-name)
	     #+sbcl (sb-alien:load-shared-object ,so-file-name)

	     #+(and acl-50 (not acl-70))
	     (ff:def-foreign-call (,c-ff ,c-ff-name)
				  ((datar (* :float) array) (len :int) (datai (* :int) array) (ilen :int))
				  :returning :int)
	     
	     #+acl-70
	     (ff:def-foreign-call (,c-ff ,c-ff-name)
				  ((datar (:array :double)) (len :int) (datai (:array :int)) (ilen :int))
				  :returning :int)

	     #+cmu
	     (alien:def-alien-routine (,c-ff-name ,c-ff-cmu) c-call:int
				      (datar (* double-float)) (len c-call:int) (datai (* c-call:int)) (ilen c-call:int))
	     
	     #+sbcl
	     (sb-alien:define-alien-routine (,c-ff-name ,c-ff-cmu) sb-alien:int
				      (datar (* double-float))
				      (len sb-alien:int) (datai (* sb-alien:int)) (ilen sb-alien:int))
	     
	     #+cmu
	     (defun ,c-ff (c &optional d e f)
	       (,c-ff-cmu (array-data-address c) d (array-data-address e) f))

	     #+sbcl
	     (defun ,c-ff (c &optional d e f)
	       (,c-ff-cmu (array-data-address c) d (array-data-address e) f))

	     #+clisp (ffi:def-call-out ,c-ff 
				       (:name ,c-ff-name) 
				       (:library (expand-filename->string ,so-file-name)) 
				       (:language :stdc)
				       (:return-type ffi:int)
				       (:arguments (datar (ffi:c-array-ptr ffi:double-float))
						   (len ffi:int)
						   (datai (ffi:c-array-ptr ffi:int))
						   (ilen ffi:int)))

	     #+openmcl
	     ;; this (openmcl instrument linkage) probably needs the -to-c-and-lisp linkages for run*
	     ;;   I can't remember now what that ^ comment refers to!
	     (defun ,c-ff (c &optional d e f)
	       (ccl::external-call ,(concatenate 'string "_" (string c-ff)) :address c
				   :signed d :address e :signed f :signed))

	     (eval-when (eval load) (pushnew ',name *clm-instruments*))
	     (defun ,name ,args
	       (setf *clm-ins* ',name)
	       (if (or (not *notehook*)
		       (not (eq (funcall *notehook*
					 (symbol-name ',name)
					 ,@(let ((defargs (set-difference args lambda-list-keywords)))
					     (nreverse (loop for arg in defargs collect (if (symbolp arg) arg (first arg))))))
				:done)))
		   (if (zerop clm::*interrupted*)
		       (let ((val nil))
			 (tagbody
			  (restart-case
			   (setf val (progn ,ins-code))
			   (nil ()
				:report "abort current note."
				(go C-INSTRUMENT-DONE)))
			  C-INSTRUMENT-DONE)
			 val))))
	     (defun ,silly-name ()
	       (pushnew ',name *clm-instruments*)
	       #-clisp (set-instrument-properties ',name ,c-file-name ,*c-print-function*)
	       #+clisp (set-instrument-properties ',name ,c-file-name ,*c-print-function* (expand-filename->string ,so-file-name))
	       )
	     (eval-when (eval load) (,silly-name)))
          ))))


(defun clm-initialize-links ()
  (when (not *clm-linked*)
    (mus-sound-initialize)
    (initialize-cmus)
    )
  #-windoze (if (probe-file "/etc/clm.conf") (load "/etc/clm.conf"))
  (setf *clm-linked* t))

(defvar clm-cleanup-functions nil)

(defun cleanup-clm ()
  #+excl (when *dac-pid* (loop while (numberp (system:os-wait))))
  (when clm-cleanup-functions
    (loop for func in clm-cleanup-functions do (funcall func))))

#+excl (eval-when (compile load eval) (excl:advise excl:exit :before clm-exit-advice nil (cleanup-clm)))
#+cmu (defun bye () (common-lisp::quit)) ;who can remember which lisp uses which!?!?!?
#+cmu (defun exit () (common-lisp::quit))
#+sbcl (defun bye () (sb-ext:quit))
#+sbcl (defun exit () (sb-ext:quit))
#+excl (defun bye () (excl:exit))
#+excl (defun quit () (excl:exit))
#+openmcl (defun bye () (ccl::quit))
#+openmcl (defun exit () (ccl::quit))

#+(or cmu sbcl openmcl excl)
(defun restart-clm ()
  #+cmu (load-foreign *libclm-pathname*)
  #+sbcl (sb-alien:load-shared-object *libclm-pathname*)
  (setf *clm-linked* nil)
  (reset-headers)
  (reset-audio)
  (reset-io)
  (clm-initialize-links))


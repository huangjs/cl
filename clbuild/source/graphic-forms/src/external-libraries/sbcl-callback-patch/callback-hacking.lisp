;;;;
;;;; hacking.lisp
;;;;
;;;; Compiler and runtime damage for callbacks
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(sb-ext:without-package-locks
 (defun alien-callback-assembler-wrapper (index return-type arg-types &optional (stack-offset 0))
   "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
   (declare (ignore arg-types))
   (let* ((segment (make-segment))
	  (eax eax-tn)
	  (edx edx-tn)
	  (ebp ebp-tn)
	  (esp esp-tn)
	  ([ebp-8] (make-ea :dword :base ebp :disp -8))
	  ([ebp-4] (make-ea :dword :base ebp :disp -4)))
     (assemble (segment)
	       (inst push ebp)		; save old frame pointer
	       (inst mov  ebp esp)	; establish new frame
	       (inst mov  eax esp)	;
	       (inst sub  eax 8)	; place for result
	       (inst push eax)		; arg2
	       (inst add  eax 16)	; arguments
	       (inst push eax)		; arg1
	       (inst push (ash index 2)) ; arg0
	       (inst push (get-lisp-obj-address #'enter-alien-callback)) ; function
	       (inst mov  eax (foreign-symbol-address "funcall3"))
	       (inst call eax)
	       ;; now put the result into the right register
	       (cond
		 ((and (alien-integer-type-p return-type)
		       (eql (alien-type-bits return-type) 64))
		  (inst mov eax [ebp-8])
		  (inst mov edx [ebp-4]))
		 ((or (alien-integer-type-p return-type)
		      (alien-pointer-type-p return-type)
		      (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
				    return-type))
		  (inst mov eax [ebp-8]))
		 ((alien-single-float-type-p return-type)
		  (inst fld  [ebp-8]))
		 ((alien-double-float-type-p return-type)
		  (inst fldd [ebp-8]))
		 ((alien-void-type-p return-type))
		 (t
		  (error "unrecognized alien type: ~A" return-type)))
	       (inst mov esp ebp)	; discard frame
	       (inst pop ebp)		; restore frame pointer
	       (inst ret stack-offset))
     (finalize-segment segment)
     ;; Now that the segment is done, convert it to a static
     ;; vector we can point foreign code to.
     (let ((buffer (sb-assem::segment-buffer segment)))
       (make-static-vector (length buffer)
			   :element-type '(unsigned-byte 8)
			   :initial-contents buffer)))))

(in-package "SB-ALIEN")

(defun %alien-callback-sap (specifier result-type argument-types function wrapper &optional (call-type :cdecl))
  (let ((key (list specifier function call-type)))
    (or (gethash key *alien-callbacks*)
        (setf (gethash key *alien-callbacks*)
              (let* ((index (fill-pointer *alien-callback-trampolines*))
                     ;; Aside from the INDEX this is known at
                     ;; compile-time, which could be utilized by
                     ;; having the two-stage assembler tramp &
                     ;; wrapper mentioned in [1] above: only the
                     ;; per-function tramp would need assembler at
                     ;; runtime. Possibly we could even pregenerate
                     ;; the code and just patch the index in later.
                     (assembler-wrapper (alien-callback-assembler-wrapper
                                         index result-type argument-types
					 (if (eq call-type :stdcall)
					     (* 4 (length argument-types))
					     0))))
                (vector-push-extend
                 (alien-callback-lisp-trampoline wrapper function)
                 *alien-callback-trampolines*)
                (let ((sap (vector-sap assembler-wrapper)))
                  (push (cons sap (make-callback-info :specifier specifier
                                                      :function function
                                                      :wrapper wrapper
                                                      :index index))
                        *alien-callback-info*)
                  sap))))))

(sb-ext:without-package-locks
 (defmacro alien-callback (specifier function &optional (call-type :cdecl) &environment env)
   "Returns an alien-value with of alien ftype SPECIFIER, that can be passed to
an alien function as a pointer to the FUNCTION. If a callback for the given
SPECIFIER and FUNCTION already exists, it is returned instead of consing a new
one."
   ;; Pull out as much work as is convenient to macro-expansion time, specifically
   ;; everything that can be done given just the SPECIFIER and ENV.
   (multiple-value-bind (result-type argument-types) (parse-alien-ftype specifier env)
     `(%sap-alien
       (%alien-callback-sap ',specifier ',result-type ',argument-types
	,function
	(or (gethash ',specifier *alien-callback-wrappers*)
	 (setf (gethash ',specifier *alien-callback-wrappers*)
	       ,(alien-callback-lisp-wrapper-lambda
		 specifier result-type argument-types env))) ,call-type)
       ',(parse-alien-type specifier env)))))

#|
(sb-alien::alien-callback (function int int int) #'+ :stdcall)
 => #<SB-ALIEN-INTERNALS:ALIEN-VAUE :SAP ... :TYPE ...>
(alien-funcall-stdcall * 3 4) => 9
"Hey everybody, callbacks work!"
|#

;;; EOF

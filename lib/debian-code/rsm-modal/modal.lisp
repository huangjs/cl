;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modal.lisp
;;;; Purpose:       Modal Logic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: modal.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.modal)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(defstruct modal
  name                                  ; The name of the modal system
  K                                     ; K(agent world) -> worlds
  props                                 ; props(world prop) -> {true | false}
  primitive-truth-function              ; Computes default truth for a 
                                        ; proposition in a world.
  worlds)                               ; All the possible worlds.


(defvar *modal-system* 
    nil
  "The current modal system instance."
  )

(defvar *modal-hash* 
    (make-hash-table :test #'equalp)
  "Stores modal systems by name.")

(defconstant +primitive-truth-function+
    #'(lambda (world prop)
        (declare (ignore world prop))
        nil)
  "The default function to compute the default value of 
a proposition in a given world."
  )


(defun get-worlds (agent world &key (modal-system *modal-system*))
  "Return the worlds that agent <agent> considers possible in 
world <world>."
  (gethash (cons agent world) (modal-K modal-system)))

(defun get-primitive-truth (world prop &key (modal-system *modal-system*))
  "Get the truth value of the proposition <prop> in world <world> in 
the modal system <modal-system>."
  (multiple-value-bind (truth ok)
      (gethash (cons world prop) (modal-props modal-system))
    (if ok
        truth
      (funcall (modal-primitive-truth-function modal-system) world prop))))


(defun is-primitive-true? (world prop &key (modal-system *modal-system*))
  "Is the primitive proposition <prop> true in world <world> in 
modal system <modal-system>?"
  (cond ((eql prop t) t)
        ((eql prop nil) nil)
        (t
         (get-primitive-truth world prop :modal-system modal-system))))

(defun primitive-prop? (prop)
  "Is <prop>, a primitive proposition?"
  (cl:or (eql prop t)
         (eql prop nil)
         (stringp prop)))

(defun and (&rest args)
  "Function that behaves like the cl macro/special-form and."
  (if args
      (progn
        (loop :for arg :in args :do
              (unless arg
                (return-from and nil)))
        t)
    t))

(defun or (&rest args)
  "Function that behaves like the cl macro/special-form or."
  (if args
      (progn
        (loop :for arg :in args :do
              (when arg
                (return-from or t)))
        nil)
    nil))

(defun xor (&rest args)
  "The exclusive or function."
  (if args
      (let ((last (car args)))
        (loop :for arg :in (cdr args) :do
              (setf last (cl:not (eql last arg))))
        last)      
    nil))

(defun not (&rest args)
  "Function that behaves like the cl macro/special-form not."
  (cl:not (car args)))

(defun op-num (op)
  "Return the number part of the operator.
Example: (op-num 'k1) 
          1."
  (let ((name (symbol-name op)))
    (parse-integer (subseq name (position-if #'digit-char-p name)))))

(defun get-op-func (op-sym world &key (modal-system *modal-system*))
  "Return the appropriate function for the specified symbol, <op-sym>.
Currently, the symbols are 'and, 'or, 'xor, 'not - which represent the 
usual logic functions; as well as symbols of the form:
kN prop - agent N knows a proposition prop to be true (N an integer)
pN prop - agent N believes proposition prop is possible
dN prop - agent N does not know whether proposition pro is true or not."
  (if (symbolp op-sym)
      (let ((name (symbol-name op-sym)))
        (cond 
         
         ;; Handle the primitive functions and, or, xor, not."
         ((string-equal name "and") 
          (values #'and t))
         ((string-equal name "or") 
          (values #'or t))
         ((string-equal name "xor") 
          (values #'xor t))
         ((string-equal name "not") 
          (values #'not t))
         
         ;; Handle the knowledge operators:
         ;; kN (agent N knows proposition); 
         ;; pN (agent N believes proposition is possible) ;
         ;; dN (agent N does not know whether proposition is true or not.)
         ((string-equal (subseq name 0 1) "k")
          (values 
           #'(lambda (prop)
               (agent-knows (op-num op-sym) world prop 
                            :modal-system modal-system))
           nil))
         ((string-equal (subseq name 0 1) "p")
          (values 
           #'(lambda (prop)
               (not 
                (agent-knows (op-num op-sym) world (list 'not prop)
                             :modal-system modal-system)))
           nil))
         ((string-equal (subseq name 0 1) "d")
          (values 
           #'(lambda (prop)
               (let ((op-num (op-num op-sym)))
                 (and
                  (not 
                   (agent-knows op-num world prop
                                :modal-system modal-system))
                  (not 
                   (agent-knows op-num world (list 'not prop)
                                :modal-system modal-system))))
               nil)))
         
         ;; Throw error if bad op.
         (t
          (error "get-op-func: Bad operator ~s~%" op-sym))))
    
    ;; The first arg is bad.
    (error "get-op-func: First arg, ~s is not a valid operator.~%" op-sym)))


(defun is-true? (world prop &key (modal-system *modal-system*))
  "Is proposition <prop> true in world <world> in 
modal system <modal-system>?"
  (if (primitive-prop? prop)
      (is-primitive-true? world prop :modal-system modal-system)
    (multiple-value-bind (op ordinary-op?)
        (get-op-func (car prop) world :modal-system modal-system)
      (if ordinary-op?
          (apply op (mapcar #'(lambda (prop)
                                (is-true? world prop 
                                          :modal-system modal-system))
                            (cdr prop)))
        (apply op (cdr prop))))))


(defun agent-knows (agent world prop &key (modal-system *modal-system*))
  "For all the worlds that agent <agent> regards as possible while in world
<world>, check that the proposition <prop> is true in the modal system
<modal-system>."
  (let ((worlds (get-worlds agent world :modal-system modal-system)))
    (when worlds
      (loop :for world :in worlds :do
        (unless (is-true? world prop :modal-system modal-system)
          (return-from agent-knows nil)))
      t)))


(defun satisfies? (prop &key (modal-system *modal-system*))
  "Is proposition <prop> true in all worlds in modal system <modal-system>?"
  (let ((worlds (modal-worlds modal-system)))
    (loop :for world :in worlds :do
      (unless (is-true? world prop :modal-system modal-system)
        (return-from satisfies? nil)))
    t))

(defun make-world (name kripke-info prop-info 
                   &key (primitive-truth-function +primitive-truth-function+) )
  "Make a modal system from a form representing a kripke structure 
and a form representing the truth of primitive propositions in the 
in various worlds. There is also a keyword option that defines a default
truth function which is used when the <prop-info> does not define the 
logic of a particular world and proposition.
kripke-info has the form: '((agent world (worlds)) ...)
This means that agent <agent> in world <world> believes that the worlds in 
<worlds> are possible.
Example:
'((0 1 (1 2)) (0 2 (1 2)) (0 3 (3))
  (1 1 (1 3)) (1 2 (2 1)) (1 3 (3 2))))

prop-info has the form: '((world prop logical)...)
This means that in world <world> proposition <prop> has logical value <logical>.
Example:
 '((1 \"sky is white\" false) (2 \"sky is white\" true)
   (3 \"light is on\" true))"
  (let ((package *package*))
    (unwind-protect
        (progn
          (in-package rsm.modal)
          (let ((ms (make-modal :name name))
                (k-hash (make-hash-table :test #'equalp))
                (prop-hash (make-hash-table :test #'equalp))
                worlds)
            (setf (modal-K ms) k-hash)
            (setf (modal-props ms) prop-hash)
            (setf (modal-primitive-truth-function ms) primitive-truth-function)
            (loop :for inf :in kripke-info :do
              (setf (gethash (cons (car inf) (cadr inf)) k-hash) (caddr inf)))
            (loop :for inf :in prop-info :do
              (setf (gethash (cons (car inf) (cadr inf)) prop-hash) (caddr inf))
              (pushnew (car inf) worlds))
            (setf (modal-worlds ms) worlds)
            (setf (gethash name *modal-hash*) ms)
            (setf *modal-system* ms)
            ms))
      (setf *package* package))))


(defmacro defmodal (sym 
                    &key 
                    kripke props 
                    (primitive-truth-function +primitive-truth-function+))
  "Produces a modal logic system based on a kripke structure
and a basic proposition structure. The kripke form is 
((agent world (worlds))...)
This means that agent <agent> in world <world> believes that the worlds in 
<worlds> are possible.

The props form is: 
((world proposition truth-value)...).
This means that in world <world> proposition <prop> has logical value <logical>.

Note: After defmodal has been executed, 
the supporting logic functions will use this modal system as the basis 
of all logic queries. Change the modal system that the query functions work
on by using the macro set-modal-system.
Example: (rsm.modal:defmodal my-world 
             :kripke 
           ((0 1 (1 2)) (0 2 (1 2)) (0 3 (3))
                        (1 1 (1 3)) (1 2 (2 1)) (1 3 (3 2)))
           :props
           ((1 \"sky is white\" t) (2 \"sky is white\" t)
                                   (3 \"light is on\" t)))"
  `(progn
     (unless ,(symbolp sym)
       (error "First argument must be a symbol."))
     (unless ',kripke
       (error "defmodal: Must specify a kripke structure."))
     (unless ',props
       (error "defmodal: Must specify a props structure."))
     (when ,primitive-truth-function
       (unless (functionp ,primitive-truth-function)
         (error "defmodal: primitive-truth-function must be a function.")))
     (make-world ,(symbol-name sym) ',kripke ',props 
                 :primitive-truth-function ,primitive-truth-function)))

(defun clear-modal-systems ()
  "Clear the modal systems hash table."
  (clrhash *modal-hash*))

(defmacro get-modal-system (sym &optional (error nil))
  "Gets the current modal system represented by the symbol, <sym>.
Throw an error if <error> is true and the modal system is not found;
otherwise, return nil."
  `(let ((modal (gethash ,(symbol-name sym) *modal-hash*)))
     (if modal
         modal
       (when ,error
         (error "set-modal-system: Unknown modal system ~s~%" 
                ,(symbol-name sym))))))

(defmacro set-modal-system (sym &optional (error t))
  "Set the current modal system to the one represented by the symbol, <sym>.
Throw an error if <error> is true and the modal system is not found;
otherwise, leave the current modal system as it is and return nil."
  `(let ((modal (gethash ,(symbol-name sym) *modal-hash*)))
     (if modal
         (setf *modal-system* modal)
       (when ,error
         (error "set-modal-system: Unknown modal system ~s~%" 
                ,(symbol-name sym))))))

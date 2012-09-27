;;; this is a stupid implementation of the flexichain and flexicursor
;;; protocols.  The idea is to use this as a reference implementation
;;; and test the other one by generating random operations issued to
;;; both implementations and compare the result.  

(defpackage :stupid
  (:use :common-lisp)
  (:export #:flexichain #:standard-flexichain
           #:flexi-error #:flexi-initialization-error
           #:flexi-position-error #:flexi-incompatible-type-error
           #:nb-elements #:flexi-empty-p
           #:insert* #:element* #:delete*
           #:push-start #:pop-start #:push-end #:pop-end #:rotate
           #:cursorchain #:standard-cursorchain 
           #:flexicursor #:standard-flexicursor
           #:left-sticky-flexicursor #:right-sticky-flexicursor
           #:chain
           #:clone-cursor #:cursor-pos
           #:at-beginning-error #:at-end-error
           #:at-beginning-p #:at-end-p
           #:move> #:move<
           #:insert #:insert-sequence
           #:element< #:element> #:delete< #:delete>))

(in-package :stupid)

(defclass flexichain () ()
  (:documentation "The protocol class for flexichains."))

(define-condition flexi-error (simple-error)
  ())

(define-condition flexi-initialization-error (flexi-error)
  ((cause :reader flexi-initialization-error-cause
          :initarg :cause :initform ""))
  (:report (lambda (condition stream)
             (format stream "Error initializing FLEXICHAIN (~S)"
                     (flexi-initialization-error-cause condition)))))

(define-condition flexi-position-error (flexi-error)
  ((chain :reader flexi-position-error-chain
          :initarg :chain :initform nil)
   (position :reader flexi-position-error-position
             :initarg :position :initform nil))
  (:report (lambda (condition stream)
             (format stream "Position ~D out of bounds in ~A"
                     (flexi-position-error-position condition)
                     (flexi-position-error-chain condition)))))

(define-condition flexi-incompatible-type-error (flexi-error)
  ((chain :reader flexi-incompatible-type-error-chain
          :initarg :chain :initform nil)
   (element :reader flexi-incompatible-type-error-element
            :initarg :element :initform nil))
  (:report (lambda (condition stream)
             (let ((element (flexi-incompatible-type-error-element
                             condition)))
               (format stream "Element ~A of type ~A cannot be inserted in ~A"
                       element
                       (type-of element)
                       (flexi-incompatible-type-error-chain condition))))))

(defgeneric nb-elements (chain)
  (:documentation "Returns the number of elements in the flexichain."))

(defgeneric flexi-empty-p (chain)
  (:documentation "Checks whether CHAIN is empty or not."))

(defgeneric insert* (chain position object)
  (:documentation "Inserts an object before the element at POSITION
in the chain. If POSITION is out of range (less than 0 or greater
than the length of CHAIN, the FLEXI-POSITION-ERROR condition will be
signaled."))

(defgeneric delete* (chain position)
  (:documentation "Deletes an element at POSITION of the chain.
If POSITION is out of range (less than 0 or greater than or equal
to the length of CHAIN, the FLEXI-POSITION-ERROR condition
will be signaled."))

(defgeneric element* (chain position)
  (:documentation "Returns the element at POSITION of the chain.
If POSITION is out of range (less than 0 or greater than or equal
to the length of CHAIN, the FLEXI-POSITION-ERROR condition
will be signaled."))

(defgeneric (setf element*) (object chain position)
  (:documentation "Replaces the element at POSITION of CHAIN by OBJECT.
If POSITION if out of range (less than 0 or greater than or equal to
the length of CHAIN, the FLEXI-POSITION-ERROR condition will be signaled."))

(defgeneric push-start (chain object)
  (:documentation "Inserts an object at the beginning of CHAIN."))

(defgeneric push-end (chain object)
  (:documentation "Inserts an object at the end of CHAIN."))

(defgeneric pop-start (chain)
  (:documentation "Pops and returns the element at the beginning of CHAIN."))

(defgeneric pop-end (chain)
  (:documentation "Pops and returns the element at the end of CHAIN."))

(defgeneric rotate (chain &optional n)
  (:documentation "Rotates the elements of CHAIN so that the element
that used to be at position N is now at position 0. With a negative
value of N, rotates the elements so that the element that used to be
at position 0 is now at position N."))

(defclass standard-flexichain (flexichain)
  ((elements :initform (list '()) :accessor elements)))

(defmethod nb-elements ((chain standard-flexichain))
  (/ (1- (length (elements chain))) 2))

(defmethod flexi-empty-p ((chain standard-flexichain))
  (zerop (nb-elements chain)))

(defmethod insert* ((chain standard-flexichain) position object)
  (assert (<= 0 position (nb-elements chain)) ()
          'flexi-position-error :chain chain :position position)
  (let* ((remainder (nthcdr (* 2 position) (elements chain))))
    (push (remove-if-not (lambda (x) (typep x 'right-sticky-flexicursor)) (car remainder))
          (cdr remainder))
    (push object (cdr remainder))
    (setf (car remainder)
          (remove-if (lambda (x) (typep x 'right-sticky-flexicursor)) (car remainder)))))

(defmethod delete* ((chain standard-flexichain) position)
  (assert (< -1 position (nb-elements chain)) ()
          'flexi-position-error :chain chain :position position)
  (let* ((remainder (nthcdr (* 2 position) (elements chain))))
    (pop (cdr remainder))
    (setf (car remainder) (append (cadr remainder) (car remainder)))
    (pop (cdr remainder))))

(defmethod element* ((chain standard-flexichain) position)
  (assert (< -1 position (nb-elements chain)) ()
          'flexi-position-error :chain chain :position position)
  (nth (1+ (* 2 position)) (elements chain)))

(defmethod (setf element*) (object (chain standard-flexichain) position)
  (assert (< -1 position (nb-elements chain)) ()
          'flexi-position-error :chain chain :position position)
  (setf (nth (1+ (* 2 position)) (elements chain)) object))

(defmethod push-start ((chain standard-flexichain) object)
  (insert* chain 0 object))

(defmethod push-end ((chain standard-flexichain) object)
  (insert* chain (nb-elements chain) object))

(defmethod pop-start ((chain standard-flexichain))
  (prog1 (element* chain 0)
         (delete* chain 0)))

(defmethod pop-end ((chain standard-flexichain))
  (let ((position (1- (nb-elements chain))))
    (prog1 (element* chain position)
           (delete* chain position))))

(defmethod rotate ((chain standard-flexichain) &optional (n 1))
  (when (> (nb-elements chain) 1)
    (cond ((plusp n) (loop repeat n do (push-start chain (pop-end chain))))
          ((minusp n) (loop repeat (- n) do (push-end chain (pop-start chain))))
          (t nil))))

(defclass cursorchain (flexichain)
  ()
  (:documentation "The protocol class for cursor chains."))

(defclass flexicursor ()
  ()
  (:documentation "The protocol class for flexicursors."))

(define-condition at-beginning-error (flexi-error)
  ((cursor :reader at-beginning-error-cursor
           :initarg :cursor :initform nil))
  (:report (lambda (condition stream)
             (let ((cursor (at-beginning-error-cursor condition)))
               (format stream "Cursor ~A already at the beginning of ~A"
                       cursor
                       (chain cursor))))))

(define-condition at-end-error (flexi-error)
  ((cursor :reader at-end-error-cursor
           :initarg :cursor :initform nil))
  (:report (lambda (condition stream)
             (let ((cursor (at-end-error-cursor condition)))
               (format stream "Cursor ~A already at the end of ~A"
                       cursor
                       (chain cursor))))))

(defgeneric clone-cursor (cursor)
  (:documentation "Creates a cursor that is initially at the same location
as the one given as argument."))

(defgeneric cursor-pos (cursor)
  (:documentation "Returns the position of the cursor."))

(defgeneric (setf cursor-pos) (posistion cursor)
  (:documentation "Set the position of the cursor."))

(defgeneric at-beginning-p (cursor)
  (:documentation "Returns true if the cursor is at the beginning
of the chain."))

(defgeneric at-end-p (cursor)
  (:documentation "Returns true if the cursor is at the beginning
of the chain."))

(defgeneric move> (cursor &optional n)
  (:documentation "Moves the cursor forward N positions."))

(defgeneric move< (cursor &optional n)
  (:documentation "Moves the cursor backward N positions."))

(defgeneric insert (cursor object)
  (:documentation "Inserts an object at the cursor."))

(defgeneric insert-sequence (cursor sequence)
  (:documentation "The effect is the same as if each element of the
sequence was inserted using INSERT."))

(defgeneric delete< (cursor &optional n)
  (:documentation "Deletes N objects before the cursor."))

(defgeneric delete> (cursor &optional n)
  (:documentation "Deletes N objects after the cursor."))

(defgeneric element< (cursor)
  (:documentation "Returns the element immediately before the cursor."))

(defgeneric (setf element<) (object cursor)
  (:documentation "Replaces the element immediately before the cursor."))

(defgeneric element> (cursor)
  (:documentation "Returns the element immediately after the cursor."))

(defgeneric (setf element>) (object cursor)
  (:documentation "Replaces the element immediately after the cursor."))

(defclass standard-cursorchain (cursorchain standard-flexichain)
  ()
  (:documentation "The standard instantiable subclass of CURSORCHAIN"))

(defclass standard-flexicursor (flexicursor)
  ((chain :reader chain :initarg :chain))
  (:documentation "The standard instantiable subclass of FLEXICURSOR"))

(defmethod initialize-instance :after ((cursor standard-flexicursor)
                                       &rest args &key (position 0)) 
  (declare (ignore args))
  (push cursor (car (nthcdr (* 2 position) (elements (chain cursor))))))

(defclass left-sticky-flexicursor (standard-flexicursor) ())

(defclass right-sticky-flexicursor (standard-flexicursor) ())

(defmethod cursor-pos ((cursor standard-flexicursor))
  (loop for sublist on (elements (chain cursor)) by #'cddr
        for pos from 0
        when (member cursor (car sublist) :test #'eq)
          do (return pos)))

(defun sublist-of-cursor (cursor)
  (nthcdr (* 2 (cursor-pos cursor)) (elements (chain cursor))))

(defmethod clone-cursor ((cursor standard-flexicursor))
  (make-instance (class-of cursor)
     :chain (chain cursor)
     :position (cursor-pos cursor)))

(defmethod (setf cursor-pos) (position (cursor standard-flexicursor))
  (assert (<= 0 position (nb-elements (chain cursor))) ()
          'flexi-position-error :chain (chain cursor) :position position)
  (let ((sublist1 (sublist-of-cursor cursor))
        (sublist2 (nthcdr (* 2 position) (elements (chain cursor)))))
    (setf (car sublist1) (remove cursor (car sublist1) :test #'eq))
    (push cursor (car sublist2))))

(defmethod at-beginning-p ((cursor standard-flexicursor))
  (zerop (cursor-pos cursor)))

(defmethod at-end-p ((cursor standard-flexicursor))
  (= (cursor-pos cursor) (nb-elements (chain cursor))))

(defmethod move> ((cursor standard-flexicursor) &optional (n 1))
  (incf (cursor-pos cursor) n))

(defmethod move< ((cursor standard-flexicursor) &optional (n 1))
  (decf (cursor-pos cursor) n))

(defmethod insert ((cursor standard-flexicursor) object)
  (insert* (chain cursor) (cursor-pos cursor) object))

(defmethod insert-sequence ((cursor standard-flexicursor) sequence)
  (map nil
       (lambda (object)
         (insert cursor object))
       sequence))

(defmethod delete> ((cursor standard-flexicursor) &optional (n 1))
  (let ((chain (chain cursor))
        (position (cursor-pos cursor)))
    (assert (plusp n) ()
            'flexi-position-error :chain chain :position n)
    (loop repeat n
          do (delete* chain position))))

(defmethod delete< ((cursor standard-flexicursor) &optional (n 1))
  (let ((chain (chain cursor))
        (position (cursor-pos cursor)))
    (assert (plusp n) ()
            'flexi-position-error :chain chain :position n)
    (loop repeat n
          do (delete* chain (- position n)))))

(defmethod element> ((cursor standard-flexicursor))
  (assert (not (at-end-p cursor)) ()
          'at-end-error :cursor cursor)
  (element* (chain cursor) (cursor-pos cursor)))

(defmethod (setf element>) (object (cursor standard-flexicursor))
  (assert (not (at-end-p cursor)) ()
          'at-end-error :cursor cursor)
  (setf (element* (chain cursor) (cursor-pos cursor))
        object))

(defmethod element< ((cursor standard-flexicursor))
  (assert (not (at-beginning-p cursor)) ()
          'at-beginning-error :cursor cursor)
  (element* (chain cursor) (1- (cursor-pos cursor))))

(defmethod (setf element<) (object (cursor standard-flexicursor))
  (assert (not (at-beginning-p cursor)) ()
          'at-beginning-error :cursor cursor)
  (setf (element* (chain cursor) (1- (cursor-pos cursor)))
        object))

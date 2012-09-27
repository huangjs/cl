(in-package #:EQUIV)

(defgeneric object-constituents (type)
  (:documentation "Returns list of accessors used to determine
equivalence of objects of type TYPE.")
  (:method ((type (eql 'cons)))
    (load-time-value (list #'car #'cdr)))
  (:method ((type (eql 'pathname)))
    (load-time-value
     (list #'pathname-directory
           #'pathname-name
           #'pathname-type
           #'pathname-version
           #'pathname-host
           #'pathname-device))))

(defgeneric object-frozenp (object)
  (:documentation "Indicates whether OBJECT is frozen.  That is,
this function may return true only if OBJECT will not be mutated
in an observable way from the point of the call until the end of
its life time, otherwise false.")
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((cons cons))
    (declare (ignore cons))
    nil)
  (:method ((string string))
    (declare (ignore string))
    nil)
  (:method ((vector vector))
    (declare (ignore vector))
    nil)
  (:method ((pathname pathname))
    (declare (ignore pathname))
    t)
  (:method ((number number))
    (declare (ignore number))
    t)
  (:method ((char character))
    (declare (ignore char))
    t))

(declaim (inline object-sequence= object-vector=))
(defun object-sequence= (xs ys)
  "Checks whether sequences XS and YS are element-wise equivalent,
by means of OBJECT=, and of the same length."
  (and (= (length xs) (length ys))
       (every #'object= xs ys)))

(defun object-vector= (xs ys)
  "Checks whether vectors XS and YS are element-wise equivalent,
by means of OBJECT=, and of the same length.  
Use OBJECT-SEQUENCE= instead."
  (object-sequence= xs ys))

(defun object= (x y &optional frozenp)
  "Returns true if X and Y are (observationally) equivalent.
Hence, OBJECT= is an equivalence relation:
1. (object= x x)
2. (equal (object= x y frozenp) (object= y x frozenp))
3. (implies (and (object= x y frozenp) (object= y z frozenp))
            (object= x z frozenp))

Frozen objects \(i.e., objects which are promised not to mutate)
are compared by recursing into their constituents, as specified by
OBJECT-CONSTITUENTS.  Mutable \(i.e., not frozen) objects are
compared with the pointer equality EQ.

FROZENP can be used to override the defaults for X and Y given by
OBJECT-FROZENP.  It is a promise that none of the objects X and Y
are referring to with their constituents, or **any of the
constituents' constituents** are mutated from the time of the
call to OBJECT= onwards.

If one lies with FROZENP, OBJECT-FROZENP, or OBJECT-CONSTITUENTS,
all bets are off and the result of OBJECT= is meaningless.

OBJECT= diverges if both X and Y are circular data structures.

See also: <http://home.pipeline.com/~hbaker1/ObjectIdentity.html>"
  (declare (optimize (speed 3)))
  (labels
      ((object=/rec (x y)
         (cond ((eq x y) t)
               ;; special cases
               ((stringp x) 
                (and (stringp y) frozenp (string= x y)))
               ((vectorp x)
                (and (vectorp y) frozenp (object-vector= x y)))
               ((bit-vector-p x)
                (and frozenp (equal x y)))
               ((not (equal (type-of x) (type-of y))) nil)
               ;; from here: objects which have the same type
               ;; from here: immutable objects
               ((numberp x) (= x y))
               ((characterp x) (char= x y))
               ((pathnamep x) (equal x y))
               ((not (or frozenp
                         (and (object-frozenp x)
                              (object-frozenp y)))) nil)
               ;; from here: only frozen mutable objects
               ((consp x)
                (loop
                 (cond ((not (object=/rec (pop x) (pop y)))
                        (return nil))
                       ((not (and (consp x) (consp y)))
                        (return (object=/rec x y))))))
               (t (let ((constituents (object-constituents (type-of x))))
                    (and constituents
                         (every (lambda (key)
                                  (object=/rec (funcall key x)
                                               (funcall key y)))
                                constituents)))))))
    (object=/rec x y)))

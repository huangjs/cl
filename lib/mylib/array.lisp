(defpackage :hjs.data.array
  (:use :cl)
  (:export #:copy-array
	   #:array-index
	   #:array-length
	   #:array-sap
	   ))

(in-package :hjs.data.array)

(deftype array-index (&optional (length array-dimension-limit))
  "Type designator for an index into array of LENGTH: an integer between
0 (inclusive) and LENGTH (exclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
  `(integer 0 (,length)))

(deftype array-length (&optional (length array-dimension-limit))
  "Type designator for a dimension of an array of LENGTH: an integer between
0 (inclusive) and LENGTH (inclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
  `(integer 0 ,length))

(defun copy-array (array &key (undisplace nil))
  "Shallow copies the contents of any array into another array with
equivalent properties.  If array is displaced, then this function will
normally create another displaced array with similar properties,
unless UNDISPLACE is non-NIL, in which case the contents of the array
will be copied into a completely new, not displaced, array."
  (declare (type array array))
  (let ((copy (%make-array-with-same-properties array undisplace)))
    (unless (array-displacement copy)
      (dotimes (n (array-total-size copy))
        (declare (type array-index n))
        (setf (row-major-aref copy n) (row-major-aref array n))))
    copy))

(defun %make-array-with-same-properties (array undisplace)
  "Make an array with the same properties (size, adjustability, etc.)
as another array, optionally undisplacing the array."
  (apply #'make-array
	 (list* (array-dimensions array)
		:element-type (array-element-type array)
		:adjustable (adjustable-array-p array)
		:fill-pointer (when (array-has-fill-pointer-p array)
				(fill-pointer array))
		(multiple-value-bind (displacement offset)
		    (array-displacement array)
		  (when (and displacement (not undisplace))
		    (list :displaced-to displacement
			  :displaced-index-offset offset))))))

;;; originally from nlisp, but optimized and added more constraints :)
;;; TODO: add more simple array types
#+sbcl
(defun %array-address (specialized-array)
  ;; with-array-data will get us to the actual data.  However, because
  ;; the array could have been displaced, we need to know where the
  ;; data starts.
  (assert (arrayp specialized-array))
  (sb-kernel:with-array-data ((data specialized-array)
			      (start)
			      (end))
    (declare (ignore end))
    ;; DATA is a specialized simple-array.  Memory is laid out like this:
    ;;
    ;; e.g. double-float vector in 32bit sbcl
    ;;
    ;;   byte offset    Value
    ;;        0         type code 
    ;;        4         4 * number of elements in vector (4 because of fixnum tag)
    ;;        8         1st element of vector
    ;;      ...         ...
    ;;
    ;; e.g. double-float vector in 64bit sbcl
    ;;
    ;;   byte offset    Value
    ;;        0         type code 
    ;;        8         8 * number of elements in vector (8 because of fixnum tag)
    ;;        16        1st element of vector
    ;;      ...         ...
    #+x86
    (let ((addr (the (unsigned-byte 32) (+ 8 (logandc1 7 (sb-kernel:get-lisp-obj-address data))))))
      (if (zerop start)
	  ;; not an displaced array
	  addr
	  ;; and displaced array
	  (let ((type-bit-width (let ((data-element-type (array-element-type data)))
				  (cond ((or (equal data-element-type '(signed-byte 8))
					     (equal data-element-type '(unsigned-byte 8)))
					 0)
					((or (equal data-element-type '(signed-byte 16))
					     (equal data-element-type '(unsigned-byte 16)))
					 1)
					((or (equal data-element-type '(signed-byte 32))
					     (equal data-element-type '(unsigned-byte 32)))
					 2)
					((equal data-element-type 'single-float)
					 2)
					((equal data-element-type 'double-float)
					 3)
					((equal data-element-type '(complex single-float))
					 3)
					((equal data-element-type '(complex double-float))
					 4)
					(t
					 (error "Unknown specialized array element type"))))))
	    (sb-sys:int-sap (the (unsigned-byte 32) (+ addr (the (unsigned-byte 32) (ash start type-bit-width))))))))
    
    #+x86-64
    (let ((addr (the (unsigned-byte 64) (+ 16 (logandc1 15 (sb-kernel:get-lisp-obj-address data))))))
      (if (zerop start)
	  ;; not an displaced array
	  addr
	  ;; and displaced array
	  (let ((type-bit-width (let ((data-element-type (array-element-type data)))
				  (cond ((or (equal data-element-type '(signed-byte 8))
					     (equal data-element-type '(unsigned-byte 8)))
					 0)
					((or (equal data-element-type '(signed-byte 16))
					     (equal data-element-type '(unsigned-byte 16)))
					 1)
					((or (equal data-element-type '(signed-byte 32))
					     (equal data-element-type '(unsigned-byte 32)))
					 2)
					((or (equal data-element-type '(signed-byte 64))
					     (equal data-element-type '(unsigned-byte 64)))
					 3)
					((equal data-element-type 'single-float)
					 2)
					((equal data-element-type 'double-float)
					 3)
					((equal data-element-type '(complex single-float))
					 3)
					((equal data-element-type '(complex double-float))
					 4)
					(t
					 (error "Unknown specialized array element type"))))))
	    (sb-sys:int-sap (the (unsigned-byte 64) (+ addr (the (unsigned-byte 64) (ash start type-bit-width))))))))))

;;; TODO: add more test restrictions using with-test macro
#+sbcl
(defun array-sap (specialized-array)
  "Return the physical address of where the actual data of an array is
stored.

Specialized-Array must be an array of one of the following
types:

                double-float
                single-float
                (unsigned-byte 32)
                (unsigned-byte 16)
                (unsigned-byte  8)
                (signed-byte 32)
                (signed-byte 16)
                (signed-byte  8)
		(complex single-float)
		(complex double-float)

and on x86-64 SBCL, with additional

		(unsigned-byte 64)
		(single-float 64)

"
  (sb-sys:int-sap (%array-address specialized-array)))

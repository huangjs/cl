;;; Some simple defclass examples

(defclass food () ())
(defclass fruit (food) ())
(defclass apple (fruit) ())
(defclass orange (fruit) ())
(defclass seafood (food) ())
(defclass shrimp (seafood) ())
(defclass prawn (shrimp) ())

;;; Some simple defmethod examples

(defmethod prepare ((item apple))
 (core item))

(defmethod prepare ((item orange))
 (peel item))

(defmethod prepare ((item shrimp))
 (peel item)
 (devein item))

;;; Examples of unspecialized parameters

(defmethod core (item)
 (format t "~&Coring ~S~%" item))

(defmethod devein (item)
 (format t "~&Deveining ~S~%" item))

(defmethod peel (item)
 (format t "~&Peeling ~S~%" item))

(defmethod wash (item)
 (format t "~&Washing ~S~%" item))

;;; :Example slot specifications

(defclass fruit (food)
  ((color :initarg :color
          :reader fruit-color)
   (price :initarg :price
          :accessor fruit-price)))

;;; Example class option (default initargs)

(defclass apple (fruit) ()
  (:default-initargs
    :color 'red))

;;; Two ways to define a before method...
;;; ... the latter is clearer. Save call-next-method
;;; for situations where you need more control
;;; over the return value or arguments passed
;;; to the next method.

(defmethod prepare ((item prawn))
  (wash item)
  (call-next-method))

(defmethod prepare :before ((item prawn))
  (wash item))

;;; Example of an eql specializer

(defmethod cook ((item food) (time integer))
  (format t "~&Cooking ~S ~S seconds~%"
            item time))

(defmethod cook ((item food) (goal symbol))
  (format t "~&Cooking ~S until ~S%"
            item goal))

(defmethod cook ((item food) (time (eql 0)))
  (format t "~&Leaving ~S raw~%" item))


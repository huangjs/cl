
;;; Graham's BST code, with my fixes and cleanup.
;;;
;;; General changes:
;;;  - COND instead of nested IF's
;;;  - Replaced EQL with second call to <. x = y if
;;;    neither x < y nor y < x.

(in-package :cs325-user)

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
    (let ((elt (node-elt bst)))
      (cond ((funcall < obj elt)
             (make-node 
              :elt elt
              :l   (bst-insert obj (node-l bst) <)
              :r   (node-r bst)))
            ((funcall < elt obj)
             (make-node 
              :elt elt
              :r   (bst-insert obj (node-r bst) <)
              :l   (node-l bst)))
            (t bst)))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
    (let ((elt (node-elt bst)))
      (cond ((funcall < obj elt)
             (bst-find obj (node-l bst) <))
            ((funcall < elt obj)
             (bst-find obj (node-r bst) <))
            (t bst)))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

;;; Changes from Graham's BST-REMOVE: 
;;;  - Replace BST-REMOVE-MAX/BST-REMOVE-MIN with
;;;    existing BST functions
;;;  - Rename PERCOLATE to BST-JOIN

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
    (let ((elt (node-elt bst))
          (l (node-l bst))
          (r (node-r bst)))
      (cond ((funcall < obj elt)
             (make-node
              :elt elt :l (bst-remove obj l <) :r r))
            ((funcall < elt obj)
             (make-node
              :elt elt :l l :r (bst-remove obj r <)))
            (t
             (bst-join l r <))))))

;;; BST-JOIN joins 2 binary search trees.
;;; Precondition: (bst-max l) <= (bst-min r)
(defun bst-join (l r <)
  (cond ((null l) r)
        ((null r) l)
        ((zerop (random 2))  ;; random choice for new root
         (let ((root (node-elt (bst-max l))))
           (make-node :elt root
                      :l (bst-remove root l <)
                      :r r)))
        (t
         (let ((root (node-elt (bst-min r))))
           (make-node :elt root
                      :l l
                      :r (bst-remove root r <))))))


(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))


;;; Destructive BST functions from Chapter 12

;;; Changes from Graham's code:
;;;  - Just one function

(defun bst-insert! (obj bst <)
  (cond ((null bst)
         (make-node :elt obj))
        (t
         (let ((elt (node-elt bst))
               (l (node-l bst))
               (r (node-r bst)))
           (cond ((funcall < obj elt)
                  (setf (node-l bst)
                        (bst-insert! obj l <)))
                 ((funcall < elt obj)
                  (setf (node-r bst)
                        (bst-insert! obj r <))))
           bst))))

;;; Changes from Graham's code:
;;;  - Replaced percolate! with bst-join!

(defun bst-delete (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst))
            (l (node-l bst))
            (r (node-r bst)))
        (cond ((eql obj elt)
               (bst-join! bst l r <))
              ((funcall < obj elt)
               (setf (node-l bst) (bst-delete obj l <))
               bst)
              (t
               (setf (node-r bst) (bst-delete obj r <))
               bst)))))

;;; Destructively joins 2 left and right subtrees of node.
;;; Precondition: (bst-max l) <= (bst-min r)
(defun bst-join! (node l r <)
  (cond ((null l) r)
        ((null r) l)
        ((zerop (random 2))  ;; random choice for new root
         (let ((root (node-elt (bst-max l))))
           (setf (node-elt node) root
                 (node-l node) (bst-delete root l <))
           node))
        (t
         (let ((root (node-elt (bst-min r))))
            (setf (node-elt node) root
                 (node-r node) (bst-delete root r <))
            node))))


;;; Graham's fixed Binary Search Tree code
;;; including bst-insert! and bst-delete from
;;; Chapter 12.
;;;
;;; Source: http://www.paulgraham.com/lib/paulgraham/acl2.lisp

(in-package :cs325-user)

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                  :elt elt
                  :l   (bst-insert obj (node-l bst) <)
                  :r   (node-r bst))
                (make-node
                  :elt elt
                  :r   (bst-insert obj (node-r bst) <)
                  :l   (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

; >>> Replaces bst-remove from book, which was broken.

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node
                  :elt elt
                  :l (bst-remove obj (node-l bst) <)
                  :r (node-r bst))
                (make-node
                  :elt elt
                  :r (bst-remove obj (node-r bst) <)
                  :l (node-l bst)))))))

(defun percolate (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2))
                 (make-node :elt (node-elt (bst-max l))
                            :r r
                            :l (bst-remove-max l))
                 (make-node :elt (node-elt (bst-min r))
                            :r (bst-remove-min r)
                            :l l)))))) 

(defun bst-remove-min (bst)
  (if (null (node-l bst))  
      (node-r bst)
      (make-node :elt (node-elt bst)
                 :l   (bst-remove-min (node-l bst))
                 :r   (node-r bst))))

(defun bst-remove-max (bst)
  (if (null (node-r bst)) 
      (node-l bst)
      (make-node :elt (node-elt bst)
                 :l (node-l bst)
                 :r (bst-remove-max (node-r bst)))))



;;; Destructive BST functions from Chapter 12

(defun bst-insert! (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (progn (bsti obj bst <)
             bst)))

(defun bsti (obj bst <)
  (let ((elt (node-elt bst)))
    (if (eql obj elt)
        bst
        (if (funcall < obj elt)
            (let ((l (node-l bst)))
              (if l
                  (bsti obj l <)
                  (setf (node-l bst) 
                        (make-node :elt obj))))
            (let ((r (node-r bst)))
              (if r
                  (bsti obj r <)
                  (setf (node-r bst) 
                        (make-node :elt obj))))))))

; >>> Replaces bst-delete from book, which was broken.

(defun bst-delete (obj bst <)
  (if (null bst)
      nil       
      (if (eql obj (node-elt bst))
          (del-root bst)
          (progn
            (if (funcall < obj (node-elt bst))
                (setf (node-l bst) (bst-delete obj (node-l bst) <))
                (setf (node-r bst) (bst-delete obj (node-r bst) <)))
            bst)))) 
  
(defun del-root (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t        (if (zerop (random 2))
                        (cutnext r bst nil)
                        (cutprev l bst nil))))))

(defun cutnext (bst root prev)
  (if (node-l bst)
      (cutnext (node-l bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-l prev)   (node-r bst))
            root)
          (progn
            (setf (node-l bst)    (node-l root))
            bst))))

(defun cutprev (bst root prev)
  (if (node-r bst)
      (cutprev (node-r bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-r prev)   (node-l bst))
            root)
          (progn
            (setf (node-r bst)    (node-r root))
            bst))))



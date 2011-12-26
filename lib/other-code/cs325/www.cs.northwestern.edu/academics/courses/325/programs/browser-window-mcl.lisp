;;; browser-window.lisp 
;;; --------------------
;;; 
;;; Author: Chris Riesbeck (riesbeck@ils.nwu.edu)
;;; Date: 10/19/99
;;; Platform: Macintosh Common Lisp, v. 4.1
;;;
;;; This started out as a port of mop-browser.lisp
;;; by Tom McDougal (mcdougal@cs.uchicago.edu) from
;;; MACL v. 1.0. It's changed quite a bit, though. 
;;;
;;; This is now a generic utility for browsing nested
;;; structures. Browser means the same thing as inspecting.
;;;
;;; The main function it defines is GET-BROWSER-WINDOW.
;;;
;;; (GET-BROWSER-WINDOW item items :reuse)
;;;   Returns a window for browsing the given item.
;;;   If reuse is true, looks for a window already
;;;   dsplaying an item. If none found or reuse is
;;;   false, generates a new window.
;;;
;;;   item  - an object, usually a symbol, identifying
;;;           what's being browsed
;;;   items - a list of items to put in the browser
;;;           window
;;;   reuse - true or false, specifying whether to 
;;;           reuse an existing browsing window for item,
;;;           if any
;;;
;;;   The window titled with item is returned.
;;;   In the items list, a string becomes a label. A
;;;   list becomes a clickable list of items. Anything
;;;   else becomes a list of one clickable item.
;;;
;;;   When an item in an on-screen list is double-clicked,
;;;   BROWSE is called to generate the next browsing window.
;;;   other is a string and each value is an
;;;   object or list of objects for which BROWSE is
;;;   defined. BROWSE is prototyped in browser-api.lisp
;;;   and must be defined by an application, such as
;;;   mop-browser.lisp.


;;; MODULES
;;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "browser-api")
  )

;;; PACKAGES
;;; --------

(cl:defpackage "BROWSER-WINDOW"
  (:use "COMMON-LISP" "BROWSER-API" "CCL")
  (:export "BROWSER-WARN" "GET-BROWSER-WINDOW")
  )

(in-package "BROWSER-WINDOW")



;;; GLOBALS
;;; ---------------


(defvar *browser-window-stack* nil)


;;; CLASSES
;;; -------


;;; Autosizing view
;;; --------------------
;;;
;;; An autosizing view supports the method (adjust-size view).
;;; When called, the view recalculates its size to be just big
;;; enough to hold its subviews, plus insets.

(defclass autosizing-view (view) 
  ((insets :initarg :insets
           :accessor view-insets)
   (minimum-size :initarg :minimum-size
                 :accessor view-minimum-size)
   )
  (:default-initargs
    :insets #@(4 4)
    :minimum-size #@(10 10))
  )

(defmethod initialize-instance :after ((v autosizing-view) &rest initargs)
  (declare (ignore initargs))
  (adjust-size v))

(defmethod adjust-size ((v autosizing-view))
  (set-view-size v
    (max-point (view-minimum-size v)
               (add-points (subviews-minimum-size v)
                           (add-points (view-insets v)
                                       (view-insets v))))))

(defmethod subviews-minimum-size ((v view))
  (reduce #'(lambda (size subview)
              (max-point size
                         (add-points (view-position subview)
                                     (view-size subview))))
          (view-subviews v)
          :initial-value #@(0 0)))

(defun max-point (point-1 point-2)
  (make-point (max (point-h point-1) (point-h point-2))
              (max (point-v point-1) (point-v point-2))))


;;; Column view
;;; --------------------
;;;
;;; A column view, at creation time, places its subviews
;;; in a vertical column, with :PADDING space between them.

(defclass column-view (view)
  ((padding :initarg :padding
            :accessor view-padding))
  (:default-initargs
    :padding #@(4 4))
  )

(defmethod initialize-instance :after ((v column-view) &rest initargs)
  (declare (ignore initargs))
  (adjust-subviews v))

(defmethod adjust-subviews ((v column-view))
  (let* ((insets (view-insets v))
         (inset-h (point-h insets))
         (inset-v (point-v insets))
         (pad-v (point-v (view-padding v))))
   (reduce #'(lambda (pos-v view)
              (set-view-position view inset-h pos-v)
              (+ pos-v (point-v (view-size view)) pad-v))
          (view-subviews v)
          :initial-value inset-v)))



;;; Browse window
;;; --------------------
;;;
;;; A browse window contains a column of browsing lists.
;;; Double-clicking a browsable item in a browsing list opens
;;; a new browser window on that item.


(defclass browser-window (window autosizing-view column-view)
  ((item :reader window-item
         :initarg :item))
  (:default-initargs
    :minimum-size #@(100 10)
    :window-type :document  
    ))


;;; Browse lists
;;; ----------------

;;; A browse list is a scrollable, double-clickable list of
;;; items installed in a browse window.

(defclass browse-list (sequence-dialog-item) ()
  (:default-initargs
    :table-print-function
    #'(lambda (item stream)
        (princ (item-text item) stream))))
  

;;; GET-BROWSER-WINDOW
;;; ------------------

(defun get-browser-window (item items &key reuse)
  (window-select
   (or (and reuse 
            (find item (windows :class 'browser-window)
                  :key #'window-item))
       (make-browser-window item items))))


;;; Window makers
;;; ----------------

(defun make-browser-window (item items)
  (let ((bw (make-instance 'browser-window
              :window-title (window-title item)
              :window-show nil
              :view-position (generate-window-position)
              :item item
              :view-subviews (make-browser-dialog-items items))))
    (push bw *browser-window-stack*)
    (window-show bw)
    bw))

(defun make-browser-dialog-items (items)
  (mapcar #'make-browser-item items))

(defmethod make-browser-item ((item t))
  (make-instance 'browse-list
    :table-sequence (list item)))

(defmethod make-browser-item ((items cons))
  (make-instance 'browse-list
    :table-sequence items))

(defmethod make-browser-item ((item string))
  (make-instance 'static-text-dialog-item
    :dialog-item-text item))


;;; Window close
;;; ------------


(defmethod window-close :after ((bw browser-window))
  (setq *browser-window-stack*
        (delete bw *browser-window-stack*)))


;;; Window positioning
;;; ------------------

;;; generate-window-position finds a position to put a new
;;; browsing window, offset from an existing window.
;;;
;;; If the option key is down, the new window will replace
;;; the old window.
;;;

(defun generate-window-position ()
  (cond ((null *browser-window-stack*)
         *window-default-position*)
        (t
         (add-points #@(10 10)
                     (view-position
                      (first *browser-window-stack*))))))


;;; Browser utilities
;;; ----------------

;;; Default methods for getting items and converting them
;;; to strings and window titles. 

(defmethod browser-items (item)
  (declare (ignore item))
  nil)

(defmethod browser-items ((item cons))
  item)

(defmethod item-text (item)
  (->string item))

(defmethod window-title (item)
  (prin1-to-string  item))

(defmethod window-title ((item cons))
  (format nil "(~a...)" (first item)))

(defun browser-warn (format-string &rest args)
  (apply #'format t format-string args)
  (ed-beep))


;;; Browser list selection support
;;; ------------------------------

;;; Double-click to select an item in a browse list

(defmethod view-click-event-handler
           ((browse-list browse-list) where)
  (if (double-click-p)
    (select-list-item browse-list where)
    (call-next-method)))

(defmethod select-list-item ((browse-list browse-list) where)
  (let ((cell (point-to-cell browse-list where)))
    (unless (null cell)
      (browse (cell-contents browse-list cell))
      (cell-deselect browse-list cell))))


(provide "browser-window-mcl")


;;; -------- end of file -------


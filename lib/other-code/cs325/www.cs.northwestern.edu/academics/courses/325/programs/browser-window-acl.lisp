;;; browser.lisp 
;;; --------------------
;;; 
;;; Author: Chris Riesbeck (riesbeck@ils.nwu.edu)
;;; Last update: 10/14/99
;;; Platform: Allegro Common Lisp, v. 5.0.1 Lite
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
  (:use "COMMON-LISP" "BROWSER-API")
  (:export "BROWSER-WARN" "GET-BROWSER-WINDOW")
  )

(in-package "BROWSER-WINDOW")


;;; ---------
;;; MAIN CODE
;;; ---------

;;; GLOBALS
;;; ---------------

;;; insets determine how far items are set in from the border
;;; of the containing window
;;;
;;; paddings determine how much space is put between items
;;;
;;; fudge numbers are what I have to add to make calculations of
;;; string widths and height come out right
 
(defparameter *inset-x* 4)
(defparameter *inset-y* 4)
(defparameter *pad-x* 4)
(defparameter *pad-y* 4)
(defparameter *fudge-x* 4)
(defparameter *fudge-y* 0)

;;; The browser window stack supports
;;;   - placing new windows offset from previous ones
;;;   - closing all windows at once with control or option click

(defvar *browser-window-stack* nil)

;;; CLASSES
;;; -------

;;; Browser window
;;; --------------------
;;;
;;; A browse window contains a column of browsing lists.
;;; Double-clicking a browsable item in a browsing list opens
;;; a new browser window on that item.


(defclass browser-window (cg:dialog) ())

(defclass browser-window-list (cg:single-item-list) ()
   (:default-initargs
    :on-double-click 'item-selected-response
    :key #'->string)
   )

;;; Double-click to browse an item in a browse list
;;; Exactly what browsing an item means, i.e., what sub-items
;;; to display, is up to the particular application. The
;;; default method does nothing.

(defmethod item-selected-response ((dialog browser-window) list-box)
   (browse (cg:value list-box))
   t)

(defun browser-warn (format &rest args)
   (apply #'cg:window-warning 
     (cg:development-main-window cg:*system*)
     format args))


;;; close means close, not hide!

(defmethod cg:user-close ((dialog browser-window))
  (cond ((cg:key-was-down-p cg:vk-control)
         (close-all dialog))
        (t
         (setq *browser-window-stack*
               (delete dialog *browser-window-stack*))
         (close dialog))))

(defmethod close-all ((dialog browser-window))
   (dolist (win *browser-window-stack*)
      (close win))
   (setq *browser-window-stack* nil)
   t)


;;; Utilities
;;; ---------


;;; Generalized object to string method, comparable to
;;; Java's toString().

(defmethod ->string (x) (string x))


;;; Size calculating utilities
;;; --------------------------

(defun stream-itemlist-width (stream items)
   (+ *fudge-x*
      (reduce #'(lambda (width item)
                  (max width
                    (stream-item-width stream item)))
        items :initial-value 0)))

(defun stream-item-width (stream item)
   (* (cg:stream-string-width stream (->string item))))


(defun stream-itemlist-height (stream items)
   (+ *fudge-y* (* (cg:font-height (cg:fontmetrics stream)) (length items))))
      
(defun stream-string-height (stream)
   (cg:font-height (cg:fontmetrics stream)))


;;; Widget positioning
;;; ------------------

(defmethod adjust-items ((dialog browser-window))
   (let ((items (cg:dialog-items dialog)))
      (adjust-item-sizes items)
      (align-vertically items)))

(defun adjust-item-sizes (items)
   (mapc #'adjust-size items))

(defmethod adjust-size ((item cg:static-text))
   (let ((win (cg:window item)))
      (cg:resize-window win
        (cg:make-position (cg:stream-string-width win (cg:value item))
          (stream-string-height win)))))
   
(defmethod adjust-size ((item browser-window-list))
   (let ((win (cg:window item))
         (items (cg:range item)))
      (cg:resize-window win
        (cg:make-position (stream-itemlist-width win items)
          (stream-itemlist-height win items)))))

(defun align-vertically (dialog-items)
   (let ((top *inset-y*) (left *inset-x*))
      (dolist (item dialog-items)
         (let ((win (cg:window item)))
            (cg:move-window win (cg:make-position left top))
            (incf top (+ (cg:box-height (cg:exterior win))
                         *pad-y*))))
      dialog-items))

(defmethod adjust-size ((win browser-window))
   (cg:resize-window win
     (cg:make-position (+ *inset-x* (max-right win))
                    (+ *inset-y* (max-bottom win)))))
                   
(defun max-bottom (win)
   (cg:box-bottom
      (cg:exterior
        (cg:window (first (last (cg:dialog-items win)))))))

(defun max-right (win)
   (reduce #'(lambda (right item)
               (max right (cg:box-right (cg:exterior
                                       (cg:window item)))))
     (cg:dialog-items win)
     :initial-value 0))

(defun adjust-position (win)
   (cg:move-window win
     (cg:position+ (last-browser-window-position)
       (cg:make-position 10 10))))

(defun last-browser-window-position ()
   (if (null *browser-window-stack*)
      (cg:make-position 10 10)
      (cg:box-top-left (cg:exterior (first *browser-window-stack*)))))


;;; Window and widget construction
;;; ------------------------------

;;; Window makers
;;; ----------------

(defmethod get-browser-window (item items &key reuse)
  (let* ((name (intern item))
         (old-window (and reuse (cg:find-window name))))
      (cond ((null old-window)
             (make-item-browser-window name items))
            (t
              (cg:select-window old-window)
              old-window))))

(defmethod make-item-browser-window (item items)
   (unless (null items)
     (make-browser-window items (->string item) item)))

(defun make-browser-window (items title name)
 (let ((bw (cg:make-window name
             :device 'browser-window
             :widgets (mapcar #'make-item items)
             :title title
             :user-resizable t)))
    (adjust-items bw)
    (adjust-size bw)
    (adjust-position bw)
    (push bw *browser-window-stack*)
    (cg:expand-window bw)
    bw))

(defun make-item (item)
   (cond ((stringp item)
          (make-label-item item))
         ((consp item)
          (make-list-item item))
         (t (error "Unknown item type ~S" item))))

(defun make-label-item (item)
   (make-instance 'cg:static-text
     :title nil
     :value item
     :box (cg:make-box 0 0 50 30)))

(defun make-list-item (items)
   (make-instance 'browser-window-list
     :box (cg:make-box 0 0 50 30)
     :range items))


(provide "browser-window-acl")


;;; -------- end of file -------


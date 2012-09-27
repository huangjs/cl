;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-CLASS-BROWSER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A basic class browser in CLIM
;;;   Created: 2003-05-07
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann


;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
  
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
  
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.


;; Note: Don't try with a non PCL-based CLOS.
;; After loading try calling (clim-class-browser::class-browser)

(in-package :clim-class-browser)

(define-application-frame class-browser ()
  ((class :initarg :class :initform 'climi::basic-pane))
  (:pointer-documentation t)
  (:panes
   (app :application
    :width 3000 :height 2000
    :scroll-bars nil
    :incremental-redisplay t
    :display-function 'class-browser-display)
   (io :interactor
    :height 50))
  (:layouts
   (default
       (vertically (:width 800 :min-width 100 :max-width +fill+)
         (:fill
          (horizontally ()
            (scrolling (:scroll-bars t)
              app)
            ))
         io))))

(defvar *expanded* nil)

(define-presentation-type node ())

(define-class-browser-command toggle-node ((node 'node :gesture :select))
  (if (member node *expanded*)
      (setf *expanded* (remove node *expanded*))
      (push node *expanded*)))

(defun class-browser-display (app pane)
  app pane
  (with-slots (class) app
(let ((*standard-output* pane))
    (format-graph-from-roots
     (list (sb-pcl::find-class class) )
     #'(lambda (node *standard-output*)
         (let ((*print-case* :downcase))
           (surrounding-output-with-border
            (*standard-output* :shape :drop-shadow)
            (with-text-style (t (make-text-style :sans-serif nil nil))
              (with-output-as-presentation (t node 'node)
                (with-text-style (t (make-text-style :sans-serif :bold :large))
                  (princ (sb-pcl:class-name node))))
              (terpri)
              (with-drawing-options (*standard-output* :ink +red4+)
                (princ (clean-docu-string(class-documentation node))))
              (terpri)
              (formatting-table ()
                (dolist (sd (sb-pcl:class-direct-slots node))
                  (formatting-row ()
                    (formatting-cell (t :align-y :top)
                      (princ (sb-pcl:slot-definition-name sd))
                      (princ " "))
                    (formatting-cell (t :align-y :top)
                      (with-drawing-options (*standard-output* :ink +red4+)
                        (princ (clean-docu-string (slot-documentation sd))))))))
              (terpri)) )))
     #'(lambda (node)
         (if (member node *expanded*)
             ;;(sb-pcl:class-direct-subclasses node)
             (sb-pcl:class-direct-superclasses node)
             nil))
     :cutoff-depth nil
     :graph-type :tree
     :merge-duplicates t
     :arc-drawer #'climi::arrow-arc-drawer
     :arc-drawing-options (list :ink +gray66+ :line-thickness 1)
     :generation-separation 30)
    (terpri))))

;; some tweaks:

(defun clean-docu-string (string)
  (with-output-to-string (bag)
    (let ((last-was-nl nil))
      (loop for c across string do
            (cond ((eql c #\newline)
                   (princ c bag)
                   (setf last-was-nl t))
                  ((member c '(#\space #\tab))
                   (if last-was-nl
                       nil
                       (princ c bag)))
                  (t
                   (setf last-was-nl nil)
                   (princ c bag)))))))

(defun class-documentation (class)
  (getf (slot-value class 'sb-pcl::plist) 'sb-pcl::documentation))

(defun slot-documentation (slot-def)
  (slot-value slot-def 'sb-pcl::documentation))  

(defun class-browser (&optional (class 'climi::basic-pane))
  (run-frame-top-level (make-application-frame 'class-browser :class class)))


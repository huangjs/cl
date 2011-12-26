;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: views.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;; 
;;; view methods and functions

(in-package "CCL")

;;; ======================================================================
;;; METHODS
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; alpaca-text-view
;;; ----------------------------------------------------------------------

(define-objc-method ((:void :init-with-frame (:<NSR>ect frame-rect))
					 alpaca-text-view)
  (send-super :init-with-frame frame-rect)
  (send self :set-allows-undo t))

(define-objc-method ((:void :init-with-frame (:<NSR>ect frame-rect)
							:text-container (:id container))
					 alpaca-text-view)
  (send-super :init-with-frame frame-rect
			  :text-container container)
  (send self :set-allows-undo t))

(define-objc-method ((:void :key-down (:id the-event))
					 alpaca-text-view)
  (let ((keyname (get-event-name the-event)))
	(unless (alpaca-handle-keydown keyname *alpaca-keymap*)
	  (send-super :key-down the-event))))

;;; ----------------------------------------------------------------------
;;; alpaca-page-view
;;; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :init-with-frame frame-rect
							  :with-content-frame content-rect
							  :with-header-frame header-rect
							  :with-footer-frame footer-rect
							  :with-document a-document
							  :with-pager a-pager)
					   alpaca-page-view)
	(send-super :init-with-frame frame-rect)
	(setf document a-document)
	(setf pager a-pager)
	(setf textcontainer (send (send (@class ns-text-container) 'alloc)
							  :init-with-container-size (pref content-rect :<NSR>ect.size)))
	(send (send document 'layout-manager) :add-text-container textcontainer)
	(setf textview (send (send (@class alpaca-text-view) 'alloc)
						 :init-with-frame content-rect
						 :text-container textcontainer))
	(setf headerview (send (send (@class ns-text-field) 'alloc) :init-with-frame header-rect))
	(send headerview :set-editable nil)
	(send headerview :set-bezeled nil)
	(send headerview :set-alignment #$NSCenterTextAlignment)
	(send headerview :set-text-color (send (@class ns-color) 'gray-color))
	(send headerview :set-string-value #@"Header")

	(setf footerview (send (send (@class ns-text-field) 'alloc) :init-with-frame footer-rect))
	(send footerview :set-editable nil)
	(send footerview :set-bezeled nil)
	(send footerview :set-alignment #$NSCenterTextAlignment)
	(send footerview :set-text-color (send (@class ns-color) 'gray-color))
	(send footerview :set-string-value #@"Footer")

	(send self :set-frame frame-rect)
	(send self :add-subview textview)
	(send self :add-subview headerview)
	(send self :add-subview footerview)	))

(define-objc-method ((:<BOOL> is-flipped)
					 alpaca-page-view)
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id textview)
					   alpaca-page-view)
	textview))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :set-page-number number-string)
					   alpaca-page-view)
	(setf pagenumberstring number-string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void update-header)
					   alpaca-page-view)
	(send headerview :set-string-value (send document 'display-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void update-footer)
					   alpaca-page-view)
	(send footerview :set-string-value pagenumberstring)))

(define-objc-method ((:void :draw-rect (:<NSR>ect rect))
					 alpaca-page-view)
  (send-super :draw-rect rect)
  (send (send (@class ns-color) 'white-color) 'set)
  (send (@class ns-bezier-path) :fill-rect rect)
  (send self 'update-header)
  (send self 'update-footer))

;;; ----------------------------------------------------------------------
;;; alpaca-pager-view
;;; ----------------------------------------------------------------------

(define-objc-method ((:id document)
					 alpaca-pager-view)
  document)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void init-first-responder)
					   alpaca-pager-view)
	(let* ((layout (send document 'layout-manager))
		   (tv (send layout 'first-text-view))
		   (window (send self 'window))
		   (view-accepts? (send tv 'accepts-first-responder)))
	  (when view-accepts?
		(send window :make-first-responder tv)))))

(define-objc-method ((:void :draw-rect (:<NSR>ect rect))
					 alpaca-pager-view)
  (send-super :draw-rect rect)
  (send (send (@class ns-color) 'gray-color) 'set)
  (send (@class ns-bezier-path) :fill-rect rect))

(define-objc-method ((:<BOOL> is-flipped)
					 alpaca-pager-view)
  t)

(define-objc-method ((:id document)
					 alpaca-pager-view)
  document)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id get-rtf-text)
					   alpaca-pager-view)
	(let* ((layout (send document 'layout-manager))
		   (tv (send layout 'first-text-view))
		   (len (send (send tv 'string) 'length)))
	  (rlet ((range :<NSR>ange :location 0 :length len))
			(send tv :rtf-from-range range)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void update-frame)
					   alpaca-pager-view)
	(let ((printinfo (send document 'print-info))
		  (page-count (send document 'page-count)))
	  (rlet ((pager-size :<NSS>ize))
			(slet ((paper-size (send printinfo 'paper-size)))
				  (setf (pref pager-size :<NSS>ize.width) (+ $page-horizontal-inset
															 (pref paper-size :<NSS>ize.width)
															 $page-horizontal-inset))
				  (setf (pref pager-size :<NSS>ize.height) (+ $page-vertical-inset
															  (* page-count
																 (pref paper-size :<NSS>ize.height))
															  (* (1- page-count) $page-separation)
															  $page-vertical-inset))
				  (send self :set-frame-size pager-size))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void add-page)
					   alpaca-pager-view)
	(rlet ((page-size :<NSS>ize)
		   (page-frame  :<NSR>ect)
		   (content-frame  :<NSR>ect)
		   (header-frame  :<NSR>ect)
		   (footer-frame  :<NSR>ect))
		  (let ((printinfo (send document 'print-info)))
			(slet ((page-size (send printinfo 'paper-size)))
				  ;; page frame
				  (multiple-value-bind (x y width height)
					  (frame-for-page document)
					(setf (pref page-frame :<NSR>ect.origin.x) x)
					(setf (pref page-frame :<NSR>ect.origin.y) y)
					(setf (pref page-frame :<NSR>ect.size.width) width)
					(setf (pref page-frame :<NSR>ect.size.height) height))
				  ;; content frame
				  (multiple-value-bind (x y width height)
					  (frame-for-content document)
					(setf (pref content-frame :<NSR>ect.origin.x) x)
					(setf (pref content-frame :<NSR>ect.origin.y) y)
					(setf (pref content-frame :<NSR>ect.size.width) width)
					(setf (pref content-frame :<NSR>ect.size.height) height))
				  ;; header-frame
				  (multiple-value-bind (x y width height)
					  (frame-for-header document)
					(setf (pref header-frame :<NSR>ect.origin.x) x)
					(setf (pref header-frame :<NSR>ect.origin.y) y)
					(setf (pref header-frame :<NSR>ect.size.width) width)
					(setf (pref header-frame :<NSR>ect.size.height) height))
				  ;; footer-frame
				  (multiple-value-bind (x y width height)
					  (frame-for-footer document)
					(setf (pref footer-frame :<NSR>ect.origin.x) x)
					(setf (pref footer-frame :<NSR>ect.origin.y) y)
					(setf (pref footer-frame :<NSR>ect.size.width) width)
					(setf (pref footer-frame :<NSR>ect.size.height) height))
				  ;; set up the page
				  (let* ((start-page (send document 'start-page))
						 (next-page (send document 'next-page))
						 (next-page-left $page-horizontal-inset)
						 (next-page-top (+ $page-vertical-inset
										   (* (- next-page start-page)
											  (+ $page-separation
												 (pref page-frame :<NSR>ect.size.height))))))
					;; translate the page rectangle to where it belongs
					(setf (pref page-frame :<NSR>ect.origin.x) $page-horizontal-inset)
					(setf (pref page-frame :<NSR>ect.origin.y) next-page-top)
					;; make and add the page view
					(let* ((page-view (send (@class alpaca-page-view) 'alloc))
						   (pagenum (send document 'next-page))
						   (numstring (format nil "~D" pagenum)))
					  (send page-view
							:init-with-frame page-frame
							:with-content-frame content-frame
							:with-header-frame header-frame
							:with-footer-frame footer-frame
							:with-document document
							:with-pager self)
					  (send self :add-subview page-view)
					  (send document 'inc-page-count)
					  (send self 'update-frame)
					  (send page-view :set-page-number (%make-nsstring numstring)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id :set-text-from-data data)
					   alpaca-pager-view)
	(send self 'add-page)
	(unless (%null-ptr-p data)
	  (let* ((layout (send document 'layout-manager))
			 (tv (send layout 'first-text-view)))
		(rlet ((empty-range :<NSR>ange :location 0 :length 0))
			  (send tv :replace-characters-in-range empty-range
				  :with-rtf data))
		(send self 'update-frame)))))

(defun get-rtf-text (pager-view)
  (send pager-view 'get-rtf-text))

(defun set-text-from-data (pager-view data)
  (send pager-view :set-text-from-data data))

(defun init-first-responder (pager-view)
  (send pager-view 'init-first-responder))

(defun add-page (view)
  (send view 'add-page))
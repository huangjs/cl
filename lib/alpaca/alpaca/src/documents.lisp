;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: documents.lisp,v 1.1.1.1 2004/03/25 06:43:08 mevins Exp $
;;; 
;;; document methods and functions

(in-package "CCL")


;;; ======================================================================
;;; CLOS DOCUMENT MODEL
;;; ======================================================================

(defclass document ()
  ((objc-document :accessor objc-document :initform nil :initarg :objc-document)))

(defclass rtf-document (document)
  ())

(defclass text-document (document)
  ())

(defparameter *documents* nil)

;;; ======================================================================
;;; METHODS
;;; ======================================================================

(define-objc-method ((:int tag)
					 alpaca-document)
  tag)

(defmethod tag ((doc document))
  (send (objc-document doc) 'tag))

(define-objc-method ((:void init)
					 alpaca-document)
  (send-super 'init)
  (setf tag (next-document-number)))

(define-objc-method ((:void close)
					 alpaca-document)
  (send-super 'close)
  (setf *documents*
		(delete-if #'(lambda (x)
					   (eql tag (send (objc-document x) 'tag)))
				   *documents*)))

(define-objc-method ((:void init)
					 alpaca-rtf-document)
  (send-super 'init)
  (setf pagecount 0)
  (setf startpage 1)
  (setf textstorage (send (send (@class ns-text-storage) 'alloc) 'init))
  (setf layoutmanager (send (send (@class ns-layout-manager) 'alloc) 'init))
  (send textstorage :add-layout-manager layoutmanager)
  (send layoutmanager :set-delegate self)
  (send layoutmanager 'release)
  (pushnew
   (make-instance 'rtf-document :objc-document (copy-macptr self))
   *documents*))

(define-objc-method ((:void init)
					 alpaca-text-document)
  (send-super 'init)
  (pushnew
   (make-instance 'text-document :objc-document (copy-macptr self))
   *documents*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id layout-manager)
					   alpaca-rtf-document)
	layoutmanager))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:int start-page)
					   alpaca-rtf-document)
	startpage))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:int last-page)
					   alpaca-rtf-document)
	(1- (+ startpage pagecount))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:int next-page)
					   alpaca-rtf-document)
	(+ startpage pagecount)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:int page-count)
					   alpaca-rtf-document)
	pagecount))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:int inc-page-count)
					   alpaca-rtf-document)
	(setf pagecount (1+ pagecount))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:int dec-page-count)
					   alpaca-rtf-document)
	(setf pagecount (1- pagecount))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id print-info)
					   alpaca-rtf-document)
	;; first make sure we have the printinfo
	(when (%null-ptr-p printinfo)
	  (setq printinfo (send (@class ns-print-info) 'shared-print-info)))
	;; next make sure it knows what the paper size is
	(let ((papername (send printinfo 'paper-Name)))
	  (when (%null-ptr-p papername)
		(send printinfo :set-paper-name #@"Letter")))
	printinfo))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id :data-representation-of-type type)
					   alpaca-rtf-document)
	(declare (ignorable type))
	(let* ((len (send textstorage 'length)))
	  (get-rtf-text pagerview))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id :data-representation-of-type type)
					   alpaca-text-document)
	(declare (ignorable type))
	(let* ((textStorage (send textview 'text-storage))
		   (len (send textStorage 'length)))
	  (rlet ((range :<NSR>ange :location 0 :length len))
			(send (send textview 'string)
				  :data-Using-Encoding #$NSASCIIStringEncoding
				  :allow-Lossy-Conversion t)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void force-text-layout)
					   alpaca-rtf-document)
	(let* ((len (send textstorage 'length))
		   (lastchar (1- len)))
	  (when (> len 0)
		;; find the glyph index of the last char
		(rlet ((charrange :<NSR>ange :location lastchar :length 1))
			  (slet ((glyphrange (send layoutmanager
									   :glyph-range-for-character-range charrange
									   :actual-character-range (%null-ptr))))
					(when (> (pref glyphrange :<NSR>ange.location) 0)
					  ;; we ask for the textcontainer of the last character, which forces
					  ;; layout of all the text
					  (let* ((loc (pref glyphrange :<NSR>ange.location))
							 (tc (send layoutManager
									   :text-container-for-glyph-at-index (1- loc)
									   :effective-range (%null-ptr))))
						nil))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void remove-last-page)
					   alpaca-rtf-document)
	(let* ((num-pages (send self 'page-count))
		   (containers (send layoutmanager 'text-containers))
		   (last-container (send containers 'last-object))
		   (last-view (send (send last-container 'text-view) 'superview)))
	  (send self 'dec-page-count)
	  (send last-view 'remove-from-superview)
	  (send layoutmanager :remove-text-container-at-index (1- (send containers 'count))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :window-controller-did-load-nib controller)
					   alpaca-rtf-document)
	(send-super :window-controller-did-load-nib controller)
	(set-text-from-data pagerview filedata)
	(send self 'force-text-layout)
	(init-first-responder pagerview)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :window-controller-did-load-nib controller)
					   alpaca-text-document)
	(send-super :window-controller-did-load-nib controller)
	(send textview :set-rich-text nil)
	(send textview :set-uses-ruler nil)
	(send textview :set-ruler-visible nil)
	(send textview
		  :insert-text (send (send (@class ns-string) 'alloc)
							 :init-with-data filedata
							 :encoding #$NSASCIIStringEncoding))))

(define-objc-method ((:void make-window-controllers)
					 alpaca-document)
  (let* ((nib-name (send self 'window-nib-name))
		 (controller (send (send (@class ns-window-controller) 'alloc)
						   :init-with-window-nib-name nib-name
						   :owner self)))
	(send self :add-window-controller controller)
	(send controller 'release)))

(define-objc-method ((:<BOOL> :load-data-representation data
							  :of-type type)
					 alpaca-document)
  (declare (ignorable type))
  (setq filedata data)
  (not (%null-ptr-p data)))

(define-objc-method ((:id window-nib-name)
					 alpaca-rtf-document)
  #@"AlpacaRTFEditor")

(define-objc-method ((:id window-nib-name)
					 alpaca-text-document)
  #@"AlpacaTextEditor")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:id textview)
					   alpaca-text-document)
	textview))

(define-objc-method ((:id window-nib-name)
					 lisp-editor-document)
  #@"lispeditor")

;;; layoutManager delegate method
(define-objc-method ((:void :layout-manager layoutmanager
							:did-complete-layout-for-text-container textcontainer
							:at-end (:<BOOL> layout-finished-flag))
					 alpaca-rtf-document)
  (let* ((containers (send layoutmanager 'text-containers)))
	(if (or (not layout-finished-flag)
			(%null-ptr-p textcontainer))
		;; either layout isn't finished or there are too many glyphs for the
		;; existing pages. if there are too many glyphs, add a page
		(let* ((last-container (send containers 'last-object)))
		  (if (or (eql textcontainer last-container)
				  (%null-ptr-p textcontainer))
			  (add-page pagerview)))
	  ;; layout is done and all the glyphs fit the pages. check to see if there
	  ;; are empty pages at the end that we can remove
	  (let* ((index-of-last-used (send containers :index-of-object-identical-to textcontainer))
			 (num-containers (send containers 'count)))
		(do ((ind (1+ index-of-last-used)(1+ ind)))
			((>= ind num-containers) nil)
		  (send self 'remove-last-page))))))


;;; ======================================================================
;;; FUNCTIONS
;;; ======================================================================

(defun frame-for-page (document)
  (let ((printinfo (send document 'print-info)))
	(slet ((page-size (send printinfo 'paper-size)))
		  (values 0.0 0.0 (pref page-size :<NSS>ize.width)(pref page-size :<NSS>ize.height)))))

(defun frame-for-header (document)
  (let* ((printinfo (send document 'print-info)))
	(slet ((page-size (send printinfo 'paper-size)))
		  (let* ((left-margin (send printinfo 'left-margin))
				 (top-margin (send printinfo 'top-margin))
				 (right-margin (send printinfo 'right-margin))
				 (header-width (- (pref page-size :<NSS>ize.width)
								  (+ left-margin right-margin))))
			(values left-margin top-margin header-width $header-height)))))

(defun frame-for-content (document)
  (let* ((printinfo (send document 'print-info)))
	(slet ((page-size (send printinfo 'paper-size)))
		  (let* ((left-margin (send printinfo 'left-margin))
				 (top-margin (send printinfo 'top-margin))
				 (right-margin (send printinfo 'right-margin))
				 (bottom-margin (send printinfo 'bottom-margin))
				 (content-top (+ top-margin $header-height $page-part-separation))
				 (content-width (- (pref page-size :<NSS>ize.width)
								   (+ left-margin right-margin)))
				 (content-height (- (pref page-size :<NSS>ize.height)
									(+ top-margin $header-height $page-part-separation
									   $page-part-separation $footer-height bottom-margin))))
			(values left-margin content-top content-width content-height)))))

(defun frame-for-footer (document)
  (let* ((printinfo (send document 'print-info)))
	(slet ((page-size (send printinfo 'paper-size)))
		  (let* ((left-margin (send printinfo 'left-margin))
				 (top-margin (send printinfo 'top-margin))
				 (right-margin (send printinfo 'right-margin))
				 (bottom-margin (send printinfo 'bottom-margin))
				 (footer-width (- (pref page-size :<NSS>ize.width)
								  (+ left-margin right-margin)))
				 (footer-height $footer-height)
				 (footer-top (- (pref page-size :<NSS>ize.height)
								(+ bottom-margin footer-height))))
			(values left-margin footer-top footer-width footer-height)))))

(let ((document-number 0))
  (defun next-document-number ()
	(incf document-number)))

(defun find-document (tagnum)
  (find tagnum *documents*
		:test #'(lambda (x y)
				  (eql x (tag y)))))

;;; ======================================================================
;;; DOCUMENT UTILITIES
;;; ======================================================================

(defmethod filename ((doc document))
  (lisp-string-from-nsstring (send (objc-document doc) 'file-name)))

(defmethod layout-manager ((doc rtf-document))
  (send (objc-document doc) 'layout-manager))

(defmethod layout-manager ((doc text-document))
  (send (send (objc-document doc) 'textview) 'layout-manager))

(defmethod word-count ((doc document))
  (let* ((layout-manager (layout-manager doc))
		 (storage (send layout-manager 'text-storage))
		 (word-array (send storage 'words)))
	(send word-array 'count)))

(defmethod character-count ((doc document))
  (let* ((layout-manager (layout-manager doc))
		 (storage (send layout-manager 'text-storage))
		 (word-array (send storage 'characters)))
	(send word-array 'count)))

(defmethod page-count ((doc rtf-document))
  (send (objc-document doc) 'page-count))
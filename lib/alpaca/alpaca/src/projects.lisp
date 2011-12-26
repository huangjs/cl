;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: projects.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;; 
;;; multiple-document projects

(in-package "CCL")


;;; ======================================================================
;;; CLOS PROJECT MODEL
;;; ======================================================================

(defclass project ()
  (
   ;; documents must be kept in order, because the
   ;; order of documents in a project is user-determined
   (documents :accessor project-documents :initform nil :initarg :project-documents)))

(defmethod word-count ((proj project))
  (reduce #'+ (mapcar #'(lambda (d) (word-count d)) (project-documents proj))))

(defmethod character-count ((proj project))
  (reduce #'+ (mapcar #'(lambda (d) (character-count d)) (project-documents proj))))

;;; we don't count pages in non-rtf documents, 
;;; because we don't paginate them
(defmethod page-count ((proj project))
  (if (every #'(lambda (d) (typep d 'rtf-document)) (project-documents proj))
	  (reduce #'+ (mapcar #'(lambda (d) (page-count d)) (project-documents proj)))
	nil))
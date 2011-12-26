;;; ----------------------------------------------------------------
;;; DMAP-Lite
;;; ----------------------------------------------------------------
;;;
;;; Everything you wanted in a Direct Memory Access Parser and less.
;;;
;;; Change log
;;; ----------

;;; 06/07/2006: changed monitors to support local with-monitors macro,
;;;     merged remove-monitor and remove-all-monitors functionality [CKR]
;;; 06/06/2006: changed monitors to use tag ids, because equality not 
;;;    guaranteed for functions [CKR]
;;; 06/06/2006: Updated packaging code to standard CL2 [CKR]
;;; 11/16/1998 Added :ADD argument to call to find-instances [CKR]


;;; PACKAGES
;;; --------

(defpackage #:dmap
  (:use #:common-lisp #:tables #:mops)
  (:export #:parse #:defphrase
           #:reset-cseqs #:remove-all-cseqs #:remove-cseqs
           #:add-monitor #:remove-monitors
           #:with-monitors)
  )

(in-package #:dmap)

;;; ----------------------------------------------------------------
;;; Globals (external)
;;; ----------------------------------------------------------------

(defvar *dmap-concept-package* nil
  "Default package for DMAP concept symbols")

;;; ----------------------------------------------------------------
;;; Globals (internal)
;;; ----------------------------------------------------------------

(defvar *dmap-pos* 0 "Global text position for DMAP")

(defvar *monitors* nil)

;;; ----------------------------------------------------------------
;;; Structures
;;; ----------------------------------------------------------------

(defstruct cseq base seq start end slots)

(defstruct (monitor (:type list)) base tag function)


;;; ----------------------------------------------------------------
;;; Defining concept sequences
;;; ----------------------------------------------------------------

(deftable base-cseqs)
(deftable target-cseqs)

(defmacro defphrase (base &rest seq)
  (if (and (eql base (car seq)) (null (cdr seq)))
      (error "Can't DEFPHRASE ~S to itself" base)
      `(progn (install-cseq (make-cseq :base ',base :seq ',seq))
              ',seq)))

(defun install-cseq (cseq)
  (push cseq (target-cseqs (cseq-target cseq))))


;;; ----------------------------------------------------------------
;;; Removing concept sequences
;;; ----------------------------------------------------------------

(defun reset-cseqs ()
  (remove-cseqs
   #'(lambda (cseq) (not (null (cseq-start cseq))))))

(defun remove-all-cseqs ()
  (clear-table (base-cseqs))
  (clear-table (target-cseqs)))

(defun remove-cseqs (pred)
  (remove-base-cseqs pred)
  (remove-target-cseqs pred))

(defun remove-base-cseqs (pred)
  (map-table #'(lambda (base cseqs)
                 (setf (base-cseqs base)
                       (delete-if pred cseqs)))
             (base-cseqs)))

(defun remove-target-cseqs (pred)
  (map-table #'(lambda (target cseqs)
                 (setf (target-cseqs target)
                       (delete-if pred cseqs)))
             (target-cseqs)))
		     


;;; ----------------------------------------------------------------
;;; PARSE
;;; ----------------------------------------------------------------

(defun parse (sent &key package)
  (dolist (w sent)
    (increment-text-position)
    (reference w (text-position) (text-position) package)))

(defun reference (item start end package)
  (dolist (abst (all-absts-of item))
    (mapc #'(lambda (monitor)
              (when (eql abst (monitor-base monitor))
                (funcall (monitor-function monitor) item start end)))
          *monitors*)
    (advance-cseqs (target-cseqs abst) item start end package)))

(defun advance-cseqs (cseqs item start end package)
  (dolist (cseq cseqs)
    (when (cseq-applies-p cseq start)
      (advance-cseq cseq item start end package))))


(defun cseq-applies-p (cseq start)
  (or (null (cseq-end cseq))
      (= (1+ (cseq-end cseq)) start)))

(defun advance-cseq (cseq item start end package)
  (let ((base (cseq-base cseq))
        (seq (rest (cseq-seq cseq)))
        (slots (extend-slots cseq item))
        (start (or (cseq-start cseq) start)))
    (if (null seq)
        (reference-instances base slots start end package)
        (install-cseq (make-cseq :base base :seq seq :slots slots
                                 :start start :end (text-position))))))

(defun extend-slots (cseq item)
  (let ((spec (car (cseq-seq cseq)))
        (slots (cseq-slots cseq)))
    (if (consp spec)
        (if (abstp item (cseq-target cseq))
            slots
            (list* (car spec) item slots))
        slots)))

(defun reference-instances (base slots start end package)
  (dolist (instance (find-instances base slots :add t :package package))
    (reference instance start end package)))

(defun cseq-target (cseq)
  (let ((spec (car (cseq-seq cseq))))
    (if (consp spec)
        (let ((base (cseq-base cseq))
              (role (car spec)))
          (or (inherit-filler base role)
              (error "~S not a role in ~S" role base)))
        spec)))


(defun text-position () *dmap-pos*)
(defun increment-text-position () (incf *dmap-pos*))
(defun reset-text-position () (setq *dmap-pos* 0))


;;; ----------------------------------------------------------------
;;; Monitors
;;; ----------------------------------------------------------------

(defmacro with-monitors (monitors &rest body)
  `(let ((*monitors* (list ,@(mapcar #'expand-monitor monitors)))) ,@body))

(defun expand-monitor (monitor)
  (if (atom monitor) 
      (expand-monitor (list monitor))
    (destructuring-bind 
        (base &optional (tag :print) (function '#'print-monitor))
        monitor
      `(make-monitor :base ',base :tag ',tag :function ,function))))

(defun print-monitor (item start end)
  (declare (ignore start end))
  (print item))

(defun add-monitor (base &optional (tag :print) (function #'print-monitor))
  (remove-monitor base tag)
  (let ((monitor (make-monitor :base base :tag tag :function function)))
    (push monitor *monitors*)
    monitor))

(defun remove-monitors (&key (base nil base-p) (tag nil tag-p))
  (setq *monitors*
        (delete-if #'(lambda (monitor)
                       (and (or (null base)
                                (eql base (monitor-base monitor)))
                            (or (null tag-p)
                                (eql tag (monitor-tag monitor)))))
                   *monitors*)))

;;; ----------------------------------------------------------------
;;; End of module
;;; ----------------------------------------------------------------

(provide "dmap")



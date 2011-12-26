;;; ----------------------------------------------------------------
;;; An extended answer to the 1997 Qual Programming Question

;;; Main functions
;;;
;;;   (SCORE-FILE pathname) => undefined
;;;     Applies the code scoring functions in the list *STAT-FNS*
;;;     to a file of Lisp function definitions and prints a table of the results.
;;;
;;;   (SCORE-CODE code) => number
;;;     Calculates a score for the given code. Bigger is worse. Uses
;;;     the scoring patterns stored in *SCORE-PATS*.
;;;
;;; Note: SCORE-FILE only passes the function body to the scoring functions
;;; so that definitions are not penalized for their parameter lists.


;;; ----------------------------------------------------------------
;;; SCORE-FILE
;;; ----------------------------------------------------------------

(require "tables")
(use-package :tables)

(deftable score-fn)

(defparameter *max-name-width* 15)
(defparameter *min-col-width* 4)
(defparameter *stat-fns* '(max-length max-depth atom-count list-count score-code))

(defun score-file (file)
  (let ((widths (print-column-labels *stat-fns*)))
    (file-map #'(lambda (defn)
                  (when (definer-p defn)
                    (print-stats (second defn) 
                                 (calc-stats (nthcdr 3 defn))
                                 widths)))
              file)))

(defun definer-p (defn)
  (and (consp defn)
       (member (first defn) '(defun defmacro))))

(defun score-definition (defn)
  (let ((widths (print-column-labels *stat-fns*)))
    (print-stats (second defn) (calc-stats (nthcdr 3 defn)) widths)))

(defun score-list (list)
  (let ((widths (print-column-labels *stat-fns*)))
    (print-stats '--- (calc-stats list) widths)))


(defun calc-stats (form)
  (mapcar #'(lambda (stat-fn)
              (funcall stat-fn form))
          *stat-fns*))

;;; Stats printer

(defun print-column-labels (labels)
  (let ((widths (mapcar #'get-column-width labels)))
    (format t "~VT" *max-name-width* )
    (mapc #'(lambda (width label)
              (format t " ~VS" width label))
          widths labels)
    (terpri)
    (cons *max-name-width* widths)))

(defun get-column-width (label)
  (max *min-col-width* (length (string label))))

(defun print-stats (id stats widths)
  (let ((row-title (truncate-name id (first widths))))
    (format t "~VA" (pop widths) row-title)
    (mapc #'(lambda (width stat)
              (format t " ~VD" width stat))
          widths stats)
    (terpri)))

(defun truncate-name (id limit)
  (let ((s (string id)))
    (subseq s 0 (min limit (length s)))))


;;; File mapper

(defun file-map (fn file)
  (with-open-file (in file)
    (let ((eof (list nil)))
      (do ((form (read in nil eof) (read in nil eof)))
          ((eq form eof) nil)
        (funcall fn form)))))


;;; ----------------------------------------------------------------
;;; The scoring functions
;;; ----------------------------------------------------------------

;;; baseline metrics

(defun max-length (form)
  (cond ((atom form) 1)
        (t (reduce #'max form
                   :initial-value (length form)
                   :key #'max-length))))


(defun max-depth (form &optional (depth 0))
  (cond ((atom form) depth)
        (t (max (max-depth (car form) (1+ depth))
                (max-depth (cdr form) depth)))))

(defun atom-count (form)
  (cond ((atom form) 1)
        (t (reduce #'+ form :key #'atom-count))))

(defun list-count (form)
  (cond ((atom form) 0)
        (t (reduce #'+ form
                   :key #'list-count
                   :initial-value (length form)))))


;;; ----------------------------------------------------------------
;;; SCORE-CODE
;;; ----------------------------------------------------------------

(defparameter *score-fn* 'default-score-fn)
(defparameter *binding-scorer* 'length)

(defun default-score-fn (x)
  (if (atom x) 0 (length x)))


(defparameter *score-pats* 
  '((if 2 1 etc)
    (cond (2 1 etc) etc) 
    (let ((2 1) etc) 1 etc)
    ))


(defun score-code (code)
  (let ((match (get-pat-match code)))
    (cond ((null match) (get-base-score code))
          (t (score-pat-match code (rest match))))))

(defun get-pat-match (code)
  (some #'(lambda (pat) (match-score-pat pat code))
        *score-pats*))

;;; Matcher

(defun match-score-pat (pat code &optional (bindings (list pat)))
  (cond ((null bindings) nil)
        ((numberp pat) (add-binding pat code bindings))
        ((repeat-pat-p pat) (match-repeat-pat pat code bindings))
        ((atom pat) (if (eql pat code) bindings nil))
        ((atom code) nil)
        (t (match-score-pat (car pat) (car code)
                            (match-score-pat (cdr pat) (cdr code) 
                                             bindings)))))

(defun repeat-pat-p (pat)
  (and (consp pat) 
       (eql (second pat) 'etc)))

(defun match-repeat-pat (pat code bindings)
  (and (consp code)
       (let ((repeat-pat (first pat)))
         (reduce #'(lambda (bindings form)
                     (match-score-pat repeat-pat form bindings))
                 code
                 :initial-value bindings))))
                 

(defun add-binding (pat code bindings)
  (nconc bindings (list (cons pat code))))


;;; Score adder

(defun get-base-score (code)
  (let ((base-score (funcall *score-fn* code)))
    (if (atom code)
      base-score
      (reduce #'+ code 
              :key #'score-code
              :initial-value base-score))))


(defun score-pat-match (code bindings)
  (+ (funcall *score-fn* code) (score-bindings bindings)))
          
(defun score-bindings (bindings)
  (reduce #'+ bindings
          :key #'(lambda (binding)
                   (* (first binding) 
                      (score-code (cdr binding))))
          :initial-value (funcall *binding-scorer* bindings)))



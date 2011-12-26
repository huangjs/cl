;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic backtracker and parser example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (require "btrack")
;;; (pprint (state-parse (atn '(the boy saw the girl))))
;;;  or                         the block is in the box

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun btrack (alternatives donep)
  (do ((l alternatives
          (append (expand-alternative (car l)) (cdr l))))
      ((or (null l)
           (and (null (cdar l))
                (funcall donep (caar l))))
       (caar l))))

(defun expand-alternative (alternative)
  (when (cdr alternative)
    (let ((state (car alternative))
          (action (cadr alternative))
          (actions (cddr alternative)))
      (mapcar #'(lambda (new-choice) (append new-choice actions))
              (apply (car action) (cons state (cdr action)))))))

(provide "btrack")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "tables")
(use-package :tables)
(require "btrack")

(defstruct state sentence parse)

(defun atn (sentence)
  (btrack `((,(make-state :sentence sentence :parse nil)
             (parse s)))
          #'(lambda (state) (null (state-sentence state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse (state net-name)
  `((,(make-state :sentence (state-sentence state)
                  :parse (cons net-name (state-parse state)))
     ,@(net-of net-name)
     (done ,net-name))))

(defun seq (state &rest actions)
  `((,state ,@actions)))

(defun done (state name)
  `((,(make-state :sentence (state-sentence state)
                  :parse (collect-constituent (state-parse state)
                                              name)))))

(defun category (state cat)
  (let ((sentence (state-sentence state)))
    (and (categoryp (car sentence) cat)
         `((,(make-state :sentence (cdr sentence)
                         :parse (cons `(,cat ,(car sentence))
                                      (state-parse state))))))))

(defun either (state &rest actions)
  (mapcar #'(lambda (action) `(,state ,action))
          actions))

(defun optional* (state action)
  `((,state ,action (optional* ,action))
    (,state)))

(defun optional (state action)
  `((,state ,action) (,state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-constituent (parse-tree name)
  (let ((l (member name parse-tree)))
    (if (null l)
        (error "constituent not found: ~s" name)
        (cons (reverse (ldiff parse-tree (cdr l)))
              (cdr l)))))

(defun add-constituent (state constituent)
  (push constituent (state-parse state)))

(defun categoryp (word cat)
 (member cat (categories-of word)))

(deftable categories-of)

(defmacro defword (word &rest cats)
 `(progn (setf (categories-of ',word) ',cats)
         ',word))

(deftable net-of)

(defmacro defnet (name &rest net)
 `(progn (setf (net-of ',name) ',net)
         ',name))

(defword john proper-noun)
(defword you pronoun)
(defword a det)
(defword the det)
(defword boy noun)
(defword saw noun verb)
(defword girl noun)
(defword box noun verb)
(defword is to-be aux)
(defword did aux verb)
(defword block noun verb)
(defword table noun verb)
(defword in prep)
(defword red adj)
(defword not neg)

(defnet s
    (either
       (seq (parse np) (parse vp))
       (seq (category to-be)
            (parse np)
            (parse pred))))

(defnet np
    (either (category proper-noun)
            (category pronoun)
            (seq (category det)
                 (optional* (category adj))
                 (category noun)
                 (optional* (parse pp)))))

(defnet vp
    (either (seq (optional (seq (category aux)
                                (category neg)))
                 (category verb)
                 (parse np))
            (seq (category to-be)
                 (optional (category neg))
                 (parse pred))))

(defnet pred
    (either (category adj)
            (parse pp)))

(defnet pp
  (category prep)
  (parse np))

(provide "atn")


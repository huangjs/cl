;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bitcomp.lisp
;;;; Purpose:       Bit compression.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: bitcomp.lisp,v 1.2 2003/10/03 16:59:20 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.bitcomp)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  
  (define-condition null-compressed ()
    ((message :reader message :initform "" :initarg :message))
    (:report (lambda (condition stream)
               (format stream "Warning: ~a~%" (message condition))))))

(defconstant +null-rep+ (make-condition 'null-compressed))

;;;;
;;;; Representation Layer.
;;;;

;;;; DATA DESCRIPTION: field data in struct compressed is a queue.

;;; Usage: (make-compressed :list '(0 1 0 0 1 1 1 0 0 1))
;;;        (make-compressed :number 11)
;;;        (make-compressed :comp '((1 . 2) (2 . 3))) 
;;;        (make-compressed :rep '((1 . 2) (4 . 3))) 
(defstruct (compressed
            ;; A BOA-constructor
            (:constructor 
             make-compressed
             (&key 
              (list 'list)
              (number 'number)
              (comp 'comp)
              (rep 'rep)
              (data 
               (progn
                 (cond ((cl:not (eql number 'number))
                        (compress 
                         (coerce (extract-bits number) 'vector)))
                       ((cl:not (eql list 'list))
                        (compress (coerce list 'vector)))
                       ((cl:not (eql comp 'comp))
                        (merge comp))
                       ((cl:not (eql rep 'rep))
                        rep)
                       (t 
                        (error "Bad args for make-compressed"))))))))
  "Compressed bit strings are represented in the form of dotted pairs.
Example: The form ((1 . 3) (6 . 2)) means that bits 1 through 3 and bits 
6 through 7 are 1."
  data)

(defun get-compressed-pairs (compressed &key (fresh t))
  "Get the list of compressed pairs that represent the compressed bit string
<compressed>. If <fresh> is true, make a copy of the list; otherwise, the 
internal list of compressed pairs is returned."
  (when (cl:not (compressed-p compressed))
    (error "First arg is not of type compressed."))
  (if fresh
      (rsm.queue:queue->list (compressed-data compressed))
    (rsm.queue:nget-list (compressed-data compressed))))


(defun extract-bits (num)
  "Extract the bits of number <num>. Returns a list of 
0's and 1's. 0's and 1's are ordered from smallest to largest."
  (let ((bin-string 
         (let ((*print-base* 2)
               (*print-radix* t))
           (format nil "~s" num)))
        (result nil))
    (loop 
        :for char :across bin-string 
        :for i :from 0 
        :when (> i 1) :do
          (if (eql char #\0)
              (push 0 result)
            (push 1 result))
        :finally (return result))))


(defun compress (vec)
  "Compress vector <vec> of 1's and 0's into a queue of 
pairs (begin . duration). Returns the queue."
  (let ((queue (rsm.queue:create))
        (len (1- (length vec))))
    (loop 
        :with last = 0 
        :with last-pos = 0 
        :with on = nil  
        :with n = 0
        :for elem :across vec
        :for i :from 0 :do
          (cond  ((cl:and on            ; At the end of a string of ones. 
                          (= i len)
                          (= elem 1))
                  (rsm.queue:enqueue (cons (1+ last-pos) (1+ n)) queue))
                 ((cl:and (cl:not on)   ; at end and a one occurs.
                          (= i len)
                          (= elem 1))
                  (rsm.queue:enqueue (cons (1+ i) 1) queue))
                 ((cl:and               ; 0 1 pattern on
                   (= last 0)
                   (= elem 1))
                  (setf on t)
                  (setf last-pos i)
                  (setf n 1))
                 ((cl:and on            ; 1 0 pattern off
                          (= elem 0))
                  (rsm.queue:enqueue (cons (1+ last-pos) n) queue)
                  (setf on nil))
                 (on
                  (incf n)))
          (setf last elem))
    queue))
  




(defun merge (comps)
  "Merge compressed bit strings.
Example: (rsm.bitcomp:merge '((1 . 3) (4 . 2) (6 . 2)))
          yields '((1 . 7)) 
         (actually a queue which has this as a list.)
Assumption: <comps> is a list of association pairs ordered 
             (using the first element of the pair) from lowest to highest."
  (labels ((merge-helper (start duration comps queue)
             (cond ((null comps)
                    (rsm.queue:enqueue (cons start duration) queue)
                    queue)
                   (t
                    (let ((cs (caar comps))
                          (cd (cdar comps)))
                      (cond ((>= (+ start duration) (+ cs cd))
                             (merge-helper
                              start duration (cdr comps) queue))
                            ((>= (+ start duration) cs)
                             (merge-helper 
                              start (- (+ cs cd) start) (cdr comps) queue))
                            (t
                             (rsm.queue:enqueue (cons start duration) queue)
                             (merge-helper 
                              (caar comps) (cdar comps) 
                              (cdr comps) queue))))))))
    (when comps
      (merge-helper 
       (caar comps) (cdar comps) (cdr comps) (rsm.queue:create)))))


(defun intersect (comps)
  "Intersect compressed bit strings.
 Example: (rsm.bitcomp:intersect '((1 . 3) (3 . 2) (6 . 2)))
          yields '((3 . 1)).
          (actually a queue which has this as a list.)
 Assumption: <comps> is a list of association pairs ordered 
             (using the first element of the pair) from lowest to highest."
  (labels ((intersect-helper (start duration comps queue)
             (cond ((null comps)
                    (rsm.queue:nget-list queue))
                   (t
                    (let ((cs (caar comps))
                          (cd (cdar comps)))
                      (cond ((>= (+ start duration) (+ cs cd))
                             (rsm.queue:enqueue (cons cs cd) queue)
                             (intersect-helper 
                              start duration (cdr comps) queue))
                            ((> (+ start duration) cs)
                             (rsm.queue:enqueue 
                              (cons cs 
                                    (- (+ start duration) cs)) queue)
                             (intersect-helper 
                              (+ start duration) 
                              (- (+ cs cd) (+ start duration))
                              (cdr comps) queue))
                            (t
                             (intersect-helper 
                              (caar comps) (cdar comps) 
                              (cdr comps) queue))))))))
    (when comps
      (merge 
       (intersect-helper 
        (caar comps) (cdar comps) (cdr comps) (rsm.queue:create))))))


(defun diff (comps)
  "Diff compressed bit strings.
 Example: (rsm.bitcomp:diff '((1 . 3) (3 . 2) (6 . 2)))
          yields '((1 . 2) (4 . 1) (6 . 2)).
          (actually a queue which has this as a list.)
 Assumption: <comps> is a list of association pairs ordered 
             (using the first element of the pair) from lowest to highest."
  (labels ((diff-helper (start duration comps queue)
             (if (cl:not (null comps))
               (let ((cs (caar comps))
                     (cd (cdar comps)))
                 (cond ((= start cs)
                        (cond ((= duration cd)
                               (diff-helper 
                                (caadr comps) (cdadr comps) 
                                (cddr comps) queue))
                              ((> duration cd)
                               (diff-helper 
                                (+ cs cd) (- duration cd) 
                                (cdr comps) queue))
                              (t
                               (diff-helper 
                                (+ cs duration) (- cd duration) 
                                (cdr comps) queue))))
                       (t
                        (let ((st-end (+ start duration))
                              (csd-end (+ cs cd)))
                          (cond ((< (1- st-end) cs)
                                 (rsm.queue:enqueue (cons start duration) queue)
                                 (diff-helper 
                                  (caar comps) (cdar comps) (cdr comps) queue))
                                ((> csd-end st-end)
                                 (rsm.queue:enqueue (cons start (- cs start)) 
                                              queue)
                                 (diff-helper 
                                  st-end (- csd-end st-end) (cdr comps) queue))
                                ((> st-end csd-end)
                                 (rsm.queue:enqueue (cons start 
                                                    (- cs start)) queue)
                                 (diff-helper 
                                  csd-end (- st-end csd-end) (cdr comps) queue))
                                (t
                                 (rsm.queue:enqueue 
                                  (cons start (- cs start)) queue)
                                 (diff-helper 
                                  (caadr comps) (cdadr comps) 
                                  (cddr comps) queue)))))))
               (progn
                 (when (cl:or start duration)
                   (rsm.queue:enqueue (cons start duration) queue))
                 (rsm.queue:nget-list queue)))))
    (when comps
      (merge 
       (diff-helper (caar comps) (cdar comps) (cdr comps) (rsm.queue:create))))))


(defun exclude (begin end comps)
  "Exclude compressed bit strings.
 Returns a compressed bit string by complimenting the bit string 
 represented by <comps> in the closed interval [<begin>, <end>].
 Example: (rsm.bitcomp:exclude 1 10 '((1 . 3) (3 . 2) (6 . 2)))
          (actually a queue which has this as a list.)
 Assumption: <comps> is a list of association pairs ordered 
             (using the first element of the pair) from lowest to highest."
  (labels ((exclude-helper (last comps queue)
             (if (cl:not (null comps))
                 (let ((cs (caar comps))
                       (cd (cdar comps)))
                   (cond ((< last cs)
                          (rsm.queue:enqueue (cons last (- cs last)) queue)
                          (exclude-helper 
                           (+ cs cd) (cdr comps) queue))
                         (t
                          (exclude-helper 
                           (+ cs cd) (cdr comps) queue))))
               (progn
                 (rsm.queue:enqueue (cons last (1+ (- end last))) queue)
                 queue))))
    (if comps
        (exclude-helper begin comps (rsm.queue:create))
        (rsm.queue:create (cons begin end)))))


(declaim (inline get-rep-as-list))
(defun get-rep-as-list (rep)
  "Get the list of start . duration pairs that represent the 
representation <rep>."
  (rsm.queue:nget-list (compressed-data rep)))



(defun sort-reps (reps)
  "Sort reps by their span; from smallest to largest span."
  (sort reps #'(lambda (bep1 bep2)
                 (< (- (cdr bep1) (car bep1))
                    (- (cdr bep2) (car bep2))))
        :key #'get-begin-end))


(defun  get-restricted-list (data-as-list begin-end-pair)
  "Return the sublist that just contains the interval described 
by <begin-end-pair>."
  (let ((queue (rsm.queue:create))
        (begin (car begin-end-pair))
        (end (cdr begin-end-pair)))
    (let ((begin-cursor
           (do ((b-cursor data-as-list (cdr b-cursor))
                (b-last data-as-list))
               ((null b-cursor))
             (when (> (caar b-cursor) begin)
               (return b-last))
             (setf b-last b-cursor))))
      (do ((e-cursor begin-cursor (cdr e-cursor)))
          ((null e-cursor) (rsm.queue:nget-list queue))
        (rsm.queue:enqueue (car e-cursor) queue)
        (when (> (1- (+ (caar e-cursor) (cdar e-cursor))) end)
          (return-from get-restricted-list (rsm.queue:nget-list queue)))))))

(defun get-begin-end (rep)
  "Get the beginning and ending index of the representation <rep>."
  (let ((queue (compressed-data rep)))
    (cons (car (rsm.queue:get-first queue))
          (1- 
           (+ (car (rsm.queue:get-last queue))
              (cdr (rsm.queue:get-last queue)))))))

(defun get-number-of-bits (rep)
  "Get the number of bits in the representation <rep>."
  (let ((lst (rsm.queue:nget-list (compressed-data rep))))
    (loop 
        :for elem :in lst 
        :sum (cdr elem))))


(defun make-rep-from-begin-end-pair (begin end)
  "Make a compressed bit string from a begin/end pair."
  (let ((queue (rsm.queue:create)))
    (rsm.queue:enqueue (cons begin (- end begin)) queue)
    (make-compressed :rep queue)))


;;;;
;;;; Abstraction Layer
;;;;


(defun or (&rest reps)
  "Computes the representation of the ORING of <reps>.
Example: (rsm.bitcomp:or rep1 rep2)
          where rep1 represents '((1 . 3) (5 . 2)) 
          and rep2 represents '((4 . 2) (10 . 2))
          yields ((1 . 6) (10 . 2)).
          (actually a compressed type which has this as a
           run length encoded list.)"
  (make-compressed 
   :rep
   (merge 
    (sort 
     (reduce #'nconc
             (mapcar #'(lambda (rep)
                         (copy-list (get-rep-as-list rep))) reps))
     #'< :key #'car))))

      

(defun and (&rest reps)
  "Computes the representation of the ANDING of <reps>.
Example: (rsm.bitcomp:and rep1 rep2)
          where rep1 represents '((1 . 3) (5 . 2))
          and rep2 represents '((4 . 2) (10 . 2))
          yields ((5 . 1)).
          (actually a compressed type which has this as a
           run length encoded list.)"
    (handler-case 
        (mapcar #'(lambda (rep)
                    (when (cl:not (get-rep-as-list rep))
                      (signal +null-rep+)))
                reps)
      (null-compressed () 
        (return-from and (make-compressed :rep (rsm.queue:create)))))
    (let ((begin-end
           (handler-case 
               (let ((begin-ends
                      (mapcar #'(lambda (rep)
                                  (get-begin-end rep))
                              reps)))
                 (let ((begin-end 
                        (loop 
                            :for pair :in begin-ends 
                            :maximize (car pair) :into begin 
                            :minimize (cdr pair) :into end
                            :finally (return (cons begin end)))))
                   (if (> (car begin-end) (cdr begin-end))
                         (signal +null-rep+)
                     begin-end)))
             (null-compressed () 
               (return-from and (make-compressed :rep (rsm.queue:create)))))))
      (handler-case 
          (intersect 
           (sort 
            (reduce 
             #'nconc
             (mapcar #'(lambda (rep)
                         (let ((lst 
                                (get-restricted-list 
                                 (copy-list 
                                  (get-rep-as-list rep))
                                 begin-end)))
                           (if lst
                               lst
                             (signal +null-rep+))))
                     (sort-reps reps)))
            #'< :key #'car))
        (null-compressed () (make-compressed :rep (rsm.queue:create)))
        (:no-error (result)
          (return-from and (make-compressed :rep result))))))
  

(defun xor (&rest reps)
  "Computes the representation of the XORING of <reps>.
Example: (rsm.bitcomp:xor rep1 rep2)
          where rep1 represents '((1 . 3) (5 . 2))
          and rep2 represents '((4 . 2) (10 . 2))
          yields '((1 . 4) (6 . 1) (10 . 2)).
          (actually a compressed type which has this as a
           run length encoded list.)"
  (let ((result 
         (sort 
          (reduce 
           #'nconc
           (mapcar #'(lambda (rep)
                       (let ((data (get-rep-as-list rep)))
                         (copy-list data)))
                   reps))
          #'< 
          :key #'car)))
    (make-compressed :rep (diff result))))


(defun not (begin end rep)
  "Computes the new representation of the COMPLEMENT of <rep> in the range 
 [begin, end].
 Example: (rsm.bitcomp:not 0 11 rep)
           where rep represents '((2 . 1) (5 . 3) (10 . 1))
           yields '((0 . 2) (3 . 2) (8 . 2) (11 . 1)).
          (actually a compressed type which has this as a
           run length encoded list.)"
  (let ((data (compressed-data rep)))
    (if data
        (make-compressed 
         :rep
         (exclude begin end 
                  (sort 
                   (copy-list (get-rep-as-list rep))
                   #'< :key #'car)))
      (make-rep-from-begin-end-pair begin end))))


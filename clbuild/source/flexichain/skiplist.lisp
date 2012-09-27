;;; Skiplist
;;; Skiplist data structure definition
;;;
;;; Copyright (C) 2004  Robert Strandh (strandh@labri.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(in-package :skiplist)

(defclass skiplist ()
  ((maxlevel :initarg :maxlevel :initform 25 :reader maxlevel)
   (start :reader start)
   (lessfun :initarg :lessfun :initform #'< :reader lessfun)
   (current-maxlevel :initform -1 :accessor current-maxlevel)))

(defmethod initialize-instance :after ((s skiplist) &rest args)
  (declare (ignore args))
  (with-slots (maxlevel start) s
     (assert (plusp maxlevel))
     (setf start (make-array (+ maxlevel 3) :initial-element nil))))

(defmethod print-object ((s skiplist) stream)
  (print-unreadable-object (s stream :type t)
    (with-slots (start) s
       (when (entry-next start 0)
         (loop for entry = (entry-next start 0) then (entry-next entry 0)
               do (format stream "(~W ~W) "
                                 (entry-key entry)
                                 (entry-obj entry))
               until (last-entry-p start entry 0))))))

(defun entry-obj (entry)
  (aref entry 0))

(defun (setf entry-obj) (obj entry)
  (setf (aref entry 0) obj))

(defun entry-key (entry)
  (aref entry 1))

(defun (setf entry-key) (key entry)
  (setf (aref entry 1) key))

(defun entry-next (entry level)
  (aref entry (+ level 2)))

(defun (setf entry-next) (next entry level)
  (setf (aref entry (+ level 2)) next))

(defun key-< (skiplist key1 key2)
  (funcall (lessfun skiplist) key1 key2))

(defun key-<= (skiplist key1 key2)
  (not (funcall (lessfun skiplist) key2 key1)))

(defun key-= (skiplist key1 key2)
  (and (not (funcall (lessfun skiplist) key1 key2))
       (not (funcall (lessfun skiplist) key2 key1))))

(defun key-> (skiplist key1 key2)
  (funcall (lessfun skiplist) key2 key1))

(defun key->= (skiplist key1 key2)
  (not (funcall (lessfun skiplist) key1 key2)))

(defun last-entry-p (start entry level)
  (eq (entry-next entry level) (entry-next start level)))

(defun skiplist-empty-p (skiplist)
  (= (slot-value skiplist 'current-maxlevel) -1))

;;; From a given entry return an entry such that the key of the
;;; following one is the smallest one greater than or equal to the key
;;; given; or the last element if no such element exists. 

(defun find-entry-level (skiplist entry level key)
  (with-slots (start) skiplist
     (loop until (or (key-= skiplist (entry-key (entry-next entry level)) key)
                     (and (key-< skiplist (entry-key entry) key)
                          (key-> skiplist (entry-key (entry-next entry level)) key))
                     (and (key-< skiplist (entry-key entry) key)
                          (key-< skiplist (entry-key (entry-next entry level)) key)
                          (last-entry-p start entry level)
                          (eq (entry-next entry level) (entry-next start level)))
                     (and (key-> skiplist (entry-key entry) key)
                          (key-> skiplist (entry-key (entry-next entry level)) key)
                          (last-entry-p start entry level)))
           do (setf entry (entry-next entry level))))
  entry)
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find

(defun skiplist-find (skiplist key)
  (when (skiplist-empty-p skiplist)
    (return-from skiplist-find (values nil nil)))
  (with-slots (current-maxlevel start) skiplist
     (let ((entry (entry-next start current-maxlevel)))
       (loop for l downfrom current-maxlevel to 0
             do (setf entry (find-entry-level skiplist entry l key)))
       (if (key-= skiplist (entry-key (entry-next entry 0)) key)
           (values (entry-obj (entry-next entry 0)) t)
           (values nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find first

(defun skiplist-find-first (skiplist)
  (assert (not (skiplist-empty-p skiplist)))
  (with-slots (start) skiplist
     (values (entry-obj (entry-next start 0))
             (entry-key (entry-next start 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert

(defun pick-a-level (maxlevel)
  (loop for level from 0 to maxlevel
        while (zerop (random 2))
        finally (return level)))

(defun make-entry (level key obj)
  (let ((entry (make-array (+ level 3) :initial-element nil)))
    (setf (aref entry 0) obj
          (aref entry 1) key)
    entry))

(defun (setf skiplist-find) (obj skiplist key)
  (with-slots (current-maxlevel start) skiplist
     (if (second (multiple-value-list (skiplist-find skiplist key)))
         (let ((entry (entry-next start current-maxlevel)))
           (loop for l downfrom current-maxlevel to 0
                 do (setf entry (find-entry-level skiplist entry l key)))
           (setf (entry-obj (entry-next entry 0)) obj))
         (let* ((level (pick-a-level (maxlevel skiplist)))
                (new-entry (make-entry level key obj)))
           (loop for l downfrom level above current-maxlevel
                 do (setf (entry-next start l) new-entry
                          (entry-next new-entry l) new-entry))
           (let ((entry (entry-next start current-maxlevel)))
             (loop for l downfrom current-maxlevel above level
                   do (setf entry (find-entry-level skiplist entry l key)))
             (loop for l downfrom (min level current-maxlevel) to 0
                   do (setf entry (find-entry-level skiplist entry l key))
                      (setf (entry-next new-entry l) (entry-next entry l)
                            (entry-next entry l) new-entry)
                      (when (key-< skiplist key (entry-key entry))
                        (setf (entry-next start l) new-entry))))
           (setf current-maxlevel (max current-maxlevel level)))))
  skiplist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete

(defun skiplist-delete (skiplist key)
  (assert (second (multiple-value-list (skiplist-find skiplist key))))
  (with-slots (current-maxlevel start) skiplist
     (let ((entry (entry-next start current-maxlevel)))
       (loop for l downfrom current-maxlevel to 0
             do (setf entry (find-entry-level skiplist entry l key))
             when (key-= skiplist (entry-key (entry-next entry l)) key)
               do (cond ((key-= skiplist (entry-key entry) key)
                         (setf (entry-next start l) nil))
                        ((key-< skiplist (entry-key entry) key)
                         (setf (entry-next entry l)
                               (entry-next (entry-next entry l) l)))
                        (t (setf (entry-next entry l)
                                 (entry-next (entry-next entry l) l))
                           (setf (entry-next start l)
                                 (entry-next entry l)))))
       (loop while (and (null (entry-next start current-maxlevel))
                        (>= current-maxlevel 0))
             do (decf current-maxlevel))))
  skiplist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slide keys

(defun update-interval (skiplist entry to update-key)
  (with-slots (start) skiplist
     (flet ((update-entry (entry)
              (setf (entry-key entry)
                    (funcall update-key (entry-key entry) (entry-obj entry)))))
       (loop while (key-<= skiplist (entry-key entry) to)
             do (update-entry entry)
             until (last-entry-p start entry 0)
             do (setf entry (entry-next entry 0))))))

(defun skiplist-slide-keys (skiplist from to update-key)
  (unless (skiplist-empty-p skiplist)
    (with-slots (current-maxlevel start) skiplist
       (let ((entry (entry-next start current-maxlevel)))
         (loop for l downfrom current-maxlevel to 0
               do (setf entry (find-entry-level skiplist entry l from)))
         (when (key->= skiplist (entry-key (entry-next entry 0)) from)
           (update-interval skiplist (entry-next entry 0) to update-key)))))
  skiplist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rotate prefix

(defun skiplist-rotate-prefix (skiplist to update-key)
  (unless (skiplist-empty-p skiplist)
    (with-slots (current-maxlevel start) skiplist
       (let ((entry (entry-next start current-maxlevel)))
         (loop for l downfrom current-maxlevel to 0
               do (setf entry (find-entry-level skiplist entry l to)))
         (when (key-= skiplist (entry-key (entry-next entry 0)) to)
           (setf entry (entry-next entry 0)))
         (cond ((and (key-> skiplist (entry-key entry) to)
                     (key-> skiplist (entry-key (entry-next entry 0)) to))
                nil)
               ((and (key-<= skiplist (entry-key entry) to)
                     (key-<= skiplist (entry-key (entry-next entry 0)) to))
                (update-interval skiplist (entry-next entry 0) to update-key))
               (t (update-interval skiplist (entry-next start 0) to update-key)
                  (loop with entry = (entry-next entry 0)
                        for level from 0 to current-maxlevel
                        do (loop until (>= (length entry) (+ 3 level))
                                 do (setf entry (entry-next entry (1- level))))
                           (setf (entry-next start level) entry)))))))
  skiplist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rotate suffix

(defun update-interval-to-end (skiplist entry update-key)
  (with-slots (start) skiplist
     (flet ((update-entry (entry)
              (setf (entry-key entry)
                    (funcall update-key (entry-key entry) (entry-obj entry)))))
       (loop do (update-entry entry)
             until (last-entry-p start entry 0)
             do (setf entry (entry-next entry 0))))))

(defun skiplist-rotate-suffix (skiplist from update-key)
  (unless (skiplist-empty-p skiplist)
    (with-slots (current-maxlevel start) skiplist
       (let ((entry (entry-next start current-maxlevel)))
         (loop for l downfrom current-maxlevel to 0
               do (setf entry (find-entry-level skiplist entry l from)))
         (cond ((and (key-< skiplist (entry-key entry) from)
                     (key-< skiplist (entry-key (entry-next entry 0)) from))
                nil)
               ((and (key->= skiplist (entry-key entry) from)
                     (key->= skiplist (entry-key (entry-next entry 0)) from))
                (update-interval-to-end skiplist (entry-next entry 0) update-key))
               (t (update-interval-to-end skiplist (entry-next entry 0) update-key)
                  (loop with entry = (entry-next entry 0)
                        for level from 0 to current-maxlevel
                        do (loop until (>= (length entry) (+ 3 level))
                                 do (setf entry (entry-next entry (1- level))))
                           (setf (entry-next start level) entry)))))))
  skiplist)

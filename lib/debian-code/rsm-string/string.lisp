;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          string.lisp
;;;; Purpose:       String Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: string.lisp,v 1.6 2003/10/10 14:38:11 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:rsm.string)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(declaim (inline string->number))

(defun string->number (str)
  "Convert a string <str> representing a number to a number. A second value is
returned indicating the success of the conversion.
Example: (rsm.string:string->number \"123\")
          123
          t"
  (let ((*read-eval* nil))
    (let ((num (read-from-string str)))
      (values num (numberp num)))))


(declaim (ftype (function (string 
                           &key 
                           (:delims string) 
                           (:reverse t) 
                           (:missing-value-marker t)) list) split))

(defun split (str &key (delims " ") 
                       (reverse nil) 
                       (missing-value-marker nil))
  "Returns a list of strings formed by splitting <str> at any character
contained in <delims>. If <missing-value-marker> is non-nil, then
<missing-value-marker> is used when no text is found between delimiters.
Example: (rsm.string:split \"abc def ghi\" :delims \" \")
         (\"abc\" \"def\" \"ghi\")"
  (labels 
      ((rec (str acc)
         (let ((pos (position-if #'(lambda (ch)
                                     (position ch delims)) str)))
           (if pos
               (rec (subseq str (1+ pos)) 
                    (cond ((> pos 0)
                           (cons (subseq str 0 pos) acc))
                          (missing-value-marker
                           (cons missing-value-marker acc))
                          (t
                           acc)))
             (if reverse
                 (cons str acc)
               (nreverse (cons str acc)))))))
    (delete "" (rec str nil) :test #'string-equal)))


(declaim (ftype (function (list &key (:join-string t)) list) join))

(defun join (str-list &key (join-string nil))
  "Returns a new string formed by joining all the strings in <str-list>. 
If <join-string> is non-nil, it is used as a joining string between 
elements of the list.
Example: (rsm.string:join '(\"abc\" \"def\" \"ghi\") :join-string \"_\")
         \"abc_def_ghi\""
  (cond ((atom str-list) str-list)
        (t
         (reduce #'(lambda (s1 s2)
                     (concatenate 'string s1 join-string s2)) str-list))))


(declaim (ftype (function ((list string) (list string) 
                                         &key (function t)) list)
                contains))

(defun contains (str-list elem-list &key (contain-meaning #'some))
  "Returns all strings in <str-list> that contain strings in <elem-list>; where
the meaning of contains is provided in the function <contain-meaning>.
<contain-meaning> must take a function and a list as an argument.
Example: (rsm.string:contains '(\"abc\" \"def\" \"ghi\") '(\"bc\" \"gh\")) 
         (\"abc\" \"ghi\")"
  (loop for str in str-list 
      if (funcall contain-meaning 
                  #'(lambda (elem)
                      (search elem str)) elem-list)
      collect str))


(declaim (ftype (function (list string &key (:unique t)) list)
                does-not-contain))

(defun does-not-contain (str-list str-elems &key (unique nil))
  "Return a list of strings from the string list, <str-list>, that do not
contain any of the strings from the list of strings <str-elems.> If unique is
non nil, then the resulting list will remove duplicates. In any event, the
order of the list may change.
Example: (rsm.string:does-not-contain '(\"abc\" \"def\" \"ghi\") '(\"ab\" \"gh\"))
         (\"def\")"
  (let ((filtered  (set-difference str-list str-elems 
                                   :test #'(lambda (s1 s2) (search s2 s1)))))
    (if unique
        (delete-duplicates filtered :test #'string=)
      filtered)))


(defun fluff-string (str about-string &key (fluff-char #\Space))
  "Return a new string that wraps the character <fluff-char> around every
character in the string <about-string>.
Example: (rsm.string:fluff-string \"123+345+567\" \"+\" :fluff-string #\Space )
          \"123 + 345 + 567\""
  (let ((new-str (make-string (* 3 (length str)) :initial-element #\ )))
    (loop 
        with k = 0 
        for char across str 
        for i from 0 do
          (if (position char about-string)
              (progn
                (setf (char new-str k) fluff-char)
                (incf k)
                (setf (char new-str k) char)
                (incf k)
                (setf (char new-str k) fluff-char))
            (setf (char new-str k) (char str i)))
          (incf k))
    (string-trim (make-string 1 :initial-element fluff-char) new-str)))

(defun string->file (string file-name)
  "Writes the string, <string>, out to the file, <file-name>. If the 
file exists it will be replaced."
  (with-open-file (str file-name :direction :output :if-exists :supersede)
    (format str "~a" string)))

(defun file->string (file-name)
  "Reads file <file-name> returning a string of its contents."
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream file-name :direction :input)
      (do ((char (read-char stream nil nil)
                 (read-char stream nil nil)))
          ((null char))
        (vector-push-extend char result)))
    (coerce result 'string)))


(declaim (ftype (function (string 
                           &key 
                           (:delims string)
                           (:missing-value-marker t)) list) file->string-list))

(defun file->string-list (file-name &key (delims " ")
                                         (missing-value-marker nil))
  "Takes a file name and a key word of delimiters and returns a list of
strings that are delimited by any character in <delims>. If 
<missing-value-marker> is non nil, then <missing-value-marker> 
will be used for missing values."
  (let (result)
    (with-open-file (stream file-name :direction :input)
      (do ((line (read-line stream nil :eof)
                 (read-line stream nil :eof)))
          ((eql line :eof))
        (let ((filtered-line 
               (loop 
                   for str in (split line :delims delims
                                     :missing-value-marker missing-value-marker)
                   collect str)))
          (when filtered-line
            (setf result (nconc result filtered-line))))))
    result))


(declaim (ftype (function (string &key 
                                  (:delims string) 
                                  (:missing-value-marker t)) list)
                file->string-list))

(defun file->number-list (file-name &key (delims " ")
                                         (missing-value-marker nil))
  "Takes a file name and a key word of delimiters and returns a list of
strings that are delimited by any character in <delims>. If 
<missing-value-marker> is non nil, then <missing-value-marker> will be used
for missing values."
  (let (result)
    (with-open-file (stream file-name :direction :input)
      (do ((line (read-line stream nil :eof)
                 (read-line stream nil :eof)))
          ((eql line :eof))
        (let ((filtered-line 
               (loop with f-line = nil
                   for str in (split line :delims delims
                                     :missing-value-marker missing-value-marker)
                   if (equal missing-value-marker str) do
                     (push str f-line)
                   else do
                        (push (parse-integer str) f-line)
                   finally (return (nreverse f-line)))))
          (when filtered-line
            (setf result (nconc result filtered-line))))))
    result))


(declaim (ftype (function (string 
                           &key 
                           (:delims string)
                           (:missing-value-marker t)
                           (:header t)) list)
                file->string-table))

(defun file->string-table (file-name &key (delims " ")
                                          (missing-value-marker nil)
                                          (header nil))
  "Takes a file name and a key word of delimiters and returns a table - 
a list of lists of strings. The rows are the lines of the file; within 
each row, the nth column element is the the nth (non-null) delimited string. 
If <missing-value-marker> is non nil, then <missing-value-marker> will be used
for missing values. If <header> is non-nil and a number, the first <header>
lines will be discarded. Otherwise, if <header> is non-nil, then the first
line will be discarded."
  (let (result)
    (with-open-file (stream file-name :direction :input)
      (cond ((numberp header)
             (loop repeat header do 
                   (read-line stream nil :eof)))
            (header
             (read-line stream nil :eof))
            (t ))
      (do ((line (read-line stream nil :eof)
                 (read-line stream nil :eof)))
          ((eql line :eof))
        (let ((filtered-line 
               (loop
                   for str in (split line :delims delims
                                     :missing-value-marker missing-value-marker)
                   collect str)))
          (when filtered-line
            (push filtered-line result)))))
    (nreverse result)))

(declaim (ftype (function (string 
                           &key 
                           (:delims string)
                           (:missing-value-marker t)
                           (:header t)) list)
                file->string-table))

(defun file->number-table (file-name &key (delims " ") 
                                          (missing-value-marker nil)
                                          (header nil))
  "Takes a file name and a a key word of delimiters and returns a table - 
a list of lists of numbers. The rows are the lines of the file; within 
each row, the nth column element is the nth (non-null) number. If 
<missing-value-marker> is non nil, then <missing-value-marker> will be used
for missing values. If <header> is non-nil and a number, the first <header>
lines will be discarded. Otherwise, if <header> is non-nil, then the first
line will be discarded."
  (let (result)
    (with-open-file (stream file-name :direction :input)
      (cond ((numberp header)
             (loop repeat header do 
                   (read-line stream nil :eof)))
            (header
             (read-line stream nil :eof))
            (t ))
      (do ((line (read-line stream nil :eof)
                 (read-line stream nil :eof)))
          ((eql line :eof))
        (let ((filtered-line 
               (loop 
                   with f-line = nil
                   for str in (split line :delims delims 
                                     :missing-value-marker missing-value-marker)
                   if (equal missing-value-marker str) do
                     (push str f-line)
                   else do
                        (push (string->number str) f-line)
                   finally (return (nreverse f-line)))))
          (when filtered-line
            (push filtered-line result)))))
    (nreverse result)))


(defun number-list->file (num-list file-name)
  "Writes a list of numbers out to a file with one line for each number."
  (with-open-file (str file-name :direction :output 
                   :if-exists :supersede)
    (loop for num in num-list do
          (format str "~a~%" num))))

(defun list->file (list file-name)
  "Write out a list to a file. As the list structure is intact, the list can be
read back in with a single call to the reader."
  (with-open-file (str file-name :direction :output 
                   :if-exists :supersede)
    (format str "~a" list)))


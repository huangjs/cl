#|

 This software is Copyright (c) 2005 Peter Herth <herth@peter-herth.de>

 Peter Herth grants you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
|#

#|
Notebook widget wrapper by Frank Buss

Example usage:

 (defun test-note-book ()
  (with-ltk ()
   (let* ((nb (make-instance 'note-book))
          (page1 (insert-page nb "end" :text "Page 1"))
          (page2 (insert-page nb "end" :text "Page 2"))
          (label1 (make-instance 'label :master page1 :text "Hello World!"))
          (label2 (make-instance 'label :master page2 :text "This is the 2nd page")))
     (pack nb)
     (pack label1 :padx 20 :pady 20)
     (pack label2)
     (compute-size nb)
     (raise-page page1))))

I've decided to use an extra class for the note-book-page, because
referencing it by name, like in the Tk interface, doesn't look like
the way the other widgets are used. Only the functions I need are
implemented, perhaps someone can complete it.

|#

(defpackage :bwidget
  (:use :common-lisp
	:ltk
	)
  (:export
   	#:note-book-page
	#:note-book
	#:compute-size
	#:insert-page
	#:delete-page
	#:raise-page
   ))

(in-package :bwidget)

(eval-when (:load-toplevel)
  (setf *init-wish-hook* (append *init-wish-hook*
				 (list (lambda ()
					 (send-wish "package require BWidget")
				       )))))

(defclass note-book-page (widget)
  ((page-name :accessor page-name :initarg :page-name :initform nil)
   (note-book :accessor note-book :initarg :note-book :initform nil)))

(defclass note-book (widget) ())

(defmethod initialize-instance :after ((nb note-book) &key font activebackground 
                                       activeforeground background borderwidth
                                       disabledforeground foreground repeatdelay
                                       repeatinterval arcradius height homogeneous
                                       side tabbevelsize tabpady width)  
  (format-wish "NoteBook ~a ~@[ -font ~(~A~)~]~
     ~@[ -activebackground ~(~A~)~]~@[ -activeforeground ~(~A~)~]~
     ~@[ -background ~(~A~)~]~@[ -borderwidth ~(~A~)~]~
     ~@[ -disabledforeground ~(~A~)~]~@[ -foreground ~(~A~)~]~
     ~@[ -repeatdelay ~(~A~)~]~@[ -repeatinterval ~(~A~)~]~
     ~@[ -arcradius ~(~A~)~]~@[ -height ~(~A~)~]~@[ -homogeneous ~(~A~)~]~
     ~@[ -side ~(~A~)~]~@[ -tabbevelsize ~(~A~)~]~@[ -tabpady ~(~A~)~]~
     ~@[ -width ~(~A~)~]"
               (widget-path nb) font activebackground activeforeground background
               borderwidth disabledforeground foreground repeatdelay repeatinterval
               arcradius height homogeneous side tabbevelsize tabpady width))

(defmethod insert-page ((nb note-book) index &key text)
  (let ((page-name (ltk::create-name)))
    (format-wish "senddata [~a insert ~a ~a ~@[ -text {~A}~]]"
                 (widget-path nb) index page-name text)
    (let ((path (ltk::read-data)))
      (if path
          (make-instance 'note-book-page 
                         :page-name page-name
                         :note-book nb
                         :path (string-downcase path))
        (error "error while inserting page")))))

(defmethod raise-page ((nbp note-book-page))
  (format-wish "~a raise ~a" (widget-path (note-book nbp)) (page-name nbp)))

(defmethod delete-page ((nbp note-book-page))
  (format-wish "~a delete ~a" (widget-path (note-book nbp)) (page-name nbp)))

(defmethod compute-size ((nb note-book))
  (format-wish "~a compute_size" (widget-path nb)))



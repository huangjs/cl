;; climplayer -- a frontend for mplayer

;; Copyright (C) 2005  Thomas Persson

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :climplayer)

(defvar *media-directories* nil)

(defclass container-node ()
  ((contents :initarg :contents
             :accessor node-contents)))

(defclass media-node ()
  ((string-name :initarg :string-name
                :reader node-string-name)))

(define-presentation-method accept ((type media-node) *standard-input* (view textual-view) &key)
  (with-delimiter-gestures (nil :override t)
    (with-accessors ((net-nodes net-nodes)
                     (container-node container-node))
      *application-frame*
      (complete-input *standard-input*
                      (lambda (so-far mode)
                        (complete-from-possibilities so-far
                                                     (mapcar (lambda (node)
                                                               (list
                                                                (node-string-name node)
                                                                node))
                                                             (let* ((regex (regexify (string-downcase so-far)))
                                                                    (found (append (find-node net-nodes
                                                                                              regex)
                                                                                   (find-node container-node
                                                                                              regex))))
                                                               (if (and (= (length found) 1)
                                                                        (typep (car found)
                                                                               'container-node))
                                                                   (progn (com-browse (car found))
                                                                          (redisplay-frame-pane *application-frame*
                                                                                                (find-pane-named *application-frame*
                                                                                                                 'browser))
                                                                          (node-contents (car found)))
                                                                   (progn (ensure-that-node-is-visible (car found)
                                                                                                       :top t
                                                                                                       :tabs :browser)
                                                                          found))))
                                                     nil
                                                     :action mode
                                                     :value-key #'cadr))))))

(defclass directory-node (container-node file)
  ((string-name :initarg :string-name
                :reader node-string-name)))

(defclass file ()
  ((path-name :initarg :path-name
              :reader node-path-name)
   (parent :initarg :parent
           :accessor node-parent)))

(defclass file-node (media-node file)
  ())

(defclass net-node (media-node)
  ())

(define-presentation-method accept ((type net-node) *standard-input* (view textual-view) &key)
  (completing-from-suggestions (*standard-input* :partial-completers '(#\Space))
    (mapcar (lambda (node)
              (suggest (node-string-name node)
                       node))
            (net-nodes *application-frame*))))

(defgeneric title (node))

(defgeneric extended-title (node))

(defmethod title ((node media-node ))
  (node-string-name node))

(defmethod extended-title ((node media-node ))
  (node-string-name node))

(defun correct-file-namestring (path)
  (format nil
          "~A.~A"
          (pathname-name path)
          (pathname-type path)))
  
(defmethod title ((node file-node))
  (correct-file-namestring (node-path-name node)))

(defmethod extended-title ((node file-node))
  (let ((path (node-path-name node)))
    (format nil "~A/~A"
            (car (last (pathname-directory path)))
            (correct-file-namestring path))))

(defmethod print-object ((node media-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (prin1 (title node) stream)))

(defmethod print-object ((node directory-node) stream)
  (let ((path (node-path-name node)))
    (print-unreadable-object (node stream :type t :identity t)
      (prin1 (directory-namestring path) stream))))

(defun make-container-node (dir-list)
  (let ((node (make-instance 'container-node)))
    (setf (node-contents node)
          (mapcar (lambda (dir)
                    (make-file-tree dir node))
                  dir-list))
    node))

(defun find-contents (dir parent)
  (append (mapcar (lambda (d)
                    (make-file-tree d parent))
                  (list-dirs dir))
          (mapcar (lambda (file)
                    (make-instance 'file-node
                                   :string-name (pathname-to-string file)
                                   :path-name file
                                   :parent parent))
                  (list-files dir))))
          
(defun make-file-tree (dir parent)
  (let ((dir-node (make-instance 'directory-node
                                 :string-name (pathname-to-string dir)
                                 :path-name dir
                                 :parent parent)))
    (setf (node-contents dir-node)
          (remove nil (find-contents dir dir-node)))
    (when (or (node-contents dir-node)
              (member dir *media-directories*))
      dir-node)))

(defgeneric refresh-node (node))

(defmethod refresh-node ((node container-node))
  (mapcar #'refresh-node
          (delete-if-not (lambda (n)
                           (probe-file (node-path-name n)))
                         (node-contents node)))
  node)
        
(defmethod refresh-node ((node directory-node))
  (if (probe-file (node-path-name node))
      (unless (setf (node-contents node)
                    (remove nil (find-contents (node-path-name node)
                                               node)))
        (refresh-node (node-parent node)))
      (refresh-node (node-parent node))))

(defmethod refresh-node ((node file-node))
  (unless (probe-file (node-path-name node))
    (refresh-node (node-parent node))))

(defun pathname-to-string (path)
  (if (pathnamep path)
      (let ((dir (cdr (pathname-directory path)))
            (name (pathname-name path))
            (type (pathname-type path)))
        (format nil
                "/~{~A/~}~A~A"
                dir
                (if name
                    name
                    "")
                (if type
                    (format nil ".~A" type)
                    "")))
      path))

(defun list-files (dir)
  (list-files-matching #'media-filep dir))

(defun list-dirs (dir)
  (list-files-matching #'directoryp dir))

(defun list-files-matching (predicate dir)
  (sort (remove-if-not predicate
                       (ignore-errors (directory
                                       (make-pathname :name :wild
                                                      :type :wild
                                                      :defaults dir))))
        (lambda (path1 path2)
          (string-lessp (pathname-to-string path1)
                        (pathname-to-string path2)))))

(defun directoryp (file)
  (not (pathname-name file)))

(defparameter *file-formats* '("rmvb" "ogg" "ogv" "vob" "mp3" "mp4" "avi" "mpg" "mpeg" "wav" "wmv" "wma" "asf" "qt" "mov" "m4a" "m4v" "rm" "bin" "mkv" "flv" "flac" "ape"))

(defun media-filep (path)
  (member (pathname-type path)
              *file-formats*
              :test #'string-equal))

(defgeneric flatten (node))

(defmethod flatten ((n file-node))
  (list n))

(defmethod flatten ((n container-node))
  (flatten (node-contents n)))

(defmethod flatten ((n null)))

(defmethod flatten ((lst cons))
  (apply #'append
         (mapcar #'flatten
                 lst)))

(defun find-node (node regex)
  (if (regex-valid-p regex)
      (let ((scanner (create-scanner regex)))
        (search-node-or-list node scanner))))


(defgeneric search-node-or-list (node regex))

(defmethod search-node-or-list ((n container-node) regex)
  (apply #'append
         (mapcar (lambda (node)
                   (search-node-or-list node
                                        regex))
                 (node-contents n))))

(defmethod search-node-or-list ((n media-node) regex)
  (if (scan regex
            (string-downcase (node-string-name n)))
      (list n)))

(defmethod search-node-or-list ((n directory-node) regex)
  (if (scan regex
            (string-downcase (node-string-name n)))
      (list n)
      (apply #'append
             (mapcar (lambda (node)
                       (search-node-or-list node
                                    regex))
                     (node-contents n)))))

(defmethod search-node-or-list ((lst list) regex)
  (let ((results nil))
    (dolist (a lst)
      (if (scan regex (string-downcase (node-string-name a)))
          (push a results)))
    results))

(defun regex-valid-p (regex)
  (unless (eql (handler-case
                   (cl-ppcre:scan regex "foo")
                 (cl-ppcre:ppcre-syntax-error () 'error))
               'error)
    t))

(defgeneric play (node))

(defvar *mplayer-options* nil)




;; Abuse mplayers ability to read filenames from a file in order to
;; work around sbcl unicode weirdness which prevents you from sending
;; anything but simple-base-strings as arguments to run-program.
(defmethod play ((node media-node))
  (with-open-file (out #P"/tmp/climplayer-temp-file"
                       :direction :output
                       :if-exists :supersede)
    (write-line (node-string-name node)
                out))
  (cl-user::run-program "mplayer"
                        (append *mplayer-options*
                                (list "-slave"
                                      "-quiet"
                                      "-playlist"
                                      "/tmp/climplayer-temp-file"))
                        #+sbcl :search #+sbcl t
                        :input :stream
                        :output :stream
                        :wait nil))

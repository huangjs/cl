;;; flexichain-test
;;; ASDF system definition
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;; Copyright (C) 2008       Cyrus Harmon (ch-lisp@bobobeach.com)
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

(asdf:defsystem :flexichain-test
  :name "flexichain-test"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (flexichain)
  :components ((:file "tester-package")
               (:file "tester" :depends-on ("tester-package"))
               (:file "rtester" :depends-on ("tester-package"))
               (:file "skiplist-package")
               (:file "skiplist" :depends-on ("skiplist-package"))
               (:file "stupid")))


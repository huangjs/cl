;;; -*- Mode: Lisp; Package: CLIM-DESKTOP; -*-

;;;  (c) copyright 2005-2006 by
;;;           Robert Strandh (strandh@labri.fr)
;;;           David Murray (splittist@yahoo.com)
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Try to integrate Closure with some CLIM applications...

(in-package :clim-desktop)

(clim-launcher:add-app "Closure" 'closure:start)

(in-package :climacs-gui)

(define-command (com-browse-url :name t :command-table base-table) ()
  (let ((url (accept 'url :prompt "Browse URL")))
    (closure:visit url)))

(in-package :drei-lisp-syntax)

(define-command (com-lookup-symbol-documentation :name t :command-table lisp-table)
    ()
  "Look up a symbol in the Common Lisp HyperSpec or CLIM spec."
  (let* ((symbol (or (form-to-object *current-syntax*
                                     (symbol-at-mark *current-syntax*
                                                     *current-point*))
                     (accept 'symbol :prompt "Lookup documentation for symbol")))
         (name (symbol-name symbol))
         (*standard-output* *debug-io*)
         (url (or (clhs-lookup:spec-lookup name)
                  (when (eq (symbol-package symbol)
                            (find-package :clim))
                    (clhs-lookup:climspec-lookup symbol))
                  (when (eq (symbol-package symbol)
                            (find-package :xlib))
                    (clhs-lookup:clxdoc-lookup symbol)))))
    (if (null url)
        (esa:display-message "Symbol not found.")
        (closure:visit url))))

;; We want it to work in the Listener, where C-c is an abort gesture.
(esa:set-key 'com-lookup-symbol-documentation
             'lisp-table
             '((#\c :control :meta) (#\d :control) (#\h)))

(in-package :beirc)

(define-beirc-command (com-browse-url :name t) ((url 'url :prompt "url"))
  (closure:visit url))

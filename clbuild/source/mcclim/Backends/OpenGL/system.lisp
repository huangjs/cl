;;; -*- Mode: Lisp; Package: User -*-

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

(in-package :common-lisp-user)

(clim-defsystem (:clim-opengl :depends-on (:clim))
   "Backends/OpenGL/opengl-x-frame-manager"
   "Backends/OpenGL/opengl-frame-manager"
   "Backends/OpenGL/opengl-x-port-before"
   "Backends/OpenGL/opengl-port"
   "Backends/OpenGL/opengl-x-port-after"
   "Backends/OpenGL/opengl-medium"
   "Backends/OpenGL/opengl-x-graft"
   )


#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(in-package :cl-user)
(defpackage <% @var name %>-test-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-test-asd)

(defsystem <% @var name %>-test
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on (:<% @var name %>
               :stefil)
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "unit-test" :depends-on ("package"))
                 ))))

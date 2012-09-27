(cl:defpackage :beirc
    (:use :clim :clim-lisp :clim-sys :clim-tab-layout)
    (:export #:beirc
             #:*beirc-user-init-file*
             #:*hyperspec-base-url* #:*default-fill-column* #:*timestamp-column-orientation*
             #:*default-nick* #:*nickserv-password-alist* #:*default-web-browser
             #:*auto-join-alist* #:*auto-focused-alist*)
    (:import-from #:cl-irc #:&req))

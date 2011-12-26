(in-package :cs325-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :frames)
  (require :show-frame)
  (require :mops))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :frames)
  (use-package :mops))

(publish :path "/cs325/paramdemo" :function 'show-params)

(defun show-params (req ent)
  (with-http-response (req ent)
    (let ((source (request-query-value "source" req))
          (save (request-query-value "save" req)))
      (with-http-body (req ent)
        (html 
         (:head
          ((:script :language "javascript")
           (:princ
            "function storeChanges() {"
            "var loc = document.location;"
            "var form = document.mainForm;"
            "loc.href = loc.pathname + '?source=' + form.name.value + '&save=yes';"
            "}")
           :newline
           (:princ
            "function switchMOPs() {"
            "var loc = document.location;"
            "var form = document.mainForm;"
            "loc.href = loc.pathname + '?source=' + form.name.value;"
            "}")))
         (:body
          ((:form :name "mainForm" :method "post")
           (:p 
            ((:input :type "text" :name "name" :value source)))
           (:p 
            ((:input :type "text" :name "save" :value save)))
           (:p 
            ((:input :type "button" :value "Store"
                     :onClick "storeChanges()"))
            ((:input :type "button" :value "Switch"
                     :onClick "switchMOPs()"))
            ((:input :type "reset" :value "Cancel"))
            ))))))))
  
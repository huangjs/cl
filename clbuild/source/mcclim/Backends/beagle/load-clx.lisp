(format t "Ensure you have issued the command: (require \"asdf\")~%")
(format t "~%Loading CLX...~%")
(load "/Users/duncan/clx/clx.asd")
(asdf:operate 'asdf:load-op 'clx)
(asdf:operate 'asdf:load-op 'clim-clx)
(format t "~%Done.~%")

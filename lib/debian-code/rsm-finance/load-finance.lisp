(in-package #:cl-user)

;; 1). Start up Web interface to Financial Loan Utilities.

(net.aserve:start)
(load "rsm-finance.asd")
(load "rsm-web-finance.asd")
(asdf:operate 'asdf:load-op 'rsm-web-finance)

;; 2). Now set web browser to http://localhost/loan


;; 3). When done, run 
;;     (net.aserve:shutdown)
;;     to stop server.
#|
  Book:      proof-fibs
  Copyright: (c) 2005 Galois Connections, Inc.
  Author:    Lee Pike, Galois Connections, Inc. <leepike@galois.com>
|#

(in-package "ACL2")

(include-book "make-theorems" :dir :books)

;; --------------------------------------------------------
;; COMPILER OUTPUT
(include-book "fibs-source-shallow-canon")
(include-book "fibs-source-shallow-flatten")
;; --------------------------------------------------------

(make-thm :name |inv-fibs-thm| 
          :thm-type invariant
          :ind-name |fibs_2|
          :itr-name |iter_fibs_3| 
          :init-hist (0 0)
          :hist-widths (1) 
          :branches (|fibs_2|))

(make-thm :name |fibs-thm|
          :thm-type fn
          :itr-term (|itr_fib| i)
          :ind-term (|ind_fib| i))

include ../../../../Makefile-generic
ACL2 = ../../../../../saved_acl2r
# Dependencies:

continuity.cert: continuity.lisp ../../../nsa/nsa.cert ../../../arithmetic/realp.cert

derivatives.cert: derivatives.lisp continuity.cert exercise4.cert exercise5.cert

exercise1.cert: exercise1.lisp ../../../arithmetic/top.cert

exercise2.cert: exercise2.lisp continuity.cert

exercise3.cert: exercise3.lisp continuity.cert

exercise4.cert: exercise4.lisp continuity.cert

exercise5.cert: exercise5.lisp exercise4.cert

exercise6.cert: exercise6.lisp ../../../nsa/nsa.cert ../../../arithmetic/realp.cert continuity.cert

exercise7.cert: exercise7.lisp continuity.cert exercise4.cert exercise5.cert

exercise8.cert: exercise8.lisp derivatives.cert

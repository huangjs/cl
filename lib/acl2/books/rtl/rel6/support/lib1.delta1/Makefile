include ../../../../Makefile-generic
ACL2 = ../../../../../saved_acl2

# Don't alphabetize this list!
BOOKS = float-extra2 round-extra2 mult-proofs float round mult
 
# Dependencies:

float-extra2.cert: float-extra2.lisp
float-extra2.cert: ../lib1/top.cert
float-extra2.cert: ../../arithmetic/basic.cert
float-extra2.cert: ../../arithmetic/basic.cert

round-extra2.cert: round-extra2.lisp
round-extra2.cert: ../lib1/top.cert
round-extra2.cert: ../support/trunc.cert
round-extra2.cert: ../support/trunc.cert
round-extra2.cert: ../../arithmetic/basic.cert
round-extra2.cert: ../../arithmetic/even-odd.cert
round-extra2.cert: ../support/away.cert
round-extra2.cert: ../../arithmetic/basic.cert
round-extra2.cert: ../../arithmetic/top.cert
round-extra2.cert: ../../arithmetic/expt.cert
round-extra2.cert: ../../arithmetic/expt.cert
round-extra2.cert: float-extra2.cert
round-extra2.cert: ../../arithmetic/basic.cert

mult-proofs.cert: mult-proofs.lisp
mult-proofs.cert: ../support/rtl.cert
mult-proofs.cert: ../support/sumbits.cert
mult-proofs.cert: ../support/util.cert
mult-proofs.cert: ../../arithmetic/top.cert
mult-proofs.cert: ../support/cat.cert
mult-proofs.cert: ../support/cat.cert
mult-proofs.cert: ../support/lior.cert
mult-proofs.cert: ../support/land.cert
mult-proofs.cert: ../support/lxor.cert

float.cert: float.lisp
float.cert: ../lib1/log.cert
float.cert: ../lib1/float.cert
float.cert: float-extra2.cert

round.cert: round.lisp
round.cert: float.cert
round.cert: ../lib1/round.cert
round.cert: round-extra2.cert

mult.cert: mult.lisp
mult.cert: ../lib1/add.cert
mult.cert: mult-proofs.cert


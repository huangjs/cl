# Instructions:

# Before starting, see "Dependencies" below.

# In the shell, execute
# make
# to certify all books.
# To clean up (removing files *.cert, *.o, *.fasl, etc.), in the shell execute
# make clean

include ../../../../Makefile-generic

ACL2 = ../../../../../saved_acl2

# Dependencies:
# In the shell, execute
# make -s dependencies
# to obtain the result below.

all: sol1.cert sol2.cert

sol1.cert: sol1.lisp
sol1.cert: ../../../../../books/misc/records.cert
sol1.cert: ../../../../../books/arithmetic/top-with-meta.cert

sol2.cert: sol2.lisp
sol2.cert: ../../../../../books/misc/records.cert
sol2.cert: ../../../../../books/arithmetic/top-with-meta.cert
sol2.cert: ../../../../ordinals/e0-ordinal.cert

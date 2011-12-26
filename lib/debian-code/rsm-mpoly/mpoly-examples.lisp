;;;; Examples:

#|
> (rsm.mpoly:term 2 (1 3))
(2 . #(1 3))

> (setf *foo* (rsm.mpoly:poly (2 (1 3)) (10 (2 1)) (4 (1 1))))
#< 10X1^2*X2 + 2X1*X2^3 + 4X1*X2 >

> (setf *goo* (rsm.mpoly:poly (3 (1 1)) (-5 (1 2)) (-1 (2 1))))
#< - X1^2*X2 - 5X1*X2^2 + 3X1*X2 >

> (setf *boo* (rsm.mpoly:poly (3.2 (1 1)) (#c(3 4) (1 2)) (-1 (2 1))))
#< - X1^2*X2 + #c(3 4)X1*X2^2 + 3.2X1*X2 >

> (rsm.mpoly:+ *foo* *goo*)
#< 9X1^2*X2 + 2X1*X2^3 - 5X1*X2^2 + 7X1*X2 >

(rsm.mpoly:+ *foo* 2 *goo* 3)
#< 9X1^2*X2 + 2X1*X2^3 - 5X1*X2^2 + 7X1*X2 + 5 >

> (rsm.mpoly:* *foo* *goo*)
#< - 10X1^4*X2^2 - 2X1^3*X2^4 - 50X1^3*X2^3 + 26X1^3*X2^2 - 
10X1^2*X2^5 + 6X1^2*X2^4 - 20X1^2*X2^3 + 12X1^2*X2^2 >

(rsm.mpoly:* *foo* 2 *goo* 3)
#< - 60X1^4*X2^2 - 12X1^3*X2^4 - 300X1^3*X2^3 + 156X1^3*X2^2 - 60X1^2*X2^5 + 
36X1^2*X2^4 - 120X1^2*X2^3 + 72X1^2*X2^2 >

> (rsm.mpoly:^ *foo* 3)
#< 1000X1^6*X2^3 + 600X1^5*X2^5 + 1200X1^5*X2^3 + 120X1^4*X2^7 + 
480X1^4*X2^5 + 480X1^4*X2^3 + 8X1^3*X2^9 + 48X1^3*X2^7 + 
96X1^3*X2^5 + 64X1^3*X2^3 >

> (rsm.mpoly:report-state)
order-type = LEX-ORDER
modulus = NIL
power-modulus = NIL

> (rsm.mpoly:set-modulus 17)
17

> (rsm.mpoly:report-state)
order-type = LEX-ORDER
modulus = 17
power-modulus = T

> (rsm.mpoly:^ *foo* 2000)
#< 4X1^16*X2^16 + 13X1^16*X2^14 + 8X1^16*X2^12 + X1^16*X2^10 + 
15X1^16*X2^8 + 4X1^16*X2^6 + 9X1^16*X2^4 + 16X1^16*X2^2 + 
3X1^15*X2^16 + 10X1^15*X2^2 + 9X1^14*X2^16 + 15X1^14*X2^4 + 
9X1^14*X2^2 + 10X1^13*X2^16 + 14X1^13*X2^6 + 16X1^13*X2^4 + 
15X1^13*X2^2 + 13X1^12*X2^16 + 4X1^12*X2^8 + 15X1^12*X2^6 + 
11X1^12*X2^4 + 9X1^12*X2^2 + 5X1^11*X2^16 + 6X1^11*X2^10 + 
9X1^11*X2^8 + 2X1^11*X2^6 + 4X1^11*X2^4 + 4X1^11*X2^2 + 
15X1^10*X2^16 + 9X1^10*X2^12 + 6X1^10*X2^10 + 13X1^10*X2^8 + 
12X1^10*X2^6 + X1^10*X2^4 + 11X1^10*X2^2 + 11X1^9*X2^16 + 
5X1^9*X2^14 + 2X1^9*X2^12 + 12X1^9*X2^10 + 6X1^9*X2^8 + 
12X1^9*X2^6 + 11X1^9*X2^4 + 13X1^9*X2^2 + 15X1^8*X2^16 + 
X1^8*X2^14 + 7X1^8*X2^12 + 11X1^8*X2^10 + 2X1^8*X2^8 + 10X1^8*X2^6 + 
10X1^8*X2^4 + 13X1^8*X2^2 + 4X1^7*X2^16 + 5X1^7*X2^14 + 
12X1^7*X2^12 + 2X1^7*X2^10 + 4X1^7*X2^8 + 11X1^7*X2^6 + 
7X1^7*X2^4 + 2X1^7*X2^2 + 11X1^6*X2^16 + 16X1^6*X2^14 + 
5X1^6*X2^12 + 12X1^6*X2^10 + 3X1^6*X2^8 + X1^6*X2^6 + 7X1^6*X2^4 + 
12X1^6*X2^2 + 6X1^5*X2^16 + 13X1^5*X2^14 + 16X1^5*X2^12 + 
15X1^5*X2^10 + 2X1^5*X2^8 + 5X1^5*X2^6 + 5X1^5*X2^4 + 10X1^5*X2^2 + 
12X1^4*X2^16 + 12X1^4*X2^14 + 11X1^4*X2^12 + 14X1^4*X2^10 + 5X1^4*X2^8 + 
14X1^4*X2^6 + 13X1^4*X2^4 + 5X1^4*X2^2 + 3X1^3*X2^16 + 10X1^3*X2^14 + 
3X1^3*X2^12 + 7X1^3*X2^10 + 2X1^3*X2^8 + 2X1^3*X2^6 + 4X1^3*X2^4 + 
16X1^3*X2^2 + 7X1^2*X2^16 + 9X1^2*X2^14 + 11X1^2*X2^12 + 7X1^2*X2^10 + 
9X1^2*X2^8 + 12X1^2*X2^6 + 2X1^2*X2^4 + 10X1^2*X2^2 + 9X1*X2^16 + 
X1*X2^14 + X1*X2^12 + 9X1*X2^10 + 11X1*X2^8 + 5X1*X2^6 + 
4X1*X2^4 + 15X1*X2^2 >

> (rsm.mpoly:set-modulus nil)
nil

> (rsm.mpoly:report-state)
order-type = LEX-ORDER
modulus = NIL
power-modulus = NIL

> (setf *boo* (rsm.mpoly:poly (3.2 (1 1)) (5 (1 3)) (-1 (2 1))))
#< - X1^2*X2 + 5X1*X2^3 + 3.2X1*X2 >

>  (rsm.mpoly:get-order)
lex-order

(rsm.mpoly:set-order deglex-order)
lex-order

(rsm.mpoly:get-order)
deglex-order

> *boo*
#< 5X1*X2^3 - X1^2*X2 + 3.2X1*X2 >

> (rsm.mpoly:set-order lex-order)
deglex-order

>  (rsm.mpoly:get-order)
lex-order

> *boo*
#< - X1^2*X2 + 5X1*X2^3 + 3.2X1*X2 >


|#

; ACL2 Univariate Polynomials over a Field books -- Sum Congruences
;; Congruences for Sums of Univariate Polynomials over a Field
; Copyright (C) 2006  John R. Cowles and Ruben A. Gamboa, University of
; Wyoming

; This book is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This book is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this book; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Modified by J. Cowles

;;   Last modified July 2006 (for ACL2 Version 3.0).

;; Based on
;;; ------------------------------------------------------------------
;;; Congruencia de la igualdad con la suma de polinomios
;;;
;;; Autores:
;;;
;;; Inmaculada Medina Bulo
;;; Francisco Palomo Lozano
;;;
;;; Descripci�n:
;;;
;;; Aqu� se demuestran las congruencias de la igualdad de polinomios
;;; con la suma. Las demostraciones son complejas debido a que
;;; necesitan reglas expansivas. Estas reglas son peligrosas, ya que
;;; pueden producir f�cilmente ciclos en el demostrador. Para
;;; restringir su aplicaci�n caben dos opciones:
;;;
;;; 1. Desactivarlas y usarlas expl�citamente donde sea necesario. Una
;;; variante es no generar la regla en absoluto (es decir, introducir
;;; el teorema con la clases de reglas vac�a).
;;;
;;; 2. Restringir su aplicaci�n sint�cticamente para prevenir
;;; expansiones en cadena. Esto se puede lograr graciasa syntaxp.
;;;
;;; Elegimos la segunda opci�n porque se consigue un mayor grado de
;;; automatizaci�n y hace a las demostraciones menos sensibles a los
;;; cambios.
;;; ------------------------------------------------------------------
#|
To certify this book, first, create a world with the following packages:

(in-package "ACL2")

(defconst *import-symbols*
  (set-difference-eq
   (union-eq *acl2-exports*
	     *common-lisp-symbols-from-main-lisp-package*)
     '(null + * - < = / commutativity-of-* associativity-of-* 
	    commutativity-of-+ associativity-of-+ distributivity)))

(defpkg "FLD"
  *import-symbols*)

(defpkg "FUTER"
  *import-symbols*)

(defpkg "FUMON"
  (union-eq *import-symbols*
	    '(FLD::fdp FUTER::terminop)))

(defpkg "FUPOL"
  (union-eq *import-symbols*
	    '(FUTER::naturalp FUTER::terminop FUMON::monomio FUMON::coeficiente
			    FUMON::termino FUMON::monomiop)))

(certify-book "fucongruencias-suma"
	      5
	      nil ;;compile-flg
	      )
|#
(in-package "FUPOL")

;;(include-book "suma")
(include-book "fusuma"
	      :load-compiled-file nil)

;;; ----------------------------------------------------
;;; Congruencia de la igualdad de polinomios con la suma
;;; ----------------------------------------------------

;;; Segundo par�metro

;;; NOTA:
;;;
;;; Esta propiedad es expansiva; restringimos su aplicaci�n sint�cticamente

(defthm
  polinomiop-implies-true-listp
  (implies (polinomiop p)
	   (true-listp p))
  :rule-classes :compound-recognizer)

(defthm
  Right-identity-append
  (implies (true-listp p)
	   (equal (append p nil) p)))

(defthm |p + q = p + fn(q)|
  (implies (syntaxp (not (and (consp q) (eq (primero q) 'fn))))
	   (= (+ p q) (+ p (fn q)))))

(defthm
  =P-implies-=P-append-1
  (implies (=P p1 p2)
	   (=P (append p1 q)
	       (append p2 q)))
  :rule-classes :congruence)

(defthm
  =P-implies-=P-append-2
  (implies (=P q1 q2)
	   (=P (append p q1)
	       (append p q2)))
  :rule-classes :congruence)

(defthm
  =P-implies-=P-fn
  (implies (=P p1 p2)
	   (=P (fn p1)
	       (fn p2)))
  :rule-classes :congruence)

;;(defcong = = (+ p q) 2)
(defthm
  =-implies-=-+-2
  (implies (= q1 q2)
	   (= (+ p q1)
	      (+ p q2)))
  :rule-classes :congruence)

;;; Primer par�metro

;; (defcong = = (+ p q) 1
;;   :hints (("Goal"
;; 	   :in-theory (disable |p + q = q + p| + =)
;; 	   :use (|p + q = q + p|
;; 		 (:instance |p + q = q + p| (p p-equiv))))))
(defthm
  =-implies-=-+-1
  (implies (= p1 p2)
	   (= (+ p1 q)
	      (+ p2 q)))
  :rule-classes :congruence
  :hints (("Goal"
	   :in-theory (disable |p + q = q + p| + =)
	   :use ((:instance
		  |p + q = q + p|
		  (p p1))
		 (:instance 
		  |p + q = q + p|
		  (p p2))))))

;;; NOTA:
;;;
;;; Esta propiedad es expansiva; restringimos su aplicaci�n sint�cticamente

(defthm |p + q = fn(p) + q|
  (implies (syntaxp (not (and (consp p) (eq (primero p) 'fn))))
	   (= (+ p q) (+ (fn p) q)))
  :hints (("Goal"
	   :in-theory (disable |p + q = p + fn(q)|)
	   :use ((:instance |p + q = p + fn(q)| (p q) (q p))))))

(defthm |fn(p) + fn(q) = p + q|
  (= (+ (fn p) (fn q)) (+ p q)))

(in-theory (disable |p + q = p + fn(q)|
		    |p + q = fn(p) + q|
		    |fn(p) + fn(q) = p + q|))

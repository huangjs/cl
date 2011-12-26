;; Processing Unicode Files with ACL2
;; Copyright (C) 2005-2006 by Jared Davis <jared@cs.utexas.edu>
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package "ACL2")

(include-book "app")

(defund pos-listp (x)
  (declare (xargs :guard t))
  (if (consp x)
      (and (posp (car x))
           (pos-listp (cdr x)))
    t))

(defthm pos-listp-when-not-consp
  (implies (not (consp x))
           (pos-listp x))
  :hints(("Goal" :in-theory (enable pos-listp))))

(defthm pos-listp-of-cons
  (equal (pos-listp (cons a x))
         (and (posp a)
              (pos-listp x)))
  :hints(("Goal" :in-theory (enable pos-listp))))

(defthm pos-listp-of-list-fix
  (equal (pos-listp (list-fix x))
         (pos-listp x))
  :hints(("Goal" :induct (len x))))

(defthm pos-listp-of-app
  (equal (pos-listp (app x y))
         (and (pos-listp x)
              (pos-listp y)))
  :hints(("Goal" :induct (len x))))

(defthm posp-of-car-when-pos-listp
  (implies (pos-listp x)
           (and (equal (integerp (car x))
                       (consp x))
                (equal (< 0 (car x))
                       (consp x))))
  :hints(("Goal" :induct (len x))))

(defthm posp-listp-of-cdr-when-pos-listp
  (implies (pos-listp x)
           (pos-listp (cdr x))))
;
; Memories: Array-like Records for ACL2
; Copyright (C) 2005-2006 by Jared Davis <jared@cs.utexas.edu>
;
; This program is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free Software
; Foundation; either version 2 of the License, or (at your option) any later
; version.
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
; details.
;
; You should have received a copy of the GNU General Public License along with
; this program; if not, write to the Free Software Foundation, Inc., 51
; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;


; memory.lisp
;
; This is the correct file to include to load the tree-based memories.  After
; including this book, see :doc MEM::memory for documentation.

(in-package "MEM")
(set-verify-guards-eagerness 2)
(set-enforce-redundancy t)

(local (include-book "memory-impl"))

(include-book "private")
(include-book "misc/records" :dir :system)

(defdoc memory
  ":Doc-Section memory
  special records designed for array-like usage~/

  Memories are specialized records that are designed for array-like usage.
  Memories have a fixed size, and elements are accessed by the natural numbers 
  0, 1, ..., size-1, where size is the maximum size of the memory.

  Unlike arrays, memories are based on  trees.  As a result, loading and
  storing into memories is slower than array access in a typical programming 
  language, and requires an O(log_2 n) search for the right element.  However, 
  there are benefits to this system as well.  We populate the tree structure 
  as needed when writes occur, allowing us to conceptually represent very large
  arrays so long as we use them sparesely.  Hence, memories are well suited for
  uses such as simulating the memory systems of processors or virtual machines 
  with many gigabytes of memory, only some of which is used during simulation.

  Memories are as easy to reason about as records (see misc/records.lisp) and
  we provide the same core 'record theorems' about them.  However, the load and
  store operations on memories are guarded more strongly than the records book,
  in order to achieve efficiency.
  ~/
  ~l[arrays] and also ~pl[stobjs] for more efficient implementations of small
  arrays.  The records book (misc/records.lisp) provides the same reasoning
  strategy as memories, and may be an appropriate substitution for memories
  depending upon your needs.")



; DEFINITIONS
;
; Most of the functions below are introduced with private, a macro which is
; defined in private.lisp.  This macro adds theory invariants which prohibit
; you from enabling their definitions or type prescriptions.  The intention is
; that you should not be allowed to reason about these functions directly.
; 
; The end user of the library can feel free to skip over all of the detail
; here.  The only useful functions from your perspective should be memory-p,
; size, new, load, and store.  You can read theorems about them by skipping all
; the way down to "THEOREMS" below.



; Imported from log2.lisp

(private _log2-tr (n acc)
  (declare (xargs :guard (and (natp n)
                              (natp acc))))
  (if (zp n)
      acc
    (_log2-tr (mbe :logic (floor n 2)
                   :exec (ash n -1))
              (1+ acc))))

(private _log2 (n)
  (declare (xargs :guard (natp n)))
  (mbe :logic (if (zp n)
                  0
                (1+ (_log2 (floor n 2))))
       :exec (_log2-tr n 0)))



; Imported from memtree.lisp

(private _memtree-p (mtree depth)
  (declare (xargs :guard (natp depth)))
  (if (zp depth)
      t
    (if (atom mtree)
        (null mtree)
      (and (_memtree-p (car mtree) (1- depth))
           (_memtree-p (cdr mtree) (1- depth))
           (not (and (null (car mtree))
                     (null (cdr mtree))))))))

(private _memtree-fix (mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_memtree-p mtree depth))))
  (if (mbt (_memtree-p mtree depth))
      mtree
    nil))

(private _address-p (addr depth)
  (declare (xargs :guard (natp depth)))
  (and (natp addr)
       (< addr (expt 2 (nfix depth)))))

(private _address-fix (addr depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth))))
  (if (mbt (_address-p addr depth))
      addr
    0))

(private _memtree-load (addr mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth)
                              (_memtree-p mtree depth))))
  (let ((addr  (_address-fix addr depth))
        (mtree (_memtree-fix mtree depth)))
    (if (zp depth)
        mtree
      (_memtree-load (floor addr 2)
                     (if (= (mod addr 2) 0)
                         (car mtree)
                       (cdr mtree))
                     (1- depth)))))

(private _fix-addr/depth-memtree-load (addr mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth)
                              (_memtree-p mtree depth)))
           (type (signed-byte 29) depth)
           (type (signed-byte 29) addr))
  (mbe :logic (_memtree-load addr mtree depth)
       :exec (if (= depth 0)
                 mtree
               (_fix-addr/depth-memtree-load 
                (the-fixnum (ash addr -1))
                (if (= (the-fixnum (logand addr 1)) 0)
                    (car mtree)
                  (cdr mtree))
                (the-fixnum (1- depth))))))

(private _fixnum-memtree-load (addr mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth)
                              (_memtree-p mtree depth)))
           (type (signed-byte 29) depth))
  (mbe :logic (_memtree-load addr mtree depth)
       :exec (if (<= depth 28)
                 (_fix-addr/depth-memtree-load addr mtree depth)
               (_fixnum-memtree-load (ash addr -1)
                                     (if (= (the-fixnum (logand addr 1)) 0)
                                         (car mtree)
                                       (cdr mtree))
                                     (the-fixnum (1- depth))))))

(private _memtree-store (addr elem mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (not (null elem))
                              (_address-p addr depth)
                              (_memtree-p mtree depth))
                  :measure (acl2-count depth)))
  (let ((addr  (_address-fix addr depth))
        (mtree (_memtree-fix mtree depth)))
    (if (zp depth)
        elem
      (let ((quotient (floor addr 2)))
        (if (= (mod addr 2) 0)
            (cons (_memtree-store quotient elem (car mtree) (1- depth))
                  (cdr mtree))
          (cons (car mtree)
                (_memtree-store quotient elem (cdr mtree) 
                                (1- depth))))))))

(private _fix-addr/depth-memtree-store (addr elem mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (not (null elem))
                              (_address-p addr depth)
                              (_memtree-p mtree depth)))
           (type (signed-byte 29) depth)
           (type (signed-byte 29) addr))
  (mbe :logic (_memtree-store addr elem mtree depth)
       :exec (if (= depth 0)
                 elem
               (let ((quotient (the-fixnum (ash addr -1))))
                 (if (= (the-fixnum (logand addr 1)) 0)
                     (cons (_fix-addr/depth-memtree-store 
                            quotient elem (car mtree) (the-fixnum (1- depth)))
                           (cdr mtree))
                   (cons (car mtree)
                         (_fix-addr/depth-memtree-store 
                          quotient elem (cdr mtree) 
                          (the-fixnum (1- depth)))))))))

(private _fixnum-memtree-store (addr elem mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (not (null elem))
                              (_address-p addr depth)
                              (_memtree-p mtree depth)))
           (type (signed-byte 29) depth))
  (mbe :logic (_memtree-store addr elem mtree depth)
       :exec (if (<= depth 28)
                 (_fix-addr/depth-memtree-store addr elem mtree depth)
               (let ((quotient (ash addr -1)))
                 (if (= (the-fixnum (logand addr 1)) 0)
                     (cons (_fixnum-memtree-store 
                            quotient elem (car mtree) (the-fixnum (1- depth)))
                           (cdr mtree))
                   (cons (car mtree)
                         (_fixnum-memtree-store 
                          quotient elem (cdr mtree) 
                          (the-fixnum (1- depth)))))))))

(private _memtree-store-nil (addr mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth)
                              (_memtree-p mtree depth))
                  :measure (acl2-count depth)))
  (let ((addr  (_address-fix addr depth))
        (mtree (_memtree-fix mtree depth)))
    (if (zp depth)
        nil
      (if (atom mtree)
          nil
        (let ((quotient (floor addr 2)))
          (if (= (mod addr 2) 0)              
              (let ((left (_memtree-store-nil quotient (car mtree) 
                                              (1- depth)))
                    (right (cdr mtree)))
                (if (and (null left) (null right))
                    nil
                  (cons left right)))
            (let ((left (car mtree))
                  (right (_memtree-store-nil quotient (cdr mtree) (1- depth))))
                (if (and (null left) (null right))
                    nil
                  (cons left right)))))))))

(private _fix-addr/depth-memtree-store-nil (addr mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth)
                              (_memtree-p mtree depth)))
           (type (signed-byte 29) depth)
           (type (signed-byte 29) addr))
  (mbe :logic (_memtree-store-nil addr mtree depth)
       :exec (if (= depth 0)
                 nil
               (if (null mtree)
                   nil
                 (let ((quotient (the-fixnum (ash addr -1))))
                   (if (= (the-fixnum (logand addr 1)) 0)
                       (let ((left (_fix-addr/depth-memtree-store-nil 
                                    quotient (car mtree) 
                                    (the-fixnum (1- depth))))
                             (right (cdr mtree)))
                         (if (and (null left)
                                  (null right))
                             nil
                           (cons left right)))
                     (let ((left (car mtree))
                           (right (_fix-addr/depth-memtree-store-nil 
                                   quotient (cdr mtree) 
                                   (the-fixnum (1- depth)))))
                       (if (and (null left)
                                (null right))
                           nil
                         (cons left right)))))))))

(private _fixnum-memtree-store-nil (addr mtree depth)
  (declare (xargs :guard (and (natp depth)
                              (_address-p addr depth)
                              (_memtree-p mtree depth)))
           (type (signed-byte 29) depth))
  (mbe :logic (_memtree-store-nil addr mtree depth)
       :exec (if (<= depth 28)
                 (_fix-addr/depth-memtree-store-nil addr mtree depth)
               (if (null mtree)
                   nil
                 (let ((quotient (ash addr -1)))
                   (if (= (the-fixnum (logand addr 1)) 0)
                       (let ((left (_fixnum-memtree-store-nil 
                                    quotient (car mtree) 
                                    (the-fixnum (1- depth))))
                             (right (cdr mtree)))
                         (if (and (null left)
                                  (null right))
                             nil
                           (cons left right)))
                     (let ((left (car mtree))
                           (right (_fixnum-memtree-store-nil 
                                   quotient (cdr mtree) 
                                   (the-fixnum (1- depth)))))
                       (if (and (null left)
                                (null right))
                           nil
                         (cons left right)))))))))




; Imported from memory-impl.lisp

(private _memory-p (mem)
  (and (consp mem)
       (consp (car mem))
       (consp (cdr mem))
       (consp (cddr mem))
       (let ((mtree  (caar mem))
             (fast   (cdar mem))
             (size   (cadr mem))
             (depth  (caddr mem)))
         (and (natp size)
              (natp depth)
              (booleanp fast)
              (implies fast (signed-byte-p 29 depth))
              (<= size (expt 2 depth))
              (_memtree-p mtree depth)))))

(private _memory-fix (mem)
  (declare (xargs :guard (_memory-p mem)))
  (if (mbt (_memory-p mem))
      mem
    (list* (cons nil t) 0 0 nil)))

(private _memory-size (mem)
  (declare (xargs :guard (_memory-p mem)))
  (cadr (_memory-fix mem)))

(private _memory-depth (mem)
  (declare (xargs :guard (_memory-p mem)))
  (caddr (_memory-fix mem)))

(private _memory-fast (mem)
  (declare (xargs :guard (_memory-p mem)))
  (cdar (_memory-fix mem)))

(private _memory-mtree (mem)
  (declare (xargs :guard (_memory-p mem)))
  (caar (_memory-fix mem)))

(private _memory-record (mem)
  (declare (xargs :guard (_memory-p mem)))
  (cdddr (_memory-fix mem)))

(private _bad-memory-p (x)
  (or (not (_memory-p x))
      (and (equal (_memory-fast x) t)
           (equal (_memory-depth x) 0)
           (equal (_memory-size x) 0)
           (equal (_memory-record x) nil)
           (_bad-memory-p (_memory-mtree x)))))

(private _to-mem (x)
  (if (_bad-memory-p x)
      (list* (cons x t) 0 0 nil)
    x))

(private _from-mem (x)
  (declare (xargs :guard (_memory-p x)))
  (if (_bad-memory-p x)
      (_memory-mtree x)
    x))


(defdoc memory-p
  ":Doc-Section memory
  recognizes valid ~il[memory] structures~/
  ~bv[]
     (MEM::memory-p mem)
  ~ev[]
  ~c[memory-p] has a guard of ~c[t] and can be called on any object.  

  ~c[memory-p] returns ~c[t] if ~c[mem] is a memory, ~c[nil] otherwise.  

  The implementation of memory-p is ~il[private].
  ~/
  ~l[memory] and also ~pl[new]")

(private memory-p (mem)
  (and (_memory-p mem)
       (posp (_memory-size mem))
       (posp (_memory-depth mem))))


(defdoc size 
  ":Doc-Section memory
  returns the capacity of a memory structure~/
  ~bv[]
     (MEM::size mem)
  ~ev[]
  ~c[size] is guarded with ~c[(memory-p mem)].

  ~c[size] returns the capacity of a memory, i.e., the number of elements that
  the memory can hold.  Addresses for ~c[mem] are naturals in the range ~c[0] 
  through ~c[(size mem) - 1].

  A memory's size is specified when it is created with ~c[new], and is fixed 
  throughout its lifetime.  The implementation of ~c[size] is ~il[private].
  ~/
  ~l[memory] and also ~pl[new].")

(private size (mem)
  (declare (xargs :guard (memory-p mem)))
  (mbe :logic (if (memory-p mem)
                  (_memory-size mem)
                1)
       :exec (cadr mem)))


(defdoc address-p
  ":Doc-Section memory
  recognizes valid addresses for a particular memory~/
  ~bv[]
     (MEM::address-p addr mem)
  ~ev[]
  ~c[address-p] is guarded with ~c[(memory-p mem)].

  ~c[address-p] returns true if ~c[addr] is a valid address for ~c[mem] --
  that is, if ~c[(and (natp addr) (< addr (size mem)))].  It is not 
  ~il[private], and is left enabled by default.
  ~/
  ~l[memory] and ~pl[address-fix]~/")

(defun address-p (addr mem)
  (declare (xargs :guard (memory-p mem)))
  (and (natp addr)
       (< addr (size mem))))


(defdoc new
  ":Doc-Section memory
  create a new memory object with a given capacity~/
  ~bv[]
     (MEM::new size)
  ~ev[]
  ~c[new] is guarded so that ~c[size] must be a positive integer.

  ~c[new] creates a new memory structure with the given capacity.  For 
  example, ~c[(new 30)] creates a memory that can hold 30 elements.  The
  capacity of a memory is fixed througout its lifetime.  The implementation
  of ~c[new] is ~il[private].
  ~/
  ~l[memory], ~pl[memory-p] and also ~pl[address-p].")

(private new (size)
  (declare (xargs :guard (posp size)))
  (if (or (not (posp size))
          (equal size 1))
      (cons (cons nil t) (cons 1 (cons 1 nil)))
    (let ((depth (_log2 (1- size))))
      (cons 
       (cons nil (signed-byte-p 29 depth))
       (cons size 
             (cons depth nil))))))

(private _load (addr mem)
  (declare (xargs :guard (and (memory-p mem)
                              (address-p addr mem))))
  (mbe :logic (let ((mtree (_memory-mtree mem))
                    (depth (_memory-depth mem))
                    (record (_memory-record mem)))
                (if (address-p addr mem)
                    (_memtree-load addr mtree depth)
                  (g addr record)))
       :exec (let* ((fast (cdar mem))
                    (mtree (caar mem))
                    (depth (caddr mem)))
               (if fast 
                   (_fixnum-memtree-load addr mtree depth)
                 (_memtree-load addr mtree depth)))))

(private _store (addr elem mem)
  (declare (xargs :guard (and (memory-p mem)
                              (address-p addr mem))))
  (mbe :logic (let ((fast   (_memory-fast mem))
                    (mtree  (_memory-mtree mem))
                    (size   (_memory-size mem))
                    (depth  (_memory-depth mem))
                    (record (_memory-record mem)))
                (if (address-p addr mem)
                    (cons (cons (if elem
                                    (_memtree-store addr elem mtree depth)
                                  (_memtree-store-nil addr mtree depth))
                                fast)
                          (cons size (cons depth record)))
                  (cons (cons mtree fast)
                        (cons size (cons depth (s addr elem record))))))
       :exec (let* ((mtree  (caar mem))
                    (fast   (cdar mem))
                    (memcdr (cdr mem))
                    (depth  (cadr memcdr)))
               (cons (cons (if fast 
                               (if elem
                                   (_fixnum-memtree-store addr elem mtree depth)
                                 (_fixnum-memtree-store-nil addr mtree depth))
                             (if elem
                                 (_memtree-store addr elem mtree depth)
                               (_memtree-store-nil addr mtree depth)))
                           fast)
                     memcdr))))

(defdoc load
  ":Doc-Section memory
  access memory, retrieving the value at some address~/
  ~bv[]
     (MEM::load addr mem)
  ~ev[]
  ~c[load] has a guard that requires ~c[(memory-p mem)] and also requires
  ~c[(address-p addr mem)].

  ~c[load] looks up the current value stored at ~c[addr] in ~c[mem] and 
  returns that value to the user.  This is analagous to ~c[nth], ~c[assoc], 
  ~c[aref1], and so forth.  The implementation of load is ~il[private].
  ~/
  ~l[memory], ~pl[memory-p], ~pl[address-p] and also ~pl[store].")

(private load (addr mem)
  (declare (xargs :guard (and (memory-p mem)
                              (address-p addr mem))))
  (mbe :logic (_load addr (_to-mem mem))
       :exec  (let* ((fast (cdar mem))
                     (mtree (caar mem))
                     (depth (caddr mem)))
                (if fast 
                    (_fixnum-memtree-load addr mtree depth)
                  (_memtree-load addr mtree depth)))))

(defdoc store
  ":Doc-Section memory
  update memory, overwriting an address with a new value~/
  ~bv[]
     (MEM::store addr elem mem)
  ~ev[]
  ~c[store] has a guard that requires ~c[(memory-p mem)] and also requires
  ~c[(address-p addr mem)].

  ~c[store] returns a copy of ~c[mem], except that the element at address 
  ~c[addr] is overwritten with ~c[elem].  This is analagous to ~c[update-nth],
  ~c[acons], ~c[aset1], and the like.  The implementation of ~c[store] is 
  ~il[private].
  ~/
  ~l[memory], ~pl[memory-p], ~pl[address-p], and also ~pl[load].")

(private store (addr elem mem)
  (declare (xargs :guard (and (memory-p mem)
                              (address-p addr mem))))
  (mbe :logic (_from-mem (_store addr elem (_to-mem mem)))
       :exec (let* ((mtree  (caar mem))
                    (fast   (cdar mem))
                    (memcdr (cdr mem))
                    (depth  (cadr memcdr)))
               (cons (cons (if fast 
                               (if elem
                                   (_fixnum-memtree-store addr elem mtree depth)
                                 (_fixnum-memtree-store-nil addr mtree depth))
                             (if elem
                                 (_memtree-store addr elem mtree depth)
                               (_memtree-store-nil addr mtree depth)))
                           fast)
                     memcdr))))



; THEOREMS
;
; Users of the library should base their work on the following theormes.  First
; we note that memory-p is a boolean-valued function, "new" always creates a
; memory-p object, and "store" always produces a memory-p object if it was
; given one to begin with.

(defthm memory-p-is-boolean
  (or (equal (memory-p mem) t)
      (equal (memory-p mem) nil))
  :rule-classes :type-prescription)

(defthm memory-p-of-new
  (memory-p (new size)))

(defthm memory-p-of-store
  (implies (memory-p mem)
           (memory-p (store addr elem mem))))



; We also observe that the size of a memory is always a positive number,
; creating a new memory always gives a memory that has the desired size (or 1
; if the stated size is invalid), and that storing into a memory does not
; change its size.

(defthm size-is-positive
  (and (integerp (size m))
       (< 0 (size m)))
  :rule-classes :type-prescription)

(defthm size-of-new
  (equal (size (new size))
         (if (posp size)
             size
           1)))

(defthm size-of-store
  (implies (memory-p mem)
           (equal (size (store addr elem mem))
                  (size mem))))



; We also note that upon creation, the value of every address in a memory 
; happens to be nil.

(defthm load-of-new
  (equal (load addr (new size))
         nil))



; Finally, we give the classic record lemmas.  As with the records book, we
; have gone to great lengths to make these hypothesis free, so your rewriting
; should never be burdened with extra hypotheses.  Of course, our rules are
; more strongly guarded than the records book, so you may need to work harder
; on guard verification to get the speed benefits we have to offer.

(defthm load-of-same-store
  (equal (load a (store a elem mem))
         elem))

(defthm load-of-store-when-diff
  (implies (not (equal a b))
           (equal (load a (store b elem mem))
                  (load a mem))))

(defthm store-of-same-store
  (equal (store a e1 (store a e2 mem))
         (store a e1 mem)))

(defthm store-of-store-when-diff
  (implies (not (equal a b))
           (equal (store a e1 (store b e2 mem))
                  (store b e2 (store a e1 mem)))))

(defthm store-of-same-load
  (equal (store a (load a mem) mem)
         mem))

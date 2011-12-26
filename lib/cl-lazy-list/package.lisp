(in-package :cl-user)

(defpackage :lazy-list
  (:nicknames :lazy :ll)
  (:use :cl)
  (:shadow cl:cons
		   cl:make-list
		   ;; accessors
		   ;; not included accessors:
		   cl:last cl:butlast
		   cl:cdr
		   cl:cadr
		   cl:cddr
		   cl:rest
		   cl:second
		   cl:third
		   cl:fourth
		   cl:fifth
		   cl:sixth
		   cl:seventh
		   cl:eighth
		   cl:ninth
		   cl:tenth
		   cl:nth
		   cl:nthcdr
		   ;; iteration operators
		   ;; not included iteration operators:
		   cl:reduce cl:mapc cl:dolist
		   cl:mapcar
		   cl:mapcan
		   cl:mapcon
		   cl:maplist
		   ;; list function
		   ;; not included ones:
		   cl:reverse cl:every cl:notany cl:notevery cl:member
		   cl:pushnew cl:set-exclusive-or cl:set-difference cl:subsetp
		   cl:adjoin cl:intersection cl:union
		   cl:length					; return current length only
		   cl:append
		   cl:revappend
		   cl:subst
		   cl:subst-if
		   cl:subst-if-not
		   cl:pop
		   cl:acons
		   cl:assoc
		   cl:rassoc
		   cl:pairlis
		   cl:copy-alist
		   cl:sublis
		   ;; sequence functions
		   ;; not included ones:
		   cl:find cl:position cl:sort cl:search cl:fill
		   cl:remove-duplicates
		   cl:elt
		   cl:copy-seq
		   cl:subseq
		   cl:merge
		   cl:remove
		   cl:remove-if
		   cl:remove-if-not
		   cl:substitute
		   cl:substitute-if
		   cl:substitute-if-not)
  (:export #:delay
		   #:force
		   #:enumerate-all
		   ;; constructors
		   #:cons
		   #:make-list
		   #:copy-list					; same
		   ;; accessors
		   ;; not included accessors:
		   ;; last, butlast
		   #:car
		   #:cdr
		   #:caar
		   #:cadr
		   #:cdar
		   #:cddr
		   #:rest
		   #:first
		   #:second
		   #:third
		   #:fourth
		   #:fifth
		   #:sixth
		   #:seventh
		   #:eighth
		   #:ninth
		   #:tenth
		   #:nth
		   #:nthcdr
		   ;; iteration operators
		   ;; not included iteration operators:
		   ;; reduce, mapc, dolist
		   #:mapcar
		   #:mapcan
		   #:maplist
		   #:filter
		   ;; list function
		   ;; not included ones:
		   ;; reverse, every, some, notany, notevery, member
		   ;; pushnew, set-exclusive-or, set-difference, subsetp
		   ;; adjoin union intersection
		   #:length					; Note: return current length only
		   #:append
		   #:revappend
		   #:subst
		   #:subst-if
		   #:subst-if-not
		   #:push
		   #:pop
		   ;; sequence functions
		   ;; not included ones:
		   ;; find, position, sort, search, fill
		   #:remove-duplicates
		   #:subseq
		   #:merge
		   #:remove
		   #:remove-if
		   #:remove-if-not
		   #:substitute
		   #:substitute-if
		   #:substitute-if-not))


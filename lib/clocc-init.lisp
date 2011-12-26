;;; clocc
(defparameter *clocc-root* (directory-namestring
			    (MERGE-PATHNAMES
			     "cl/lib/clocc/" (USER-HOMEDIR-PATHNAME)))) 
(load (concatenate 'string *clocc-root* "clocc"))
(load (translate-logical-pathname "clocc:src;cllib;base")) ; or whatever ...
(load (translate-logical-pathname "clocc:src;defsystem;defsystem"))

(defparameter *clocc-systems-locations*
  '("clocc:src;cllib;"
	"clocc:src;port;"
	"clocc:src;port;configuration;"
	"clocc:src;port;environment;"
	"clocc:src;ext;queues;"
	"clocc:src;ext;seqences;"
	"clocc:src;ext;union-find;"
	"clocc:src;gui;clue;"
	"clocc:src;gui;clx;"
	"clocc:src;screamer;"
	"clocc:src;tools;metering;"
	"clocc:src;tools;clunit;"
	"clocc:src;ytools;"
	"clocc:src;f2cl;"
	"clocc:src;f2cl;packages;"))

(defparameter *clocc-systems-all*
  '("cllib" "queues" "seq" "union-find" "f2cl" "blas" "hompack" "minipack" "odepack" "quadpack" "toms715" "clio-examples" "clio" "clue" "pictures" "clx" "defconf" "env" "port" "screamer" "clunit" "metering"))

;;; TODO: homepack -- seems need very long time for compiling
(defparameter *clocc-systems-working*
  '("cllib" "f2cl" "blas" "odepack" "quadpack" "toms715" "port" "screamer" "metering"))

(dolist (l *clocc-systems-locations*)
  (mk:add-registry-location (translate-logical-pathname l)))


;;; Dummy settings file to load the original Albert configuration from
;;; /etc/albert/settings.lisp and additionally the user-specific
;;; configuration in ~/.albert-settings.lisp
;;;
;;; Better do modify your settings in the mentioned-above places than
;;; here, if you do not want your settings to be overwritten on
;;; upgrades.
;;;
;;;                                    René van Bevern <rvb@progn.org>

(let ((configs (list #p"/etc/albert/settings.lisp"
		     (merge-pathnames (user-homedir-pathname)
				      ".albert-settings.lisp"))))
  (mapc (lambda (x)
	  (load x :if-does-not-exist nil))
	configs))

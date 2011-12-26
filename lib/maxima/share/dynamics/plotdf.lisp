;; plotdf.mac - Adds a function plotdf() to Maxima, which draws a Direction
;;              Field for an ordinary 1st order differential equation,
;;              or for a system of two autonomous 1st order equations.
;;   
;; Copyright (C) 2004, 2008 Jaime E. Villate <villate@gnu.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA  02111-1307  USA
;;
;; See plotdf.usg (which should come together with this program) for
;; a usage summary
;;
;; $Id: plotdf.lisp,v 1.11 2009/04/10 13:46:28 villate Exp $

(in-package :maxima)

;; parses a plotdf option into a command-line option for tcl scripts
(defun plotdf-option-to-tcl (value s1 s2)
  (let (vv)
    (unless (and  ($listp value)
                  (symbolp (setq name (second value))))
      (merror "~M is not a plotdf option.  Must be [symbol,..data]" value))
    (setq value
      (case name
        (($xradius $yradius $xcenter $ycenter $tinitial $tstep)
         (check-list-items name (rest (rest value)) 'number 1))
        (($width $height $nsteps $versus_t)
         (check-list-items name (rest (rest value)) 'fixnum 1))
        ($trajectory_at
         (check-list-items name (rest (rest value)) 'number 2))
        ($bbox (check-list-items name (rest (rest value)) 'number 4))
        (($xfun $parameters $sliders $vectors $fieldlines $curves 
		$windowtitle $xaxislabel $yaxislabel $psfile) value)
	($axes
	 (if (not (third value))
	     (setq value '((mlist simp) $axes 0))  
	   (case (third value)
		 ($x (setq value '((mlist simp) $axes "x")))
		 ($y (setq value '((mlist simp) $axes "y")))
		 (t (setq value '((mlist simp) $axes "xy"))))))
	($box
	 (if (not (third value))
	     (setq value '((mlist simp) $nobox 1))
	   (setq value '((mlist simp) $nobox 0))))
        ($direction
         (or (member (third value) '($forward $backward $both))
             (merror "direction: choose one of [forward,backward,both]")) 
         value)
        (t (cond
            ((eql name s1)
	     (setq value (check-range value))
             (check-list-items '$x (rest (rest value)) 'number 2))
            ((eql name s2)
	     (setq value (check-range value))
             (check-list-items '$y (rest (rest value)) 'number 2))
            (t (merror "Unknown option ~M" name))))))
    (setq vv (mapcar #'stripdollar (rest value)))
    (with-output-to-string (st)
      (cond ((or (eql (first vv) 'x) (eql (first vv) 'y))
             (format st "-~(~a~)center " (first vv))
             (format st "{~a} " (/ (+ (third vv) (second vv)) 2))
             (format st "-~(~a~)radius " (first vv))
             (format st "{~a}" (/ (- (third vv) (second vv)) 2)))
            (t
             (format st "-~(~a~) " (first vv))
             (format st "{~{~a~^ ~}}" (rest vv)))))))

;; applies float(ev(expression, numer)) to an expression, and returns a string

(defun expr_to_str (fun)
  (mstring (mfuncall '$float (mfuncall '$ev fun '$numer))))

;; plots the direction field for an ODE  dy/dx = f(x,y), or for an autonomous
;; system of 2 equations dx/dt = f(x,y), dy/dt = g(x,y) 
;;
(defun $plotdf (ode &rest options)
  
  (let (cmd (opts " ") (s1 '$x) (s2 '$y))
    (unless ($listp ode) (setf ode `((mlist) ,ode)))
    ;; parse arguments and prepare string cmd with the equation(s)
    (unless
	(member (second (first options))
		'($xradius $yradius $xcenter $ycenter $tinitial $tstep
			   $width $height $nsteps $versus_t $xfun $parameters
			   $sliders $vector $trajectory $orthogonal))
      (if (and (listp (first options)) (= (length (first options)) 3)
	       (symbolp (second (first options)))
	       (symbolp (third (first options))))
	  (progn
	    (setf s1 (second (first options)))
	    (setf s2 (third (first options)))
	    (defun subxy (expr)
	      (if (listp expr)
		  (mapcar #'subxy expr)
		(cond ((eq expr s1) '$x) ((eq expr s2) '$y) (t expr))))
	    (setf ode (mapcar #'subxy ode))
	    (setf options (cdr options)))))
;; the next two lines should take into account parameters given in the options
;;    (if (delete '$y (delete '$x (rest (mfuncall '$listofvars ode))))
;;        (merror "The equation(s) can depend only on 2 variable which must be specified!"))
    (case (length ode)
          (3 (setq cmd (concatenate 'string " -dxdt \""
                                    (expr_to_str (second ode)) "\" -dydt \""
                                    (expr_to_str (third ode)) "\"")))
          (2 (setq cmd (concatenate 'string " -dydx \""
                                    (expr_to_str (second ode)) "\"")))
          (t (merror "Argument must be either dydx or [dxdt, dydt]")))
    
    ;; parse options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                  (plotdf-option-to-tcl v s1 s2))))))

    (unless (search "-xaxislabel " opts)
      (setq opts (concatenate 'string opts " -xaxislabel " (ensure-string s1))))
    (unless (search "-yaxislabel " opts)
      (setq opts (concatenate 'string opts " -yaxislabel " (ensure-string s2))))

    (show-open-plot
     (with-output-to-string (st)
                  (cond ($show_openplot (format st "plotdf ~a ~a~%" cmd opts))
                              (t (format st "{plotdf ~a ~a}" cmd opts)))))))

;; plot equipotential curves for a scalar field f(x,y)
(defun $ploteq (fun &rest options)
  
  (let (cmd mfun (opts " ") (s1 '$x) (s2 '$y))
    (setf mfun `((mtimes) -1 ,fun))
    ;; parse arguments and prepare string cmd with the equation(s)
    (unless
	(member (second (first options))
		'($xradius $yradius $xcenter $ycenter $tinitial $tstep
			   $width $height $nsteps $versus_t $xfun $parameters
			   $sliders $vector $trajectory $orthogonal))
      (if (and (listp (first options)) (= (length (first options)) 3)
	       (symbolp (second (first options)))
	       (symbolp (third (first options))))
	  (progn
	    (setf s1 (second (first options)))
	    (setf s2 (third (first options)))
	    (defun subxy (expr)
	      (if (listp expr)
		  (mapcar #'subxy expr)
		(cond ((eq expr s1) '$x) ((eq expr s2) '$y) (t expr))))
	    (setf mfun (mapcar #'subxy mfun))
	    (setf options (cdr options)))))
;; the next two lines should take into account parameters given in the options
;;    (if (delete '$y (delete '$x (rest (mfuncall '$listofvars ode))))
;;        (merror "The equation(s) can depend only on 2 variable which must be specified!"))
    (setq cmd (concatenate 'string " -dxdt \""
			   (expr_to_str (mfuncall '$diff mfun '$x))
			   "\" -dydt \""
			   (expr_to_str (mfuncall '$diff mfun '$y)) 
			   "\" "))
    
    ;; parse options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                  (plotdf-option-to-tcl v s1 s2))))))

    (unless (search "-vectors " opts)
      (setq opts (concatenate 'string opts " -vectors {}")))
    (unless (search "-fieldlines " opts)
      (setq opts (concatenate 'string opts " -fieldlines {}")))
    (unless (search "-curves " opts)
      (setq opts (concatenate 'string opts " -curves {red}")))
    (unless (search "-windowtitle " opts)
      (setq opts (concatenate 'string opts " -windowtitle {Ploteq}")))
    (unless (search "-xaxislabel " opts)
      (setq opts (concatenate 'string opts " -xaxislabel " (ensure-string s1))))
    (unless (search "-yaxislabel " opts)
      (setq opts (concatenate 'string opts " -yaxislabel " (ensure-string s2))))
							      
    (show-open-plot
     (with-output-to-string (st)
                  (cond ($show_openplot (format st "plotdf ~a ~a~%" cmd opts))
                              (t (format st "{plotdf ~a ~a}" cmd opts)))))))

(use-package :grt)

(defconstant +skyblue+ (rgb 0.6 0.6 1.0))

(defun all-examples (&key screen (width 400) (height 300))
  (mapc (lambda (f) (funcall f :screen screen :width width :height height))
	(list
	 #'box-demo
	 #'classic-demo
	 #'csg-demo
	 #'csg-demo2
	 #'csg-demo3
	 #'inside-sphere-demo
	 #'noise-demo
	 #'plane-demo
	 #'perlin-noise-demo
	 #'rotate-demo
	 #'scale-demo
	 #'translate-demo
	 #'turbulence-demo)))

(defun box-demo (&key (file "box-demo.ppm") screen (width 400) (height 300))
  (new-scene)  
  (set-ambient-light +white+)
  (set-background-color +skyblue+)
  (scene (light :location '(0 5 -15)))
  (scene (box '(0 0.01 0) '(1 1.01 1)
	      :transform (rotate -30.0 45.0 0.0)
	      :pigment (make-pigment :pattern (function gradient)
				     :list `(,+blue+ ,+red+))
	      :ambient 0.1))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(2 3 -10) 
		   :look-at '(0.5 0.5 0)
		   :focal-length 8.0)
   :refresh 3
   :keep-open 1
   :screen screen
   :width width		  
   :height height))

(defun classic-demo (&key (file "classic-demo.ppm") screen
			  (width 400)
			  (height 300))
  (new-scene)    
  (set-ambient-light +white+)
  (set-background-color +skyblue+)  
  (scene (light :location '(0 15 -15)))  
  (scene (plane
	  +Y+ '(0 0.01 0)
	  :pigment (make-pigment :pattern (function checker)
				 :list `(,+white+ ,+black+))
	  :reflection (make-solid 0.2)
	  :ambient 0.2
	  :diffuse 0.8))
  (scene (sphere
	  1.0 '(0 1 0)
	    :pigment (make-pigment :solid +red+)
	    :filter (make-solid 0.5)
	    :reflection (make-solid 0.2)
	    :ior 1.4))  
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(2 3.5 -10) 
		   :look-at '(0 1 0))
   :depth 6
   :keep-open 1
   :screen screen
   :width width		  
   :height height))

(defun csg-demo (&key (file "csg-demo.ppm") screen (width 400) (height 300))
  (new-scene)
  (set-background-color +skyblue+)
  (scene (light :location '(20 10 -20)))
  (scene (light :location '(-10 -10 -10)))
  (scene (csg-difference (csg-intersection (sphere 1.0 '(0 -0.5 0))
					   (sphere 1.0 '(0 0.5 0))
					   :pigment (make-pigment :solid +red+))
			 (sphere 0.6 '(0 0 -1)
				 :pigment (make-pigment :solid +blue+))
			 (sphere 0.6 '(1 0 0))))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :look-at +O+ :location '(2 2 -4) :focal-length 5)
   :screen screen
   :keep-open 1
   :width width
   :height height))

(defun csg-demo2 (&key (file "csg-demo2.ppm") screen (width 400) (height 300))
    (new-scene)
    (set-background-color +skyblue+)
    (scene (light :location '(5 50 10)))
;    (scene (box '(-1.4 0 -1.4) '(1.4 0.5 1.4)))
    (scene (csg-difference
	    (csg-intersection
	     (plane '(1 1 0) '(0 1 0) :pigment (make-pigment :solid +red+))
	     (plane '(-1 1 0) '(0 1 0):pigment (make-pigment :solid +blue+))
	     (plane '(0 1 1) '(0 1 0) :pigment (make-pigment :solid +green+))
	     (plane '(0 1 -1) '(0 1 0) :pigment (make-pigment :solid +white+)))
	    (plane +Y+ +O+)                                  ; A cheat to accel. first gen. rays:
	    :bounded-by '(box (-1.4 0 -1.4) (1.4 0.1 1.4)))) ; screen optimized (undersize) bounding box!
                                                             ; Not view independent.
    (render
     :file (concatenate 'string "examples/" file)
     :camera (camera :look-at '(0 -0.5 0) :location '(2 5 -4) :focal-length 4)
     :screen screen
     :keep-open 1
     :width width
     :height height))

(defun csg-demo3 (&key (file "csg-demo3.ppm") screen (width 400) (height 300))
  (new-scene)
  (set-background-color +skyblue+)
  (scene (light :location '(5 10 10)))
  (scene (csg-difference (box +O+ '(1 1 1))
			 (box '(0.1 0.1 -0.1) '(0.9 0.9 1.1))
			 (box '(-0.1 0.1 0.1) '(1.1 0.9 0.9))
			 (box '(0.1 -0.1 0.1) '(0.9 1.1 0.9))))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :look-at '(0.5 0.5 0.5)
		   :location '(2 2.5 3)
		   :focal-length 4)
   :screen screen
   :keep-open 1
   :width width
   :height height))

(defun plane-demo (&key (file "plane-demo.ppm") screen
			(width 400)
			(height 300))
  (new-scene)    
  (set-ambient-light +white+)
  (set-background-color +skyblue+)  
  (scene (light :location '(0 15 -15)))  
  (scene (plane
	  +Y+ +O+
	  :pigment (make-pigment :pattern (function checker)
				 :list `(,+white+ ,+black+))))
  (scene (plane
	  '(0.5 0.5 0) +O+
	  :pigment (make-pigment :pattern (function checker)
				 :list `(,+white+ ,+black+))))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(2 3.5 -10) 
		   :look-at '(0 1 0))
   :keep-open 1
   :screen screen
   :width width		  
   :height height))


(defun noise-demo (&key (file "noise-demo.ppm") screen
			(width 400) 
			(height 300))
  (new-scene)  
  (scene (plane
	  +Y+ +O+
	  :pigment (make-pigment :pattern (function noise)
				 :list `(,+white+ ,+skyblue+))
	  :ambient 1.0
	  :diffuse 0.0))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(1 3 -5) 
		   :look-at +O+)
   :screen screen
   :keep-open 1
   :width width		  
   :height height))

(defun perlin-noise-demo (&key (file "perlin-noise-demo.ppm") screen
			       (width 400) 
			       (height 300))
  (new-scene)  
  (scene (plane
	  +Y+ +O+
	  :pigment (make-pigment
		    :pattern (make-perlin-noise 4 0.75)
		    :list `(,+white+ ,+skyblue+))
	  :ambient 1.0
	  :diffuse 0.0))  
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(1 3 -5) 
		   :look-at +O+)
   :screen screen
   :keep-open 1
   :width width		  
   :height height))

(defun rotate-demo (&key (file "rotate-demo.ppm") screen
			 (width 400) 
			 (height 300))
  (new-scene)    
  (set-background-color +skyblue+)
  (scene (light :location '(0 0 50)))  
  (scene (sphere 1.0 +Y+ :pigment (make-pigment :solid +green+)))  
  (scene (sphere 1.0 +Y+
		 :transform (rotate 0.0 0.0 90.0)
		 :pigment (make-pigment :solid +red+)))  
  (scene (sphere 1.0 +O+
		 :transform (combine (translate 0.0 1.0 0.0)
				     (rotate-z -90.0))
		 :pigment (make-pigment :solid +blue+)))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(0 0 10) 
		   :look-at +O+)
   :screen screen
   :keep-open 1
   :width width		  
   :height height))

(defun scale-demo (&key (file "scale-demo.ppm") screen
			(width 400) 
			(height 300))
    (new-scene)    
    (set-ambient-light (rgb 0.5 0.5 0.6))
    (set-background-color +skyblue+)  
    (scene (light :location '(0 0 50)))  
    (scene (sphere 1.0 +O+
		   :transform (scale 1.0 2.5 1.0)
		   :pigment (make-pigment :solid +red+)))
    (scene (sphere 1.0 +O+
		   :transform (scale 2 1 0.75)
		   :pigment (make-pigment :solid +blue+)))  
    (render
     :file (concatenate 'string "examples/" file)
     :camera (camera :location '(0 0 10) 
		     :look-at +O+)
     :screen screen
     :width width		  
     :height height))

(defun translate-demo (&key (file "translate-demo.ppm") screen
			    (width 400) 
			    (height 300))
  (new-scene)  
  (set-background-color +skyblue+)  
  (scene (light :location '(0 0 50)))  
  (scene (sphere 1.0 +O+ :pigment (make-pigment :solid +green+)))  
  (scene (sphere 1.0 +O+
		 :transform (translate -1.5 0.0 0.0)
		 :pigment (make-pigment :solid +red+)))  
  (scene (sphere 1.0 +O+
		 :transform (translate 1.5 0.0 0.0)
		 :pigment (make-pigment :solid +blue+)))  
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(0 -10 5) 
		   :look-at +O+)
   :screen screen
   :keep-open 1
   :width width		  
   :height height))

(defun turbulence-demo (&key (file "turbulence-demo.ppm") screen
			     (width 400) 
			     (height 300))
  (new-scene)  
  (scene (plane
	  +Y+ +O+
	  :pigment (make-pigment
		    :pattern (make-turbulence)
		    :list `(,+white+ ,+black+))
	  :ambient 1.0
	  :diffuse 0.0))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(0 3 0)
		   :look-at +O+)
   :screen screen
   :keep-open 1
   :width width		  
   :height height))

(defun inside-sphere-demo (&key (file "inside-sphere-demo.ppm") screen
			    (width 400) 
			    (height 300))
  (new-scene)  
  (set-background-color +skyblue+)  
  (scene (light :location '(0 10 0)))  
  (scene (sphere 50.0 +O+ :pigment (make-pigment :solid +green+)
		 :ambient 1.0))
  (render
   :file (concatenate 'string "examples/" file)
   :camera (camera :location '(0 -10 5)
		   :look-at +O+)
   :screen screen
   :keep-open 1
   :width width		  
   :height height))


(with-chart (:bar 600 400)
  (flet ((random-data (min max)	   
	   (loop for i from 0 to 48
		 collect (list i (+ min (random (float (- max min))))))))
    (add-series "A" (random-data 0 100))
    (add-series "B" (random-data -100 100))
    (add-series "C" (random-data -50 50))
    (add-series "D" (random-data -100 0))
    (add-series "E" (random-data -75 75)))
  (set-axis :y nil :draw-zero-p T)
  (set-axis :x nil :data-interval 1)
  
  (save-file "too-many-bars-vecto.png"))
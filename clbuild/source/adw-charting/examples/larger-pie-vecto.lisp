(with-chart (:pie 300 200)
    (loop for (label value) in '(("A" 400)
				 ("B" 217)
				 ("C" 212.5)
				 ("D" 350)
				 ("E" 1000))
	  do (add-slice label value))
    (save-file "larger-pie-vecto.png"))
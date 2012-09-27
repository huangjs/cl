(with-chart (:line 400 300)
  (add-series "test" '((1 0.0) (2 2.0) (3 3.0) (4 1.5)) :color '(0 0 1))
  (set-axis :y "amount" :label-formatter "~,2f")
  (set-axis :x "days" :data-interval 1
	    :label-formatter (lambda (v) (format nil "~d" (round v))))
  (save-file "mackey.png"))
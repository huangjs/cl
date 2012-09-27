(with-chart (:line 400 300 :background '(.7 .5 .7))
  (add-series "A" '((-.1 -.2) (0 .4) (.1 .5) (.4 .6) (.5 -.3)))
  (add-series "B" '((-.1 .4) (0 -.2) (.1 .6) (.5 -.2) (.6 .5)))
  (add-series "C" '((-.1 0) (0 .3) (.1 .1) (.2 .5) (.4 -.6))
	      :color '(.3 .7 .9))
  (set-axis :y "widgets" :label-formatter "~,2F")
  (set-axis :x nil
	    :label-formatter #'(lambda (v)
				 ;;could do something more interesting here
				 (format nil "~,1F" (expt 2 v))))
  (save-file "larger-line-vecto.png"))
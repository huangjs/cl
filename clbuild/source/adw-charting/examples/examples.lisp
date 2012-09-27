;; Copyright (c) 2008 Accelerated Data Works, Ryan Davis

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage #:net.acceleration.adw-charting-examples
  (:use #:cl #:adw-charting))

(in-package #:net.acceleration.adw-charting-examples)

(defvar +boink-data+ '((3220487700 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.0"
                 9.179666666666666d0 0.039405685894399696d0)
                (3221090100 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.1"
                 9.539666666666667d0 0.007055336829103466d0)
                (3225916800 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.2"
                 10.753d0 0.003999999999687438d0)
                (3228681600 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.3"
                 10.614333333333335d0 0.013920408678669564d0)
                (3231187200 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.4"
                 10.520666666666665d0 0.015452435982315785d0)
                (3233834588 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.5"
                 4.021333333333334d0 0.003527668414327933d0)
                (3236416994 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.6" 0.636d0
                 0.0027325202042536483d0)
                (3239264050 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.7"
                 0.6373333333333333d0 0.001333333333343252d0)
                (3242035118 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.8"
                 0.6346666666666666d0 0.0026666666666726262d0)
                (3244543999 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.9" 0.636d0
                 0.004000000000002d0)
                (3247393629 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.10"
                 0.6346666666666666d0 0.001333333333343252d0)
                (3250352729 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.11"
                 0.6373333333333333d0 0.003527668414751055d0)
                (3252508216 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.12"
                 0.6306666666666666d0 0.001333333333343252d0)
                (3255207859 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.13"
                 0.6306666666666666d0 0.001333333333343252d0)
                (3257502170 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.7.14"
                 1.168d0 0.0d0)
                (3260700452 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8alpha.0"
                 1.1833333333333333d0 0.0023333333333272308d0)
                (3262809600 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.0"
                 1.1765d0 0.001962141687032421d0)
                (3265401600 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.1"
                 1.1726666666666667d0 0.0028713140623054215d0)
                (3268166400 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.2" 1.17d0
                 0.0028751811537286397d0)
                (3270758400 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.3"
                 1.1793333333333333d0 0.001173787790758412d0)
                (3274128000 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.4"
                 1.1716666666666666d0 0.0022310934040787145d0)
                (3276028800 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.5"
                 1.3576666666666666d0 0.07292218074388936d0)
                (3278707200 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.6"
                 1.1906666666666668d0 0.0018196458751748993d0)
                (3281644800 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.7"
                 0.6421666666666667d0 0.001939358427709689d0)
                (3286648345 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.8"
                 0.6383333333333333d0 8.819171036882883d-4)
                (3289119215 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.9"
                 0.6416666666666667d0 0.0022900752049719864d0)
                (3291898308 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.10"
                 0.6426666666666666d0 0.0032110918876810885d0)
                (3295630912 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.11"
                 0.6416666666666667d0 0.001873795909670684d0)
                (3297188109 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.12"
                 0.6365d0 0.0010567244989399649d0)
                (3299770631 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.13"
                 0.6415000000000001d0 0.0028017851452220664d0)
                (3302885185 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.14"
                 0.6415000000000001d0 0.0020124611797470242d0)
                (3310671599 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.17"
                 0.6451666666666668d0 0.0028684103224187445d0)
                (3313262280 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.18"
                 0.6411666666666666d0 0.0012758439472729923d0)
                (3315681384 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.19"
                 0.6441666666666667d0 0.0030704686576772137d0)
                (3318629528 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.20"
                 0.6486666666666666d0 0.003158762064130637d0)
                (3320916959 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.8.21"
                 0.6503333333333333d0 0.002905932629030691d0)
                (3323359736 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.0"
                 0.6558888888888889d0 0.010010334166423676d0)
                (3326119900 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.1"
                 0.6521666666666667d0 0.00805834155933214d0)
                (3328891845 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.2"
                 0.6515000000000001d0 0.007651005083508171d0)
                (3331319566 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.3"
                 0.6654444444444445d0 0.006815602161582924d0)
                (3334064418 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.4"
                 0.6653888888888889d0 0.00645048454829305d0)
                (3336814695 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.5" 0.664d0
                 0.006476698159372025d0)
                (3339362596 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.6"
                 0.6683809523809523d0 0.0062241756732752465d0)
                (3342185572 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.7"
                 0.6665000000000001d0 0.006690765267835046d0)
                (3344689134 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.8"
                 0.6602777777777777d0 0.006360522420458006d0)
                (3347288951 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.9"
                 0.6913333333333335d0 0.02761568870454336d0)
                (3349994883 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.10"
                 0.6886666666666666d0 0.026655300608041676d0)
                (3352402754 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.11"
                 0.6636666666666666d0 0.007523028618579228d0)
                (3355054163 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.12"
                 0.6662499999999999d0 0.00794023604717486d0)
                (3357748000 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.13"
                 0.66825d0 0.007894306540638947d0)
                (3360332432 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.14"
                 0.6666666666666666d0 0.008575275711914507d0)
                (3362932220 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.15"
                 0.66575d0 0.008568905202162075d0)
                (3365526402 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.16"
                 0.666d0 0.008520919000087585d0)
                (3368276815 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.17"
                 0.6604166666666667d0 0.006870510471676585d0)
                (3370773520 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "0.9.18"
                 0.663d0 0.008255393094911835d0)
                (3373839403 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0"
                 0.6686666666666666d0 0.006528879124801801d0)
                (3376130264 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.1"
                 0.6531666666666667d0 0.008683695391742307d0)
                (3378725494 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.2"
                 0.6606666666666666d0 0.00855404683011041d0)
                (3381591822 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.3"
                 0.6629999999999999d0 0.007874007874012367d0)
                (3386771278 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.5"
                 0.6925d0 0.007027327609647438d0)
                (3389213608 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.6"
                 0.6803333333333333d0 0.009360890055238592d0)
                (3391973049 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.7"
                 0.6866666666666666d0 0.007913675671029945d0)
                (3394355973 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.8"
                 0.6823333333333333d0 0.007343656645656509d0)
                (3397158615 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.9"
                 0.6728333333333333d0 0.007504375827852276d0)
                (3399722304 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.10"
                 0.6616666666666666d0 0.009451096884660209d0)
                (3402332883 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.11"
                 0.6703333333333333d0 0.010286305498480806d0)
                (3405022435 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.12"
                 0.6683333333333333d0 0.008534540634292589d0)
                (3407746349 "baker" "SBCL,(:ARCH :EMULATED-X86 :FEATURES NIL)" "1.0.13"
                 0.6656666666666667d0 0.008647414514048853d0)))

(defun boinkmark ()
  (with-line-chart (400 300)
    (add-series "baker: SBCL"
		(loop for row in +boink-data+
		      for i from 0
		      collect (list i (nth 4 row))))
    (set-axis :y "seconds" :label-formatter "~,2F")
    (set-axis :x nil
	      :draw-gridlines-p nil
	      :label-formatter #'(lambda (i)
				   (nth 3 (nth i +boink-data+))))
    (save-file "boink.png")))

(defun random-between (min max)
  (+ min
     (random (float (- max min)))))

(defun random-point (min-x min-y max-x max-y)
  (list (random-between min-x max-x)
	(random-between min-y max-y)))

(defun random-series (n min-x min-y max-x max-y)
  (sort 
   (loop for i from 1 to n
      collect (random-point min-x min-y max-x max-y))
   #'< :key #'first))

(defun mixed-mode ()
  "uses the :mode argument to add-series to mix different types of charts"
  (with-line-chart (400 300)
    (add-series "line" (random-series 20 0 -10 20 10))

    (set-axis :y "foos" :label-formatter "~,2f")
    (set-axis :x "bars"
			   :label-formatter "~,2f"
			   :draw-gridlines-p nil)
    (save-file "mixed-mode.png")))
;;; learning/domains/restaurant-multivalued.lisp
;;; Restaurant example from chapter 18, encoded
;;; using multivalued input attributes suitable for
;;; decision-tree learning.

(defvar *restaurant-multivalued* 
  '((patrons none some full)
    (none . no)
    (some . yes)
    (full 
     (waitestimate 60 30 10 0)
     (60 . no)
     (30
      (alternate yes no)
      (yes
       (fri/sat yes no)
       (no . no)
       (yes . yes))
      (no
       (reservation yes no)
       (no 
        (bar yes no)
        (no . no)
        (yes . yes))
       (yes . yes)))
     (10
      (hungry yes no)
      (no . yes)
      (yes
       (alternate yes no)
       (no . yes)
       (yes
        (raining yes no)
        (no . no)
        (yes . yes))))
     (0 . yes))))

(defvar *restaurant-multivalued-problem*)

(setq *restaurant-multivalued-problem*
      (make-learning-problem
       :attributes '((alternate yes no)
                     (bar yes no)
                     (fri/sat yes no)
                     (hungry yes no)
                     (patrons None Some Full)
                     (price D DD DDD)
                     (raining yes no)
                     (reservation yes no)
                     (type French Italian Thai Burger)
                     (waitestimate 0 10 30 60))
       :goals      '((willwait yes no))
       :examples   '(
                     ((willwait . Yes) (alternate . Yes)  (bar . No)  (fri/sat . No)  
                      (hungry . Yes)  (patrons . Some)  (price . DDD)  (raining . No)  
                      (reservation . Yes)  (type . French)   (waitestimate . 0)   )
                     ((willwait . No) (alternate . Yes)  (bar . No)  (fri/sat . No)  
                      (hungry . Yes)  (patrons . Full)  (price . D)  (raining . No)  
                      (reservation . No)  (type . Thai)   (waitestimate . 30)   )
                     ((willwait . Yes) (alternate . No)  (bar . Yes)  (fri/sat . No)  
                      (hungry . No)  (patrons . Some)  (price . D)  (raining . No)  
                      (reservation . No)  (type . Burger)   (waitestimate . 0)   )
                     ((willwait . Yes) (alternate . Yes)  (bar . No)  (fri/sat . Yes)  
                      (hungry . Yes)  (patrons . Full)  (price . D)  (raining . No)  
                      (reservation . No)  (type . Thai)   (waitestimate . 10)   )
                     ((willwait . No) (alternate . Yes)  (bar . No)  (fri/sat . Yes)  
                      (hungry . No)  (patrons . Full)  (price . DDD)  (raining . No)  
                      (reservation . Yes)  (type . French)   (waitestimate . 60)   )
                     ((willwait . Yes) (alternate . No)  (bar . Yes)  (fri/sat . No)  
                      (hungry . Yes)  (patrons . Some)  (price . DD)  (raining . Yes)  
                      (reservation . Yes)  (type . Italian)   (waitestimate . 0)   )
                     ((willwait . No) (alternate . No)  (bar . Yes)  (fri/sat . No)  
                      (hungry . No)  (patrons . None)  (price . D)  (raining . Yes)  
                      (reservation . No)  (type . Burger)   (waitestimate . 0)   )
                     ((willwait . Yes) (alternate . No)  (bar . No)  (fri/sat . No)  
                      (hungry . Yes)  (patrons . Some)  (price . DD)  (raining . Yes)  
                      (reservation . Yes)  (type . Thai)   (waitestimate . 0)   )
                     ((willwait . No) (alternate . No)  (bar . Yes)  (fri/sat . Yes)  
                      (hungry . No)  (patrons . Full)  (price . D)  (raining . Yes)  
                      (reservation . No)  (type . Burger)   (waitestimate . 60)   )
                     ((willwait . No) (alternate . Yes)  (bar . Yes)  (fri/sat . Yes)  
                      (hungry . Yes)  (patrons . Full)  (price . DDD)  (raining . No)  
                      (reservation . Yes)  (type . Italian)   (waitestimate . 10)   )
                     ((willwait . No) (alternate . No)  (bar . No)  (fri/sat . No)  
                      (hungry . No)  (patrons . None)  (price . D)  (raining . No)  
                      (reservation . No)  (type . Thai)   (waitestimate . 0)   )
                     ((willwait . Yes) (alternate . Yes)  (bar . Yes)  (fri/sat . Yes)  
                      (hungry . Yes)  (patrons . Full)  (price . D)  (raining . No)  
                      (reservation . No)  (type . Burger)   (waitestimate . 30)   ))))





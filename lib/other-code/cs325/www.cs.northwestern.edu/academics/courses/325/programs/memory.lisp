(defvar *memory* nil
 "The knowledge base")

(setq *memory*
 '(
   (dog (isa animal) (legs 4) (skin furred) (brain nil-or-small))
   (buddy (isa chihuahua pet) (age 3))
   (chihuahua (isa dog small-thing) (brain nil))
   (pet (isa animal) (owner human))
   (animal (isa living-thing))
   (living-thing (isa thing))
   (thing (size size-value))
   (small-thing (isa thing) (size small))
   

   (boat (isa thing) (navzone number-value))
   (day-boat (isa boat) (navzone 5))
   (wheel-boat (isa boat) (navzone 100))
   (engineless-boat (isa day-boat))
   (small-multi-hull-boat (isa day-boat))
   (pedal-wheel-boat(isa engineless-boat wheel-boat))
   (small-catamaran (isa small-multi-hull-boat))
   (pedalo (isa pedal-wheel-boat small-catamaran))
   ))


(defun get-frame (name)
  (assoc name *memory*))

(defun frame-name (frame) (first frame))
(defun frame-slots (frame) (rest frame))

(defun get-parent (frame)
  (second (assoc 'isa (frame-slots frame))))

(defun get-role-slot (frame role)
  (assoc role (frame-slots frame)))

(defun slot-role (slot) (first slot))
(defun slot-filler (slot) (second slot))

(defun get-role-filler (frame role) 
  (slot-filler (get-role-slot frame role)))


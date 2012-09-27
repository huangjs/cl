;;;; bits.lisp -- bit-level utilities

(in-package :chipz)

(defun read-bits (n state)
  (declare (type (integer 0 32) n))
  (declare (type inflate-state state))
  (prog1 (ldb (byte n 0) (inflate-state-bits state))
    (setf (inflate-state-bits state)
          (ash (inflate-state-bits state) (- n)))
    (decf (inflate-state-n-bits state) n)))

(defun ensure-bits (n state)
  (declare (type (integer 0 32) n))
  (declare (type inflate-state state))
  (let ((bits (inflate-state-bits state))
        (n-bits (inflate-state-n-bits state))
        (input-index (inflate-state-input-index state)))
    (declare (type (unsigned-byte 32) bits))
    (loop while (< n-bits n)
       when (>= input-index (inflate-state-input-end state))
         do (progn
              (setf (inflate-state-bits state) bits
                    (inflate-state-n-bits state) n-bits
                    (inflate-state-input-index state) input-index)
              (throw 'inflate-done nil))
       do (let ((byte (aref (inflate-state-input state) input-index)))
            (declare (type (unsigned-byte 8) byte))
            (setf bits
                  (logand #xffffffff (logior (ash byte n-bits) bits)))
            (incf n-bits 8)
            (incf input-index))
       finally (setf (inflate-state-bits state) bits
                     (inflate-state-n-bits state) n-bits
                     (inflate-state-input-index state) input-index))))

(defun ensure-and-read-bits (n state)
  (ensure-bits n state)
  (read-bits n state))

(defun align-bits-bytewise (state)
  (declare (type inflate-state state))
  (let ((n-bits (inflate-state-n-bits state)))
    (decf (inflate-state-n-bits state) (rem n-bits 8))
    (setf (inflate-state-bits state)
          (logandc2 (inflate-state-bits state) 7))
    (values)))

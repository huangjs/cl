;
;  This file is part of GRT, a Common Lisp raytracing system
;  Copyright (C) 2002 Nikodemus Siivola
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;  MA  02111-1307  USA
;

(in-package grt)
(export
 '(make-checker make-turbulence make-perlin-noise make-solid
   gradient noise checker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primitive functions
;;
;; We define primitive functions as functions that take a grt-vector and
;; possibly additional parameters and return a float from -1.0 to 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +noise+
  (make-array 512
	      :element-type '(unsigned-byte 8)
	      :initial-contents
	      (vector	       
	       151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225
	       140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247
	       120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57
	       177 33 88 237 149 56 87 174 20 125 136 171 168 68 175
	       74 165 71 134 139 48 27 166 77 146 158 231 83 111 229
	       122 60 211 133 230 220 105 92 41 55 46 245 40 244 102
	       143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89
	       18 169 200 196 135 130 116 188 159 86 164 100 109 198
	       173 186 3 64 52 217 226 250 124 123 5 202 38 147 118
	       126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28
	       42 223 183 170 213 119 248 152 2 44 154 163 70 221 153
	       101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113
	       224 232 178 185 112 104 218 246 97 228 251 34 242 193
	       238 210 144 12 191 179 162 241 81 51 145 235 249 14 239
	       107 49 192 214 31 181 199 106 157 184 84 204 176 115
	       121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24
	       72 243 141 128 195 78 66 215 61 156 180 151 160 137 91
	       90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30
	       69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0
	       26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237
	       149 56 87 174 20 125 136 171 168 68 175 74 165 71 134
	       139 48 27 166 77 146 158 231 83 111 229 122 60 211 133
	       230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63
	       161 1 216 80 73 209 76 132 187 208 89 18 169 200 196
	       135 130 116 188 159 86 164 100 109 198 173 186 3 64 52
	       217 226 250 124 123 5 202 38 147 118 126 255 82 85 212
	       207 206 59 227 47 16 58 17 182 189 28 42 223 183 170
	       213 119 248 152 2 44 154 163 70 221 153 101 155 167 43
	       172 9 129 22 39 253 19 98 108 110 79 113 224 232 178
	       185 112 104 218 246 97 228 251 34 242 193 238 210 144
	       12 191 179 162 241 81 51 145 235 249 14 239 107 49 192
	       214 31 181 199 106 157 184 84 204 176 115 121 50 45 127
	       4 150 254 138 236 205 93 222 114 67 29 24 72 243 141
	       128 195 78 66 215 61 156 180)))

(declaim
 (ftype (function (grt-vector) (grt-float -1.0 1.0)) noise3))
(defun noise3 (point)
  "3-dimensional noise. Implementation based on Ken Perlin's paper
   'Improving Noise' (http://mrl.nyu.edu/~perlin/paper445.pdf)
   and his reference implementation (http://mrl.nyu.edu/~perlin/noise).
   An interesting property -- not a bug -- of this implementation is that
   noise *seems* to be 0.0 at each point on a unit-cube lattice."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (grt-vector-bind (((x y z) point))	       
    (let-values (((cx x) (floor x))
		 ((cy y) (floor y))
		 ((cz z) (floor z)))
      (let ((cx (logand cx 255))   
	    (cy (logand cy 255))   
	    (cz (logand cz 255)))
	(declare (type (unsigned-byte 16) cx cy cz))
	(flet ((fade (e)
		     (declare (type grt-float e))            ; Fade curve
		     (+ (* 6 (expt e 5)) (* -15 (expt e 4)) (* 10 (expt e 3))))
	       (grad (hash x y z)
		     (declare (type (unsigned-byte 16) hash))
                     ; Low 4bits hash code
		     ; into 12 grad.dirs
		     (let* ((h (logand hash 15))
			    (u (if (or (< h 8) (= h 12) (= h 13)) 
				   x
				 y))
			    (v (if (or (< h 4) (= h 12) (= h 13))
				   y
				 z)))
		       (+ (if (= 0 (logand h 1)) u (- u))
			  (if (= 0 (logand h 2)) v (- v))))))
	  (let ((u (fade x)) ; Fade curves
		(v (fade y)) ; for each main axis
		(w (fade z)))
	    (declare (type grt-float u v w))
	    (let* ((A  (+ (aref +noise+ cx) cy)) ; Hash coordinates
		   (AA (+ (aref +noise+ A) cz))  ; for cube corners
		   (AB (+ (aref +noise+ (1+ A)) cz))
		   (B  (+ (aref +noise+ (1+ cx)) cy))
		   (BA (+ (aref +noise+ B) cz))
		   (BB (+ (aref +noise+ (1+ B)) cz)))
	      (linear-interpolate w
		 (linear-interpolate v                    ; Blend and add
         	    (linear-interpolate u                 ; results from
			(grad (aref +noise+ AA) x y z)    ; corners
			(grad (aref +noise+ BA) (- x 1) y z))
		    (linear-interpolate u
			(grad (aref +noise+ AB) x (- y 1) z)
			(grad (aref +noise+ BB) (- x 1) (- y 1) z)))
		 (linear-interpolate v
		    (linear-interpolate u
		        (grad (aref +noise+ (1+ AA)) x y (- z 1))
			(grad (aref +noise+ (1+ BA)) (- x 1) y (- z 1)))
		    (linear-interpolate u
			(grad (aref +noise+ (1+ AB)) x (- y 1) (- z 1))
			(grad (aref +noise+ (1+ BB)) (- x 1) (- y 1) (- z 1))))))))))))

(defun turbulence (point lo hi)
  "Turbulance function. Based on Ken Perlin's SIGGRAPH 92 course notes."
  (declare (type grt-float lo hi)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((p point)
	(r 0.0))
    (declare (type grt-float r))
    (labels ((loop (freq)
		   (declare (type grt-float freq))
		   (when (< freq hi)
		     (incf r (/ (abs (noise3 p)) freq))
		     (setq p (vector-mul p 2.0))
		     (loop (* 2.0 freq)))))
      (loop lo)
      r)))

(declaim
 (ftype (function (grt-vector (unsigned-byte 8) (grt-float 0.0 1.0))
		  (grt-float -1.0 1.0))
	perlin-noise))
(defun perlin-noise (point octaves persistence)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((total 0.0))
    (dotimes (i octaves total)
      (let ((freq (expt 2.0 i))
	    (ampl (expt persistence i)))
	(incf total (* ampl (noise3 (vector-mul point freq))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pattern functions
;;
;; We define pattern as a function that returns a float in range of
;; 0.0 to 1.0 when given a grt-vector.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim
 (ftype (function (grt-vector) (grt-float 0.0 1.0)) noise))
(defun noise (pos)
  "Noise pattern. NOISE3 scaled to 0.0 - 1.0 range."
    (/ (+ (noise3 pos) 1.0) 2.0))

(defun gradient (pos)
  "Gradient pattern. Oriented along Y-axis."
  (declare (type grt-vector pos))
  (mod (y pos) 1))

(defun checker (point)
  "Checker pattern with values 0.0 and 1.0."
  (declare (type grt-vector point))
  (if (oddp (+ (floor (+ +grt-epsilon+ (x point)))
	       (floor (+ +grt-epsilon+ (y point)))
	       (floor (+ +grt-epsilon+ (z point)))))
      0.0
    1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pattern generators
;;
;; Functions returning pattern functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-solid (v)
  "Retruns a solid pattern with the given value."
  (declare (type grt-float v))
  (lambda (pos)
    (declare (ignore pos))
    v))

(declaim
 (ftype (function ((unsigned-byte 8) (grt-float 0.0 1.0))
		  (function (grt-vector) (grt-float 0.0 1.0)))
	make-perlin-noise))
(defun make-perlin-noise (octaves persistence)
  (lambda (pos)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (/ (+ (perlin-noise pos octaves persistence) 1.0) 2.0)))

(defun make-turbulence (&key (low 0.25) (high 8.0))
  "Returns a turbulence pattern function with the specified parameters."
  (declare (type (grt-float 0.0) low high))
  (lambda (pos)
    (declare (type grt-vector pos)
	     (optimize (speed 3) (safety 0) (debug 0)))
    (mod (the (grt-float -1.0 1.0) (turbulence pos low high)) 1)))


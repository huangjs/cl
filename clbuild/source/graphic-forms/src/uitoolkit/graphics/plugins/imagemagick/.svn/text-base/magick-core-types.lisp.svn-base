;;;;
;;;; magick-core-types.lisp
;;;;
;;;; Copyright (C) 2006, Jack D. Unrue
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 
;;;;     1. Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;; 
;;;;     2. Redistributions in binary form must reproduce the above copyright
;;;;        notice, this list of conditions and the following disclaimer in the
;;;;        documentation and/or other materials provided with the distribution.
;;;; 
;;;;     3. Neither the names of the authors nor the names of its contributors
;;;;        may be used to endorse or promote products derived from this software
;;;;        without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS "AS IS" AND ANY
;;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DIS-
;;;; CLAIMED.  IN NO EVENT SHALL THE AUTHORS AND CONTRIBUTORS BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

(in-package :graphic-forms.uitoolkit.graphics.imagemagick)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cffi))

;;;
;;; see magick-type.h for the original C-language definitions
;;; of these types from ImageMagick Core.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +magick-max-text-extent+ 4096)
  (defconstant +magick-signature+       #xABACADAB))

(defconstant +undefined-channel+      #x00000000)
(defconstant +red-channel+            #x00000001)
(defconstant +gray-channel+           #x00000001)
(defconstant +cyan-channel+           #x00000001)
(defconstant +green-channel+          #x00000002)
(defconstant +magenta-channel+        #x00000002)
(defconstant +blue-channel+           #x00000004)
(defconstant +yellow-channel+         #x00000004)
(defconstant +alpha-channel+          #x00000008)
(defconstant +opacity-channel+        #x00000008)
(defconstant +matte-channel+          #x00000008) ; deprecated
(defconstant +black-channel+          #x00000020)
(defconstant +index-channel+          #x00000020)
(defconstant +all-channels+           #x000000FF)
(defconstant +default-channels+       (logand +all-channels+ (lognot +opacity-channel+))) ; (AllChannels &~ OpacityChannel)

(defctype quantum :unsigned-short)

(defctype index-packet quantum)

(defcenum boolean-type
  (:false 0)
  (:true 1))

(defcenum class-type
  :undefined
  :direct
  :pseudo)

(defcenum colorspace-type
  :undefined
  :rgb
  :gray
  :transparent
  :ohta
  :lab
  :xyz
  :ycbcr
  :ycc
  :yiq
  :ypbpr
  :yuv
  :cmyk
  :srgb
  :hsb
  :hsl
  :hwb
  :rec601luma
  :rec601ycbcr
  :rec709luma
  :rec709ycbcr
  :log)

(defcenum composite-operator
  :undefined
  :no
  :add
  :atop
  :blend
  :bump-map
  :clear
  :color-burn
  :color-dodge
  :colorize
  :copy-black
  :copy-blue
  :copy
  :copy-cyan
  :copy-green
  :copy-magenta
  :copy-opacity
  :copy-red
  :copy-yellow
  :darken
  :dst-atop
  :dst
  :dst-in
  :dst-out
  :dst-over
  :difference
  :displace
  :dissolve
  :exclusion
  :hard-light
  :hue
  :in
  :lighten
  :luminize
  :minus
  :modulate
  :multiply
  :out
  :over
  :overlay
  :plus
  :replace
  :saturate
  :screen
  :soft-light
  :src-atop
  :src
  :src-in
  :src-out
  :src-over
  :subtract
  :threshold
  :xor-composite-op)

(defcenum compression-type
  :undefined
  :no
  :bzip
  :fax
  :group4
  :jpeg
  :jpeg2000
  :lossless-jpeg
  :lzw
  :rle
  :zip)

(defcenum dispose-type
  :unrecognized
  (:undefined   0)
  (:none        1)
  (:background  2)
  (:previous    3))

(defcenum endian-type
  :undefined
  :lsb
  :msb)

(defcenum exception-type
  :undefined
  (:warning                      300)
  (:resource-limit-warning       300)
  (:type-warning                 305)
  (:option-warning               310)
  (:delegate--warning            315)
  (:missing-delegate-warning     320)
  (:corrupt-image-warning        325)
  (:file-open-warning            330)
  (:blob-warning                 335)
  (:stream-warning               340)
  (:cache-warning                345)
  (:coder-warning                350)
  (:module-warning               355)
  (:draw-warning                 360)
  (:image-warning                365)
  (:wand-warning                 370)
  (:xserver-warning              380)
  (:monitor-warning              385)
  (:registry-warning             390)
  (:configure-warning            395)
  (:error                        400)
  (:resource-limit-error         400)
  (:type-error                   405)
  (:option-error                 410)
  (:delegate-error               415)
  (:missing-delegate-error       420)
  (:corrupt-image-error          425)
  (:file-open-error              430)
  (:blob-error                   435)
  (:stream-error                 440)
  (:cache-error                  445)
  (:coder-error                  450)
  (:module-error                 455)
  (:draw-error                   460)
  (:image-error                  465)
  (:wand-error                   470)
  (:xserver-error                480)
  (:monitor-error                485)
  (:registry-error               490)
  (:configure-error              495)
  (:fatal-error                  700)
  (:resource-limit-fatal-error   700)
  (:type-fatal-error             705)
  (:option-fatal-error           710)
  (:delegate-fatal-error         715)
  (:missing-delegate-fatal-error 720)
  (:corrupt-image-fatal-error    725)
  (:file-open-fatal-error        730)
  (:blob-fatal-error             735)
  (:stream-fatal-error           740)
  (:cache-fatal-error            745)
  (:coder-fatal-error            750)
  (:module-fatal-error           755)
  (:draw-fatal-error             760)
  (:image-fatal-error            765)
  (:wand-fatal-error             770)
  (:xserver-fatal-error          780)
  (:monitor-fatal-error          785)
  (:registry-fatal-error         790)
  (:configure-fatal-error        795))

(defcenum filter-types
  :undefined
  :point
  :box
  :triangle
  :hermite
  :hanning
  :hamming
  :blackman
  :gaussian
  :quadratic
  :cubic
  :catrom
  :mitchell
  :lanczos
  :bessel
  :sinc)

(defcenum gravity-type
  :undefined
  (:forget      0)
  (:north-west  1)
  (:north       2)
  (:north-east  3)
  (:west        4)
  (:center      5)
  (:east        6)
  (:south-west  7)
  (:south       8)
  (:south-east  9)
  (:static     10))

(defcenum image-type
  :undefined
  :bi-level
  :gray-scale
  :gray-scale-matte
  :palette
  :palette-matte
  :true-color
  :true-color-matte
  :color-separation
  :color-separation-matte
  :optimize)

(defcenum interlace-type
  :undefined
  :no
  :line
  :plane
  :partition)

(defcenum orientation-type
  :undefined
  :top-left
  :top-right
  :bottom-right
  :bottom-left
  :left-top
  :right-top
  :right-bottom
  :left-bottom)

(defcenum preview-type
  :undefined
  :rotate
  :shear
  :roll
  :hue
  :saturation
  :brightness
  :gamma
  :spiff
  :dull
  :gray-scale
  :quantize
  :despeckle
  :reduce-noise
  :add-noise
  :sharpen
  :blur
  :threshold
  :edge-detect
  :spread
  :solarize
  :shade
  :raise
  :segment
  :swirl
  :implode
  :wave
  :oil-paint
  :charcoal-drawing
  :jpeg)

(defcenum rendering-intent
  :undefined
  :saturation
  :perceptual
  :absolute
  :relative)

(defcenum resolution-type
  :undefined
  :pixels-per-inch
  :pixels-per-centimeter)

  ;; from constitute.h
  ;;
(defcenum storage-type
  :undefined
  :char
  :double
  :float
  :integer
  :long
  :quantum
  :short)

(defcenum timer-state
  :undefined
  :stopped
  :running)

(defcstruct error-info
  (mean-error-per-pixel     :double)
  (normalized-mean-error    :double)
  (normalized-maximum-error :double))

(defcstruct exception-info
  (severity         exception-type)
  (error-number     :int)
  (reason           :string)
  (description      :string)
  (exceptions       :pointer)         ; void*
  (relinquish       boolean-type)
  (semaphore        :pointer)         ; Semaphore*
  (signature        :unsigned-long))

(defcstruct primary-info
  (x                :double)
  (y                :double)
  (z                :double))

(defcstruct chromaticity-info
  (red-primary      primary-info)
  (green-primary    primary-info)
  (blue-primary     primary-info)
  (white-point      primary-info))
    
(defcstruct pixel-packet
  (blue             quantum)
  (green            quantum)
  (red              quantum)
  (opacity          quantum))

(defcstruct profile-info
  (name             :string)
  (length           :unsigned-long)
  (info             :pointer)         ; char*
  (signature        :unsigned-long))

(defcstruct rectangle-info
  (width            :unsigned-long)
  (height           :unsigned-long)
  (x                :long)
  (y                :long))

(defcstruct timer
  (start            :double)
  (stop             :double)
  (total            :double))

(defcstruct timer-info
  (user             timer)
  (elapsed          timer)
  (state            timer-state)
  (signature        :unsigned-long))

(defcstruct magick-image
  (storage-class    class-type)
  (color-space      colorspace-type)
  (compression      compression-type)
  (quality          :long)
  (orientation      orientation-type)
  (taint            boolean-type)
  (matte            boolean-type)
  (columns          :unsigned-long)
  (rows             :unsigned-long)
  (depth            :unsigned-long)
  (colors           :unsigned-long)
  (colormap         :pointer)         ; PixelPacket*
  (background-color pixel-packet)
  (border-color     pixel-packet)
  (matte-color      pixel-packet)
  (gamma            :double)
  (chromaticity     chromaticity-info)
  (render-intent    rendering-intent)
  (profiles         :pointer)         ; void*
  (units            resolution-type)
  (montage          :pointer)         ; char*
  (directory        :pointer)         ; char*
  (geometry         :pointer)         ; char*
  (offset           :long)
  (x-resolution     :double)
  (y-resolution     :double)
  (page             rectangle-info)
  (extract-info     rectangle-info)
  (tile-info        rectangle-info)   ; deprecated
  (bias             :double)
  (blur             :double)
  (fuzz             :double)
  (filter           filter-types)
  (interlace        interlace-type)
  (endian           endian-type)
  (gravity          gravity-type)
  (compose          composite-operator)
  (dispose          dispose-type)
  (clip-mask        :pointer)         ; Image*
  (scene            :unsigned-long)
  (delay            :unsigned-long)
  (ticks-per-second :unsigned-long)
  (iterations       :unsigned-long)
  (total-colors     :unsigned-long)
  (start-loop       :long)
  (error            error-info)
  (timer            timer-info)
  (progress-monitor :pointer)         ; MagickBooleanType (*MagickProgressMonitor)(args)
  (client-data      :pointer)         ; void*
  (cache            :pointer)         ; void*
  (attributes       :pointer)         ; void*
  (ascii85          :pointer)         ; _Ascii85Info_*
  (blob             :pointer)         ; _BlobInfo_*
  (filename         :char :count 4096)
  (magick-filename  :char :count 4096)
  (magick           :char :count 4096)
  (exception        exception-info)
  (debug            boolean-type)
  (reference-count  :long)
  (semaphore        :pointer)         ; SemaphoreInfo*
  (color-profile    profile-info)
  (iptc-profile     profile-info)
  (generic-profile  :pointer)         ; ProfileInfo*
  (generic-profiles :unsigned-long)   ; deprecated (and ProfileInfo too?)
  (signature        :unsigned-long)
  (previous         :pointer)         ; Image*
  (list             :pointer)         ; Image*
  (next             :pointer))        ; Image*
    
(defcstruct magick-image-info
  (compression      compression-type)
  (orientation      orientation-type)
  (temporary        boolean-type)
  (adjoin           boolean-type)
  (affirm           boolean-type)
  (antialias        boolean-type)
  (size             :pointer)         ; char*
  (extract          :pointer)         ; char*
  (page             :pointer)         ; char*
  (scenes           :pointer)         ; char*
  (scene            :unsigned-long)
  (number-scenes    :unsigned-long)
  (depth            :unsigned-long)
  (interlace        interlace-type)
  (endian           endian-type)
  (units            resolution-type)
  (quality          :unsigned-long)
  (sampling-factor  :pointer)         ; char*
  (server-name      :pointer)         ; char*
  (font             :pointer)         ; char*
  (texture          :pointer)         ; char*
  (density          :pointer)         ; char*
  (point-size       :double)
  (fuzz             :double)
  (background-color pixel-packet)
  (border-color     pixel-packet)
  (matte-color      pixel-packet)
  (dither           boolean-type)
  (monochrome       boolean-type)
  (colors           :unsigned-long)
  (colorspace       colorspace-type) 
  (type             image-type)
  (prevu-type       preview-type)
  (group            :long)
  (ping             boolean-type)
  (verbose          boolean-type)
  (view             :pointer)         ; char*
  (authenticate     :pointer)         ; char*
  (channel          :unsigned-int)    ; ChannelType
  (attributes       :pointer)         ; Image*
  (options          :pointer)         ; void*
  (progress-monitor :pointer)         ; MagickBooleanType (*MagickProgressMonitor)(args)
  (client-data      :pointer)         ; void*
  (cache            :pointer)         ; void*
  (stream           :pointer)         ; size_t (*StreamHandler)(args)
  (file             :pointer)         ; FILE*
  (blob             :pointer)         ; void*
  (length           :unsigned-int)
  (magick           :char :count 4096)
  (unique           :char :count 4096)
  (zero             :char :count 4096)
  (filename         :char :count 4906)
  (debug            boolean-type)
  (tile             :pointer)         ; deprecated
  (subimage         :unsigned-long)
  (subrange         :unsigned-long)
  (pen              pixel-packet)
  (signature        :unsigned-long))
 
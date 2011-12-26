#+clm (in-package :clm)

;;; import some sndlib names to CLM
;;;
;;; file name is no longer accurate (this is now CL/CLM-specific, but aims at sndlib stuff mostly)

#+(and excl (not (or allegro-v7.0 allegro-v8.0))) (require :foreign)
#+cmu (use-package "ALIEN")
;#+cmu (use-package "C-CALL")
#+openmcl (defun get-cstring (str) (when (not (ccl:%null-ptr-p str)) (%get-cstring str)))

#+cmu (def-alien-routine ("mus_error_type_to_string" mus-error-type->string) c-call:c-string (err c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_error_type_to_string" mus-error-type->string) sb-alien:c-string (err sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-error-type-to-string-1 "mus_error_type_to_string") ((err :int)) :returning :int)
#+allegro-v5.0 (defun mus-error-type->string (name) (let ((ptr (mus-error-type-to-string-1 name))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-error-type->string "mus_error_type_to_string") ((err :int)) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-error-type->string "mus_error_type_to_string") ((err :int)) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-error-type->string (err) (get-cstring (ccl::external-call "_mus_error_type_to_string" :signed err :address)))

#+cmu (def-alien-routine ("clm_sound_samples" mus-sound-samples) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("clm_sound_samples" mus-sound-samples) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-samples "clm_sound_samples") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-samples "clm_sound_samples") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-samples "clm_sound_samples") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-samples (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_samples" :address f (:signed 64))))

#+cmu (def-alien-routine ("clm_sound_frames" mus-sound-frames) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("clm_sound_frames" mus-sound-frames) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-frames "clm_sound_frames") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-frames "clm_sound_frames") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-frames "clm_sound_frames") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-frames (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_frames" :address f (:signed 64))))

#+cmu (def-alien-routine ("mus_sound_datum_size" mus-sound-datum-size) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_datum_size" mus-sound-datum-size) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-datum-size "mus_sound_datum_size") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-datum-size "mus_sound_datum_size") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-datum-size "mus_sound_datum_size") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-datum-size (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_datum_size" :address f :signed)))

#+cmu (def-alien-routine ("clm_sound_data_location" mus-sound-data-location) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("clm_sound_data_location" mus-sound-data-location) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-data-location "clm_sound_data_location") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-data-location "clm_sound_data_location") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-data-location "clm_sound_data_location") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-data-location (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_data_location" :address f (:signed 64))))

#+cmu (def-alien-routine ("mus_sound_chans" mus-sound-chans) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_chans" mus-sound-chans) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-chans "mus_sound_chans") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-chans "mus_sound_chans") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-chans "mus_sound_chans") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-chans (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_chans" :address f :signed)))

#+cmu (def-alien-routine ("mus_sound_srate" mus-sound-srate) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_srate" mus-sound-srate) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-srate "mus_sound_srate") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-srate "mus_sound_srate") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-srate "mus_sound_srate") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-srate (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_srate" :address f :signed)))

#+cmu (def-alien-routine ("mus_sound_header_type" mus-sound-header-type) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_header_type" mus-sound-header-type) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-header-type "mus_sound_header_type") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-header-type "mus_sound_header_type") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-header-type "mus_sound_header_type") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-header-type (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_header_type" :address f :signed)))

#+cmu (def-alien-routine ("mus_sound_data_format" mus-sound-data-format) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_data_format" mus-sound-data-format) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-data-format "mus_sound_data_format") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-data-format "mus_sound_data_format") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-data-format "mus_sound_data_format") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-data-format (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_data_format" :address f :signed)))

#+cmu (def-alien-routine ("mus_sound_original_format" mus-sound-original-format) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_original_format" mus-sound-original-format) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-original-format "mus_sound_original_format") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-original-format "mus_sound_original_format") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-original-format "mus_sound_original_format") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-original-format (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_original_format" :address f :signed)))

#+cmu (def-alien-routine ("mus_sound_write_date" mus-sound-write-date) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_write_date" mus-sound-write-date) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-write-date "mus_sound_write_date") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-write-date "mus_sound_write_date") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-write-date "mus_sound_write_date") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-write-date (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_write_date" :address f :signed)))

#+cmu (def-alien-routine ("clm_sound_comment_start" mus-sound-comment-start) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("clm_sound_comment_start" mus-sound-comment-start) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-comment-start "clm_sound_comment_start") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-comment-start "clm_sound_comment_start") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-comment-start "clm_sound_comment_start") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-comment-start (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_comment_start" :address f :signed)))

#+cmu (def-alien-routine ("clm_sound_comment_end" mus-sound-comment-end) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("clm_sound_comment_end" mus-sound-comment-end) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-comment-end "clm_sound_comment_end") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-comment-end "clm_sound_comment_end") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-comment-end "clm_sound_comment_end") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-comment-end (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_comment_end" :address f :signed)))

#+cmu (def-alien-routine ("clm_sound_length" mus-sound-length) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("clm_sound_length" mus-sound-length) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-length "clm_sound_length") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-length "clm_sound_length") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-length "clm_sound_length") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-length (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_length" :address f (:signed 64))))

#+cmu (def-alien-routine ("mus_sound_type_specifier" mus-sound-type-specifier) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_type_specifier" mus-sound-type-specifier) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-type-specifier "mus_sound_type_specifier") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-type-specifier "mus_sound_type_specifier") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-type-specifier "mus_sound_type_specifier") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-type-specifier (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_type_specifier" :address f :signed)))

#+cmu (def-alien-routine ("mus_sound_bits_per_sample" mus-sound-bits-per-sample) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_bits_per_sample" mus-sound-bits-per-sample) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-bits-per-sample "mus_sound_bits_per_sample") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-bits-per-sample "mus_sound_bits_per_sample") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-bits-per-sample "mus_sound_bits_per_sample") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-bits-per-sample (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_bits_per_sample" :address f :signed)))

#+cmu (def-alien-routine ("mus_header_type_name" mus-header-type-name) c-call:c-string (type c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_type_name" mus-header-type-name) sb-alien:c-string (type sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-type-name-1 "mus_header_type_name") ((type :int)) :returning :int)
#+allegro-v5.0 (defun mus-header-type-name (name) (let ((ptr (mus-header-type-name-1 name))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-type-name "mus_header_type_name") ((type :int)) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-type-name "mus_header_type_name") ((type :int)) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-header-type-name (typ) (get-cstring (ccl::external-call "_mus_header_type_name" :signed typ :address)))

#+cmu (def-alien-routine ("mus_data_format_name" mus-data-format-name) c-call:c-string (format c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_data_format_name" mus-data-format-name) sb-alien:c-string (format sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-data-format-name-1 "mus_data_format_name") ((format :int)) :returning :int)
#+allegro-v5.0 (defun mus-data-format-name (name) (let ((ptr (mus-data-format-name-1 name))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-data-format-name "mus_data_format_name") ((format :int)) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-data-format-name "mus_data_format_name") ((format :int)) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-data-format-name (typ) (get-cstring (ccl::external-call "_mus_data_format_name" :signed typ :address)))

#+cmu (def-alien-routine ("mus_sound_comment" mus-sound-comment) c-call:c-string (name c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_comment" mus-sound-comment) sb-alien:c-string (name sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-comment-1 "mus_sound_comment") ((name (* :char) string)) :returning :int)
#+allegro-v5.0 (defun mus-sound-comment (name) (let ((ptr (mus-sound-comment-1 name))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-comment "mus_sound_comment") ((name (* :char))) :returning ((* :char)))
#+allegro-v7.0 (ff:def-foreign-call (mus-sound-comment "mus_sound_comment") ((name (* :char))) :strings-convert t :returning ((* :char)))
#+allegro-v8.0 (ff:def-foreign-call (mus-sound-comment "clm_sound_comment") ((name (* :char))) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-sound-comment-1 (file) (ccl:with-cstrs ((f file)) (get-cstring (ccl::external-call "_mus_sound_comment" :address f :address))))
#+openmcl (defun mus-sound-comment (file) (let ((str (mus-sound-comment-1 file))) (or str "")))

#+cmu (def-alien-routine ("mus_sound_duration" mus-sound-duration) double-float (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_duration" mus-sound-duration) double-float (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-duration "mus_sound_duration") ((arg (* :char) string)) :returning :float)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-duration "mus_sound_duration") ((arg (* :char))) :returning :float)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-duration "mus_sound_duration") ((arg (* :char))) :strings-convert t :returning :float)
#+openmcl (defun mus-sound-duration (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_duration" :address f :single-float)))

#+cmu (def-alien-routine ("mus_sound_initialize" mus-sound-initialize) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_sound_initialize" mus-sound-initialize) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-initialize "mus_sound_initialize") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-initialize "mus_sound_initialize") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-initialize "mus_sound_initialize") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-initialize () (ccl::external-call "_mus_sound_initialize" :signed))

#+cmu (def-alien-routine ("mus_sound_forget" mus-sound-forget) c-call:int (name c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_sound_forget" mus-sound-forget) sb-alien:int (name sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-sound-forget "mus_sound_forget") ((name (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-sound-forget "mus_sound_forget") ((name (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-sound-forget "mus_sound_forget") ((name (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-sound-forget (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_sound_forget" :address f :signed)))

#+cmu (def-alien-routine ("mus_audio_describe" mus-audio-describe) c-call:void )
#+sbcl (sb-alien:define-alien-routine ("mus_audio_describe" mus-audio-describe) sb-alien:void )
#+allegro-v5.0 (ff:def-foreign-call (mus-audio-describe "mus_audio_describe") (:void) :returning :void)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-audio-describe "mus_audio_describe") (:void) :returning :void)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-audio-describe "mus_audio_describe") (:void) :strings-convert t :returning :void)
#+openmcl (defun mus-audio-describe () (ccl::external-call "_mus_audio_describe" :void))

#+cmu (def-alien-routine ("mus_audio_report" mus-audio-report) c-call:c-string )
#+sbcl (sb-alien:define-alien-routine ("mus_audio_report" mus-audio-report) sb-alien:c-string )
#+allegro-v5.0 (ff:def-foreign-call (mus-audio-report-1 "mus_audio_report") (:void) :returning :int)
#+allegro-v5.0 (defun mus-audio-report () (let ((ptr (mus-audio-report-1))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-audio-report "mus_audio_report") (:void) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-audio-report "mus_audio_report") (:void) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-audio-report () (get-cstring (ccl::external-call "_mus_audio_report" :address)))

(defun mus-audio-mixer-read (dev field chan arr) (clm-audio-mixer-read dev field chan arr (length arr)))
(defun mus-audio-mixer-write (dev field chan arr) (clm-audio-mixer-write dev field chan arr (length arr)))

#+cmu (def-alien-routine ("clm_audio_mixer_read" clm-audio-mixer-read-1) c-call:int
	(dev c-call:int) (field c-call:int) (chan c-call:int) (val (* double-float)) (len c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_audio_mixer_read" clm-audio-mixer-read-1) sb-alien:int
	  (dev sb-alien:int) (field sb-alien:int) (chan sb-alien:int) (val (* double-float)) (len sb-alien:int))
#+(or cmu sbcl) (defun clm-audio-mixer-read (dev field chan arr len) (clm-audio-mixer-read-1 dev field chan (array-data-address arr) len))
#+allegro-v5.0 (ff:def-foreign-call (clm-audio-mixer-read "clm_audio_mixer_read")
				    ((dev :int) (field :int) (chan :int) (val (* :double) array) (len :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (clm-audio-mixer-read "clm_audio_mixer_read")
				    ((dev :int) (field :int) (chan :int) (val (* :double) array) (len :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (clm-audio-mixer-read "clm_audio_mixer_read")
				    ((dev :int) (field :int) (chan :int) (val (* :double) array) (len :int)) :strings-convert t :returning :int)
#+openmcl (defun clm-audio-mixer-read (dev field chan val len)
	    (ccl::with-foreign-double-float-array-to-c-and-lisp (p val) ; make sure values get back to lisp
	      (ccl::external-call "_clm_audio_mixer_read" :signed dev :signed field :signed chan :address p :signed len :signed)))

#+cmu (def-alien-routine ("clm_audio_mixer_write" clm-audio-mixer-write-1) c-call:int
	(dev c-call:int) (field c-call:int) (chan c-call:int) (val (* double-float)) (len c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_audio_mixer_write" clm-audio-mixer-write-1) sb-alien:int
	  (dev sb-alien:int) (field sb-alien:int) (chan sb-alien:int) (val (* double-float)) (len sb-alien:int))
#+(or cmu sbcl) (defun clm-audio-mixer-write (dev field chan arr len) (clm-audio-mixer-write-1 dev field chan (array-data-address arr) len))
#+allegro-v5.0 (ff:def-foreign-call (clm-audio-mixer-write "clm_audio_mixer_write")
				    ((dev :int) (field :int) (chan :int) (val (* :double) array) (len :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (clm-audio-mixer-write "clm_audio_mixer_write")
				    ((dev :int) (field :int) (chan :int) (val (* :double) array) (len :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (clm-audio-mixer-write "clm_audio_mixer_write")
				    ((dev :int) (field :int) (chan :int) (val (* :double) array) (len :int)) :strings-convert t :returning :int)
#+openmcl (defun clm-audio-mixer-write (dev field chan val len)
	    (ccl::with-foreign-double-float-array-to-c-and-lisp (p val) ; make sure values get back to lisp
	      (ccl::external-call "_clm_audio_mixer_write" :signed dev :signed field :signed chan :address p :signed len :signed)))

#+cmu (def-alien-routine ("mus_audio_initialize" mus-audio-initialize) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_audio_initialize" mus-audio-initialize) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-audio-initialize "mus_audio_initialize") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-audio-initialize "mus_audio_initialize") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-audio-initialize "mus_audio_initialize") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-audio-initialize () (ccl::external-call "_mus_audio_initialize" :signed))

#+cmu (def-alien-routine ("mus_audio_systems" mus-audio-systems) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_audio_systems" mus-audio-systems) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-audio-systems "mus_audio_systems") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-audio-systems "mus_audio_systems") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-audio-systems "mus_audio_systems") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-audio-systems () (ccl::external-call "_mus_audio_systems" :signed))

#+cmu (def-alien-routine ("mus_audio_system_name" mus-audio-system-name) c-call:c-string (system c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_audio_system_name" mus-audio-system-name) sb-alien:c-string (system sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-audio-system-name-1 "mus_audio_system_name") ((system :int)) :returning :int)
#+allegro-v5.0 (defun mus-audio-system-name (name) (let ((ptr (mus-audio-system-name-1 name))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-audio-system-name "mus_audio_system_name") ((system :int)) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-audio-system-name "mus_audio_system_name") ((system :int)) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-audio-system-name (sys) (get-cstring (ccl::external-call "_mus_audio_system_name" :signed sys :address)))

#+cmu (def-alien-routine ("mus_audio_moniker" mus-audio-moniker) c-call:c-string )
#+sbcl (sb-alien:define-alien-routine ("mus_audio_moniker" mus-audio-moniker) sb-alien:c-string )
#+allegro-v5.0 (ff:def-foreign-call (mus-audio-moniker-1 "mus_audio_moniker") (:void) :returning :int)
#+allegro-v5.0 (defun mus-audio-moniker () (let ((ptr (mus-audio-moniker-1))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-audio-moniker "mus_audio_moniker") (:void) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-audio-moniker "mus_audio_moniker") (:void) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-audio-moniker () (get-cstring (ccl::external-call "_mus_audio_moniker" :address)))

#+cmu (def-alien-routine ("mus_file_probe" mus-file-probe) c-call:int (arg c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_file_probe" mus-file-probe) sb-alien:int (arg sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-file-probe "mus_file_probe") ((arg (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-file-probe "mus_file_probe") ((arg (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-file-probe "mus_file_probe") ((arg (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-file-probe (file) (ccl:with-cstrs ((f file)) (ccl::external-call "_mus_file_probe" :address f :signed)))

#+cmu (def-alien-routine ("mus_set_clipping" mus-set-clipping) c-call:int (clipped c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_set_clipping" mus-set-clipping) sb-alien:int (clipped sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-set-clipping "mus_set_clipping") ((clipped :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-set-clipping "mus_set_clipping") ((clipped :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-set-clipping "mus_set_clipping") ((clipped :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-set-clipping (on) (ccl::external-call "_mus_set_clipping" :signed on :signed))

#+cmu (def-alien-routine ("mus_clipping" mus-clipping) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_clipping" mus-clipping) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-clipping "mus_clipping") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-clipping "mus_clipping") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-clipping "mus_set_clipping") (:void) :returning :int)
#+openmcl (defun mus-clipping () (ccl::external-call "_mus_clipping" :signed))

#+cmu (def-alien-routine ("mus_prescaler" mus-prescaler) double-float )
#+sbcl (sb-alien:define-alien-routine ("mus_prescaler" mus-prescaler) double-float )
#+allegro-v5.0 (ff:def-foreign-call (mus-prescaler "mus_prescaler") (:void) :returning :double)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-prescaler "mus_prescaler") (:void) :returning :double)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-prescaler "mus_prescaler") (:void) :returning :double)
#+openmcl (defun mus-prescaler () (ccl::external-call "_mus_prescaler" :double-float))

#+cmu (def-alien-routine ("mus_set_prescaler" mus-set-prescaler) double-float (val double-float))
#+sbcl (sb-alien:define-alien-routine ("mus_set_prescaler" mus-set-prescaler) double-float (val double-float))
#+allegro-v5.0 (ff:def-foreign-call (mus-set-prescaler "mus_set_prescaler") ((val :double)) :returning :double)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-set-prescaler "mus_set_prescaler") ((val :double)) :returning :double)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-set-prescaler "mus_set_prescaler") ((val :double)) :strings-convert t :returning :double)
#+openmcl (defun mus-set-prescaler (val) (ccl::external-call "_mus_set_prescaler" :double-float val :single-float))

#+cmu (def-alien-routine ("clm_header_samples" mus-header-samples) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("clm_header_samples" mus-header-samples) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-samples "clm_header_samples") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-samples "clm_header_samples") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-samples "clm_header_samples") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-samples () (ccl::external-call "_mus_header_samples" (:signed 64)))

#+cmu (def-alien-routine ("clm_header_data_location" mus-header-data-location) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("clm_header_data_location" mus-header-data-location) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-data-location "clm_header_data_location") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-data-location "clm_header_data_location") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-data-location "clm_header_data_location") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-data-location () (ccl::external-call "_mus_header_data_location" (:signed 64)))

#+cmu (def-alien-routine ("mus_header_chans" mus-header-chans) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_chans" mus-header-chans) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-chans "mus_header_chans") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-chans "mus_header_chans") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-chans "mus_header_chans") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-chans () (ccl::external-call "_mus_header_chans" :signed))

#+cmu (def-alien-routine ("mus_header_srate" mus-header-srate) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_srate" mus-header-srate) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-srate "mus_header_srate") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-srate "mus_header_srate") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-srate "mus_header_srate") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-srate () (ccl::external-call "_mus_header_srate" :signed))

#+cmu (def-alien-routine ("mus_header_type" mus-header-type) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_type" mus-header-type) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-type "mus_header_type") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-type "mus_header_type") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-type "mus_header_type") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-type () (ccl::external-call "_mus_header_type" :signed))

#+cmu (def-alien-routine ("mus_header_format" mus-header-format) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_format" mus-header-format) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-format "mus_header_format") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-format "mus_header_format") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-format "mus_header_format") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-format () (ccl::external-call "_mus_header_format" :signed))

#+cmu (def-alien-routine ("clm_header_comment_start" mus-header-comment-start) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("clm_header_comment_start" mus-header-comment-start) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-comment-start "clm_header_comment_start") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-comment-start "clm_header_comment_start") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-comment-start "clm_header_comment_start") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-comment-start () (ccl::external-call "_mus_header_comment_start" :signed))

#+cmu (def-alien-routine ("clm_header_comment_end" mus-header-comment-end) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("clm_header_comment_end" mus-header-comment-end) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-comment-end "clm_header_comment_end") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-comment-end "clm_header_comment_end") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-comment-end "clm_header_comment_end") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-comment-end () (ccl::external-call "_mus_header_comment_end" :signed))

#+cmu (def-alien-routine ("mus_header_type_specifier" mus-header-type-specifier) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_type_specifier" mus-header-type-specifier) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-type-specifier "mus_header_type_specifier") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-type-specifier "mus_header_type_specifier") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-type-specifier "mus_header_type_specifier") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-type-specifier () (ccl::external-call "_mus_header_type_specifier" :signed))

#+cmu (def-alien-routine ("mus_header_bits_per_sample" mus-header-bits-per-sample) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_bits_per_sample" mus-header-bits-per-sample) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-bits-per-sample "mus_header_bits_per_sample") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-bits-per-sample "mus_header_bits_per_sample") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-bits-per-sample "mus_header_bits_per_sample") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-bits-per-sample () (ccl::external-call "_mus_header_bits_per_sample" :signed))

#+cmu (def-alien-routine ("mus_header_loop_mode" mus-header-loop-mode) c-call:int (which c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_loop_mode" mus-header-loop-mode) sb-alien:int (which sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-loop-mode "mus_header_loop_mode") ((which :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-loop-mode "mus_header_loop_mode") ((which :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-loop-mode "mus_header_loop_mode") ((which :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-loop-mode (w) (ccl::external-call "_mus_header_loop_mode" :signed w :signed))

#+cmu (def-alien-routine ("mus_header_loop_start" mus-header-loop-start) c-call:int (which c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_loop_start" mus-header-loop-start) sb-alien:int (which sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-loop-start "mus_header_loop_start") ((which :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-loop-start "mus_header_loop_start") ((which :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-loop-start "mus_header_loop_start") ((which :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-loop-start (w) (ccl::external-call "_mus_header_loop_start" :signed w :signed))

#+cmu (def-alien-routine ("mus_header_loop_end" mus-header-loop-end) c-call:int (which c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_loop_end" mus-header-loop-end) sb-alien:int (which sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-loop-end "mus_header_loop_end") ((which :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-loop-end "mus_header_loop_end") ((which :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-loop-end "mus_header_loop_end") ((which :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-loop-end (w) (ccl::external-call "_mus_header_loop_end" :signed w :signed))

#+cmu (def-alien-routine ("mus_header_mark_position" mus-header-mark-position) c-call:int (id c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_mark_position" mus-header-mark-position) sb-alien:int (id sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-mark-position "mus_header_mark_position") ((id :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-mark-position "mus_header_mark_position") ((id :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-mark-position "mus_header_mark_position") ((id :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-mark-position (w) (ccl::external-call "_mus_header_mark_position" :signed w :signed))

#+cmu (def-alien-routine ("mus_header_base_note" mus-header-base-note) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_base_note" mus-header-base-note) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-base-note "mus_header_base_note") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-base-note "mus_header_base_note") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-base-note "mus_header_base_note") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-base-note () (ccl::external-call "_mus_header_base_note" :signed))

#+cmu (def-alien-routine ("mus_header_base_detune" mus-header-base-detune) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_base_detune" mus-header-base-detune) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-base-detune "mus_header_base_detune") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-base-detune "mus_header_base_detune") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-base-detune "mus_header_base_detune") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-base-detune () (ccl::external-call "_mus_header_base_detune" :signed))

#+cmu (def-alien-routine ("mus_header_set_raw_defaults" mus-header-set-raw-defaults) c-call:void (sr c-call:int) (chn c-call:int) (frm c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_set_raw_defaults" mus-header-set-raw-defaults) sb-alien:void (sr sb-alien:int) (chn sb-alien:int) (frm sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-set-raw-defaults "mus_header_set_raw_defaults") ((sr :int) (chn :int) (frm :int)) :returning :void)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-set-raw-defaults "mus_header_set_raw_defaults") ((sr :int) (chn :int) (frm :int)) :returning :void)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-set-raw-defaults "mus_header_set_raw_defaults") ((sr :int) (chn :int) (frm :int)) :strings-convert t :returning :void)
#+openmcl (defun mus-header-set-raw-defaults (sr chn frm)
	    (ccl::external-call "_mus_header_set_raw_defaults" :signed sr :signed chn :signed frm :void))

#+cmu (def-alien-routine ("clm_header_true_length" mus-header-true-length) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("clm_header_true_length" mus-header-true-length) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-true-length "clm_header_true_length") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-true-length "clm_header_true_length") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-true-length "clm_header_true_length") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-true-length () (ccl::external-call "_mus_header_true_length" (:signed 64)))

#+cmu (def-alien-routine ("mus_header_original_format" mus-header-original-format) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_original_format" mus-header-original-format) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-original-format "mus_header_original_format") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-original-format "mus_header_original_format") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-original-format "mus_header_original_format") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-original-format () (ccl::external-call "_mus_header_original_format" :signed))

#+cmu (def-alien-routine ("clm_samples_to_bytes" mus-samples-to-bytes) c-call:int (format c-call:int) (size c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_samples_to_bytes" mus-samples-to-bytes) sb-alien:int (format sb-alien:int) (size sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-samples-to-bytes "clm_samples_to_bytes") ((format :int) (size :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-samples-to-bytes "clm_samples_to_bytes") ((format :int) (size :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-samples-to-bytes "clm_samples_to_bytes") ((format :int) (size :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-samples-to-bytes (frm siz) (ccl::external-call "_mus_samples_to_bytes" :signed frm (:signed 64) siz (:signed 64)))

#+cmu (def-alien-routine ("clm_bytes_to_samples" mus-bytes-to-samples) c-call:int (format c-call:int) (size c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_bytes_to_samples" mus-bytes-to-samples) sb-alien:int (format sb-alien:int) (size sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-bytes-to-samples "clm_bytes_to_samples") ((format :int) (size :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-bytes-to-samples "clm_bytes_to_samples") ((format :int) (size :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-bytes-to-samples "clm_bytes_to_samples") ((format :int) (size :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-bytes-to-samples (frm siz) (ccl::external-call "_mus_bytes_to_samples" :signed frm (:signed 64) siz (:signed 64)))

#+cmu (def-alien-routine ("mus_header_read" mus-header-read) c-call:int (name c-call:c-string))
#+sbcl (sb-alien:define-alien-routine ("mus_header_read" mus-header-read) sb-alien:int (name sb-alien:c-string))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-read "mus_header_read") ((name (* :char) string)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-read "mus_header_read") ((name (* :char))) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-read "mus_header_read") ((name (* :char))) :strings-convert t :returning :int)
#+openmcl (defun mus-header-read (name) (ccl:with-cstrs ((f name)) (ccl::external-call "_mus_header_read" :address f :signed)))

#+cmu (def-alien-routine ("clm_header_write" mus-header-write) c-call:int
	(name c-call:c-string) (type c-call:int) (srate c-call:int) (chans c-call:int) (loc c-call:int) (size c-call:int) (format c-call:int) (comment c-call:c-string) (len c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_header_write" mus-header-write) sb-alien:int
	(name sb-alien:c-string) (type sb-alien:int) (srate sb-alien:int) (chans sb-alien:int) (loc sb-alien:int) (size sb-alien:int) (format sb-alien:int) (comment sb-alien:c-string) (len sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-write "clm_header_write")
				    ((name (* :char) string) (type :int) (srate :int) (chans :int) (loc :int) (size :int)
				     (format :int) (comment (* :char) string) (len :int))
				    :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-write "clm_header_write")
				    ((name (* :char)) (type :int) (srate :int) (chans :int) (loc :int) (size :int) (format :int)
				     (comment (* :char)) (len :int))
				    :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-write "clm_header_write")
				    ((name (* :char)) (type :int) (srate :int) (chans :int) (loc :int) (size :int) (format :int)
				     (comment (* :char)) (len :int))
				    :strings-convert t :returning :int)
#+openmcl (defun mus-header-write (name type srate chans loc size format comment len)
	    (ccl:with-cstrs ((f name) (c comment))
              (ccl::external-call "_mus_header_write" :address f :signed type :signed srate :signed chans
				  (:signed 64) loc (:signed 64) size :signed format :address c :signed len
				  :signed)))

#+cmu (def-alien-routine ("clm_header_aux_comment_start" mus-header-aux-comment-start) c-call:int (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_header_aux_comment_start" mus-header-aux-comment-start) sb-alien:int (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-aux-comment-start "clm_header_aux_comment_start") ((n :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-aux-comment-start "clm_header_aux_comment_start") ((n :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-aux-comment-start "clm_header_aux_comment_start") ((n :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-aux-comment-start (n) (ccl::external-call "_mus_header_aux_comment_start" :signed n (:signed 64)))

#+cmu (def-alien-routine ("clm_header_aux_comment_end" mus-header-aux-comment-end) c-call:int (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("clm_header_aux_comment_end" mus-header-aux-comment-end) sb-alien:int (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-aux-comment-end "clm_header_aux_comment_end") ((n :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-aux-comment-end "clm_header_aux_comment_end") ((n :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-aux-comment-end "clm_header_aux_comment_end") ((n :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-aux-comment-end (n) (ccl::external-call "_mus_header_aux_comment_end" :signed n (:signed 64)))

#+cmu (def-alien-routine ("mus_header_initialize" mus-header-initialize) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_initialize" mus-header-initialize) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-initialize "mus_header_initialize") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-initialize "mus_header_initialize") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-initialize "mus_header_initialize") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-initialize () (ccl::external-call "_mus_header_initialize" :signed))

#+cmu (def-alien-routine ("mus_header_writable" mus-header-writable) c-call:int (type c-call:int) (format c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_writable" mus-header-writable) sb-alien:int (type sb-alien:int) (format sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-writable "mus_header_writable") ((type :int) (format :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-writable "mus_header_writable") ((type :int) (format :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-writable "mus_header_writable") ((type :int) (format :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-writable (typ frm) (ccl::external-call "_mus_header_writable" :signed typ :signed frm :signed))

#+cmu (def-alien-routine ("mus_header_sf2_entries" mus-header-sf2-entries) c-call:int )
#+sbcl (sb-alien:define-alien-routine ("mus_header_sf2_entries" mus-header-sf2-entries) sb-alien:int )
#+allegro-v5.0 (ff:def-foreign-call (mus-header-sf2-entries "mus_header_sf2_entries") (:void) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-sf2-entries "mus_header_sf2_entries") (:void) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-sf2-entries "mus_header_sf2_entries") (:void) :strings-convert t :returning :int)
#+openmcl (defun mus-header-sf2-entries () (ccl::external-call "_mus_header_sf2_entries" :signed))

#+cmu (def-alien-routine ("mus_header_sf2_name" mus-header-sf2-name) c-call:c-string (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_sf2_name" mus-header-sf2-name) sb-alien:c-string (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-sf2-name-1 "mus_header_sf2_name") ((n :int)) :returning :int)
#+allegro-v5.0 (defun mus-header-sf2-name (name) (let ((ptr (mus-header-sf2-name-1 name))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-sf2-name "mus_header_sf2_name") ((n :int)) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-sf2-name "mus_header_sf2_name") ((n :int)) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-header-sf2-name (n) (get-cstring (ccl::external-call "_mus_header_sf2_name" :signed n :address)))

#+cmu (def-alien-routine ("mus_header_sf2_start" mus-header-sf2-start) c-call:int (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_sf2_start" mus-header-sf2-start) sb-alien:int (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-sf2-start "mus_header_sf2_start") ((n :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-sf2-start "mus_header_sf2_start") ((n :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-sf2-start "mus_header_sf2_start") ((n :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-sf2-start (n) (ccl::external-call "_mus_header_sf2_start" :signed n :signed))

#+cmu (def-alien-routine ("mus_header_sf2_end" mus-header-sf2-end) c-call:int (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_sf2_end" mus-header-sf2-end) sb-alien:int (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-sf2-end "mus_header_sf2_end") ((n :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-sf2-end "mus_header_sf2_end") ((n :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-sf2-end "mus_header_sf2_end") ((n :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-sf2-end (n) (ccl::external-call "_mus_header_sf2_end" :signed n :signed))

#+cmu (def-alien-routine ("mus_header_sf2_loop_start" mus-header-sf2-loop-start) c-call:int (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_sf2_loop_start" mus-header-sf2-loop-start) sb-alien:int (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-sf2-loop-start "mus_header_sf2_loop_start") ((n :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-sf2-loop-start "mus_header_sf2_loop_start") ((n :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-sf2-loop-start "mus_header_sf2_loop_start") ((n :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-sf2-loop-start (n) (ccl::external-call "_mus_header_sf2_loop_start" :signed n :signed))

#+cmu (def-alien-routine ("mus_header_sf2_loop_end" mus-header-sf2-loop-end) c-call:int (n c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_sf2_loop_end" mus-header-sf2-loop-end) sb-alien:int (n sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-sf2-loop-end "mus_header_sf2_loop_end") ((n :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-sf2-loop-end "mus_header_sf2_loop_end") ((n :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-sf2-loop-end "mus_header_sf2_loop_end") ((n :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-header-sf2-loop-end (n) (ccl::external-call "_mus_header_sf2_loop_end" :signed n :signed))

#+cmu (def-alien-routine ("mus_header_original_format_name" mus-header-original-format-name) c-call:c-string (format c-call:int) (type c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_header_original_format_name" mus-header-original-format-name) sb-alien:c-string (format sb-alien:int) (type sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-header-original-format-name-1 "mus_header_original_format_name")
				    ((format :int) (type :int)) :returning :int)
#+allegro-v5.0 (defun mus-header-original-format-name (a b) (let ((ptr (mus-header-original-format-name-1 a b))) (if (= ptr 0) "" (ff:char*-to-string ptr))))
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-header-original-format-name "mus_header_original_format_name")
				    ((format :int) (type :int)) :returning ((* :char)))
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-header-original-format-name "mus_header_original_format_name")
				    ((format :int) (type :int)) :strings-convert t :returning ((* :char)))
#+openmcl (defun mus-header-original-format-name (format type)
	    (get-cstring (ccl::external-call "_mus_header_original_format_name" :signed format :signed type :address)))

#+cmu (def-alien-routine ("mus_bytes_per_sample" mus-bytes-per-sample) c-call:int (format c-call:int))
#+sbcl (sb-alien:define-alien-routine ("mus_bytes_per_sample" mus-bytes-per-sample) sb-alien:int (format sb-alien:int))
#+allegro-v5.0 (ff:def-foreign-call (mus-bytes-per-sample "mus_bytes_per_sample") ((format :int)) :returning :int)
#+(or allegro-v6.0 allegro-v6.1 allegro-v6.2) (ff:def-foreign-call (mus-bytes-per-sample "mus_bytes_per_sample") ((format :int)) :returning :int)
#+(or allegro-v7.0 allegro-v8.0) (ff:def-foreign-call (mus-bytes-per-sample "mus_bytes_per_sample") ((format :int)) :strings-convert t :returning :int)
#+openmcl (defun mus-bytes-per-sample (format) (ccl::external-call "_mus_bytes_per_sample" :signed format :signed))


#+(and sbcl linux) (sb-alien:define-alien-routine ("mus_oss_set_buffers" mus-oss-set-buffers) sb-alien:void 
						  (num sb-alien:int) (size sb-alien:int))
#+(and sbcl sun) (sb-alien:define-alien-routine ("mus_sun_set_outputs" mus-sun-set-outputs) sb-alien:void 
						(a sb-alien:int) (b sb-alien:int) (c sb-alien:int))
#+(and sbcl netbsd) (sb-alien:define-alien-routine ("mus_netbsd_set_outputs" mus-netbsd-set-outputs) sb-alien:void 
						   (a sb-alien:int) (b sb-alien:int) (c sb-alien:int))
#+sbcl (sb-alien:define-alien-routine ("mus_reset_audio_c" reset-audio) sb-alien:void)
#+sbcl (sb-alien:define-alien-routine ("mus_reset_headers_c" reset-headers) sb-alien:void)
#+sbcl (sb-alien:define-alien-routine ("mus_reset_io_c" reset-io) sb-alien:void)

#+(and cmu linux) (def-alien-routine ("mus_oss_set_buffers" mus-oss-set-buffers) c-call:void 
		    (num c-call:int) (size c-call:int))
#+(and cmu sun) (def-alien-routine ("mus_sun_set_outputs" mus-sun-set-outputs) c-call:void 
		    (a c-call:int) (b c-call:int) (c c-call:int))
#+(and cmu netbsd) (def-alien-routine ("mus_netbsd_set_outputs" mus-netbsd-set-outputs) c-call:void 
		    (a c-call:int) (b c-call:int) (c c-call:int))
#+cmu (def-alien-routine ("mus_reset_audio_c" reset-audio) c-call:void)
#+cmu (def-alien-routine ("mus_reset_headers_c" reset-headers) c-call:void)
#+cmu (def-alien-routine ("mus_reset_io_c" reset-io) c-call:void)

#+(and excl acl-50 linux) (ff:def-foreign-call (mus-oss-set-buffers "mus_oss_set_buffers") ((num :int) (size :int)) :returning :void)
#+(and excl acl-50) (ff:def-foreign-call (reset-audio "mus_reset_audio_c") (:void) :returning :void)
#+(and excl acl-50) (ff:def-foreign-call (reset-headers "mus_reset_headers_c") (:void) :returning :void)
#+(and excl acl-50) (ff:def-foreign-call (reset-io "mus_reset_io_c") (:void) :returning :void)


#+clisp (use-package "FFI")
;#+clisp (defun full-lib-name () (expand-filename->string "libclm.so"))  ; also used in ffi.lisp
#+clisp (ffi:default-foreign-language :stdc)

#+clisp (ffi:def-call-out mus-sound-frames (:name "clm_sound_frames") (:return-type ffi:int)
			  (:arguments (arg ffi:c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-samples (:name "clm_sound_samples") (:return-type ffi:int)
			  (:arguments (arg ffi:c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-error-type->string (:name "mus_error_type_to_string") (:return-type ffi:c-string)
			  (:arguments (err ffi:int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-datum-size (:name "mus_sound_datum_size") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-data-location (:name "clm_sound_data_location") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-chans (:name "mus_sound_chans") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-srate (:name "mus_sound_srate") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-header-type (:name "mus_sound_header_type") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-data-format (:name "mus_sound_data_format") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-original-format (:name "mus_sound_original_format") (:return-type ffi:int) 
	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-write-date (:name "mus_sound_write_date") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-comment-start (:name "clm_sound_comment_start") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-comment-end (:name "clm_sound_comment_end") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-length (:name "clm_sound_length") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-type-specifier (:name "mus_sound_type_specifier") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-bits-per-sample (:name "mus_sound_bits_per_sample") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-type-name (:name "mus_header_type_name") (:return-type ffi:c-string)
 	(:arguments (type ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-data-format-name (:name "mus_data_format_name") (:return-type ffi:c-string)
 	(:arguments (format ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-comment-1 (:name "mus_sound_comment") (:return-type ffi:c-string)
 	(:arguments (name ffi::c-string)) (:library (full-lib-name)))
#+clisp (defun mus-sound-comment (file) (let ((str (mus-sound-comment-1 file))) (or str "")))

#+clisp (ffi:def-call-out mus-sound-duration (:name "mus_sound_duration") (:return-type ffi:double-float)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-initialize (:name "mus_sound_initialize") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-sound-forget (:name "mus_sound_forget") (:return-type ffi:int)
 	(:arguments (name ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-audio-describe (:name "mus_audio_describe") (:return-type ffi:nil)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-audio-report (:name "mus_audio_report") (:return-type ffi:c-string)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out clm-audio-mixer-read-1 (:name "clm_audio_mixer_read") (:return-type ffi:int)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out clm-audio-mixer-write-1 (:name "clm_audio_mixer_write") (:return-type ffi:int)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-audio-initialize (:name "mus_audio_initialize") (:return-type ffi:int)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-audio-systems (:name "mus_audio_systems") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-audio-system-name (:name "mus_audio_system_name") (:return-type ffi:c-string)
 	(:arguments (system ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-audio-moniker (:name "mus_audio_moniker") (:return-type ffi:c-string)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-file-probe (:name "mus_file_probe") (:return-type ffi:int)
 	(:arguments (arg ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-set-clipping (:name "mus_set_clipping") (:return-type ffi:int)
 	(:arguments (clipped ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-clipping (:name "mus_clipping") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-prescaler (:name "mus_prescaler") (:return-type ffi:double-float)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-set-prescaler (:name "mus_set_prescaler") (:return-type ffi:double-float)
 	(:arguments (val double-float)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-samples (:name "clm_header_samples") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-data-location (:name "clm_header_data_location") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-chans (:name "mus_header_chans") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-srate (:name "mus_header_srate") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-type (:name "mus_header_type") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-format (:name "mus_header_format") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-comment-start (:name "clm_header_comment_start") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-comment-end (:name "clm_header_comment_end") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-type-specifier (:name "mus_header_type_specifier") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-bits-per-sample (:name "mus_header_bits_per_sample") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-loop-mode (:name "mus_header_loop_mode") (:return-type ffi:int)
 	(:arguments (which ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-loop-start (:name "mus_header_loop_start") (:return-type ffi:int)
 	(:arguments (which ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-loop-end (:name "mus_header_loop_end") (:return-type ffi:int)
 	(:arguments (which ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-mark-position (:name "mus_header_mark_position") (:return-type ffi:int)
 	(:arguments (id ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-base-note (:name "mus_header_base_note") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-base-detune (:name "mus_header_base_detune") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-set-raw-defaults (:name "mus_header_set_raw_defaults") (:return-type ffi:nil)
 	(:arguments (sr ffi::int) (chn ffi::int) (frm ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-true-length (:name "clm_header_true_length") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-original-format (:name "mus_header_original_format") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-samples-to-bytes (:name "clm_samples_to_bytes") (:return-type ffi:int)
 	(:arguments (format ffi::int) (size ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-bytes-to-samples (:name "clm_bytes_to_samples") (:return-type ffi:int)
 	(:arguments (format ffi::int) (size ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-read (:name "mus_header_read") (:return-type ffi:int)
 	(:arguments (name ffi::c-string)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-write (:name "clm_header_write") (:return-type ffi:int)
			  (:arguments (name ffi:c-string) (type ffi:int) (srate ffi:int) (chans ffi:int) 
				      (loc ffi:int) (size ffi:int) (format ffi:int) (comment ffi:c-string) (len ffi:int))
			  (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-aux-comment-start (:name "clm_header_aux_comment_start") (:return-type ffi:int)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-aux-comment-end (:name "clm_header_aux_comment_end") (:return-type ffi:int)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-initialize (:name "mus_header_initialize") (:return-type ffi:int)
 	(:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-writable (:name "mus_header_writable") (:return-type ffi:int)
 	(:arguments (type ffi::int) (format ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-sf2-entries (:name "mus_header_sf2_entries") (:return-type ffi:int) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-sf2-name (:name "mus_header_sf2_name") (:return-type ffi:c-string)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-sf2-start (:name "mus_header_sf2_start") (:return-type ffi:int)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-sf2-end (:name "mus_header_sf2_end") (:return-type ffi:int)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-sf2-loop-start (:name "mus_header_sf2_loop_start") (:return-type ffi:int)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-sf2-loop-end (:name "mus_header_sf2_loop_end") (:return-type ffi:int)
 	(:arguments (n ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-header-original-format-name (:name "mus_header_original_format_name") (:return-type ffi:c-string) 
	(:arguments (format ffi::int) (type ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out mus-bytes-per-sample (:name "mus_bytes_per_sample") (:return-type ffi:int) 
	(:arguments (format ffi::int)) (:library (full-lib-name)))

#+clisp (ffi:def-call-out reset-audio (:name "mus_reset_audio_c") (:return-type ffi:nil)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out reset-headers (:name "mus_reset_headers_c") (:return-type ffi:nil)
	(:library (full-lib-name)))

#+clisp (ffi:def-call-out reset-io (:name "mus_reset_io_c") (:return-type ffi:nil)
	(:library (full-lib-name)))


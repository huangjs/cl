;;;------------------------------------------------------------------- 
;;;
;;;  COMPUTED FORM WHERE THE FORM IS DETERMINED DYNAMICALLY
;;;

(defparameter *computed-choices* '("Computed Form"))

(defparameter *default-computed-choices* '("Not Selected"))

(defmethod compute-form ((url url:http-form) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title "Computed Form Example" :stream stream))
      (html:with-document-body (:stream stream)
        (html:with-section-heading ("Computed Form Example" :stream stream)
          (image-line :stream stream)
          (html:with-fillout-form (:post url :stream stream)
            (html:with-paragraph (:stream stream)
              (with-rendition (:bold :stream stream)
                (fresh-line stream)
                (write-string "Choices: " stream))
              (let ((choices `("Not Selected" ,@*computed-choices*)))
                (declare (dynamic-extent choices))
                (html:accept-input 'html:select-choices "CHOICES" :choices choices
                                   :default *default-computed-choices* :sequence-p t :stream stream)))
            (when (< (length *computed-choices*) 5)
              (html:with-paragraph (:stream stream)
                (with-rendition (:bold :stream stream)
                  (fresh-line stream)
                  (write-string "Add Choice: " stream))
                (html:accept-input 'html:string "ADD-CHOICE" :size 30 :stream stream)))
            (when (cdr *computed-choices*)
              (html:with-paragraph (:stream stream)
                (with-rendition (:bold :stream stream)
                  (fresh-line stream)
                  (write-string "Delete Choice: " stream))
                (html:accept-input 'html:string "DELETE-CHOICE" :size 30 :stream stream)))
	    ;; Write a hidden field to carry the state and avoid collisions across threads.
	    (html:accept-input 'html:hidden "COMPUTED-CHOICES"
			       :default (write-to-armor-plated-string *computed-choices*) :stream stream)
            (submit-and-reset-buttons stream))
          (image-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-computed-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
           (and item                            ; don't let NIL through
                (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item))))
                item)))
    (declare (dynamic-extent #'clean-up))
    (bind-query-values (choices add-choice delete-choice computed-choices)
                       (url query-alist)
                       (let ((real-choices (delete "Not Selected" (ensure-list choices) :test #'equalp))
	                     (*computed-choices* (read-from-armor-plated-string computed-choices)))
                         (setq *default-computed-choices* (if real-choices real-choices '("Not Selected")))
                         (cond-every
                          ((setq add-choice (clean-up add-choice))
                           (pushnew add-choice (cdr *computed-choices*) :test #'equalp))
                          ((setq delete-choice (clean-up delete-choice))
                           ;; Don't allow deletion of no selection and keep at least two choices.
                           (when (cdr *computed-choices*)
                             (setq *computed-choices* (delete delete-choice *computed-choices* :test #'equalp)))
                           ;; Keep the default in sync
                           (setq *default-computed-choices* (or (delete delete-choice *default-computed-choices* :test #'equalp)
                                                                '("Not Selected")))))
                         ;; generate another version of the form with the new values.
                         (compute-form url stream)))))

(export-url #u"/cl-http/computed-form.html"
            :html-computed-form
            :form-function #'compute-form
            :expiration '(:no-expiration-header)
            :response-function #'respond-to-computed-form
            :keywords '(:cl-http :demo)
            :documentation "An example of copmuting the form html on the fly and responding to the resulting submissions.")


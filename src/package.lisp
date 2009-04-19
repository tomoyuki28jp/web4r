(in-package :cl-user)

(macrolet
    ((define-web4r-package ()
       `(defpackage :web4r
          (:use :cl :my-util :sml :inflector :hunchentoot :elephant)
          (:export ; --- specials ---
                   :*web4r-dir*
                   :*public-dir*
                   :*sml-dir*
                   :*upload-save-dir*
                   :*tmp-save-dir*
                   :*tmp-files-gc-lifetime*
                   :*tmp-files-gc-probability*
                   :*image-public-dirs*
                   :*debug-log-file*
                   :*debug-mode*
                   :*valid-email-format*
                   :*page-param*
                   :*with-slots*
                   :*without-slots*
                   :*login-msgs*
                   :*thumbnail-width*
                   :*thumbnail-height*
                   ; --- util ---
                   :web4r-path
                   :public-path
                   :sml-path
                   :load-sml-path
                   :nl->br
                   :assoc*
                   :replace-assoc*
                   :add-parameter
                   :add-parameters
                   :rem-parameter
                   :omit
                   :file-length*
                   :time-format
                   ; --- debug ---
                   :ps
                   :with-post-parameters
                   :with-get-parameters
                   :debug-log
                   :backtrace-log
                   :debug-mode-on
                   :debug-mode-off
                   ; --- cont ---
                   :cont-session
                   :rem-cont-session
                   :sid
                   :a/cont
                   :form/cont
                   :multipart-form/cont
                   :last-post
                   ; --- server ---
                   :uri-path
                   :host-uri
                   :page-uri
                   :set-post-parameters
                   :set-get-parameters
                   :start-server
                   :stop-server
                   :page-lambda
                   :defpage
                   :page
                   :redirect/msgs
                   :redirect/error-msgs
                   :page/msgs
                   :page/error-msgs
                   :get-msgs
                   :msgs
                   ; --- valid ---
                   :unique-p
                   :validation-errors
                   :with-validations
                   ; --- pager ---
                   :pager
                   :total-pages
                   :get-current-page
                   :page-links
                   :page-summary
                   :w/p
                   ; --- pclass ---
                   :slot-symbol
                   :slot-id
                   :slot-label
                   :slot-unique
                   :slot-required
                   :slot-rows
                   :slot-cols
                   :slot-size
                   :slot-length
                   :slot-hide
                   :slot-options
                   :slot-comment
                   :slot-input
                   :slot-format
                   :slot-type
                   :get-slots
                   :get-slot
                   :get-excluded-slots
                   :get-slots-if
                   :file-slots
                   :date-slots
                   :unique-slots
                   :indexed-slot-p
                   :slot-display-value
                   :slot-save-value
                   :form-valid-attr
                   :form-input
                   :form-label
                   :required-mark
                   :form-comment
                   :form-for/cont
                   :select-date
                   :slot-validation-errors
                   :class-validation-errors
                   :defpclass
                   :created-at
                   :updated-at
                   :oid
                   :get-instance-by-oid
                   :drop-instance
                   :drop-instance-by-oid
                   :drop-instances-by-class
                   :edit-upload-file
                   ; --- pages ---
                   :defpages
                   :index-page
                   :show-page
                   :edit-page
                   :delete-page
                   :per-page
                   :edit/cont
                   :make-pinstance
                   :update-pinstance
                   ; --- user.lisp ---
                   :login-msg
                   :*user*
                   :id
                   :pass
                   :user
                   :user-class
                   :user-id-slot
                   :user-pass-slot
                   :user-id-label
                   :user-pass-label
                   :user-login-page
                   :user-id
                   :user-pass
                   :get-user
                   :get-user-oid
                   :login
                   :logout
                   :login-user
                   :login-user-id
                   :login-user-oid
                   :login/cont
                   :login-page
                   :logout-page
                   :regist-page
                   :owner-p
                   ; --- image ---
                   :mime-type
                   :image-file-p
                   :image-path
                   :thumbnail
                   :thumbnail-uri
                   ; --- inflector ---
                   :pluralize
                   ; --- my-util ---
                   ,@(loop for s being the external-symbol
                           in :my-util collect s)
                   ; --- sml ---
                   ,@(loop for s being the external-symbol
                           in :sml collect s)
                   ; --- hunchentoot ---
                   ,@(loop for s being the external-symbol
                           in :hunchentoot collect s)
                   ; --- elephant ---
                   ,@(loop for s being the external-symbol
                           in :elephant
                           unless (eq s 'defpclass)
                           collect s)
                   )
          (:shadow :defpclass))))
  (define-web4r-package))

(flet ((int (x) (parse-integer (remove #\. x))))
  (loop for v in '((:hunchentoot . "1.0.0")
                   (:my-util     . "0.0.2")
                   (:sml         . "0.1.1")
                   (:inflector   . "0.1.0"))
        as ver = (asdf:component-version (asdf:find-system (car v)))
        unless (>= (int ver) (int (cdr v)))
        do (error "~S must be version ~S or higher" (car v) (cdr v))))

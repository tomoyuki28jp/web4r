(in-package :cl-user)

(macrolet
    ((define-web4r-package ()
       `(defpackage :web4r
          (:use :cl :my-util :sml :hunchentoot :elephant)
          (:export ; --- specials ---
                   :*web4r-dir*
                   :*public-dir*
                   :*image-dir*
                   :*sml-dir*
                   :*upload-save-dir*
                   :*tmp-save-dir*
                   :*tmp-files-gc-lifetime*
                   :*tmp-files-gc-probability*
                   :*image-public-dirs*
                   :*debug-log-file*
                   :*debug-mode*
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
                   :slot-nullable
                   :slot-rows
                   :slot-cols
                   :slot-size
                   :slot-length
                   :slot-hide
                   :slot-options
                   :slot-comment
                   :slot-input
                   :slot-type
                   :get-slots
                   :get-slot
                   :get-excluded-slots
                   :slot-display-value
                   :slot-save-value
                   :form-input
                   :form-label
                   :must-mark
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
                   :drop-class-instances
                   :edit-upload-file
                   ; --- scaffold ---
                   :scaffold
                   :scaffold-index
                   :scaffold-show
                   :scaffold-edit
                   :scaffold-delete
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
                   ; --- my-util ---
                   ,@(loop for s being the external-symbol
                           in :my-util collect s)
                   ; --- sml ---
                   ,@(loop for s being the external-symbol
                           in :sml collect s)
                   ; --- hunchentoot ---
                   ,@(loop for s being the external-symbol
                           in :hunchentoot collect s)
                   )
          (:shadow :defpclass))))
  (define-web4r-package))

(let ((version hunchentoot::*hunchentoot-version*))
  (unless (string= "1" (subseq version 0 1))
    (error "Hunchentoot must be version 1.0.0 or higher")))

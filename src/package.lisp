(in-package :cl-user)

(macrolet
    ((define-web4r-package ()
       `(defpackage :web4r
          (:use :cl :my-util :sml :hunchentoot :elephant)
          (:export ; --- util ---
                   :*web4r-dir*
                   :*sml-dir*
                   :web4r-file-path
                   :sml-file-path
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
                   :*debug-log-file*
                   :*debug-mode*
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
                   :*default-page*
                   :uri-path
                   :host-uri
                   :page-uri
                   :default-page
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
                   ; --- validations ---
                   :validation-errors
                   :with-validations
                   ; --- pager ---
                   :*page-param*
                   :pager
                   :total-pages
                   :get-current-page
                   :page-links
                   :page-summary
                   :w/p
                   ; --- pclass ---
                   :*tmp-save-dir*
                   :*upload-save-dir*
                   :*tmp-files-gc-lifetime*
                   :*tmp-files-gc-probability*
                   :*with-slots*
                   :*without-slots*
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
                   :*login-msgs*
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
                   :*image-dirs*
                   :mime-type
                   :image-file-p
                   :serve-image
                   :image-uri
                   :noimage
                   :noimage-uri
                   :*thumbnail-width*
                   :*thumbnail-height*
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

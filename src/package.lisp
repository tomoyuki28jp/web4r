(in-package :cl-user)

(defpackage :web4r
  (:use :cl :my-util :sml :inflector :hunchentoot :elephant)
  (:export ; --- specials ---
           :*web4r-dir*
           :*public-dir*
           :*sml-dir*
           :*example-bdb*
           :*upload-save-dir*
           :*tmp-save-dir*
           :*tmp-files-gc-lifetime*
           :*tmp-files-gc-probability*
           :*image-public-dirs*
           :*debug-log-file*
           :*cont-gc-lifetime*
           :*cont-gc-probability*
           :*validation-error-messages*
           :*valid-email-format*
           :*page-param*
           :*items-per-page*
           :*links-per-page*
           :*with-slots*
           :*without-slots*
           :*max-items-per-page*
           :*max-links-per-page*
           :*user*
           :*login-msgs*
           :*thumbnail-width*
           :*thumbnail-height*
           ; --- util ---
           :web4r-path
           :public-path
           :sml-path
           :example-path
           :load-sml-path
           :add-parameter
           :add-parameters
           :rem-parameter
           :omit
           :time-format
           ; --- debug ---
           :ps
           :with-post-parameters
           :with-get-parameters
           :debug-log
           :debug-mode-on
           :debug-mode-off
           ; --- cont ---
           :renew-cont-lifetime
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
           :prev-link
           :next-link
           :prev-link*
           :next-link*
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
           :slot-hide-for
           :slot-options
           :slot-comment
           :slot-input
           :slot-format
           :slot-type
           :get-slots
           :get-slot
           :hide-slot-p
           :get-excluded-slots
           :get-slots-if
           :get-excluded-slots-if
           :get-file-slots
           :get-excluded-file-slots
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
           :make-pinstance
           :update-pinstance
           :edit-upload-file
           ; --- pages ---
           :genpages
           :index-page
           :show-page
           :edit-page
           :delete-page
           :edit/cont
           :per-page
           :item-list
           ; --- user.lisp ---
           :login-msg
           :user*
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
           ; --- http ---
           :http-get-instance-by-oid
           :http-test-get-instance-by-oid
           )
  (:shadow :defpclass))

(in-package :web4r)

(asdf-version<= :hunchentoot "1.0.0")
(asdf-version<= :my-util     "0.0.4")
(asdf-version<= :sml         "0.1.5")
(asdf-version<= :inflector   "0.1.0")
(asdf-version=  :elephant    "0.9")

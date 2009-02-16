(in-package :cl-user)

(defpackage :web4r
  (:use :cl :usocket :cl-fad :elephant :flexi-streams)
  (:export ; --- util.lisp ---
           :*crlf*
           :*nl*
           :pm
           :pm1
           :slots
           :while
           :for
           :aif
           :awhen
           :it
           :acond
           :aand
           :awhile
           :concat
           :mkstr
           :symb
           :make-keyword
           :->list
           :->int
           :with-struct
           :with-gensyms
           :position-str
           :split
           :replace-str
           :assoc-ref
           :hash
           :sethash
           :hash-list
           :parse-float
           :preg-match
           :preg-match-all
           :is-readable
           :join
           :substr
           :substrlen
           :nl->br
           :remseq
           :iso-time
           :time-format
           :ensure-file-exist
           :random-hex-string
           :qw
           :uri-encode
           :uri-decode
           :with-flexi-stream
           :build-query
           :add-get-param
           :add-get-params
           :rem-get-param
           :uniq-file-name
           :merge-cons-tree
           ; --- shtml.lisp ---
           :*http-char-stream*
           :*doctype-strict*
           :*doctype-transitional*
           :*doctype-frameset*
           :*doctype*
           :doctype
           :*escape*
           :escape
           :escape-string
           :escape-char
           :safe
           :p
           :pe
           :pr
           :attr
           :defined-tag-p
           :start-tag
           :end-tag
           :deftag
           :deftags
           :br
           :br/
           :*web4r-dir*
           :*shtml-dir*
           :*shtml*
           :define-shtml
           :get-shtml
           :read-shtml
           :shtml-path
           :web4r-path
           :load-shtml
           :with-shtml
           :with-shtml-file
           :input-text/
           :input-checked/
           :submit/
           :select-form/
           :select-date/
           :form-for/cont/
           ; --- validations.lisp ---
           :validation-errors
           :with-validations
           ; --- server.lisp ---
           :*server*
           :*request*
           :*response*
           :*http-stream*
           :*http-binary-stream*
           :*sid*
           :get-route
           :add-route
           :rem-route
           :destroy-session
           :route-static
           :route-regex
           :make-server
           :validate-server
           :start-server
           :stop-server
           :server-is-running-p
           :access-log
           :error-log
           :debug-log
           :get-cookies
           :get-cookie
           :set-cookie
           :get-cookie-sid
           :get-session
           :rem-session
           :set-session
           :session-file
           :session-name
           :generate-sid
           :valid-sid-p
           :*cont-gc-probability*
           :*cont-gc-lifetime*
           :get-cid
           :get-cont
           :generate-cid
           :set-cont
           :call-cont
           :destroy-cont
           :lambda/cont
           :a/cont/
           :form/cont/
           :multipart-form/cont/
           :get-mid
           :set-msgs
           :set-error-msgs
           :get-msgs
           :redirect/msgs
           :redirect/error-msgs
           :add-header
           :status-page
           :add-hook
           :get-page
           :set-page
           :public-file-p
           :image-type
           :host-uri
           :page-uri
           :page-lambda
           :defpage
           :default-page
           :page
           :page/msgs
           :page/error-msgs
           :msgs/
           :uri-path
           :get-params
           :get-param
           :post-params
           :post-param
           :header-fields
           :header-field
           :redirect
           :exit
           :request-post-params
           :public-dir
           :req-uri
           :get-file-data
           :file-name
           :file-type
           :file-tmp-name
           :file-size
           :file-save-name
           :set-last-post
           :rem-last-post
           :last-posts
           :last-post
           ; --- pager.lisp ---
           :pager
           :*page-param*
           :get-current-page
           :total-pages
           :item-start
           :page-links/
           :page-summary/
           :w/p
           ; --- pclass.lisp ---
           :*slots*
           :*with-slots*
           :*without-slots*
           :get-slots
           :get-excluded-slots
           :get-slot
           :slot-label
           :slot-input
           :defpclass
           :created-at
           :updated-at
           :oid
           :slot-values=
           :drop-instance
           :get-instance-by-oid
           :per-page
           :slot-display-value
           :slot-save-value
           :class-validation-errors
           :form-input
           :form-label
           :scaffold
           :delete/cont
           :edit/cont
           :make-pinstance
           :update-pinstance
           :scaffold-index-page
           :scaffold-show-page
           :scaffold-edit-page
           :*upload-save-dir*
           :upload-file-path
           :upload-dir-uri
           :upload-file-uri
           :upload-file-thumbnail-uri
           :delete-upload-file
           ; --- image.lisp ---
           :noimage
           :noimage-uri
           :image-file-p
           :thumbnail
           :thumbnail-uri
           ; --- user.lisp ---
           :*msgs*
           :*user*
           :id
           :pass
           :user
           :msg
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
           )
  (:shadow :defpclass))

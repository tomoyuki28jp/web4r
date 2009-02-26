(in-package :web4r)

(defvar *server*  nil
  "Instance of the server structure")

(defvar *request* nil
  "Instance of the request structure")

(defvar *response* nil
  "Instance of the response structure")

(defvar *http-stream* *standard-output*
  "HTTP stream")

(defvar *http-binary-stream* *standard-output*
  "HTTP binary stream")

(defvar *pages* (make-hash-table)
  "Page hash table")

(defvar *sid* nil
  "Session ID")

; --- URI rewrite rule ------------------------------------------

; Sample rewrite rule (You can also use add-route function):
;(defun sample-rewrite-rule (uri)
;  (when (aand (preg-match "(\.ico|\.gif|\.jpeg|\.jpg|\.png|)$" uri)
;              (not (string= it "")))
;    "/test.html"))

(defun rewrote-uri ()
  (let ((script (request-script-name *request*)))
    (awhen (server-rewrite-rule *server*)
      (aif (funcall it (request-uri *request*))
           (return-from rewrote-uri it)))
    script))

; --- Route -----------------------------------------------------

(defstruct route
  (static (make-hash-table :test 'equal) :type hash-table)
  (regex  (make-hash-table :test 'equal) :type hash-table))

(defun get-route (server type)
  (case type
    (:static (route-static (server-route server)))
    (:regex  (route-regex  (server-route server)))
    (otherwise (error "invalid route type: ~A" type))))

(defun add-route (server type route dispatcher)
  (unless (or (functionp dispatcher) (fboundp dispatcher))
    (error "invalid dispatcher function"))
  (sethash route (get-route server type) dispatcher))

(defun rem-route (server type route)
  (remhash route (get-route server type)))

(defun matched-route (uri)
  (aif (gethash uri (get-route *server* :static))
       it
       (maphash #'(lambda (regex dispatcher)
                    (when (preg-match regex uri)
                      (return-from matched-route dispatcher)))
                (get-route *server* :regex))))

; --- Server ----------------------------------------------------

(defstruct server
  (host                   "127.0.0.1"           :type string)
  (port                   8080                  :type integer)
  (reuse-address          t                     :type boolean)
  (backlog                5                     :type integer)
  (public-dir             "/tmp/web4r/public/"  :type string)
  (rewrite-rule           nil                   :type symbol)
  (route                  (make-route)          :type route)
  (session-save-dir       "/tmp/web4r/session/" :type string)
  (log-save-dir           "/tmp/web4r/log/"     :type string)
  (upload-tmp-dir         "/tmp/web4r/tmp/"     :type string)
  (session-gc-lifetime    1440                  :type integer)
  (session-gc-probability 100                   :type integer)
  (session-name           "sid"                 :type string)
  (session-cookie-path    "/"                   :type string)
  (timeout-sec            30                    :type integer)
  (thread))

(defmacro ensure-directories (&rest dirs)
  "ensure directories end with a slash and exist"
  `(progn
     ,@(loop for d in dirs
             collect `(unless (string= (substr ,d -1) "/")
                        (setf ,d (concat ,d "/")))
             collect `(ensure-directories-exist ,d :verbose nil))))

(defun validate-server (server)
  (with-struct
      (server public-dir rewrite-rule thread session-save-dir
              log-save-dir upload-tmp-dir) server
    (when (aand thread (bordeaux-threads:thread-alive-p it))
      (error "server is already running"))
    (unless (string= public-dir "")
      (ensure-directories public-dir)
      (unless (is-readable public-dir)
        (error "invalid public-dir")))
    (when (and rewrite-rule (not (fboundp rewrite-rule)))
      (error "invalid rewrite-rule"))
    (ensure-directories session-save-dir log-save-dir upload-tmp-dir)))

(defun start-server (&optional (server (make-server)))
  (validate-server server)
  (setf (server-thread server)
        (bordeaux-threads:make-thread
         (lambda () (start-server-thread server))
         :name (mkstr "server-" (server-port server))))
  server)

(defun start-server-thread (server)
  (with-struct (server host port reuse-address backlog) server
    (let ((socket (socket-listen host port :backlog backlog
                                 :reuse-address reuse-address
                                 :element-type '(unsigned-byte 8))))
      (unwind-protect
           (loop (handle-request server (socket-accept socket)))
        (socket-close socket)))))

(defun server-is-running-p (server)
  (when (aand (server-thread server)
              (bordeaux-threads:thread-alive-p it))
    t))

(defun stop-server (server)
  (if (server-is-running-p server)
      (bordeaux-threads:destroy-thread (server-thread server))
      (error "server is not running"))
  t)

; --- Logging ---------------------------------------------------

(defun write-log (file content)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream (mkstr content "~%"))))

(defun access-log ()
  (write-log
   (concat (server-log-save-dir *server*)
           "access." (car (split " " (iso-time))) ".log")
   (join " "
         (list (request-remote-addr *request*)
               (concat "[" (iso-time) "]")
               (qw (request-request-line *request*))
               (response-status-code *response*)
               (qw (header-field "User-Agent"))))))

(defun error-log (&rest content)
  (write-log
   (concat (if *server* (server-log-save-dir *server*) "/tmp/")
           "error." (car (split " " (iso-time))) ".log")
   (join " " (list (concat "[" (iso-time) "]")
                   (apply #'mkstr content)))))

(defun debug-log (&rest content)
  (write-log
   (concat (if *server* (server-log-save-dir *server*) "/tmp/")
               "debug." (car (split " " (iso-time))) ".log")
   (join " " (list (concat "[" (iso-time) "]")
                   (apply #'mkstr content)))))

; --- Session ---------------------------------------------------

(defun %sid ()
  (aif (get-cookie-sid)
       (progn
         (renew-cookie-lifetime it)
         it)
       (let ((sid (generate-sid)))
         (set-cookie (session-name) sid)
         sid)))

(defun get-cookies (&optional request)
  (awhen (or request *request*)
    (request-cookies it)))

(defun get-cookie (name &optional request)
  (assoc-ref name (get-cookies request) :test #'equalp))

(defun set-cookie (name &optional (value "")
                   (expires "Sun, 17-Jan-2038 19:14:07 GMT"))
  (add-header
   (format nil "Set-Cookie: ~A=~A; expires=~A; path=~A"  name
           value expires (server-session-cookie-path *server*))))

(defun get-cookie-sid ()
  (awhen (get-cookie (session-name))
    (when (valid-sid-p it)
      it)))

(defun renew-cookie-lifetime (sid)
  (trivial-shell:shell-command
   (mkstr "touch -m " (session-file sid))))

(defun get-session (&optional name)
  (awhen (aand *sid* (session-file it))
    (let ((file it))
      (when (probe-file file)
        (let ((s (with-open-file (in file)
                   (with-standard-io-syntax
                       (read in nil nil)))))
          (if name
            (assoc-ref name s :test #'equalp)
            s))))))

(defun rem-session (name)
  (let ((session (get-session)))
    (when (assoc-ref name session :test #'equalp)
      (set-session
       (remove-if #'(lambda (x) (string= name (car x)))
                  session)))))

(defun destroy-session ()
  (set-session '()))

(defun set-session (key-or-vals &optional val)
  (if (listp key-or-vals)
      (save-session-data key-or-vals)
      (let ((session (get-session)))
        (aif (assoc key-or-vals session :test #'equalp)
             (setf (cdr it) val)
             (setf session (append session
                                   (list (cons key-or-vals val)))))
        (save-session-data session))))

(defun save-session-data (data)
  (with-open-file (out (session-file *sid*)
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print data out))))

(defun session-file (sid)
  (awhen (aand sid *server*)
    (merge-pathnames sid (server-session-save-dir it))))

(defun session-name ()
  (server-session-name *server*))

(defun generate-sid ()
  (let ((sid (random-hex-string 32)))
    (if (valid-sid-p sid)
        (generate-sid)
        (progn
          (ensure-file-exist (session-file sid))
          sid))))

(defun valid-sid-p (sid)
  (let ((file (session-file sid)))
    (when (and sid (probe-file file))
      (if (> (- (get-universal-time) (file-write-date file))
             (server-session-gc-lifetime *server*))
          (progn
            (delete-file file)
            nil)
          t))))

(defun session-gc (server)
  (with-struct (server session-save-dir session-gc-lifetime) server
    (let* ((f "find ~A -maxdepth 1 -type f -cmin +~A -exec rm {} \\;")
           (min (floor (/ session-gc-lifetime 60)))
           (gc-command (format nil f session-save-dir min)))
      (trivial-shell:shell-command gc-command))))

; --- Continuations ---------------------------------------------

(defvar *cid->cont* (make-hash-table :test 'equalp)
  "cid(continuation-id) -> instance of cont structure")

(defvar *sid->cid*  (make-hash-table :test 'equalp)
  "sid(session-id) -> cid(continuation-id) : mapping index")

(defstruct cont
  (sid)
  (cont)
  (create-time))

(defvar *cid-created-order* (make-array 0 :fill-pointer 0 :adjustable t)
  "cids by the order of their created time
   this is used to destroy expired continuations")

(defvar *cont-gc-lifetime* 1440
  "continuations lifetime")

(defvar *cont-gc-probability* 100
  "probability to start a gc process")

(defun get-cid ()
  (or (post-param "cid") (get-param  "cid")))

(defun get-cont (cid)
  (awhen (gethash cid *cid->cont*)
    (if (string= (cont-sid it) *sid*)
        (cont-cont it))))

(defun generate-cid ()
  (let ((cid (random-hex-string 32)))
    (if (gethash cid *cid->cont*)
        (generate-cid)
        cid)))

(defun set-cont (cont)
  (let ((cid (generate-cid)))
    (setf (gethash cid *cid->cont*)
          (make-cont :sid *sid* :cont cont
                     :create-time (get-universal-time)))
    (setf (gethash *sid* *sid->cid*)
          (push cid (gethash *sid* *sid->cid*)))
    (vector-push-extend cid *cid-created-order*)
    (set-last-post :cid cid)
    cid))

; This function destroys a cont after calling it.
; Is there a case we want to leave it?
; (gc process will clean it up anyways.)
(defun call-cont (cid)
  (awhen (get-cont cid)
    (set-last-post :cid cid)
    (funcall it)
    (destroy-cont cid))
  (when (= (random *cont-gc-probability*) 0)
    (bordeaux-threads:make-thread
     (lambda () (cont-gc)))))

(defun cont-gc (&optional (end (length *cid-created-order*)))
  (when (and (not (zerop end))
             (cont-expired-p (elt *cid-created-order* 0)))
    (if (= end 1)
        (destroy-cont (elt *cid-created-order* 0))
        (let ((mid (round (/ end 2))))
          (if (cont-expired-p (elt *cid-created-order* mid))
              (progn
                (destroy-conts 0 mid)
                (cont-gc))
              (cont-gc (1- mid)))))))

(defun cont-created-time (cid)
  (awhen (gethash cid *cid->cont*)
    (cont-create-time it)))

(defun cont-expired-p (cid)
  (awhen (gethash cid *cid->cont*)
    (> (- (get-universal-time) *cont-gc-lifetime*)
       (cont-create-time it))))

(defun destroy-cont (&optional (cid (get-cid)) index)
  (setf *cid-created-order*
        (if index
            (delete cid *cid-created-order*
                    :test #'equal :start index :end (1+ index))
            (delete cid *cid-created-order* :test #'equalp)))
  (awhen (gethash cid *cid->cont*)
    (setf (gethash (cont-sid it) *sid->cid*)
          (delete cid (gethash (cont-sid it) *sid->cid*) :test #'equalp))
    (remhash cid *cid->cont*))
  (rem-last-post :cid cid))

(defun destroy-conts (start end)
  (loop for i from start to (1- end)
        do (destroy-cont (elt *cid-created-order* i) i)))

(defmacro lambda/cont (cont)
  (if (member (car cont) '(lambda page-lambda))
      cont
      `(lambda () ,cont)))

(defmacro a/cont/ (cont &rest body)
  (with-gensyms (cid)
    `(let ((,cid (set-cont (lambda/cont ,cont))))
       (a/ :href (mkstr (host-uri) "?cid=" ,cid) ,@body))))

(defun input-submit-p (x)
  (when (listp x)
    (case (car x)
      (submit/ t)
      (input/ 
       (equalp (awhen (member :type x) (nth 1 it)) "submit"))
      (otherwise
       (remove nil (loop for i in (cdr x)
                         when (listp i)
                         collect (input-submit-p i)))))))

(defmacro %form/cont/ (multipart-p cont &rest body)
  (with-gensyms (cid)
    `(let ((,cid (set-cont (lambda/cont ,cont))))
       (p "<FORM METHOD=\"post\" ACTION=\"\"")
       (when ,multipart-p
         (p "ENCTYPE=\"multipart/form-data\""))
       (p ">")
       (p ,(format nil "~%"))
       (input/ :type "hidden" :name "cid" :value ,cid)
       ,@body
       (unless (find-if #'input-submit-p ',body)
         (submit/))
       (p "</FORM>"))))

(defmacro form/cont/ (cont &rest body)
  `(%form/cont/ nil ,cont ,@body))

(defmacro multipart-form/cont/ (cont &rest body)
  `(%form/cont/ t   ,cont ,@body))

; --- Messages passing ------------------------------------------

(defun get-mid ()
  (or (post-param "mid") (get-param  "mid")))

(defun set-msgs (msgs)
  (set-cont (list :msgs (->list msgs))))

(defun set-error-msgs (error-msgs)
  (set-cont (list :error-msgs (->list error-msgs))))

(defun get-msgs (&optional (destroy t))
  (let* ((mid  (get-mid))
         msgs
         error-msgs)
    (awhen (awhen mid (get-cont it))
      (if (eq (car it) :msgs)
          (setf msgs (nth 1 it))
          (setf error-msgs (nth 1 it)))
      (when destroy
        (destroy-cont mid)))
    (values msgs error-msgs)))

(defun redirect/msgs (uri msgs)
  (redirect (add-get-param uri "mid" (set-msgs msgs))))

(defun redirect/error-msgs (uri error-msgs)
  (redirect (add-get-param uri "mid" (set-error-msgs error-msgs))))

; --- Page ------------------------------------------------------

(defun get-page (name)
  (gethash (make-keyword name) *pages*))

(defun set-page (name fn)
  (sethash (make-keyword name) *pages* fn))

(defmacro page-lambda ((&rest args) &rest body)
  (flet ((parse-args (args)
           (loop for a in args until (keywordp a) collect a)))
    (let ((args* (parse-args args)))
      `(lambda ,(awhen args* `(&optional ,@it))
         (let (,@(loop for p in args*
                       as  n = 2 then (1+ n)
                       collect `(,p (or ,p (uri-path ,n))))
               ,@(awhen (position :post args)
                        (loop for p in (parse-args (subseq args (1+ it)))
                              collect `(,p (post-param ,(mkstr p)))))
               ,@(awhen (position :get  args)
                        (loop for p in (parse-args (subseq args (1+ it)))
                              collect `(,p (get-param ,(mkstr p))))))
           (if (and (member :auth ',args) (null (login-user)))
               (redirect (page-uri (string-downcase
                                    (mkstr (user-login-page)))))
               (progn
                 ,@body)))))))

(defmacro defpage (name (&rest args) &rest body)
  `(set-page ',name (page-lambda (,@args) ,@body)))

(defun index-page ()
  (with-shtml (:basic)
    :title (title/ "Default index page")
    :body  (body/  (p/ "Default index page"))))

(defun %status-page (status-code)
  (let ((reason (reason-phrase status-code)))
    (with-shtml (:basic)
      :title (title/ status-code " " reason)
      :body  (body/  status-code " " reason))))

(defun status-page (status-code)
  (setf (response-status-code *response*) status-code)
  (%status-page status-code))

(defun default-page (&rest args)
  (aif (get-page 'default)
       (apply it args)
       (index-page)))

(defun page (page &rest args)
  (aif (get-page page)
       (apply it args)
       (default-page)))

(defun page/msgs (page msgs &rest args)
  (setf (response-msgs *response*) (->list msgs))
  (apply #'page `(,page ,@args)))

(defun page/error-msgs (page error-msgs &rest args)
  (setf (response-error-msgs *response*) (->list error-msgs))
  (apply #'page `(,page ,@args)))

(defun msgs/ ()
  (multiple-value-bind (msgs error-msgs) (get-msgs)
    (awhen (or msgs (when *response* (response-msgs *response*)))
      (let ((msgs it))
        (load-shtml (shtml-path "common/msgs.shtml"))))
    (awhen (or error-msgs
               (when *response* (response-error-msgs *response*)))
      (let ((error-msgs it))
        (load-shtml (shtml-path "common/error_msgs.shtml"))))))

; --- Hooks -----------------------------------------------------
; Defined hooks
; - before-handle-request
; - after-handle-request

(defvar *hooks* (make-hash-table))

(defun add-hook (hook fn)
  "Add function to hook"
  (let ((hooks (gethash hook *hooks*)))
    (unless (member fn hooks)
      (sethash hook *hooks* (append hooks (list fn))))))

(defun call-hooks (hook)
  (awhen (gethash hook *hooks*)
    (loop for h in it
          do (funcall h))))

(add-hook 'after-handle-request #'access-log)

; --- Response --------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reason-phrase (status-code)
    (case status-code
      (200 "OK")
      (201 "Created")
      (202 "Accepted")
      (204 "No Content")
      (301 "Moved Permanently")
      (302 "Moved Temporarily")
      (304 "Not Modified")
      (400 "Bad Request")
      (401 "Unauthorized")
      (403 "Forbidden")
      (404 "Not Found")
      (500 "Internal Server Error")
      (501 "Not Implemented")
      (502 "Bad Gateway")
      (503 "Service Unavailable")
      (otherwise
       (error
        (format nil "unknown status code: ~A" status-code))))))

(defun basic-header ()
  (list (rfc-1123-date)
        "Server: web4r-server"
        "Connection: close"))

(defun add-header (&rest headers)
  (setf (response-header *response*)
        (append (response-header *response*) headers)))

(defun response-status-line (status-code)
  (format nil "HTTP/1.0 ~d ~a"
          status-code (reason-phrase status-code)))

(defun charset ()
  (aif (response-charset *response*)
       (mkstr "; charset=" (string-downcase (mkstr it)))
       ""))

(defun content-length (content)
  (aif (response-charset *response*)
       (octet-length content :external-format it)
       (char-length  content)))

(defun send-header ()
  (flet ((send-http-line (line)
           (princ (mkstr line *crlf*) *http-stream*)))
    (with-flexi-stream (*http-stream* :iso-8859-1)
      (send-http-line (response-status-line (response-status-code *response*)))
      (awhen (response-content-type *response*)
        (add-header (format nil "Content-Type: ~A~A" it (charset))))
      (mapcar #'send-http-line (response-header *response*))
      (unless (eq 'HEAD (request-method *request*))
        (send-http-line "")))))

(defmacro with-header  (&rest body)
  (with-gensyms (content)
    `(let* ((*http-binary-stream* (make-in-memory-output-stream))
            (*http-char-stream*
             (make-flexi-stream *http-binary-stream* :external-format
                                (response-charset *response*))))
       (call-hooks 'before-handle-request)
       (unwind-protect
            (progn ,@body)
         (unless (response-sent *response*)
           (let ((,content (get-output-stream-sequence *http-binary-stream*)))
             (add-header (format nil "Content-Length: ~D" (content-length ,content)))
             (send-header)
             (unless (eq 'HEAD (request-method *request*))
               (aif (response-charset *response*)
                    (with-flexi-stream (*http-stream* it)
                      (princ (octets-to-string ,content :external-format it)
                             *http-stream*))
                    (write-sequence ,content *http-stream*)))))
         (call-hooks 'after-handle-request)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rfc-1123-date (&optional (time (get-universal-time)))
    (let ((days   #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
          (months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                    "Aug" "Sep" "Oct" "Nov" "Dec")))
      (multiple-value-bind (sec min hr date mon year day)
          (decode-universal-time time 0)
        (format nil "Date: ~a, ~2,'0d ~a ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
                (svref days day) date (svref months (1- mon))
                year hr min sec)))))

; --- Handle requests -------------------------------------------

(defstruct request
  (method          nil :type symbol)
  (uri             ""  :type string)
  (query-string    ""  :type string)
  (script-name     ""  :type string)
  (http-version    ""  :type string)
  (remote-addr)
  (request-line)
  (header-fields)
  (get-params)
  (post-params)
  (cookies))

(defun handle-request (server stream-usocket)
  (let ((timeout-sec (server-timeout-sec server)))
    (bordeaux-threads:make-thread
     (lambda ()
       (handler-case
           (bordeaux-threads:with-timeout (timeout-sec)
             (when (= (random (server-session-gc-probability server)) 0)
               (session-gc server))
             (let* ((*http-stream* (socket-stream stream-usocket))
                    (*server* server)
                    (*request* (parse-request)))
               (set-remote-addr stream-usocket)
               (unwind-protect
                    (handle-request-thread)
                 (close *http-stream*))))
             (bordeaux-threads:timeout () (timeout-handelr)))))))

(defun set-remote-addr (stream-usocket)
  (aif (get-peer-address stream-usocket)
       (setf (request-remote-addr *request*)
             (format nil "~{~A~^.~}" (coerce it 'list)))))

(defun timeout-handelr ()
  (error-log
   (join " " (append (list "[error]" (qw "Request timed out"))
                     (awhen *request*
                       (list (request-remote-addr it)
                             (qw (request-request-line it))))))))

(defun read-request ()
  (awhen (read-line *http-stream* nil)
    (string-trim *crlf* it)))

(defun parse-request ()
  (let ((req (make-request)))
    (with-struct
        (request method uri http-version header-fields request-line) req
      (with-flexi-stream (*http-stream* :iso-8859-1)
        ; Request-Line like "GET / HTTP/1.1"
        (let ((r (split #\Space (setf request-line (read-request)))))
          (awhen (pop r) (setf method (make-keyword it)))
          (awhen (pop r)
            (setf uri it)
            (multiple-value-bind (script query get) (parse-uri uri)
              (setf (request-script-name  req) (or script "")
                    (request-query-string req) (or query  "")
                    (request-get-params req) get)))
          (awhen (pop r) (setf http-version it)))
        ; Request header fields like "[key]: [value]"
        (loop for line = (read-request)
           do (let ((h (split ": " line)))
                (push (cons (car h) (nth 1 h)) header-fields)
                (when (string= (car h) "Cookie")
                  (setf (request-cookies req) (parse-params (nth 1 h) "; "))))
           until (string= line ""))
        ; Entity body
        (when (eq method :POST)
          (let ((length (->int (header-field "Content-Length" req)))
                (type (header-field "Content-Type" req)))
            (when (and type length (> length 0))
              (setf (request-post-params req)
                    (parse-request-body length type)))))))
    req))

(defun parse-request-body (content-length content-type)
  (multiple-value-bind (type attr)
      (rfc2388-binary:parse-header-value content-type)
    (cond ((string= type "application/x-www-form-urlencoded")
           (with-flexi-stream (*http-stream* :utf-8)
             (let ((post-query (make-string content-length)))
               (read-sequence post-query *http-stream*)
               (parse-params (uri-decode post-query)))))
          ((string= type "multipart/form-data")
           (let ((bound (assoc-ref "boundary" attr :test #'string=)))
             (rfc2388-binary:read-mime *http-stream* bound
                                       #'rfc2388-callback))))))

(defun rfc2388-callback (mime-part)
  (let* ((header (rfc2388-binary:get-header mime-part "Content-Disposition"))
         (disposition (rfc2388-binary:header-value header))
         (name (rfc2388-binary:get-header-attribute header "name"))
         (filename (rfc2388-binary:get-header-attribute header "filename")))
    (cond ((or (string= disposition "file") (not (null filename)))
           (rfc2388-callback-file mime-part name filename))
          ((string= disposition "form-data")
           (rfc2388-callback-form-data mime-part name)))))

(defun rfc2388-callback-file (mime-part name filename)
  (let ((type (rfc2388-binary:content-type mime-part))
        (tmp-name (uniq-file-name (server-upload-tmp-dir *server*)))
        (size 0))
    (setf (rfc2388-binary:content mime-part)
          (open tmp-name :direction :output :element-type '(unsigned-byte 8)))
    (values (lambda (byte)
              (incf size)
              (write-byte byte (rfc2388-binary:content mime-part)))
            (lambda ()
              (close (rfc2388-binary:content mime-part))
              (cons name (list (cons "name"     (when (plusp size) filename))
                               (cons "type"     (when (plusp size) type))
                               (cons "tmp-name" (when (plusp size) tmp-name))
                               (cons "size" size)))))))

(defun rfc2388-callback-form-data (mime-part name)
  (setf (rfc2388-binary:content mime-part)
        (make-array 10 :element-type '(unsigned-byte 8)
                    :adjustable t :fill-pointer 0))
  (values (lambda (byte)
            (vector-push-extend
             byte (rfc2388-binary:content mime-part)))
          (lambda ()
            (cons name (octets-to-string
                        (rfc2388-binary:content mime-part)
                        :external-format :utf-8)))))

(defun parse-uri (uri)
  (let ((script uri)
        query
        get)
    (awhen (position #\? uri)
      (setf script (subseq uri 0 it))
      (setf query  (subseq uri (1+ it)))
      (unless (string= query "")
        (setf get (parse-params (uri-decode query)))))
    (values script query get)))

(defun parse-params (query &optional (delimiter "&"))
  (awhen (split delimiter query)
    (mapcar #'(lambda (x)
                (if (position #\= x)
                    (let ((s (split "=" x)))
                      (when (eq (length s) 2)
                        (cons (car s)
                              (replace-str *crlf* *nl* (nth 1 s)))))))
            it)))

(defun delete-tmp-file ()
  (loop for p in (post-params)
        when (listp (cdr p))
          do (awhen (assoc-ref "tmp-name" (cdr p) :test #'equal)
               (when (and it (probe-file it))
                 (delete-file it)))))

(add-hook 'after-handle-request #'delete-tmp-file)

; --- Response --------------------------------------------------

(defstruct response
  (header       (basic-header)  :type list)
  (status-code  200             :type integer)
  (charset      :utf-8          :type symbol)
  (sent         nil             :type boolean)
  (msgs         '()             :type list)
  (error-msgs   '()             :type list)
  (content-type "text/html")
  (exit))

(defun handle-request-thread ()
  (let* ((exit (lambda () (return-from handle-request-thread)))
         (*response* (make-response :exit exit))
         (*sid* (%sid))
         (uri (rewrote-uri))
         (path1 (uri-path 1)))
    (with-header
        (case (request-method *request*)
          ((:get :head :post)
           (cond ((get-cont (get-cid)) (call-cont (get-cid)))
                 ((get-page path1) (funcall (get-page path1)))
                 ((matched-route uri) (funcall (matched-route uri)))
                 ((string= uri "/") (default-page))
                 ((string= (public-dir) "")
                  (status-page 404))
                 (t (serve-file (merge-pathnames
                                 (subseq uri 1)
                                 (namestring (public-dir)))))))
          ((:put :delete :link :unlink) (status-page 501))
          (otherwise (status-page 400))))))

(defun content-type (file)
  (let* ((file  (namestring file))
         (type  (trivial-shell:shell-command (concat "file " file)))
         (s (split " " (string-trim '(#\Newline) type))))
    (cond ((equalp "image" (nth 2 s))
           (cond ((equalp "PNG"  (nth 1 s)) "image/png")
                 ((equalp "JPEG" (nth 1 s)) "image/jpeg")
                 ((equalp "GIF"  (nth 1 s)) "image/gif")))
          ((or (equalp "icon"  (car (last s)))
               (equalp "icon"  (nth 3 s)))
                                            "image/x-icon")
          ((equalp "HTML"  (nth 1 s))       "text/html")
          ((equalp "text"  (car (last s)))
           (let ((ext (string-downcase (car (last (split "." file))))))
             (cond ((equal "js" ext)        "application/x-javascript")
                   ((equal "css" ext)       "text/css")
                   (t nil))))
          ((equalp "Zip"   (nth 1 s))       "application/zip"))))

(defun serve-file (file &key (public-file-only t))
  (cond ((and public-file-only
              (not (public-file-p file)))
         (status-page 400))
        ((or (not (probe-file file))
             (directory-exists-p file))
         (status-page 404))
        ((not (is-readable file))
         (status-page 403))
        (t (setf (response-content-type *response*) (content-type file))
           (setf (response-charset *response*) nil)
           (with-open-file (in file :element-type '(unsigned-byte 8))
             (dotimes (i (file-length in))
               (write-byte (read-byte in) *http-binary-stream*))))))

(defun public-file-p (file)
  (let* ((file-pdir (pathname-directory file))
         (public-dir (namestring (public-dir)))
         (public-pdir (pathname-directory public-dir))
         (length (length public-pdir)))
    (unless (or (member :up file-pdir)
                (> length (length file-pdir))
                (not (equal public-pdir
                            (remove-if (constantly t) file-pdir
                                       :start length))))
      t)))

; --- Util ------------------------------------------------------

(defun host-uri ()
  (concat "http://" (header-field "Host") "/"))

(defun page-uri (&rest args)
  (mkstr (host-uri) (join "/" args) "/"))

(defun uri-path (n)
  (let* ((u1 (subseq (request-uri *request*) 1))
         (u2 (let ((u (split "?" u1)))
                   (car u)))
         (u3 (split "/" u2)))
    (aif (nth (1- n) u3)
         (unless (string= it "")
           it))))

(defun get-params (&optional request)
  (awhen (or request *request*)
    (request-get-params it)))
 
(defun get-param (name &optional request)
  (assoc-ref name (get-params request) :test #'equalp))

(defun post-params (&optional request)
  (awhen (or request *request*)
    (request-post-params it)))

(defun post-param (name &optional request)
  (assoc-ref name (post-params request) :test #'equalp))

(defun header-fields (&optional request)
  (awhen (or request *request*)
    (request-header-fields it)))

(defun header-field (name &optional request)
  (assoc-ref name (header-fields request) :test #'equalp))

(defun redirect (uri)
  (setf (response-status-code *response*) 302)
  (add-header (format nil "Location: ~A" uri))
  (exit))

(defun exit ()
  (awhen (response-exit *response*)
      (funcall it)))

(defun public-dir ()
  (server-public-dir *server*))

(defun req-uri ()
  (request-uri *request*))

(defun get-file-data (name key &optional (param-fn #'last-post))
  (awhen (funcall param-fn name)
    (when (listp it)
      (assoc-ref key it :test #'equalp))))

(defun file-name (name) (get-file-data name "name"))
(defun file-type (name) (get-file-data name "type"))
(defun file-tmp-name (name) (get-file-data name "tmp-name"))
(defun file-size (name) (get-file-data name "size"))
(defun file-save-name (name) (get-file-data name "save-name"))

(defun set-last-post (&key (cid (get-cid)) name value)
  (let ((last (last-posts)))
    (rem-last-post :cid cid)
    (let ((new (if name
                   (aif (position-if #'(lambda (x) (string= (car x) name)) last)
                        (progn
                          (setf (nth it last) (cons name value))
                          last)
                        (append (cons name value) last))
                   (merge-cons-tree
                    last
                    (awhen (post-params)
                      (remove-if #'(lambda (x) (string= (car x) "cid")) it))))))
      (set-session "last-post"
                   (append (get-session "last-post")
                           (list (cons cid new)))))))

(defun rem-last-post (&key (cid (get-cid)) name)
  (awhen (get-session "last-post")
    (if name
        (set-last-post :cid cid :name name)
        (set-session "last-post"
                     (remove-if #'(lambda (x) (string= (car x) cid)) it)))))

(defun last-posts ()
  (awhen (get-session "last-post")
    (assoc-ref (get-cid) it :test #'equalp)))

(defun last-post (name)
  (awhen (last-posts)
    (assoc-ref name it :test #'equalp)))

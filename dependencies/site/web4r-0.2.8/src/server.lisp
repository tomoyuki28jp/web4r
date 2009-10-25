(in-package :web4r)

(setf *hunchentoot-default-external-format* hunchentoot::+utf-8+
      *default-content-type* "text/html; charset=utf-8")

; --- Util ------------------------------------------------------
(defun uri-path (nth &optional (request *request*))
  "Returns the nth element of the uri path from the current page uri.

 Examples:
  (defpage one () (sml:p (uri-path 1))) 
  ; wget http://localhost:8080/one/path1 => 'path1'
  (defpage one/two/three () (sml:p (uri-path 1))) 
  ; wget http://localhost:8080/one/two/three/path1 => 'path1'"
  (nth (+ nth *page-uri-paths*)
       (split #\/ (car (split #\? (request-uri* request))))))

(defun host-uri ()
  "Returns the 'X-FORWARDED-HOST' or 'Host' incoming http header value."
  (let ((host (or (header-in "X-FORWARDED-HOST" *request*)
                  (host))))
    (concat "http://" host "/")))

(defun page-uri (&rest paths)
  "Generates and returns a page uri that contains all the PATHS
 in the order that they are supplied.

 Examples:
  (page-uri 'page' 'path1' 'path1')
  ;=> 'http://localhost:8080/page/path1/path1/'"
  (concat (host-uri) (apply #'join "/" paths) "/"))

(defun file-data (name n)
  "Returns (nth N (post-parameter NAME))."
  (aand (post-parameter name) (listp it) (nth n it)))

(defun file-path (name)
  "If the value of the post parameter named NAME is a file,
 returns a pathname denoting the place where the uploaded file was stored."
  (file-data name 0))

(defun file-name (name)
  "If the value of the post parameter named NAME is a file,
 returns the file name sent by the browser."
  (file-data name 1))

(defun file-type (name)
  "If the value of the post parameter named NAME is a file,
 returns the content type sent by the browser."
  (file-data name 2))

(defun set-post-parameters (parameters)
  "Sets the post PARAMETERS which must be an alist of key/value pairs."
  (setf (slot-value *request* 'hunchentoot::post-parameters) parameters))

(defun set-get-parameters (parameters)
  "Sets the get PARAMETERS which must be an alist of key/value pairs."
  (setf (slot-value *request* 'hunchentoot::get-parameters)  parameters))

(defun start-server (&optional acceptor)
  "Starts ACCEPTOR so that it begins accepting connections.
 Returns the ACCEPTOR."
  (start (or acceptor
             (make-instance 'acceptor :port 8080
               :request-dispatcher 'web4r-dispatcher))))

(defun stop-server (acceptor)
  "Stops ACCEPTOR so that it no longer accepts requests."
  (stop acceptor))

; --- Public directories ----------------------------------------

(dolist (x `(("/tmp/"    . ,*tmp-save-dir*)
             ("/upload/" . ,*upload-save-dir*)
             ("/images/" . ,(merge-pathnames "images/" *public-dir*))
             ("/css/"    . ,(merge-pathnames "css/"    *public-dir*))
             ("/js/"     . ,(merge-pathnames "js/"     *public-dir*))))
  (push (create-folder-dispatcher-and-handler (car x) (cdr x)) *dispatch-table*))

; default page
(setf *default-handler*
      #'(lambda ()
          (handle-static-file (public-path "default.html"))))

; --- Dispatcher ------------------------------------------------

(defun web4r-dispatcher (request)
  (start-session)
  (catch 'web4r-dispatcher
    (handler-bind ((error #'backtrace-log))
      (with-output-to-string (*sml-output*)
        (if (get-cont (cid))
            (call-cont (cid))
            (multiple-value-bind (fn paths)
                (get-page (request-uri* request))
              (if fn
                  (let ((*page-uri-paths* paths))
                    (funcall fn))
                  (hunchentoot::list-request-dispatcher request))))))))

(defun get-page (uri)
  (labels ((get-page* (idx alist &optional (path 0))
             (let ((x (cdr (assoc (pop idx) alist :test #'equal))))
               (cond ((consp x) (get-page* idx x (1+ path)))
                     (t (values (cdr (assoc nil alist)) path))))))
    (get-page* (split "/" (string-trim "/" (car (split #\? uri))))
               *pages*)))

(defun set-page (path fn)
  (let ((idx (append (split "/" (string-trim "/" path)) '(nil))))
    (setf *pages* (replace-assoc* idx *pages* fn :test #'equal))))

(defmacro page-lambda ((&rest args) &body body)
  "Creates a page procedure.

 Syntax:
  page-lambda ([path ...] [:get garg ...] [:post poarg ...] :auth
                 :redirect redirect) body
 Arguments and Values:
  path---an uri path
  garg---a symbol name of a get parameter
  poarg---a symbol name of a post parameter
  :auth---if :auth is supplied and the current user hasn't logged in,
          redirects the user to the login page
  redirect---an uri to redirect users after logging in
  body---a form
 Examples:
  (defpage said ()
    (form/cont (page-lambda (:post foo)
                 (a/cont [p \"You said: \" foo] \"click here\"))
     (input-text \"foo\")))"
  (flet ((args (args) (loop for a in args until (keywordp a) collect a))
         (car*  (x) (if (listp x) (car x) x))
         (cadr* (x) (when (listp x) (cadr x))))
    (let ((args* (args args)))
      `(lambda ,(awhen args* `(&optional ,@(mapcar #'car* it)))
         (let (,@(loop for p in args*
                       as  n = 1 then (1+ n)
                       collect `(,(car* p) (or ,(car* p) (uri-path ,n) ,(cadr* p))))
               ,@(awhen (position :post args)
                   (loop for p* in (args (subseq args (1+ it)))
                         as p = (car* p*) as d = (cadr* p*)
                         collect `(,p (or (post-parameter ,(->string-down p)) ,d))))
               ,@(awhen (position :get  args)
                   (loop for p* in (args (subseq args (1+ it)))
                         as p = (car* p*) as d = (cadr* p*)
                         collect `(,p (or (get-parameter  ,(->string-down p)) ,d)))))
           (if (and ',(member :auth args) (null (login-user)))
               (login-page (or ,(cadr (member :redirect args)) (request-uri*)))
               (progn ,@body)))))))

(defmacro defpage (page (&rest args) &rest body)
  "Defines a web page for PATHS. Users can visit a defined web page by accessing
 an uri like 'http://yourhost/PATHS'.

 Syntax:
  defpage paths ([parg ...] [:get garg ...] [:post poarg ...]
                   :auth :default :redirect redirect) body
 Arguments and Values:
  paths---a base path of a web page
  parg---a relative uri path from the base PATHS
  garg---a symbol name of a get parameter
  poarg---a symbol name of a post parameter
  :auth---if :auth is supplied and the current user hasn't logged in,
          redirects the user to a login page
  :default---if :default is supplied, the page procedure is set to *default-handler*
  redirect---an uri to redirect users after logging in
  body---a form
 Examples:
  (defpage test (path1 path2 :get get1 get2)
    [p path1 \" \" path2 \" \" get1 \" \" get2])
  ; wget http://localhost:8080/test/1/2/ => '<p>1 2  </p>'
  ; wget http://localhost:8080/test/1/2/?get1=3&get2=4 => '<p>1 2 3 4</p>'"
  `(progn
     (set-page (->string-down ',page) (page-lambda (,@args) ,@body))
     (when (member :default ',args)
       (setf *default-handler*
             (lambda () (page (->string-down ',page)))))))

(defun page (page &rest args)
  "Calls the procedure of the PAGE with ARGS."
  (multiple-value-bind (fn paths) (get-page page)
    (aif fn
         (let ((*page-uri-paths* paths))
           (apply it args))
         (setf (return-code *reply*) +http-not-found+))))

; --- Messages --------------------------------------------------

(defclass msgs ()       ((msgs :initarg :msgs)))
(defclass error-msgs () ((msgs :initarg :msgs)))

(defun mid () (parameter "mid"))

(defun w/mid (uri msgs type)
  (let ((msgs (make-instance type :msgs (->list msgs))))
    (add-parameter uri "mid" (set-cont msgs))))

(defun redirect/msgs (uri messages)
  "Redirects to the URI with the success MESSAGES."
  (redirect (w/mid uri messages 'msgs)))

(defun redirect/error-msgs (uri messages)
  "Redirects to the URI with the error MESSAGES."
  (redirect (w/mid uri messages 'error-msgs)))

(defmacro w/msgs (msgs page args type)
  `(let ((*msgs* (make-instance ,type :msgs (->list ,msgs))))
     (apply #'page ,page ,args)))

(defun page/msgs (page messages &rest args)
  "Calls the procedure of the PAGE with success MESSAGES and ARGS."
  (w/msgs messages page args 'msgs))

(defun page/error-msgs (page messages &rest args)
  "Calls the procedure of the PAGE with error MESSAGES and ARGS."
  (w/msgs messages page args 'error-msgs))

(defun get-msgs ()
  "Returns a list of messages to display if any."
  (awhen (or (when-let (mid (mid))
               (awhen (get-cont mid) (destroy-cont mid) it))
             *msgs*)
    (when (member (type-of it) '(msgs error-msgs))
      it)))

(defun msgs ()
  "Displays messages if any."
  (awhen (get-msgs)
    (when-let (msgs (slot-value it 'msgs))
      (case (type-of it)
        (msgs       (load-sml-path "msgs/msgs.sml"))
        (error-msgs (load-sml-path "msgs/error_msgs.sml"))))))

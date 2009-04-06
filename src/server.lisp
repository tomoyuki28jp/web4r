(in-package :web4r)

(defvar *page-uri-paths* 0
  "The number of current page uri paths")

(defvar *pages* nil
  "Page paths -> handler alist")

(defvar *msgs* nil
  "Instance of the msgs structure")

(defvar *default-page* nil
  "Default page uri")

; --- Util ------------------------------------------------------

(defun uri-path (n &optional (request *request*))
  (nth (1- (+ n *page-uri-paths*))
       (split #\/ (car (split #\? (request-uri* request))))))

(defun host-uri ()
  (concat "http://" (host) "/"))

(defun page-uri (&rest args)
  (concat (host-uri) (apply #'join "/" args) "/"))

(defun file-data (name n)
  (aand (post-parameter name) (listp it) (nth n it)))

(defun file-path (name) (file-data name 0))
(defun file-name (name) (file-data name 1))
(defun file-type (name) (file-data name 2))

(defun default-page ()
  (page *default-page*))

(defun set-post-parameters (params)
  (setf (slot-value *request* 'hunchentoot::post-parameters) params))

(defun set-get-parameters (params)
  (setf (slot-value *request* 'hunchentoot::get-parameters) params))

(defun start-server (&optional acceptor)
  (start (or acceptor
             (make-instance 'acceptor :port 8080
                            :request-dispatcher 'web4r-dispatcher))))

(defun stop-server (acceptor)
  (stop  acceptor))

; --- Dispatcher ------------------------------------------------

(defun web4r-dispatcher (request)
  (start-session)
  (catch 'web4r-dispatcher
    (handler-bind ((error #'backtrace-log))
      (with-output-to-string (*html-output*)
        (if (get-cont (cid))
            (call-cont (cid))
            (multiple-value-bind (fn paths) (get-page (request-uri* request))
              (if fn (let ((*page-uri-paths* paths)) (funcall fn))
                     (default-page))))))))

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
  (flet ((args (args) (loop for a in args until (keywordp a) collect a)))
    (let ((args* (args args)))
      `(lambda ,(awhen args* `(&optional ,@it))
         (let (,@(loop for p in args*
                       as  n = 2 then (1+ n)
                       collect `(,p (or ,p (uri-path ,n))))
               ,@(awhen (position :post args)
                   (loop for p in (args (subseq args (1+ it)))
                         collect `(,p (post-parameter ,(->string-down p)))))
               ,@(awhen (position :get  args)
                   (loop for p in (args (subseq args (1+ it)))
                         collect `(,p (get-parameter  ,(->string-down p))))))
           (if (and ',(member :auth args) (null (login-user)))
               (regist-page (or ,(cadr (member :redirect args)) (host-uri)))
               (progn ,@body)))))))

(defmacro defpage (name (&rest args) &rest body)
  `(progn
     (set-page (->string-down ',name) (page-lambda (,@args) ,@body))
     (when (member :default ',args)
       (setf *default-page* (->string-down ',name)))))

(defun page (page &rest args)
  (multiple-value-bind (fn paths) (get-page page)
    (aif fn (let ((*page-uri-paths* paths)) (apply it args))
            (setf (return-code *reply*) +http-not-found+))))

; --- Messages --------------------------------------------------

(defclass msgs ()       ((msgs :initarg :msgs :accessor msgs)))
(defclass error-msgs () ((msgs :initarg :msgs :accessor msgs)))

(defun mid () (parameter "mid"))

(defun w/mid (uri msgs type)
  (let ((msgs (make-instance type :msgs (->list msgs))))
    (add-parameter uri "mid" (set-cont msgs))))

(defun redirect/msgs (uri msgs)       (redirect (w/mid uri msgs 'msgs)))
(defun redirect/error-msgs (uri msgs) (redirect (w/mid uri msgs 'error-msgs)))

(defmacro w/msgs (msgs page args type)
  `(let ((*msgs* (make-instance ,type :msgs (->list ,msgs))))
     (apply #'page ,page ,args)))

(defun page/msgs (page msgs &rest args)       (w/msgs msgs page args 'msgs))
(defun page/error-msgs (page msgs &rest args) (w/msgs msgs page args 'error-msgs))

(defun get-msgs (&optional type)
  (awhen (or (when-let (mid (mid))
               (awhen (get-cont mid) (destroy-cont mid) it))
             *msgs*)
    (when (and (member (type-of it) '(msgs error-msgs))
               (or (null type) (typep it type)))
      (msgs it))))

(defun msgs/ ()
  (when-let (msgs (get-msgs 'msgs))
    (load-shtml (shtml-file-path "common/msgs.shtml")))
  (when-let (error-msgs (get-msgs 'error-msgs))
    (load-shtml (shtml-file-path "common/error_msgs.shtml"))))

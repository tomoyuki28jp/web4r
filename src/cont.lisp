(in-package :web4r)

; --- Continuations ---------------------------------------------

(defstruct cont
  (cont)
  (sid            0 :type integer)
  (generated-time 0 :type integer))

(defun sid (&optional (session *session*))
  "Returns the current session id as a string."
  (hunchentoot::session-id session))

(defun cid ()
  "Returns the current continuation id as a string if the request is 
calling a continuation and nil otherwise."
  (parameter "cid"))

(defun get-cont (continuation-id)
  "Returns the continuation associated with the CONTINUATION-ID 
if any and nil otherwise"
  (awhen (gethash continuation-id *cid->cont*)
    (when (= (cont-sid it) (sid))
      (cont-cont it))))

(defun generate-cid ()
  "Generates and returns a string unique continuation id"
  (dotimes (x 10)
    (let ((cid (hunchentoot::create-random-string 10 36)))
      (unless (gethash cid *cid->cont*)
        (return-from generate-cid cid)))))

(defun set-cont (continuation)
  "Sets the CONTINUATION and returns the string continuation id."
  (let ((cid (generate-cid)))
    (setf (gethash cid *cid->cont*)
          (make-cont :sid (sid) :cont continuation
                     :generated-time (get-universal-time)))
    (setf (gethash (sid) *sid->cid*)
          (push cid (gethash (sid) *sid->cid*)))
    (vector-push-extend cid *cid-generated-order*)
    (awhen (aand (cid) (gethash it *cont-sessions*))
      (setf (gethash cid *cont-sessions*) it))
    cid))

(defun cont-gc (&optional (end (length *cid-generated-order*)))
  "Executes garbage collection for expired continuations."
  (when (and (plusp end)
             (cont-expired-p (elt *cid-generated-order* 0)))
    (if (= end 1)
        (destroy-cont (elt *cid-generated-order* 0))
        (let ((mid (round (/ end 2))))
          (if (cont-expired-p (elt *cid-generated-order* mid))
              (progn (destroy-conts 0 mid)
                     (cont-gc))
              (cont-gc (1- mid)))))))

(defun cont-expired-p (cid)
  "Returns true if the continuation associated with the CID 
(continuation id) has expired and nil otherwise."
  (awhen (gethash cid *cid->cont*)
    (> (- (get-universal-time) *cont-gc-lifetime*)
       (cont-generated-time it))))

(defun destroy-cont (&optional (cid (cid)) index)
  "Destroys the continuation associated with the CID (continuation id)."
  (setf *cid-generated-order*
        (if index
            (delete cid *cid-generated-order*
                    :test #'equal :start index :end (1+ index))
            (delete cid *cid-generated-order* :test #'equal)))
  (awhen (gethash cid *cid->cont*)
    (setf (gethash (cont-sid it) *sid->cid*)
          (delete cid (gethash (cont-sid it) *sid->cid*) :test #'equal))
    (remhash cid *cid->cont*))
  (remhash cid *cont-sessions*))

(defun destroy-conts (start end)
  "Destroys the continuations from the START to the END by the order of 
their generated time."
  (loop for i from start to (1- end)
        do (destroy-cont (elt *cid-generated-order* i) i)))

(defun destroy-conts-by-session (session)
  "Destroys the all continuations associated with the SESSION."
  (let ((sid (sid session)))
    (when-let (cids (gethash sid *sid->cid*))
      (remhash sid *sid->cid*)
      (dolist (cid cids) (destroy-cont cid))
      (remhash sid *sid->cid*))))

(add-hook 'after-calling-cont #'destroy-cont)

(defun call-cont (cid)
  "Calls the continuation associated with the CID (continuation id). By default, 
this function destroys the continuation after calling it. If you want to leave 
it, run this code: (rem-hook 'after-calling-cont #'destroy-cont)."
  (awhen (get-cont cid)
    ; Without unwind-protect, destroy-cont and cont-gc won't be 
    ; executed when we call hunchentoot:redirect inside a cont.
    (unwind-protect (funcall it)
      (run-hook-with-args 'after-calling-cont cid)
      (when (= (random *cont-gc-probability*) 0)
        (bordeaux-threads:make-thread
         (lambda () (cont-gc)))))))

(defun renew-cont-lifetime (cid)
  "Renews the continuation lifetime associated with the CID (continuation id)."
  (when (get-cont cid)
    (let ((cont (gethash cid *cid->cont*)))
      (setf (cont-generated-time cont) (get-universal-time))
            (gethash cid *cid->cont*) cont)
    (vector-push-extend
     cid (delete cid *cid-generated-order* :test #'equal))))

(setf *session-removal-hook*
      #'(lambda (session)
          (destroy-conts-by-session session)))

; --- Cont sessions ---------------------------------------------

(defun cont-session (key &optional (cid (cid)))
  "Returns the entry for the KEY in cont-session 
(continuation based session) if any."
  (cdr (assoc key (gethash cid *cont-sessions*))))

(defun rem-cont-session (key &optional (cid (cid)))
  "Removes the entry for the KEY in cont-session 
(continuation based session) if any."
  (setf (gethash cid *cont-sessions*)
        (remove-if #'(lambda (x) (eq (car x) key))
                   (gethash cid *cont-sessions*))))

(defun (setf cont-session) (value key &optional (cid (cid)))
  "Sets the VALUE for the KEY in cont-session (continuation based session)."
  (setf (gethash cid *cont-sessions*)
        (append (rem-cont-session key cid)
                (list (cons key value)))))

; --- Form with Continuations -----------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ->lambda (body)
    (if (member (car body) '(lambda page-lambda))
        body
        `(lambda () ,body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cont-expand (continuation)
    "Expands last-post inside the CONTINUATION"
    (loop for x in continuation collect
          (cond ((atom x) x)
                ((eq 'last-post (car x))
                 (let ((g (gensym)))
                   (push (list g (nth 1 x)) *last-posts*)
                   g))
                ((unless (eq (car x) 'form/cont)
                   (cont-expand x)))
                (t x)))))

(defmacro cont/lambda (continuation)
  (let* (*last-posts*
         (expanded (->lambda (cont-expand continuation))))
    (if *last-posts*
        (append (subseq expanded 0 2)
                `((let (,@(loop for (s p) in *last-posts* collect
                                `(,s (post-parameter ,(->string p)))))
                    ,@(subseq expanded 2))))
        expanded)))

(defmacro a/cont (continuation &rest body)
  "Embeds the CONTINUATION within a link"
  (let ((cid (gensym)))
    `(let ((,cid (set-cont (cont/lambda ,continuation))))
       [a :href (concat (host-uri) "?cid=" ,cid) ,@body])))

(defmacro %form/cont (multipart-p cont &rest body)
  (let ((cid (gensym)))
    `(let ((,cid (set-cont (cont/lambda ,cont))))
       (form :method "post" :action ""
             ,@(when multipart-p '(:enctype "multipart/form-data"))
             ,@body
             [input :type "hidden" :name "cid" :value ,cid]))))

(defmacro form/cont (continuation &rest body)
  "Embeds the CONTINUATION within a form"
  `(%form/cont nil ,continuation ,@body))

(defmacro multipart-form/cont (continuation &rest body)
  "Embeds the CONTINUATION within a multi part form"
  `(%form/cont t   ,continuation ,@body))

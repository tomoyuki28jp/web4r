(in-package :web4r)

; --- Continuations ---------------------------------------------

(defstruct cont
  (cont)
  (sid            0 :type integer)
  (generated-time 0 :type integer))

(defun sid (&optional (session *session*))
  (hunchentoot::session-id session))

(defun cid ()
  (parameter "cid"))

(defun get-cont (cid)
  (awhen (gethash cid *cid->cont*)
    (when (= (cont-sid it) (sid))
      (cont-cont it))))

(defun generate-cid ()
  (dotimes (x 10)
    (let ((cid (hunchentoot::create-random-string 10 36)))
      (unless (gethash cid *cid->cont*)
        (return-from generate-cid cid)))))

(defun set-cont (cont)
  (let ((cid (generate-cid)))
    (setf (gethash cid *cid->cont*)
          (make-cont :sid (sid) :cont cont
                     :generated-time (get-universal-time)))
    (setf (gethash (sid) *sid->cid*)
          (push cid (gethash (sid) *sid->cid*)))
    (vector-push-extend cid *cid-generated-order*)
    (awhen (aand (cid) (gethash it *cont-sessions*))
      (setf (gethash cid *cont-sessions*) it))
    cid))

; This function destroys a cont after calling it.
; Is there a case we want to leave it?
; (gc process will clean it up anyways.)
(defun call-cont (cid)
  (awhen (get-cont cid)
    ; without unwind-protect, destroy-cont and cont-gc won't be
    ; executed when we call hunchentoot:redirect inside a cont.
    (unwind-protect (funcall it)
      (destroy-cont cid)
      (when (= (random *cont-gc-probability*) 0)
        (bordeaux-threads:make-thread
         (lambda () (cont-gc)))))))

(defun cont-gc (&optional (end (length *cid-generated-order*)))
  (when (and (plusp end)
             (cont-expired-p (elt *cid-generated-order* 0)))
    (if (= end 1)
        (destroy-cont (elt *cid-generated-order* 0))
        (let ((mid (round (/ end 2))))
          (if (cont-expired-p (elt *cid-generated-order* mid))
              (progn
                (destroy-conts 0 mid)
                (cont-gc))
              (cont-gc (1- mid)))))))

(defun cont-expired-p (cid)
  (awhen (gethash cid *cid->cont*)
    (> (- (get-universal-time) *cont-gc-lifetime*)
       (cont-generated-time it))))

(defun destroy-cont (&optional (cid (cid)) index)
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
  (loop for i from start to (1- end)
        do (destroy-cont (elt *cid-generated-order* i) i)))

(defun destroy-conts-by-session (session)
  (let ((sid (sid session)))
    (when-let (cids (gethash sid *sid->cid*))
      (remhash sid *sid->cid*)
      (dolist (cid cids) (destroy-cont cid))
      (remhash sid *sid->cid*))))

(setf *session-removal-hook*
      #'(lambda (session)
          (destroy-conts-by-session session)))

; --- Cont sessions ---------------------------------------------

(defun cont-session (key &optional (cid (cid)))
  (cdr (assoc key (gethash cid *cont-sessions*))))

(defun rem-cont-session (key &optional (cid (cid)))
  (setf (gethash cid *cont-sessions*)
        (remove-if #'(lambda (x) (eq (car x) key))
                   (gethash cid *cont-sessions*))))

(defun (setf cont-session) (value key &optional (cid (cid)))
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
  (defun cont-expand (cont)
    (loop for x in cont collect
          (cond ((atom x) x)
                ((eq 'last-post (car x))
                 (let ((g (gensym)))
                   (push (list g (nth 1 x)) *last-posts*)
                   g))
                ((unless (eq (car x) 'form/cont)
                   (cont-expand x)))
                (t x)))))

(defmacro cont/lambda (cont)
  (let* (*last-posts*
         (expanded (->lambda (cont-expand cont))))
    (if *last-posts*
        (append
         (subseq expanded 0 2)
         `((let (,@(loop for (s p) in *last-posts* collect
                         `(,s (post-parameter ,(->string p)))))
             ,@(subseq expanded 2))))
        expanded)))

(defmacro a/cont (cont &rest body)
  (let ((cid (gensym)))
    `(let ((,cid (set-cont (cont/lambda ,cont))))
       [a :href (concat "?cid=" ,cid) ,@body])))

(defmacro %form/cont (multipart-p cont &rest body)
  (let ((cid (gensym)))
    `(let ((,cid (set-cont (cont/lambda ,cont))))
       (form :method "post" :action ""
             ,@(when multipart-p '(:enctype "multipart/form-data"))
             ,@body
             [input :type "hidden" :name "cid" :value ,cid]))))

(defmacro form/cont (cont &rest body)
  `(%form/cont nil ,cont ,@body))

(defmacro multipart-form/cont (cont &rest body)
  `(%form/cont t   ,cont ,@body))

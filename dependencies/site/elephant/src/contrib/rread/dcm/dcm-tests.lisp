(in-package "DCM")

(defclass UserObject (managed-object)
  ((username :type 'string :initform "" :initarg :uname :accessor :username)
   (password :type 'string :initform "" :initarg :pword :accessor :password)
   (email :type 'string :initform "" :initarg :email :accessor :eml)   
   (fullname :type 'string :initform "" :initarg :fullname :accessor :fllnm)   
   (profile :type 'string :initform "" :initarg :profile :accessor :prfl)   
   (motto :type 'string :initform "" :initarg :motto :accessor :mtt)   
   (privileges :type 'list :initform '() :initarg :privileges :accessor :prvlgs)
   (preflang :type 'string :initform "en" :initarg :preflang :accessor :prflng)
;; For now, this will just be a nice big association list, and 
;; the only prefs I have right now are gridconfigurations
   (prefs :type 'list :initform '() :initarg :prefs :accessor :prfs)
;; These controls the markets that a user is a allowed to read or create an offer in
;; (keys identifying markets are stored here.)
;; We DO NOT give privilege to the public market;  We don't want to 
;; store more than necessary.  If a market is public, it is not represented here
;; (in the read list!)
))

(defclass ExObjectDirector (hash-ele-director)
  ((mtype :initform 'ExObject
	  :accessor :mtype)))

(defun test-ex-director ()
  (let ((k1 nil)
	(k2 nil))
    (let* ((o1 (make-instance 'ExObject))
	   (ed (init-director 'ExObjectDirector 'ExObjectDirector))
	   (o2 (make-instance 'ExObject)))
      (setf (slot-value o1 'username) "spud")
      (setf (slot-value o2 'username) "mud")
      (setf k1 (k (mid (register-obj ed o1))))
      (setf k2 (k (mid (register-obj ed o2))))
      )
    (let* (
	   (ed (init-director 'ExObjectDirector 'ExObjectDirector)))
      (format t "K1 ~A~%" (slot-value (lookup-obj ed (make-instance 'key :id k1)) 'username))
      (format t "K2 ~A~%" (slot-value (lookup-obj ed (make-instance 'key :id k2)) 'username))
      (and (equal (slot-value (lookup-obj ed (make-instance 'key :id k1)) 'username)
		  "spud")
	   (equal (slot-value (lookup-obj ed (make-instance 'key :id k2)) 'username)
		  "mud")
	   ))))


;; Create 10 objects, retire them, and make sure that they can 
;; still be found.
(defclass TestGenDir (GenDir)
  ((mtype :initform 'ExObject))
)
(defun test-retirement ()
  (let ((g (init-director 'TestGenDir 'TestGenDir))
	(r (randomize-managed-object 
	    (make-instance 'ExObject))))
    (setf (slot-value r 'number) 0)
    (setf (slot-value r 'username) "username")
    (setf (slot-value r 'password) "password")
    (register-obj g r)
    (assert (= 0 (find-generation g (mid r))))
    (retire g (mid r))
    (assert (= 1 (find-generation g (mid r))))
    )
  )

(defun test-deletion-from-gen ()
  (let ((g (init-director 'TestGenDir 'TestGenDir))
	(r (randomize-managed-object 
	    (make-instance 'ExObject))))
    (setf (slot-value r 'number) 0)
    (setf (slot-value r 'username) "username")
    (setf (slot-value r 'password) "password")
    (register-obj g r)
    (retire g (mid r))
    (let ((id (mid r)))
      (assert (= 1 (find-generation g (mid r))))
      (delete-all-objects-from-director g 'ExObject)
      (lookup-obj-aux g id)
      (let ((gp (init-director 'TestGenDir 'TestGenDir)))
	(assert (null (get-all-objects gp)))
    )
    )

))


(defun test-naming-uniqueness ()
  (let ((g (init-director 'TestGenDir 'TestGenDir))
	(r (randomize-managed-object 
	    (make-instance 'ExObject)))
	(s 0))
    (setf (slot-value r 'number) 0)
    (setf (slot-value r 'username) "username")
    (setf (slot-value r 'password) "password")
    (register-obj g r)
    (do ((i 0 (1+ i))
	 (dirs (subdirs g) (rest dirs)))
	((null dirs))
      (setf s (+ s (length (get-all-objects (car dirs))))))
    (assert (= s 1))))

(defun many-threads ()
  (let (
	(ed (init-director 'ExObjectDirector 'ExObjectDirector))
	)
    (dotimes (x 100)
      (sb-thread:make-thread 
       #'(lambda () (format t "YYY~A~%" (get-unused-key-value ed)))))
))
    

;; This command should test everything so far....
(defun run-all-dcm-tests () 
  (let ((dt (make-instance 'dir-test)))
    (unused-key dt)
    (tm-register-then-lookup dt)
    (tm-get-all-objects dt)
    (tm-test-elephant dt)
    (many-threads)
    ))

;;; Copyright 2004, Robert L. Read.  All rights reserved.
;;;

;; It seems silly to have to do this, but I suspect it
;; will make the inheritance work better.
;; An open question is do we want to have to subclass keys
;; or not; certainly a common error is to pass in the
;; wrong key.

;; I'm currently not sure how to load this file properly
;; and how to run ALL the test with a single command.

;; Note: as of Jun. 12, 2006, I am trying to make this thread safe.
;; according to my understanding and experiments there are two 
;; problems:  SBCL itself is not threadsafe (hash tables are mentioned 
;; in particular) although it is probably improving with every release.
;; By my experiments, I have specifically duplicated a terrible, unrecoverable
;; hang in the CLSQL connection both through elephant and with direct queries.
;; 
;; Therefore, since everything I personally do is built on top 
;; of DCM, I am imposing thread-safety at the DCM level (even though in
;; the best case this probably does not allow as much concurrency as one might like.)
;; 

(in-package "DCM")

(defparameter *SLEEPYCAT-HOME* "/home/read/testdb")
(defparameter *POSTGRES-SPEC* '(:clsql (:postgresql "localhost.localdomain" "test" "postgres" "")))
(defparameter *DCM-DEFAULT* *POSTGRES-SPEC*)
(defparameter *ELEPHANT-CAT* "elephant director")
(defparameter *DEF-STORE-NAME* "DefaultStoreX")


(asdf:operate 'asdf:load-op :elephant)
(use-package "ELEPHANT")
;; (asdf:operate 'asdf:load-op :ele-bdb)
(asdf:operate 'asdf:load-op :ele-clsql)
(use-package "SB-THREAD")

(defclass key ()
  ((id :type 'integer
       :initform -1
       :initarg :id
       :accessor k)))


(defmethod max-key-value ((a key) (b key))
  (max (k a)
       (k b)))

(defmethod max-key ((a key) (b key))
  (if (< (k a) (k b))
      b
      a))

;; I think perhas we could use a better type specifier for this
;; than integer.
(defclass managed-object ()
  ((mid :type 'key
	:initform nil
	:initarg :mid
	:accessor mid)
   (owner :type 'key
	  ;; This is basically saying that the key 0 had better specify a legitimate
	  ;; owner --- but that is the responsibility of the clients of this package.
	  :initform (make-instance 'key :id 0)
	  :initarg :owner
	  :accessor :ownr)
   (tstamp :type 'number
	   ;; This is basically saying that the key 0 had better specify a legitimate
	   ;; owner --- but that is the responsibility of the clients of this package.
	   :initform (get-universal-time)
	   :initarg :tstamp
	   :accessor :dcm-tstmp)
   )
  )



(defmethod mo-equal ((a managed-object) (b managed-object))
  (equal (get-values a) (get-values b)))

(defmethod key-equal ((a key) (b key))
  (= (k a) (k b)))

(defmethod dcm-equal (a b)
  (let ((ka 
	 (if (typep a 'managed-object)
	     (k (mid a))
	     (if (typep a 'key)
		 (k a)
		 (if (typep a 'string)
		     (parse-integer a)
		 a))))
	(kb 
	 (if (typep b 'managed-object)
	     (k (mid b))
	     (if (typep b 'key)
		 (k b)
		 (if (typep a 'string)
		     (parse-integer a)
		 b)))))
    (and ka kb
	 (= ka kb))
    )
  )



(defmethod get-values ((a managed-object))
  (mapcar #'(lambda (x)
	      (let* ((name (sb-pcl:slot-definition-name x))
		     (value (if (slot-boundp a name)
				(slot-value a name)
				nil)))
		(cons name value)))
	  (sb-mop:class-slots (class-of a))))


;; This will make red tests for now...
(defun randomize-slot-value (s mo)
  (let ((ltype (sb-pcl:slot-definition-type s))
	(name (sb-pcl:slot-definition-name s)))
    (let ((crazy (cadr ltype)))
      (let ((v 
	     (cond
	       ((eql crazy 'string) (format nil "rand~A" (random 1000)))
	       ((eql crazy 'integer) (random 1000))
	       ((eql crazy 'number) (random 1000))
	       ((eql crazy 'key) 5) 
	       (t (error "not-implemented ~A" ltype))
	       )))
	(setf (slot-value mo name) v)))))

;; We should define a function for getting the user-defined slots
;; (as opposed to key, which is not really a data field)
(defmethod get-user-defined-slots ((mo managed-object))
  (let ((slots (sb-pcl:class-slots (class-of mo))))
    (remove-if #'(lambda (x)
		   (equal (cadr (sb-pcl:slot-definition-type x))
			  'key)) slots)
    ))
(defmethod randomize-managed-object ((mo managed-object))
  (let ((slots (get-user-defined-slots mo)))
    (mapcar #'(lambda (x) (randomize-slot-value x mo)) slots)
    mo))

;; This is an example; it should have inherited a key...
(defclass ExObject (managed-object)
  ((username :type 'string)
   (password :type 'string)
   (number :type 'integer)
   ))

;; The purpose of this class is to manage client data
;; in those cases where we choose not to subclass managed-object;
;; A managed-handle has to slots, mid and cd (for client data).
;; You can put anything you want into a cd (later, I might 
;; define a type to cover what Elephant can store, those preventing
;; creating illegal objects, but I don't think that is necessary now.
(defclass managed-handle (managed-object)
  ((cd :initform nil
       :initarg :cd
       :accessor cd)))

(defun test-randomize-managed-object ()
  (let ((m (make-instance 'ExObject)))
    (setf (slot-value m 'number) 0)
    (setf (slot-value m 'username) "username")
    (setf (slot-value m 'password) "password")
    (let ((r (randomize-managed-object m)))
      (and (not (eq (slot-value r 'number) 0))
	   (not (equal (slot-value r 'username) "username"))
	   (not (equal (slot-value r 'password) "password"))))))

(defmethod max-key-value ((a managed-object) (b managed-object))
  (max (k (mid a))
       (k (mid b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below here begins directors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *DIR-CAT* "director")
(defclass director ()
  ((repository-url ;; A URL? specifying the repository (a "connect string")
    :type 'string)    
   (strategy) ;; This object is hard to type, it will vary...
   (mtype
    :type 'type
    :accessor :mtype
    :initarg :managed-type)
   )
  )

;; These are the basic operations.  I'm using "register" as
;; the "update" method; to mutate an object, you register one
;; that already exists.

(defgeneric register-obj (director managed-object)
  )

(defgeneric lookup-obj (director obj)

  )
(defgeneric lookup-obj-key (director key)

  )
(defgeneric delete-obj (director key)
  )

(defgeneric get-all-objects (director)
  )

(defmethod lookup-obj ((dir director) (id key))
  (lookup-obj-key dir id)
)

(defmethod lookup-obj ((dir director) (id integer))
  (lookup-obj-key dir (make-instance 'key :id id))
)

(defmethod lookup-obj ((dir director) (id string))
  (lookup-obj-key dir (make-instance 'key :id (parse-integer id)))
)


(defmethod get-all-cur-objects ((dir director))
  (get-all-objects dir))

;; I can imagine a time when we will need this...
;; (defgeneric map-director ((dir director) function)
;; )


;; Obviously, use this routine with caution!
(defmethod delete-all-objects-from-director ((dir director) tp)
  (mapc 
   #'(lambda (mo) 
       (delete-obj dir (mid mo)))
   (get-all-objects dir)))


;; (defmethodex delete-all-objects-from-director (dir director) (tp)
;;    (mapc 
;;     #'(lambda (mo) 
;;         (delete-obj dir (mid mo)))
;;     (get-all-objects dir)))


;; Create a hash-based subclass
(defparameter *HASH-CAT* "hash director")

(defclass hash-director (director)
  ((key-to-mo
    :type 'hashtable
    :initform (make-hash-table))))

;; This is a perfect example of how I ought to be 
;; using both defgeneric and some kind of type-reference;
;; does get-all-objects return a hashtable or a list?
(defmethod get-all-objects ((dir hash-director))
  (get-all-objects-type dir 'managed-object))


(defmethodex get-all-objects-type (dir hash-director) (tp)
  ;; (defmethod get-all-objects-type ((dir hash-director) tp)
  (let ((objs '()))
    (maphash #'(lambda (k v) 
		 (if (typep v tp)
		     (push v objs))) 
	     (slot-value dir 'key-to-mo))
    objs))

(defmethodex get-all-objects-owned-by (dir hash-director) ((o key))
  ;; (defmethod get-all-objects-owned-by ((dir hash-director) (o key))
  (let ((objs '()))
    (maphash #'(lambda (key v) 
		 (if (equal (k (:ownr v)) (k o))
		     (push v objs)))
	     (slot-value dir 'key-to-mo))
    objs))



;; There does not appear to be a "hash-reduce".
;; That would be an elegant function to have for
;; this and other purposes.
(defmethodex get-unused-key-value (dir hash-director) ()
  (get-unused-key-value-naked dir))

(defmethod get-unused-key-value-naked ((dir hash-director))
  (the integer 
    (+ 1
       (hash-keys-reduce #'max (slot-value dir 'key-to-mo)
			 ;; This is a consequence of me not being able to use the 
			 ;; "key" class as the index, because I can't get the equality test
			 ;; to work ...
			 -1))))

;; This is a very limited version of reduce, since it
;; does not support the keywords that "reduce" supports.
;; However it may suffice for our purposes....
;; The function that we reduce with must support taking
;; a single argument, as 'max does, for instance.
(defun hash-values-reduce (fun ht &optional init)
  (let ((r init))
    (with-hash-table-iterator (v ht)
      (loop
       (multiple-value-bind (more? key value) (v)
	 (unless more? (return nil))
	 (setf r (funcall fun r value)))))
    r))

(defun hash-keys-reduce (fun ht &optional init)
  (let ((r init))
    (with-hash-table-iterator (v ht)
      (loop
       (multiple-value-bind (more? key value) (v)
	 (unless more? (return nil))
	 (setf r (funcall fun r key)))))
    r))


(defmethodex register-obj (dir hash-director) ((mo managed-object))
    (progn
    (unless (mid mo)
      (setf (mid mo) (make-instance 'key :id (get-unused-key-value-naked dir))))
  (with-slots (key-to-mo) dir
    (setf (gethash (k (mid mo)) key-to-mo) mo))))


;; (defmethodex lookup-obj-key (dir hash-director) ((id key))
(defmethod lookup-obj-key ((dir hash-director) (id key))
  (with-slots (key-to-mo) dir
    (gethash (k id) key-to-mo)))

;; I kind of which LISP make interface-based programming easier.
;; I would really like to insist on create-read-update-delete functions
;; for the abstract class of director.

(defmethodex delete-obj (dir hash-director) ((id key))
  ;; (defmethod delete-obj ((dir hash-director) (id key))
  (with-slots (key-to-mo) dir
    (remhash (k id) key-to-mo)))


(defclass hash-dir-test ()
  ())

(defparameter *ELEPHANT-CAT* "elephant director")

;; To avoid running out of locks, I'm going to have to produce some
;; sort of store controller manager that can close these things.
;; These functions will have to be expanded later to include 
;; multiple controllers.  It would be really nice if I could tie
;; this to garabase collection instead.
;; (defvar *basic-store-controller* (open-store *DCM-DEFAULT*))
(defvar *basic-store-controller* nil)


(defun reconnect-db ()
  (reconnect-controller *basic-store-controller*))

(defun init-elephant-controllers (dcm-default)
  (setq *basic-store-controller* (open-store dcm-default))
  (setq elephant::*store-controller* *basic-store-controller*))

(defun release-elephant-controllers ()
  (close-controller *basic-store-controller*))

(defclass elephant-director (director)
  ;; Is this a path or a string?
  ((rootname 
    :type 'string
    :initform *dcm-default*)
   (storename :initarg :initstorename
	      :initform *DEF-STORE-NAME*
	      :accessor :storename)
   ;; I think I should actually put a btree here.
   ;; We might have to have some kind of constructor for it...
   (root
    :type 'store-controller
    :initform *basic-store-controller*
    )
   (dcm-btree 
    :type btree 
    :initform nil
    ))
  )

(defmethod initialize-btree ((dir elephant-director) c)
  (let* ((name (format nil "DCM-SPECIAL-~A" c))
	 (sc (slot-value dir 'root))
	 (bt (get-from-root name :sc sc)))
    (format t "bt of name ~A is: ~A~%" name bt)
    (unless bt
      (setf bt (add-to-root name (make-btree sc) :sc sc)))
    (setf (slot-value dir 'dcm-btree) bt))
  dir
  )

;; I really need to implement this as a way to recover from 
;; an unrecoverable class change (such as removing a slot!)
;; Better yet would be to make the deserializer more robust,
;; but having this work directly against the database (
;; and in fact, pushing this into Elephant), would be an 
;; excellent idea.
;; (defun empty-out-corrupted-btree (c sc)
;;     (let* ((name (format nil "DCM-SPECIAL-~A" (class-name c)))
;;   	 (bt (get-from-root name :sc sc)))
;;       "delete from keyvalue where clct_id = ")
;;     )

(defmethodex register-many-random (dir director) (n)
  ;; (defmethod register-many-random ((dir director) n)
  (with-slots
	(mtype)
      dir
    (do ((i 0 (+ i 1)))
	((>= i n) 'done)
      (register-obj dir
		    (randomize-managed-object
		     (make-instance mtype))))))

;; I'm goint to try using the ele::next-oid fuction here:
(defmethodex get-unused-key-value (dir elephant-director) ()
  (get-unused-key-value-naked dir))

(defmethod get-unused-key-value-naked ((dir elephant-director))
  (the integer 
    (with-slots (root) dir
      (ELEPHANT::next-oid root))))

;; (defmethodex get-all-objects (dir elephant-director) ()
(defmethod get-all-objects ((dir elephant-director))
  (get-all-objects-type dir 'managed-object))

(defmethodex get-all-objects-type (dir elephant-director) (tp)
  ;; (defmethod get-all-objects-type ((dir elephant-director) tp)
  (with-slots (dcm-btree) dir
    (let ((objs '()))
      (map-btree #'(lambda (k x) 
		     (declare (ignore k))
		     (if (typep x (:mtype dir))
			 (push x objs)))
		 dcm-btree)
      objs)))

(defmethod get-all-objects-type-xxxx ((dir elephant-director) tp)
  (with-slots (dcm-btree) dir
    (let ((objs '()))
      (map-btree #'(lambda (k x) 
		     (declare (ignore k))
		     (if (typep x (:mtype dir))
			 (push x objs)))
		 dcm-btree)
      objs)))

(defmethodex get-all-objects-owned-by (dir elephant-director) ((o key))
  ;; (defmethod get-all-objects-owned-by ((dir elephant-director) (o key))
  (with-slots (dcm-btree) dir
    (let ((objs '()))
      (map-btree #'(lambda (k x) 
		     (declare (ignore k))
		     (if (equal (k (:ownr x)) (k o))
			 (push x objs)))
		 dcm-btree)
      objs)))



(defclass elephant-dir-test ()
  ((ed :initform (let ((x
			(make-instance 
			 'elephant-director 
			 :initstorename 
			 *DEF-STORE-NAME*)))
		   ;;		    (init-ed x)
		   x)
       :accessor :elefdir)))

(defmethodex register-obj (dir elephant-director) ((mo managed-object))
  ;; (defmethod register-obj ((dir elephant-director) (mo managed-object))
  (progn
  (unless (mid mo)
    (setf (mid mo) (make-instance 'key :id (get-unused-key-value-naked dir))))
  ;;  (sb-thread:with-mutex ((insure-mutex (format nil "mutex-~A" dir)))
  (with-slots (dcm-btree) dir
    (progn
      (setf (get-value (mid mo) dcm-btree) mo)))))

(defmethodex lookup-obj-key (dir elephant-director) ((id key))
  ;; (defmethod lookup-obj-key ((dir elephant-director) (id key))
  (with-slots (dcm-btree) dir
    (get-value id dcm-btree)))

(defmethodex delete-obj (dir elephant-director) ((id key))
  ;; (defmethod delete-obj ((dir elephant-director) (id key))
  (with-slots (dcm-btree) dir
    (remove-kv id dcm-btree)))

;; Now I am going to attempt to define a class
;; which is a combination of the hash and elefant class,
;; I don't know a better way to do this than delegation....
(defclass hash-ele-director (director)
  ;; I don't think I actually need any new slots here...
  ((hd :initform (make-instance 'hash-director) 
       :accessor :hd)
   (ed :initform (make-instance 'elephant-director) 
       :accessor :ed)
   ))

(defmethod initialize-btree ((dir hash-ele-director) c)
  (initialize-btree (:ed dir) c)
  )

;; I don't think anything new has to be done here....
(defclass hash-ele-dir-test ()
  ((hed :initform (make-instance 'hash-ele-director)
	:accessor :hed)))

(defmethodex load-all (dir hash-ele-director) ()
    (let ((obs (get-all-objects (:ed dir))))
      (mapc #'(lambda (x) (register-obj (:hd dir) x))
	    obs)
      ))

(defmethod register-obj ((dir hash-ele-director) (mo managed-object))
  (register-obj (:ed dir) mo)
  (register-obj (:hd dir) mo)
  )

(defmethod get-unused-key-value ((dir hash-ele-director))
  (the integer 
    (get-unused-key-value (:ed dir))))


(defmethod lookup-obj-key ((dir hash-ele-director) (id key))
  (lookup-obj-key (:hd dir) id))

(defmethod delete-obj ((dir hash-ele-director) (id key))
  (delete-obj (:hd dir) id)
  (delete-obj (:ed dir) id))


(defmethod get-all-objects ((dir hash-ele-director))
  (get-all-objects-type dir 'managed-object))

(defmethod get-all-objects-type ((dir hash-ele-director) tp)
  (get-all-objects-type (:hd dir) tp))

(defmethod get-all-objects-owned-by ((dir hash-ele-director) o)
  (get-all-objects-owned-by (:hd dir) o))

;; This is a "factory" that produces directors.
;; Strategy is a fixed type so that I can 

(defparameter *DIR-STRATEGIES* '(hash hash-ele elephant simple))

;; I might have to rehabilitate this function...
(defun directory-factory (strategy btreeclassname type repos)
  (case strategy
    (hash (init-director 'hash-director btreeclassname :managed-type type))
    (hash-ele (init-director 'hash-ele-director btreeclassname :managed-type type))
    (elephant (init-director-noload 'elephant-director btreeclassname :managed-type type))
    (simple (init-director 'GenDir btreeclassname :managed-type type))
    )
  )



;; This is a test class for directors in general;
;; The goal of this is to masically iterate over all known 
;; strategies and test every strategy with the same code.
(defclass dir-test ()
  ((dirs :initform (mapcar 
		    #'(lambda (strategy) (directory-factory strategy '() '() '()))
		    *DIR-STRATEGIES*)
	 :accessor :dirs)))

(defmethod test-get-unused-key-value ((dir director))
  (let ((n 10)
	(key-values '()))
    (dotimes (x n)
      (let ((mo (make-instance 'managed-object)))
	(setf (mid mo) (make-instance 'key :id (get-unused-key-value dir)))
	(register-obj dir mo)
	(let ((key (k (mid mo))))
	  (if (member key key-values)
	      (return nil)
	      (push (k (mid mo)) key-values)))))
    t))
;; The basic form here is to map the test function over all the dirs...
(defmethod unused-key ((ob dir-test))
  (with-slots (dirs) ob
    (mapc #'(lambda (dir)
	      (time 
	       (assert (not (null
			     (test-get-unused-key-value dir))))))
	  dirs))
  )

(defmethod tm-register-then-lookup ((ob dir-test))
  (with-slots (dirs) ob
    (mapc #'(lambda (dir)
	      (format t "director ~A" dir)
	      (time 
	       (let ((mo (make-instance 'managed-object)))
		 (register-obj dir mo)
		 (assert (key-equal (mid mo)
				    (mid (lookup-obj-key dir (mid mo))))))))
	  dirs))
  )

;; One of th things that I don't like about clos-unit is that since it 
;; executes the test at the time of declaration, it is very hard to 
;; make sure that everthing is in place.  For example, since we are
;; using Elephant, we really shouldn't execute those tests untill all of the 
;; the classes that are currently in the database are loaded.
;; I may change that, and also add some of my own macros.
(defmethod tm-get-all-objects ((ob dir-test))
  (with-slots (dirs) ob
    (mapc #'(lambda (dir)
	      (format t "director ~A" dir)
	      (time 
	       (let ((n 100)
		     (key-values '()))
		 (dotimes (x n)
		   (let ((mo (make-instance 'managed-object)))
		     (setf (mid mo) (make-instance 'key :id (get-unused-key-value dir)))
		     (register-obj dir mo)
		     (let ((key (k (mid mo))))
		       (if (member key key-values)
			   (return nil)
			   (push (k (mid mo)) key-values)))))
		 (format t "lookup via  ~A :" dir)
		 (time
		  (let ((objs (get-all-objects dir)))
		    (mapcar 
		     #'(lambda (mo) 
			 (member (k (mid mo)) key-values))
		     objs)))
		 ;; Now delete the ojbects, so thtat we leave things in a clean state.
		 (time
		  (mapcar 
		   #'(lambda (k) 
		       (delete-obj dir (make-instance 'key :id k)))
		   key-values))
		 ;; This test that the deletion worked...
		 (time
		  (and (mapcar 
			#'(lambda (k) 
			    (not (lookup-obj-key dir (make-instance 'key :id k))))
			key-values)))))
	      )
	  dirs))
  )

;; This is really designed to work though a problem of lock allocation
;; in elephant
(defmethod tm-test-elephant ((ob dir-test))
  (with-slots (dirs) ob
    (mapc #'(lambda (dir)
	      (progn
		(format t "director ~A" (class-of dir))
		(if (equal (class-name (class-of dir)) 'elephant-director)
		    (progn
		      (format t "director ~A" dir)
		      (time 
		       (let ((n 100)
			     (key-values '()))
			 (dotimes (x n)
			   (let ((mo (make-instance 'managed-object)))
			     (setf (mid mo) (make-instance 'key :id (get-unused-key-value dir)))
			     (register-obj dir mo)
			     (let ((key (k (mid mo))))
			       (if (member key key-values)
				   (return nil)
				   (push (k (mid mo)) key-values)))))
			 (format t "lookup via  ~A :" dir)
			 (time
			  (let ((objs (get-all-objects dir)))
			    (mapcar 
			     #'(lambda (mo) 
				 (member (k (mid mo)) key-values))
			     objs)))
			 ;; Now delete the ojbects, so thtat we leave things in a clean state.
			 (time
			  (mapcar 
			   #'(lambda (k) 
			       (delete-obj dir (make-instance 'key :id k)))
			   key-values))
			 ;; This test that the deletion worked...
			 (time
			  (and (mapcar 
				#'(lambda (k) 
				    (not (lookup-obj-key dir (make-instance 'key :id k))))
				key-values)))))
		      ))))
	  dirs))
  )


;; This stuff is not likely to work if 
;; if we have different directors of the same class....
(defvar *director-class-map* (make-hash-table))

(defmethod initialize ((dir elephant-director) (cname symbol) btreeclassname)
  (initialize-btree dir btreeclassname))

(defmethod initialize ((dir hash-ele-director) (cname symbol) btreeclassname)
  (setf (:mtype (:hd dir)) (:mtype dir))
  (setf (:mtype (:ed dir)) (:mtype dir))
  (initialize-btree (:ed dir) btreeclassname))

(defmethod initialize ((dir director) (cname symbol) btreeclassname)
  )

;; (eval-when (:compile-toplevel)
;;   (load "dcm-macros.lisp")
;;   )
;; In the general case, there is nothing to do.
(defmethod load-all ((dir director))
  )

(defun get-director-by-class (cls)
  (gethash cls *director-class-map* )
  )




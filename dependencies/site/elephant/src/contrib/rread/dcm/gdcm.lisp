;; gdcm.lisp -- This file implements generational data collection management
;; based on the basic data collection management functionality.
;; The basic idea is that every object in the collection exists 
;; within a generation.  Each generation can have a different storage
;; strategy --- in general, the lower the generation number, the 
;; faster and smaller the storage strategy.  
;; Increasing the generation is a fundamental operation.

;; One fundamental feature of a GenDir is that the 
;; objects managed retain their identities across these issue.

;; A GenDir is a kind of director, but it offers generational
;; aware operations that it's superclass does not.

;; Given an object with it's id, how do you efficiently find
;; its generation?  --- you always have an index, so in theory
;; it can't take that long to find what generation it's in.

(in-package "DCM")

;; (eval-when (:compile-toplevel)
;;   (load "dcm-macros.lisp")
;;   )


(defclass GenDir (director)
  ((strategy 
    :initform '((0 . hash-ele) (1 . elephant)) ;; This means that 0 and anything less is hash-ele
    :accessor strategy)
   (final-strategy :initform 'elephant
		   :accessor final-strategy)
   (subdirs
    :initform nil
    :accessor subdirs)
   )
  (:documentation "This is an example documentation string.")
)

;; retire an object into a higher generation, probably changing 
;; it's storage strategy.
(defgeneric retire (GenDir  key)
  (:documentation 
   "Increment the generation number of a object, making number is properly stored there.")  
)

(defgeneric promote (GenDir  key)
  (:documentation 
   "Decrement the generation number of a object, making number is properly stored there.")  
)

(defgeneric find-generation (GenDir key)
  
)


(defmethod retire ((gdcm GenDir) (mid key))
  (multiple-value-bind (obj gen)
      (lookup-obj-aux gdcm mid)
    (unless (= gen (- (length (subdirs gdcm)) 1))
      (let ((ndir (nth (+ 1 gen) (subdirs gdcm)))
	    (odir (nth gen (subdirs gdcm)))
	    )
	(register-obj ndir obj)
	(delete-obj odir mid)
	)
      )
    )
  )

(defmethod promote ((gdcm GenDir) (mid key))
  (multiple-value-bind (obj gen)
      (lookup-obj-aux gdcm mid)
    (unless (= gen 0)
      (let ((ndir (nth (- gen 1) (subdirs gdcm)))
	    (odir (nth gen (subdirs gdcm)))
	    )
	(register-obj ndir obj)
	(delete-obj odir mid)
	)
      )
    )
  )

(defmethod load-all ((dir GenDir))
    (do ((i 0 (1+ i))
	 (dirs (subdirs dir) (rest dirs)))
	((null dirs))
      (if (or (typep (car dirs) 'hash-director)
	      (typep (car dirs) 'hash-ele-director))
	  (load-all (car dirs)))
))

(defmethod get-all-objects ((dir GenDir))
  (let ((ret nil))
    (dolist (x (subdirs dir))
      (setf ret (append (get-all-objects x) ret)))
  ret))

(defmethod get-all-objects-type ((dir GenDir) tp)
  (let ((ret nil))
    (dolist (x (subdirs dir))
      (setf ret (append (get-all-objects-type x tp) ret)))
  ret))


(defmethod get-all-objects-owned-by ((dir GenDir) (o key))
  (let ((ret nil))
    (dolist (x (subdirs dir))
      (setf ret (append (get-all-objects-owned-by x o) ret)))
  ret))


(defmethod get-all-cur-objects ((dir GenDir))
   (get-all-objects (car (subdirs dir))))

(defmethod get-all-objects-gen ((dir GenDir) (gen integer))
    (get-all-objects (nth gen (subdirs dir))))

(defmethod get-unused-key-value ((dir GenDir))
  (the integer 
    (get-unused-key-value (car (subdirs dir)))))


(defmethod delete-obj ((dir GenDir) (id key))
  (multiple-value-bind (obj gen)
      (lookup-obj-aux dir id)
    (if obj
	(delete-obj (nth gen (subdirs dir)) id))
    )
)
(defmethod lookup-obj-key ((dir GenDir) (mid key))
  (multiple-value-bind (obj gen)
      (lookup-obj-aux dir mid)
    obj)
)

(defmethod lookup-obj-gen ((dir GenDir) (id key) (gen integer))
  (lookup-obj (nth gen (subdirs dir)) id)
)

(defmethod lookup-obj-aux ((dir GenDir) (mid key))
  (let ((ret nil))
    (do ((i 0 (1+ i))
	 (dirs (subdirs dir) (rest dirs)))
	((or (null dirs)
	     (setf ret (lookup-obj (car dirs) mid)))
      (values ret i))
    )))

(defmethod find-generation ((gdcm GenDir) (mid key))
  ;; This is going to be a lot like lookup-obj....
  (multiple-value-bind (obj gen)
      (lookup-obj-aux gdcm mid)
    gen)
)

;; We can put things in the first director as a basic approach,
;; and wait for them to be retired.
(defmethod register-obj ((dir GenDir) (mo managed-object))
  (if (mid mo)
      (multiple-value-bind (obj gen)
	  (lookup-obj-aux dir (mid mo))
	(register-obj (nth (if obj gen 0) (subdirs dir)) mo))
      (register-obj (nth 0 (subdirs dir)) mo))
  )

;; Based on the strategy slot,
;; we will construct the list of directors that will be 
;; responsible for things...
(defmethod initialize ((gdcm GenDir) (cname symbol) (btreeclassname symbol))
  ;; Create a list of directors...
  (let ((firstdirs nil)
	(gen-num 0))
    (dolist (x (strategy gdcm))
      (progn
	(setf firstdirs (push 
	 (directory-factory 
	  (cdr x)
	  (format nil "GEN-~A-~A" btreeclassname gen-num) (:mtype gdcm) nil)
	 firstdirs))
	(incf gen-num)
	))
    (format t "~a~%" firstdirs)
    (setf (subdirs gdcm)
	  (reverse 
	   (push
	    (directory-factory 
	     (final-strategy gdcm) (format nil "GEN-~A-~A" btreeclassname gen-num) (:mtype gdcm) nil)
	    firstdirs)))
    gdcm
    ))

(defmethod print-stats ((dir GenDir))
  (do ((i 0 (1+ i))
       (dirs (subdirs dir) (rest dirs)))
      ((null dirs))
    (if (typep (car dirs) 'elephant-director)
	(format t "dcm-btree = ~A~%" (slot-value (car dirs) 'dcm-btree)))
    (format t "Dir: ~A Number of objects: ~A~%"
	    i
	    (length (get-all-objects (car dirs)))
	    )
    ))





;;; testmigration.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2005,2006 by Robert L. Read
;;; <rread@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :ele-tests)

(deftest remove-element
    (if (or (not (boundp '*test-spec-secondary*))
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring")
	  t)
	(let ((a (vector 'a 'b 'c))
	      (ans (vector 'a 'c)))
	  (setf a (ele::remove-indexed-element-and-adjust 1 a))
	  (and (equal (aref a 0) (aref ans 0))
	       (equal (aref a 1) (aref ans 1))
	       (equal (length a) (length ans)))))
  t)

;; Simple root element copy
(deftest migrate-basic
    (if (or (not (boundp '*test-spec-secondary*) )
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring")
	  t)
    (let* (*store-controller*
	   (sc1 (open-store *test-spec-primary* :recover t :deadlock-detect t))
	   (sc2 (open-store *test-spec-secondary* :recover t :deadlock-detect t)))
      (unwind-protect 
	   (progn
	     (mapcar (lambda (x) 
		       (disable-class-indexing x :sc sc1))
		     '(idx-two idx-three idx-four idx-five idx-six idx-seven idx-eight
		       idx-five-del stress-index idx-unbound-del))
	     (add-to-root "x" "y" :sc sc1)
	     (migrate sc2 sc1)
	     (equal (get-from-root "x" :sc sc1)
		    (get-from-root "x" :sc sc2)))
	(close-store sc1)
	(close-store sc2))))
  t)

;; Simple test of a btree
(deftest migrate-btree
    (if (or (not (boundp '*test-spec-secondary*) )
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring")
	  nil)
	(let ((*store-controller* nil)
	      (sc1 (open-store *test-spec-primary* :recover t))
	      (sc2 (open-store *test-spec-secondary* :recover t)))
	  (declare (special *store-controller*))
	  (unwind-protect 
	       (progn
		 (elephant::initialize-migrate-duplicate-detection)
		 (let ((ibt (make-btree sc1)))
		   (with-transaction (:store-controller sc1)
		     (loop for i from 0 to 10
			do
			  (setf (get-value i ibt) (* i i))))
		   (let ((mig (migrate sc2 ibt)))
		     (btree-differ-p ibt mig))))
	    (elephant::clear-migrate-duplicate-detection)
	    (close-store sc1)
	    (close-store sc2))))
  nil)

;; Simple test of indexed btrees
(deftest migrate-idx-btree
    (if (or (not (boundp '*test-spec-secondary*) )
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring")
	  t)
	(let ((old-store *store-controller*)
	      (*store-controller* nil)
	      (rv nil)
	      (sc1 (open-store *test-spec-primary* :recover t))
	      (sc2 (open-store *test-spec-secondary* :recover t)))
	  (unwind-protect 
	  (let* ((ibt (make-indexed-btree sc1)))
	    (elephant::initialize-migrate-duplicate-detection)
	    (let ((index
		   (add-index ibt :index-name 'crunch :key-form 'crunch
			      :populate t)))
	      (with-transaction (:store-controller sc1)
		(loop for i from 0 to 10
		   do
		   (setf (get-value i ibt) (* i i))))
	      (let* ((mig (migrate sc2 ibt))
		     (nindex (get-index ibt 'crunch)))
		(loop for i from 0 to 10
		   do
		   (if (not 
			(equal
			 (get-value i index)
			 (get-value i nindex)
			 ))
		       (progn
			 (format t "YIKES ~A ~%" i)
			 )))
		(not (btree-differ-p ibt mig)))))
	  (progn
	    (elephant::clear-migrate-duplicate-detection)
	    (setq *store-controller* old-store)
	    (close-store sc1)
	    (close-store sc2)))))
  t)

;; Simple test of persistent classes
(deftest migrate-pclass
    (if (or (not (boundp '*test-spec-secondary*) )
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring")
	  t)
	(let ((*store-controller* nil)
	      (sc1 (open-store *test-spec-primary* :recover t))
	      (sc2 (open-store *test-spec-secondary* :recover t)))
	  (declare (special *store-controller*))
	  (unwind-protect
	       (progn
		 (elephant::initialize-migrate-duplicate-detection)
		 ;; Make instances
		 (let* ((f1 (with-transaction (:store-controller sc1)
			      (make-instance 'pfoo  :sc sc1)))
			(f2 (with-transaction (:store-controller sc1)
			      (make-instance 'pfoo :slot1 "this is a string"  :sc sc1)))
			(b1 (with-transaction (:store-controller sc1)
			      (make-instance 'pbar :slot2 "another string"  :sc sc1)))
			)
		   (let ((fm1 (migrate sc2 f1))
			 (fm2 (migrate sc2 f2))
			 (bm1 (migrate sc2 b1)))
		     (and 
		      (and (not (slot-boundp fm1 'slot1))
			   (not (slot-boundp f1 'slot1)))
		      (equal (slot1 fm2) (slot1 f2))
		      (equal (slot2 bm1) (slot2 b1)))
		     )))
	    (elephant::clear-migrate-duplicate-detection)
	    (close-store sc1)
	    (close-store sc2))))
  t)

(defclass migrate-simple-class ()
  ((slot1 :accessor slot1 :initarg :slot1)
   (slot2 :accessor slot2 :initarg :slot2)))

(defstruct simple-struct s1 s2)

(deftest migrate-mult-pclass 
    (if (or (not (boundp '*test-spec-secondary*) )
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring")
	  (values t t t t t t))
    (progn
      (let* ((sc1 (open-store *test-spec-primary* :recover t :deadlock-detect t))
	     (sc2 (open-store *test-spec-secondary* :recover t :deadlock-detect t))
	     (*store-controller* nil))
	(declare (special *store-controller*))
	(unwind-protect
	     (progn (elephant::initialize-migrate-duplicate-detection)
		    (let* ((simplesrc (make-instance 'pfoo :slot1 0 :sc sc1))
			   (i1 (make-instance 'pfoo :slot1 1 :sc sc1))
			   (i2 (make-instance 'pfoo :slot1 2 :sc sc1))
			   (i3 (make-instance 'pfoo :slot1 3 :sc sc1))
			   (i4 (make-instance 'pfoo :slot1 4 :sc sc1))
			   (i5 (make-instance 'pfoo :slot1 5 :sc sc1))
			   (list (list i1 i1))
			   (array (make-array '(2 2) :initial-contents `((,i2 1)
									 (,i2 2))))
			   (hash (make-hash-table))
			   (object (make-instance 'migrate-simple-class :slot1 i4 :slot2 i4))
			   (struct (make-simple-struct :s1 i5 :s2 i5)))
		      (setf (gethash 1 hash) i3)
		      (setf (gethash 2 hash) i3)
		      (let* ((newsimple (migrate sc2 simplesrc))
			     (newlist (migrate sc2 list))
			     (newarray (migrate sc2 array))
			     (newhash (migrate sc2 hash))
			     (newobject (migrate sc2 object))
			     (newstruct (migrate sc2 struct)))
			(values (and (and (slot-boundp newsimple 'slot1)
					  (eq (slot1 newsimple) 0)))
				(and (not (eq i1 (first newlist)))
				     (eq (first newlist) (second newlist))
				     (and (slot-boundp (first newlist) 'slot1)
					  (eq (slot1 (first newlist)) 1)))
				(and (not (eq i2 (aref newarray 0 0)))
				     (eq (aref newarray 0 0) (aref newarray 1 0))
				     (and (slot-boundp (aref newarray 0 0) 'slot1)
					  (eq (slot1 (aref newarray 0 0)) 2)))
				(and (not (eq i3 (gethash 1 newhash)))
				     (eq (gethash 1 newhash) (gethash 2 newhash))
				     (and (slot-boundp (gethash 1 newhash) 'slot1)
					  (eq (slot1 (gethash 1 newhash)) 3)))
				(and (not (eq i4 (slot1 newobject)))
				     (eq (slot1 newobject) (slot2 newobject))
				     (and (slot-boundp (slot1 newobject) 'slot1)
					  (eq (slot1 (slot1 newobject)) 4)))
				(and (not (eq i5 (simple-struct-s1 newstruct)))
				     (eq (simple-struct-s1 newstruct) 
					 (simple-struct-s2 newstruct))
				     (and (slot-boundp (simple-struct-s1 newstruct) 'slot1)
					  (eq (slot1 (simple-struct-s1 newstruct)) 5)))))))
	  (close-store sc1)
	  (close-store sc2)
	  (elephant::clear-migrate-duplicate-detection)))))
  t t t t t t)

(defpclass ipfoo ()
  ((slot1 :accessor slot1 :initarg :slot1 :index t)))

;; Simple test of persistent classes with indexed slots
(deftest migrate-ipclass
    (if (or (not (boundp '*test-spec-secondary*))
	    (null *test-spec-secondary*))
	(progn
	  (format t "~%Single store mode: ignoring ")
	  (values 3 1 1 1 1 10 20 ))
	(progn
;;	  (format t "Opening store~%")
	  (let ((sc2 (open-store *test-spec-secondary* :recover t))
		(sc1 (open-store *test-spec-primary* :recover t))
		(*store-controller* nil))
	    (declare (special *store-controller*))
	    (unwind-protect
		 ;; ensure class index is initialized in sc1
		 (progn
		   (with-transaction (:store-controller sc2)
		     (remove-kv 'ipfoo (elephant::controller-class-root sc2)))
		   (setf (elephant::%index-cache (find-class 'ipfoo)) nil)
		   (find-class-index 'ipfoo :sc sc1)
;;		   (format t "Making objects~%")
;;		   (with-transaction (:store-controller sc2)
;;		     (drop-instances (get-instances-by-class 'ipfoo) :sc sc2))
		   (with-transaction (:store-controller sc1 :retries 2)
		     (drop-instances (get-instances-by-class 'ipfoo) :sc sc1)
		     (make-instance 'ipfoo :slot1 1 :sc sc1)
		     (make-instance 'ipfoo :slot1 10 :sc sc1)
		     (make-instance 'ipfoo :slot1 20 :sc sc1))
;;		   (format t "Migrating~%")
		   (migrate sc2 sc1)
		   ;; Make sure our ipfoo class now points at a cache in sc2!
		   (assert (equal (elephant::controller-spec sc2)
				  (elephant::dbcn-spc-pst (elephant::%index-cache (find-class 'ipfoo)))))
;;		   (format t "Fetching~%")
		   (let ((fm1 (get-instances-by-value 'ipfoo 'slot1 1))
			 (fm2 (get-instances-by-value 'ipfoo 'slot1 10))
			 (fm3 (get-instances-by-value 'ipfoo 'slot1 20))
			 (all (get-instances-by-class 'ipfoo)))
;;		     (format t "Clear & return~%")
;;		     (let ((insts (get-instances-by-class 'ipfoo)))
;;		       (with-transaction (:store-controller sc2)
;;			 (format t "Dropping instances~%")
;;			 (drop-instances insts :sc sc2)))
		     (values 
			  (length all)
			  (length fm1) 
			  (length fm2) 
			  (length fm3)
			  (slot1 (car fm1))
			  (slot1 (car fm2))
			  (slot1 (car fm3)))))
	      (close-store sc1)
	      (close-store sc2)))))
  3 1 1 1 1 10 20 )


;;; testserializer.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :ele-tests)

(defun in-out-value (var)
  (with-buffer-streams (out-buf)
    (deserialize (serialize var out-buf *store-controller*) *store-controller*)))

(defun in-out-eq (var)
  (with-buffer-streams (out-buf)
    (eq var (deserialize (serialize var out-buf *store-controller*) *store-controller*))))

(defun in-out-equal (var)
  (with-buffer-streams (out-buf)
    (equal var (deserialize (serialize var out-buf *store-controller*) *store-controller*))))

(defun in-out-equalp (var)
  (with-buffer-streams (out-buf)
    (equalp var (deserialize (serialize var out-buf *store-controller*) *store-controller*))))

(deftest fixnums
    (are-not-null
     (in-out-equal 0)
     (in-out-equal -1)
     (in-out-equal 1)
     (in-out-equal most-positive-fixnum)
     (in-out-equal most-negative-fixnum))
  t t t t t)

(deftest fixnum-type-1
    (are-not-null
     (typep (in-out-value 0) 'fixnum)
     (typep (in-out-value 1) 'fixnum)
     (typep (in-out-value -1) 'fixnum)
     (typep (in-out-value most-positive-fixnum) 'fixnum)
     (typep (in-out-value most-negative-fixnum) 'fixnum))
  t t t t t)

;;
;; Validate 32/64 bit memutils operation (white box test)
;;
	
(deftest read-32-bit-fixnum 
    (progn
      (with-buffer-streams (bs)
	(if (not (elephant-memutil::little-endian-p))
	    (elephant-memutil::buffer-write-byte 1 bs))
	(loop for i from 1 upto 3 do
	     (elephant-memutil::buffer-write-byte 0 bs))
	(if (elephant-memutil::little-endian-p)
	    (elephant-memutil::buffer-write-byte 1 bs))
	(elephant-memutil::buffer-read-fixnum32 bs)))
  #x1000000)

(deftest read-64-bit-fixnum
    (progn
      (with-buffer-streams (bs)
	(if (not (elephant-memutil::little-endian-p))
	    (elephant-memutil::buffer-write-byte 1 bs))
	(loop for i from 1 upto 7 do
	     (elephant-memutil::buffer-write-byte 0 bs))
	(if (elephant-memutil::little-endian-p)
	    (elephant-memutil::buffer-write-byte 1 bs))
	(elephant-memutil::buffer-read-fixnum64 bs)))
  #x100000000000000)

;;
;; Use serializer to write fixnum
;; Verify bytes and length of output
;; 

(deftest write-32-bit-fixnum
    (progn
      (with-buffer-streams (bs)
	(serialize #x01000000 bs *store-controller*)
	(elephant-memutil::buffer-read-byte bs) ;; skip tag
	(and (= (elephant-memutil::buffer-stream-size bs) 5)
	     (if (elephant-memutil::little-endian-p)
		 (= (progn (loop for i from 1 upto 3 do
				(elephant-memutil::buffer-read-byte bs))
			   (elephant-memutil::buffer-read-byte bs))
		    1)
	         (= (elephant-memutil::buffer-read-byte bs) 
		    1)))))
  t)

(deftest write-64-bit-fixnum
    (progn
      (with-buffer-streams (bs)
	(serialize #x0100000000000000 bs *store-controller*)
	(elephant-memutil::buffer-read-byte bs) ;; skip tag
	(if (< most-positive-fixnum elephant-memutil::+2^32+)
	    t
	    (and (= (elephant-memutil::buffer-stream-size bs) 9)
		 (if (elephant-memutil::little-endian-p)
		     (= (progn (loop for i from 1 upto 7 do
				    (elephant-memutil::buffer-read-byte bs))
			       (elephant-memutil::buffer-read-byte bs))
			1)
		     (= (elephant-memutil::buffer-read-byte bs)
			1))))))
  t)

(deftest bignums
    (are-not-null
     (in-out-equal  (+ most-positive-fixnum 100))
     (in-out-equal  (- most-negative-fixnum 100))
     (loop for i from 0 to 2000
	   always (in-out-equal (expt 2 i)))
     (loop for i from 0 to 2000
	   always (in-out-equal (- (expt 2 i))))
     (loop for i from 0 to 2000
	   always (in-out-equal (- (expt 2 i) 1)))
     (loop for i from 0 to 2000
	   always (in-out-equal (- 1 (expt 2 i))))
     (loop for i from 0 to 2000
	   always (in-out-equal (expt 3 i)))
     (loop for i from 0 to 2000
	   always (in-out-equal (- (expt 3 i)))))
  t t t t t t t t)

(deftest floats
    (are-not-null
     (in-out-equal 0.0)
     (in-out-equal -0.0)
     (in-out-equal 0.0d0)
     (in-out-equal -0.0d0)
     (in-out-equal -0.0d0)
     (in-out-equal double-float-epsilon)
     (in-out-equal long-float-epsilon)
     (in-out-equal short-float-epsilon)
     (in-out-equal single-float-epsilon)
     (in-out-equal double-float-negative-epsilon)
     (in-out-equal long-float-negative-epsilon)
     (in-out-equal short-float-negative-epsilon)
     (in-out-equal single-float-negative-epsilon)
     (in-out-equal least-negative-double-float)
     (in-out-equal least-negative-long-float)
     (in-out-equal least-negative-short-float)
     (in-out-equal least-negative-single-float)
     (in-out-equal least-positive-double-float)
     (in-out-equal least-positive-long-float)
     (in-out-equal least-positive-short-float)
     (in-out-equal least-positive-single-float)
     (in-out-equal most-negative-double-float)
     (in-out-equal most-negative-long-float)
     (in-out-equal most-negative-short-float)
     (in-out-equal most-negative-single-float)
     (in-out-equal most-positive-double-float)
     (in-out-equal most-positive-long-float)
     (in-out-equal most-positive-short-float)
     (in-out-equal most-positive-single-float))
  t t t t t t t t t t t t t t t t t t t t t t t t t t t t t)

(deftest rationals
    (are-not-null
     (in-out-equal 1/2)
     (in-out-equal -1/2)
     (in-out-equal (/ 1 most-positive-fixnum))
     (in-out-equal (/ 1 most-negative-fixnum))
     (in-out-equal (/ most-positive-fixnum most-negative-fixnum))
     (in-out-equal (/ (expt 2 200) (expt 3 300)))
     (in-out-equal (/ (expt 2 200) (- (expt 3 300)))))
  t t t t t t t)

(deftest complexes
   (are-not-null
    (in-out-equal (sqrt -1))
    (in-out-equal (complex 1))
    (in-out-equal (complex 1.0))
    (in-out-equal (complex (/ 1 2) (/ 2 3)))
    (in-out-equal #C(1.0 0.0))
    (in-out-equal #C(2 3))
    (in-out-equal (complex most-positive-fixnum most-negative-fixnum))
    (in-out-equal (complex (expt 2 200) (expt 3 201))))
  t t t t t t t t)
     
(deftest base-strings
    (are-not-null
     (in-out-equal (make-string 0 :element-type 'base-char))
     (in-out-equal (coerce "this is a test" 'base-string))
     (in-out-equal (make-string 400 :initial-element (code-char 127)
				:element-type 'base-char)))
  t t t)

(deftest strings
    (are-not-null
     (in-out-equal "")
     (in-out-equal "this is a test")
     (in-out-equal (make-string 400 :initial-element (code-char 254))))
  t t t)

(deftest hard-strings
    (are-not-null
     (in-out-equal (format nil "Mot~arhead is a hard rock band." (code-char 246)))
     (in-out-equal (format nil "M~atley cr~ae is a hard string and was a hard rock band." 
			   (code-char 246) (code-char 252)))
     (in-out-equal (format nil "High c~ade p~ages." (code-char #xFFFF) (code-char #x1FFFF))))
  t t t)

(defun in-out-uninterned-equal (var)
  (with-buffer-streams (out-buf)
    (serialize var out-buf *store-controller*)
    (let ((new (deserialize (serialize var out-buf *store-controller*) *store-controller*)))
      (and (equal (symbol-name new) (symbol-name var))
	   (equal (symbol-package new) (symbol-package var))))))

(deftest symbols
    (are-not-null
     (in-out-equal nil)
     (in-out-equal T)
     (in-out-equal 'foobarbazquux)
     (in-out-equal 'ele::next-oid)
     (in-out-equal :a-keyword-symbol)
     (in-out-uninterned-equal '#:foozle)
     (in-out-uninterned-equal (make-symbol "a wha wah ba ba"))
     (in-out-uninterned-equal (make-symbol "")))
  t t t t t t t t)

(deftest chars
    (loop for i from 0 below char-code-limit
	  unless (in-out-equal (code-char i))
	  do (return i)
	  finally (return T))
  t)

(deftest pathnames
    ;;; Given how implementation-specific make-pathname is,
    ;;; i don't know how to put more portable tests here!
    (are-not-null
     (in-out-equal #p"/usr/local/share/common-lisp/elephant"))
  t)

(deftest conses
    (are-not-null
     (in-out-equal (cons t 100000))
     (in-out-equal (list 1 'a "this is a test" 'c 10000 nil 1000 nil))
     (in-out-equal (cons (cons (cons t nil) (cons nil t)) (cons 1 (cons t nil)))))
  t t t)

(deftest hash-tables-1
    (let* ((ht (make-hash-table :test 'equalp :size 333 :rehash-size 1.2 
				:rehash-threshold 0.8))
	   (rehash-size (hash-table-rehash-size ht))
	   (rehash-threshold (hash-table-rehash-threshold ht))
	   (out (in-out-value ht)))
      (are-not-null
       (eq (hash-table-test out) 'equalp)
;;       (= (hash-table-size out) size)  ;; size is not equal, only kv pairs are stored
;;       (= (hash-table-rehash-size out) rehash-size) ;; hint only, implementation not constrained
;;       (= (hash-table-rehash-threshold out) rehash-threshold) ;; hints only, implementation not constrained
       (eq (hash-table-test (in-out-value (make-hash-table :test 'eq))) 'eq)
       (eq (hash-table-test (in-out-value (make-hash-table :test 'eql))) 'eql)
       (eq (hash-table-test 
	    (in-out-value (make-hash-table :test 'equal))) 'equal)
       (eq (hash-table-test 
	    (in-out-value (make-hash-table :test 'equalp))) 'equalp)))
  t t t t t)

(deftest hash-tables-2
    (let ((ht (make-hash-table :test 'equalp)))
      (setf (gethash (cons nil nil) ht) "one")
      (setf (gethash 2 ht) 2.0d0)
      (setf (gethash 'symbolsymbol ht) "three")
      (let ((out (in-out-value ht)))
	(are-not-null
	 (string= (gethash (cons nil nil) out) "one")
	 (= (gethash 2 out) 2.0d0)
	 (string= (gethash 'symbolsymbol out) "three"))))
  t t t)

(defun type= (t1 t2)
  (and (subtypep t1 t2) (subtypep t2 t1)))

(deftest arrays-1
    (are-not-null
     (array-has-fill-pointer-p 
      (in-out-value (make-array 200 :fill-pointer t)))
     (not (array-has-fill-pointer-p 
	   (in-out-value (make-array 200 :fill-pointer nil))))
     (type= (upgraded-array-element-type '(unsigned-byte 20))
	    (array-element-type 
	     (in-out-value (make-array '(3 4 5) 
				       :element-type 
				       '(unsigned-byte 20)
				       :initial-element 0))))
     (type= (upgraded-array-element-type 'fixnum)
	    (array-element-type 
	     (in-out-value (make-array '(3 4 5) 
				       :element-type 
				       'fixnum
				       :initial-element 0))))
     )
  t t t t)
     
(deftest arrays-2
    (let ((arr (make-array '(3 4 5)))
	  (vec (make-array 100 :adjustable t :fill-pointer t))
	  (svec (make-array 100 :adjustable nil :fill-pointer nil)))
      (setf (aref arr 0 0 0) 'symb)
      (setf (aref arr 1 2 3) 123132)
      (setf (aref arr 2 3 4) "this is a longish string")
      (vector-push-extend 123456789101112 vec)
      (vector-push-extend "mr t" vec)
      (vector-push-extend 'symbolic vec)
      (loop for i from 0 to 99
	    do
	    (setf (svref svec i) (expt 2 i)))
      (are-not-null
       (in-out-equalp arr)
       (in-out-equalp vec)
       (in-out-equalp svec)
       (typep (in-out-value svec) 'simple-vector)))
  t t t t)


;; depends on ele::slots-and-values
(defun deep-equalp (thing another)
  (let ((seen (make-hash-table :test 'eq)))
    (labels 
	((%deep-equalp (s1 s2)
	   (when (type= (type-of s1) (type-of s2))
	     (if (gethash s1 seen) t
		 (progn
		   (setf (gethash s1 seen) t)
		   (typecase s1
		     (cons
		      (and (%deep-equalp (car s1) (car s2))
			   (%deep-equalp (cdr s1) (cdr s2))))
		     (array
		      (loop for i from 0 below (array-total-size s1)
			    always (%deep-equalp 
				    (row-major-aref s1 i)
				    (row-major-aref s2 i))))
		     (hash-table
		      (when (= (hash-table-count s1)
			       (hash-table-count s2))
			(loop for key being the hash-key of s1 
			      using (hash-value value)
			      always (%deep-equalp value
						   (gethash key s2)))))
		     (standard-object
		      (%deep-equalp (ele::slots-and-values s1)
				    (ele::slots-and-values s2)))
		     (structure-object
		      (%deep-equalp (ele::struct-slots-and-values s1)
				    (ele::struct-slots-and-values s2)))
		     (t (equalp s1 s2))))))))
      (%deep-equalp thing another))))
		      
(defclass foo ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2)))
  
(defclass bar ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2)))

(deftest test-deep-equalp
    (let ((c1 (cons nil nil))
	  (c2 (cons nil nil))
	  (l1 (make-list 100))
	  (h (make-hash-table :test 'equal))
	  (g (make-array '(2 3 4)))
	  (f (make-instance 'foo))
	  (b (make-instance 'bar)))
      (setf (car c1) c1)
      (setf (cdr c1) c1)
      (setf (car c2) c1)
      (setf (cdr c2) c2)
      (setf (cdr (last l1)) l1)
      (setf (gethash "quux" h) l1)
      (setf (gethash "bar" h) c2)
      (setf (aref g 1 1 1) g)
      (setf (aref g 0 0 1) h)
      (setf (gethash "foo" h) g)
      (setf (slot-value f 'slot1) b)
      (setf (slot-value f 'slot2) f)
      (setf (slot-value b 'slot1) h)
      (setf (slot-value b 'slot2) f)
      (are-not-null
       (deep-equalp c1 c1)
       (deep-equalp c2 c2)
       (deep-equalp l1 l1)
       (deep-equalp h h)
       (deep-equalp g g)
       (deep-equalp f f)
       (deep-equalp b b)))
  t t t t t t t)

(defclass spud ()
    ((msgid :type list :initform "" :accessor :msgd :initarg :msgid)
     (value :type (or list string) :initform "" :accessor :vl :initarg :value)))

(deftest test-serialization-unicode-slot
    (let ((s (make-instance 'spud :msgid '(a b c) :value  "Mesaĝo Teksto")))
      (in-out-deep-equalp s)
      )
  t)

(defun in-out-deep-equalp (var)
  (assert *store-controller*)
  (with-buffer-streams (out-buf)
    (deep-equalp var (deserialize (serialize var out-buf *store-controller*) *store-controller*))))

(deftest objects
    (are-not-null
     (in-out-deep-equalp (make-instance 'foo))
     (in-out-deep-equalp (make-instance 'bar :slot1 
					(make-instance 'foo 
						       :slot2 "foo bar"))))
  t t)

(defstruct struct1 ss1 ss2)

(deftest structs
    (are-not-null
     (in-out-deep-equalp (make-struct1))
     (in-out-deep-equalp (make-struct1 :ss1 "test" :ss2 (make-struct1 :ss1 "bottom" :ss2 nil))))
  t t)

(defstruct (struct2 (:constructor make-a-struct2)) ss3 ss4)

(defmethod struct-constructor ((class (eql 'struct2)))
  #'make-a-struct2)

(deftest struct-non-std-construct
    (are-not-null
     (in-out-deep-equalp (make-a-struct2))
     (in-out-deep-equalp (make-a-struct2 :ss3 (make-a-struct2 :ss4 "foo"))))
  t t)
    

(deftest circular
    (let ((c1 (cons nil nil))
	  (c2 (cons nil nil))
	  (l1 (make-list 100))
	  (h (make-hash-table :test 'equal))
	  (g (make-array '(2 3 4)))
	  (f (make-instance 'foo ))
	  (b (make-instance 'bar )))
      (setf (car c1) c1)
      (setf (cdr c1) c1)
      (setf (car c2) c1)
      (setf (cdr c2) c2)
      (setf (cdr (last l1)) l1)
      (setf (gethash "quux" h) l1)
      (setf (gethash "bar" h) c2)
      (setf (aref g 1 1 1) g)
      (setf (aref g 0 0 1) h)
      (setf (gethash "foo" h) g)
      (setf (slot-value f 'slot1) b)
      (setf (slot-value f 'slot2) f)
      (setf (slot-value b 'slot1) h)
      (setf (slot-value b 'slot2) f)
      (are-not-null
       (in-out-deep-equalp c1)
       (in-out-deep-equalp c2)
       (in-out-deep-equalp l1)
       (in-out-deep-equalp h)
       (in-out-deep-equalp g)
       (in-out-deep-equalp f)
       (in-out-deep-equalp b)))
  t t t t t t t)

(defclass pfoo ()
  ((slot1 :initarg :slot1 :accessor slot1))
  (:metaclass persistent-metaclass))

(defclass pbar (pfoo)
  ((slot2 :initarg :slot2 :accessor slot2))
  (:metaclass persistent-metaclass))

(deftest persistent
    (let* ((f1 (make-instance 'pfoo  :sc *store-controller*))
	   (f2 (make-instance 'pfoo :slot1 "this is a string"  :sc *store-controller*))
	   (b1 (make-instance 'pbar :slot2 "another string"  :sc *store-controller*))
	   (b2 (make-instance 'pbar  :sc *store-controller*))

;; Note, this as will will have to be split on clas,s if we we want to 
;; test it both ways...since we won't know how they will want it 
;; implemented, we will have to somehow make a choice here, maybe 
;; based on the stype of *store-controller*
	   (h (make-btree *store-controller*)))
      (are-not-null
       (in-out-eq f1)
       (in-out-eq f2)
       (in-out-eq b1)
       (in-out-eq b2)
       (in-out-eq h)
       (signals-condition 
	 (slot1 f1))
       (progn (setf (slot1 f1) f1)
	      (eq f1 (slot1 f1)))
       (progn (setf (get-value f2 h) f2)
	      (eq (get-value f2 h) f2))))
   t t t t t t t t)

;;; testcollections.lisp
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
;;;

(in-package :ele-tests)

(deftest basicpersistence 
    (let ((rv nil))
      (let ((x (gensym)))
	(add-to-root "x" x)
	;; Clear instances
	(flush-instance-cache *store-controller*)
	;; Are gensyms equal across db instantiations?
	;; This forces a refetch of the object from db
	(setq rv (equal (format nil "~A" x)
			(format nil "~A" (get-from-root "x")))))
      rv)
  t)

(deftest testoid
    (progn
      (ele::next-oid *store-controller*)
      (let ((oid (ele::next-oid *store-controller*)))
	  (< oid (ele::next-oid *store-controller*))))
  t)

(defclass blob ()
  ((slot1 :accessor slot1 :initarg :slot1)
   (slot2 :accessor slot2 :initarg :slot2)))

(defvar keys (loop for i from 1 to 1000 
		   collect (concatenate 'base-string "key-" (prin1-to-string i))))

(defvar objs (loop for i from 1 to 1000
		   collect (make-instance 'blob
					  :slot1 i
					  :slot2 (* i 100))))

(defvar bt)

(deftest btree-make
    (finishes (setq bt (make-btree *store-controller*)))
  t)

(deftest btree-put
    (finishes
       (with-transaction (:store-controller *store-controller*)
         (loop for obj in objs
               for key in keys
               do (setf (get-value key bt) obj))))
  t)

(deftest btree-get
    (loop for key in keys
	  for i from 1 to 1000
	  for obj = (get-value key bt)
	  always
	  (and (= (slot1 obj) i)
	       (= (slot2 obj) (* i 100))))
  t)

(defvar first-key (first keys))


;; For some unkown reason, this fails on my server unless
;; I put the variable "first-key" here rather than use the string
;; "key-1".  I need to understand this, but don't at present....
(deftest remove-kv
     (finishes 
       (with-transaction (:store-controller *store-controller*) (remove-kv first-key bt)))
  t)

(deftest removed
    (not (get-value first-key bt))
  t)

(deftest map-btree
    (let ((ks nil)
	  (vs nil))
      (flet ((mapper (k v) (push k ks) (push v vs)))
	(map-btree #'mapper bt))
      (values
       (and (subsetp ks (cdr keys) :test #'equalp) 
	    (subsetp (cdr keys) ks :test #'equalp))))
  t)

;; I hate global variables!  Yuck!
(defvar indexed)
(defvar index1)
(defvar index2)

(deftest indexed-btree-make
    (finishes (with-transaction (:store-controller *store-controller*)
		(setq indexed (make-indexed-btree *store-controller*))))
  t)

(defun key-maker (s key value)
  (declare (ignore s key))
  (values t (slot1 value)))

(deftest add-indices
    (finishes
     (with-transaction (:store-controller *store-controller*)
       (setf index1
	     (add-index indexed :index-name 'slot1 :key-form 'key-maker))
       (setf index2
	     (add-index indexed :index-name 'slot2
			:key-form '(lambda (s key value) 
				    (declare (ignore s key))
				    (values t (slot2 value)))))))
  t)

;; ISE NOTE: indices accessor is not portable across backends in current
;; system so I'm using alternate access (map-indices) instead
(deftest test-indices
    (values
     ;; (= (hash-table-count (indices indexed)) 2)
     (let ((count 0))
       (map-indices (lambda (x y) (declare (ignore x y)) (incf count)) indexed)
       (eq count 2))
     ;; (gethash 'slot1 (indices indexed)))
     (eq index1 (get-index indexed 'slot1))
     ;; (eq index2 (gethash 'slot2 (indices indexed))))
     (eq index2 (get-index indexed 'slot2)))
  t t t)

#|
(deftest safe-indexed-put
    (finishes
     (loop for i from 1 to 1000
	   for obj in objs
	   for key in keys
	   do
	   (setf (get-value key indexed) obj)
	   (loop for j from 1 to i
		 for key2 in keys
		 for obj2 = (get-value key2 indexed)
		 always
		 (and (= (slot1 obj2) j)
		      (= (slot2 obj2) (* j 100))))))
  t) |#
   
(deftest indexed-put
    (finishes
      (with-transaction (:store-controller *store-controller*)
	(loop for obj in objs
	   for key in keys
	   do (setf (get-value key indexed) obj))))
  t)

(deftest indexed-get
    (loop for key in keys
	  for i from 1 to 1000
	  for obj = (get-value key indexed)
	  always 
	  (and (= (slot1 obj) i)
	       (= (slot2 obj) (* i 100))))
  t)


(deftest simple-slot-get
    (progn
    (setf (get-value (nth 0 keys) indexed) (nth 0 objs))
    (let ((obj
	   (get-value 1 index1)))
      	  (and (= (slot1 obj) 1)
 	       (= (slot2 obj) (* 1 100)))))
t)

(deftest indexed-get-from-slot1
    (loop with index = (get-index indexed 'slot1)
	  for i from 1 to 1000
	  for obj = (get-value i index)
	  always
	  (= (slot1 obj) i))
  t)
	  
(deftest indexed-get-from-slot2
    (loop with index = (get-index indexed 'slot2)
	  for i from 1 to 1000
	  for obj = (get-value (* i 100) index)
	  always
	  (= (slot2 obj) (* i 100)))
  t)
	  
(deftest remove-kv-indexed
    (finishes (remove-kv first-key indexed))
  t)

(deftest no-key-nor-indices
    (values
     (get-value first-key indexed)
     (get-primary-key 1 index1)
     (get-primary-key 100 index2))
  nil nil nil)


(deftest remove-kv-from-slot1
    (finishes (remove-kv 2 index1))
  t)

(deftest no-key-nor-indices-slot1
    (values
     (get-value (second keys) indexed)
     (get-primary-key 2 index1)
     (get-primary-key 200 index2))
  nil nil nil)

(deftest remove-kv-from-slot2
    (finishes (remove-kv 300 index2))
  t)

(deftest no-key-nor-indices-slot2
    (values
     (get-value (third keys) indexed)
     (get-primary-key 3 index1)
     (get-primary-key 300 index2))
  nil nil nil)

(deftest map-indexed
    (let ((ks nil)
	  (vs nil))
      (flet ((mapper (k v) (push k ks) (push v vs)))
	(map-btree #'mapper indexed))
      (values
       (and (subsetp ks (cdddr keys) :test #'equalp) 
	    (subsetp (cdddr keys) ks :test #'equalp))))
  t)

;; This is "4" below because they have removed the
;; first three keys, and are testing that the index reflect this,
;; and my code doesn't.
(deftest get-first
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index1)
	(multiple-value-bind (has k v)
	    (cursor-first c)
	  (declare (ignore has v))
	  (= k 4))))
  t)

(deftest get-first2
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index2)
	(multiple-value-bind (has k v)
	    (cursor-first c)
	  (declare (ignore has v))
	  (= k 400))))
  t)

(deftest get-last
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index1)
	(multiple-value-bind (has k v)
	    (cursor-last c)
	  (declare (ignore has v))
	  (= k 1000))))
  t)

(deftest get-last2
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index2)
	(multiple-value-bind (has k v)
	    (cursor-last c)
	  (declare (ignore has v))
	  (= k 100000))))
  t)

(deftest set
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index1)
	(multiple-value-bind (has k v)
	    (cursor-set c 200)
	  (declare (ignore has k))
	  (= (slot1 v) 200))))
  t)

(deftest set2
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index2)
	(multiple-value-bind (has k v)
	    (cursor-set c 500)
	  (declare (ignore has k))
	  (= (slot2 v) 500))))
  t)

(deftest set-range
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index1)
	(multiple-value-bind (has k v)
	    (cursor-set-range c 199.5)
	  (declare (ignore has k))
	  (= (slot1 v) 200))))
  t)

(deftest set-range2
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (c index2)
	(multiple-value-bind (has k v)
	    (cursor-set-range c 501)
	  (declare (ignore has k))
	  (= (slot2 v) 600))))
  t)

(deftest map-indexed-index
    (let ((sum 0))
      (flet ((collector (key value pkey)
	       (incf sum (slot1 value))))
	(map-index #'collector index1 :start nil :end 10)
	(map-index #'collector index1 :start 990 :end nil)
	(map-index #'collector index1 :start 400 :end 410))
      sum)
  #.(+ 49 ;; sum 4-10 inclusive (1-3 removed by here)
       4455 ;; sum 690-700 inclusive
       10945 ;; sum 990 to 1000 inclusive
       ))

(deftest map-index-from-end
    (let ((sum 0))
      (flet ((collector (key value pkey)
	       (incf sum (slot1 value))))
	(map-index #'collector index1 :start nil :end 10 :from-end t)
	(map-index #'collector index1 :start 990 :end nil :from-end t)
	(map-index #'collector index1 :start 400 :end 410 :from-end t))
      sum)
  #.(+ 49 ;; sum 4-10 inclusive (1-3 removed by here)
       4455 ;; sum 690-700 inclusive
       10945 ;; sum 990 to 1000 inclusive
       ))

(deftest rem-kv
    (with-transaction (:store-controller *store-controller*)
      (let ((ibt (make-indexed-btree *store-controller*)))
	(loop for i from 0 to 10
	      do
	      (setf (get-value i ibt) (* i i)))
	(remove-kv 0 ibt)
	(remove-kv 1 ibt)
	(remove-kv 10 ibt)
	(equal (list 
	 (get-value 0 ibt)
	 (get-value 1 ibt)
	 (get-value 10 ibt)
	 (get-value 5 ibt)
	)
	       '(nil nil nil 25))
	       ))
t
    )

(defun odd (s k v)
  (declare (ignore s k))
	   (values t (mod v 2)
))

(defun twice (s k v)
  (declare (ignore s k))
  (values t (* v 2)))

(defun half-floor (s k v)
  (declare (ignore s v))
  (values t (floor (/ k 2))))

(deftest rem-idexkv
    (with-transaction (:store-controller *store-controller*)
    (let* ((ibt (make-indexed-btree *store-controller*))
	   (id1 (add-index ibt :index-name 'idx1 :key-form 'odd)))
      (loop for i from 0 to 10
	 do
	 (setf (get-value i ibt) (* i i)))

      (with-btree-cursor (c id1)
	(cursor-first c)
	(dotimes (i 10)
	  (multiple-value-bind (has key value)
	      (cursor-next c)
	    ))
	)
      (remove-kv 4 ibt)
      (remove-kv 5 ibt)

      (equal (list
       (get-value 4 ibt)
       (get-value 5 ibt)
       (get-value 6 ibt)
       (with-btree-cursor (c ibt)
	 (cursor-first c)
	 (dotimes (i 4)
	   (multiple-value-bind (has key value)
	     (cursor-next c)
	   value))
       (multiple-value-bind (has key value)
	   (cursor-next c)
	 value
	 )
	 ))
	     '(nil nil 36 49)
      )))
    t
  )

(defvar indexed2)
(defvar index3)

(deftest make-indexed2
    (finishes (with-transaction (:store-controller *store-controller*)
		(setq indexed2 (make-indexed-btree *store-controller*))))
  t)

(defun crunch (s k v)
  (declare (ignore s k))
  (values t (floor (/ (- v) 10))))

(deftest add-indices2
    (finishes
      (with-transaction (:store-controller *store-controller*) 
	(setq index3
	      (add-index indexed2 :index-name 'crunch :key-form 'crunch))))
  t)

(deftest put-indexed2
    (finishes
      (with-transaction (:store-controller *store-controller*) 
	(loop for i from 0 to 10000
	      do
	      (setf (get-value i indexed2) (- i)))))
  t)

(deftest get-indexed2
    (loop for i from 0 to 10000
	  always (= (- i) (get-value i indexed2)))
  t)

(deftest get-from-index3
    (let ((v))
;;    (trace get-value)
;;    (trace crunch)
    (unwind-protect 
    (setf v (loop for i from 0 to 1000
;;	  always (= (- i) (floor (/ (get-value i index3) 10)))))
	  always 
	  (multiple-value-bind (bool res)
	      (crunch nil nil (get-value i index3))
	    (= res i))))
;;    (untrace))
      )
    v)
  t)

(deftest dup-test
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (curs index3)
	(loop for (more k v) = (multiple-value-list
				(cursor-first curs))
	   then (multiple-value-list (cursor-next-dup curs))
	   while more
	   collect v)))
  (0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
	      


(deftest nodup-test
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (curs index3)
	(loop for (m k v) = (multiple-value-list (cursor-next-nodup curs))
	      for i from 0 downto -9990 by 10
	      while m
	      always (= v i))))
  t)

(deftest prev-nodup-test
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (curs index3)
	(cursor-last curs)
	(loop for (m k v) = (multiple-value-list (cursor-prev-nodup curs))
	      for i from -9999 to -9 by 10
	      while m
	      always (= v i))))
  t)

(deftest pnodup-test
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (curs index3)
	(loop for (m k v p) = (multiple-value-list (cursor-pnext-nodup curs))
	      for i from 0 to 9990 by 10
	      while m
	      always (= p i))))
  t)

(deftest pprev-nodup-test
    (with-transaction (:store-controller *store-controller*)
      (with-btree-cursor (curs index3)
	(cursor-last curs)
	(loop for (m k v p) = (multiple-value-list (cursor-pprev-nodup curs))
	      for i from 9999 downto 9 by 10
	      while m
	      always (= p i))))
  t)

(deftest cur-del1 
    (with-transaction (:store-controller *store-controller*)
      (let* ((ibt (make-indexed-btree *store-controller*))
	     (id1 (add-index ibt :index-name 'idx1 :key-form 'odd)))
	(labels ((deleted (key others)
		   (and (null (get-value key ibt))
			(every #'(lambda (k2)
				   (= (get-value k2 ibt) (* k2 k2)))
			       others))))
	  (loop for i from 0 to 5 do
	       (setf (get-value i ibt) (* i i)))
	  (with-btree-cursor (c id1)
	    (cursor-last c)
	    (cursor-delete c))
	  (or (deleted 5 '(3 1))
	      (deleted 3 '(5 1))
	      (deleted 1 '(5 3))))))
  t)

(deftest indexed-delete 
    (finishes
      (with-transaction (:store-controller *store-controller*)
	(with-btree-cursor (curs index3)
	  (cursor-last curs)
	  (cursor-delete curs))))
  t)

(deftest test-deleted
    (values
     (get-value 10000 indexed2)
     (get-value 1000 index3))
  nil nil)
		      
(deftest indexed-delete2
    (finishes
      (with-transaction (:store-controller *store-controller*)
	(with-btree-cursor (curs index3)
	  (cursor-first curs)
	  (cursor-next-dup curs)
	  (cursor-delete curs))))
  t)

(deftest test-deleted2
    (values
     (get-value 0 indexed2)
     (get-value 0 index3)
     (get-value 1 indexed2)
     (with-btree-cursor (c index3)
       (cursor-first c)
       (multiple-value-bind (m k v) (cursor-next c)
	 v)))
  0 0 nil -2)


(deftest cur-del2 
    (with-transaction (:store-controller *store-controller*)
      (let* ((ibt (make-indexed-btree *store-controller*))
	     (id1 (add-index ibt :index-name 'idx1 :key-form 'half-floor)))
	(loop for i from 0 to 10
	   do
	     (setf (get-value i ibt) (* i i)))
	(with-btree-cursor (c id1)
	  (cursor-first c)
	  (cursor-next-dup c)
	  (cursor-delete c)
	  )
	(or (and (null (get-value 1 ibt))
		 (eq (get-value 0 ibt) 0))
	    (and (null (get-value 0 ibt))
		 (eq (get-value 1 ibt) 1)))))
  t)



(deftest get-both 
    (with-btree-cursor (c indexed2)
      (cursor-get-both c 200 -200))
  t 200 -200)

(deftest pget-both 
    (with-btree-cursor (c index3)
      (multiple-value-bind (m k v p)
	  (cursor-pget-both c 10 107)
	(values k v p)))
  10 -107 107)

(deftest pget-both-range
    (with-btree-cursor (c index3)
      (multiple-value-bind (m k v p)
	  (cursor-pget-both-range c 10 106.5)
	(values k v p)))
  10 -107 107)

(defmacro pcursor-pkey (form)
  `(multiple-value-bind (m k v p)
    ,form
    (declare (ignore m k v))
    p))

(deftest pcursor
    (with-btree-cursor (c index3)
      (values
       (pcursor-pkey (cursor-pfirst c))
       (pcursor-pkey (cursor-pnext c))
       (pcursor-pkey (cursor-pnext-nodup c))

       (pcursor-pkey (cursor-pnext-dup c))
       (pcursor-pkey (cursor-pprev c))
       (pcursor-pkey (cursor-pprev-nodup c))

       (pcursor-pkey (cursor-plast c))
       (pcursor-pkey (cursor-pset c 300))
       (pcursor-pkey (cursor-pset-range c 199.5))

       (pcursor-pkey (cursor-pget-both c 10 101))
       (pcursor-pkey (cursor-pget-both-range c 11 111.4))))
      
  0 2 10 11 10 9 9999 3000 2000 101 112)

(defvar index4)

(deftest newindex
    (finishes
     (with-transaction (:store-controller *store-controller*) 
       (setq index4
	     (add-index indexed2 :index-name 'crunch :key-form 'crunch
			:populate t))))
  t)

(deftest pcursor2
    (with-btree-cursor (c index4)
      (values
       (pcursor-pkey (cursor-pfirst c))
       (pcursor-pkey (cursor-pnext c))
       (pcursor-pkey (cursor-pnext-nodup c))
       (pcursor-pkey (cursor-pnext-dup c))
       (pcursor-pkey (cursor-pprev c))
       (pcursor-pkey (cursor-pprev-nodup c))
       (pcursor-pkey (cursor-plast c))
       (pcursor-pkey (cursor-pset c 300))
       (pcursor-pkey (cursor-pset-range c 199.5))
       (pcursor-pkey (cursor-pget-both c 10 101))
       (pcursor-pkey (cursor-pget-both-range c 11 111.4))))
      
  0 2 10 11 10 9 9999 3000 2000 101 112)


(deftest add-get-remove
    (let ((r1 '())
	  (r2 '()))
      (add-to-root "x1" "y1")
      (add-to-root "x2" "y2")
      (setf r1 (get-from-root "x1"))
      (setf r2 (get-from-root "x2"))
      (remove-from-root "x1")
      (remove-from-root "x2")
      (and 
       (equal "y1" r1)
       (equal "y2" r2)
       (equal nil (get-from-root "x1"))
       (equal nil (get-from-root "x2"))
       ))
  t)

(deftest add-get-remove-symbol
    (let ((foo (cons nil nil))
	  (bar (cons 'a 'b))
	  (f1 '())
	  (f2 '())
	  (b1 '())
	  (b2 '()))
      (add-to-root "my key" foo)
      (add-to-root "my other key" foo)
      (setf f1 (get-from-root "my key"))
      (setf f2 (get-from-root "my other key"))
      (add-to-root "my key" bar)
      (add-to-root "my other key" bar)
      (setf b1 (get-from-root "my key"))
      (setf b2 (get-from-root "my other key"))	
      (and 
       (equal f1 f2)
       (equal b1 b2)
       (equal f1 foo)
       (equal b1 bar)))
  t)

(deftest existsp
    (let ((exists1 '())
	  (exists2 '())
	  (exists3 '())
	  (key "my key"))
      (remove-from-root key)
      (setf exists1 (root-existsp key))
      (add-to-root key 'a)
      (setf exists2 (root-existsp key))
      (remove-from-root key)
      (setf exists3 (root-existsp key))
      (values exists1 exists2 exists3))
  nil t nil
  )

(defparameter test-items '(1 2 3 (1) (2) test1 test2))

(deftest pset
    (let ((pset1 (make-pset)))
      (mapc (lambda (item)
	      (insert-item item pset1))
	    test-items)
      (remove-item (list 2) pset1)
      (remove-item 'test2 pset1)
      (let ((list (pset-list pset1)))
	(values
	 (= (length (pset-list pset1)) 5)
	 (not (find-item 'test2 pset1))
	 (is-not-null (find-item 'test1 pset1))
	 (is-not-null (find-item 1 pset1 :key (lambda (x) (when (consp x) (car x))) :test #'eq)))))
  t t t t)
	  

;; This test not only does not work, it appears to 
;; hang BDB forcing a recovery!?!?!?!
;; (deftest cursor-put
;;     (let* ((ibt (make-indexed-btree *store-controller*)))
;;       (let (
;; 	    (index
;; 	     (add-index ibt :index-name 'crunch :key-form 'crunch
;; 			:populate t))
;; 	    )
;; 	(loop for i from 0 to 10
;; 	   do
;; 	   (setf (get-value i ibt) (* i i)))
;; 	;; Now create a cursor, advance and put...
;; 	(let ((c (make-cursor ibt)))
;; 	  (cursor-next c)
;; 	  (cursor-next c)
;; 	  (cursor-put c 4 :key 10)
;; 	  (equal (get-value 10 ibt) 4)))
;;       )
;;   t)



;; (deftest class-change-deletion
;;      (progn
;;        (defclass blob-tbc ()
;;  	((slot1 :accessor slot1 :initarg :slot1)
;;  	 (slot2 :accessor slot2 :initarg :slot2)))
;;        (add-to-root "blob" (make-instance 'blob-tbc))
;;        (defclass blob-tbc ()
;;  	((slot1 :accessor slot1 :initarg :slot1)
;;  	 (slot3 :accessor slot3 :initarg :slot3)))
;;        (remove-from-root "blob")
;;        (get-from-root "blob")
;;        )
;;    nil nil)


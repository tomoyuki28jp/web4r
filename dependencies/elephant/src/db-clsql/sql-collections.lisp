;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; sql-controller.lisp -- Interface to a CLSQL based object store.
;;; 
;;; Initial version 10/12/2005 by Robert L. Read
;;; <read@robertlread.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2005-2007 by Robert L. Read
;;; <rread@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :db-clsql)

(defmethod get-value (key (bt sql-btree-index))
  "Get the value in the primary DB from a secondary key."
  (declare (optimize (speed 3)))
      ;; Below, the take the oid and add it to the key, then look
      ;; thing up--- where?

      ;; Somehow I suspect that what I am getting back here 
      ;; is actually the main key...
  (let* ((sc (get-con bt)))
      (let ((pk (sql-get-from-clcn (oid bt) key  sc)))
	(if pk 
	    (sql-get-from-clcn (oid (primary bt)) pk sc))
	)))

(defmethod get-primary-key (key (bt sql-btree-index))
  (declare (optimize (speed 3)))
      (let* ((sc (get-con bt))
	     )
	(sql-get-from-clcn (oid bt) key sc)))


;; My basic strategy is to keep track of a current key
;; and to store all keys in memory so that we can sort them
;; to implement the cursor semantics.  Clearly, passing 
;; in a different ordering is a nice feature to have here.
(defclass sql-cursor (cursor)
  ((keys :accessor sql-crsr-ks :initarg :sql-cursor-keys :initform '())
   (curkey :accessor sql-crsr-ck :initarg :sql-cursor-curkey :initform -1 :type (or null integer)))
  (:documentation "A SQL cursor for traversing (primary) BTrees."))

(defmethod make-cursor ((bt sql-btree))
  "Make a cursor from a btree."
  (declare (optimize (speed 3)))
  (make-instance 'sql-cursor 
		 :btree bt
		 :oid (oid bt)))



(defmethod cursor-close ((cursor sql-cursor))
  (setf (sql-crsr-ck cursor) nil)
  (setf (cursor-initialized-p cursor) nil))

;; Maybe this will still work?
;; I'm not sure what cursor-duplicate is meant to do, and if 
;; the other state needs to be copied or now.  Probably soo...
(defmethod cursor-duplicate ((cursor sql-cursor))
  (declare (optimize (speed 3)))
  (make-instance (type-of cursor)
		 :initialized-p (cursor-initialized-p cursor)
		 :oid (cursor-oid cursor)
		 ;; Do we need to so some kind of copy on this collection?
		 :keys (sql-crsr-ks cursor)
		 :curkey (sql-crsr-ck cursor)))
;;		 :handle (db-cursor-duplicate 
;;			  (cursor-handle cursor) 
;;			  :position (cursor-initialized-p cursor))))

(defmethod cursor-current ((cursor sql-cursor))
  (declare (optimize (speed 3)))
  (when (cursor-initialized-p cursor)
    (has-key-value cursor)))

;; Only for use within an operation...
(defun my-generic-less-than (a b)
  (cond
    ((and (typep a 'persistent) (typep b 'persistent))
     (< (oid a) (oid b))
     )
    ((and (numberp a ) (numberp b))
     (< a b))
    ((and (stringp a) (stringp b))
     (string< a b))
    (t
     (string< (format nil "~A" a) (format nil "~A" b)))
    ))

(defun my-generic-at-most (a b)
  (cond
    ((and (typep a 'persistent) (typep b 'persistent))
     (<= (oid a) (oid b))
     )
    ((and (numberp a ) (numberp b))
     (<= a b))
    ((and (stringp a) (stringp b))
     (string<= a b))
    (t
     (string<= (format nil "~A" a) (format nil "~A" b)))
    ))

(defmethod cursor-un-init ((cursor sql-cursor) &key (returnpk nil))
  (setf (cursor-initialized-p cursor) nil)
  (if returnpk
      (values nil nil nil nil)
      (values nil nil nil)))

(clsql::locally-enable-sql-reader-syntax)

(defmethod cursor-init ((cursor sql-cursor))
  (let* ((sc (get-con (cursor-btree cursor)))
	 (con (controller-db sc))
	 (tuples
	  (clsql:select [key] 
		  :from [keyvalue]
		  :where [= [clctn_id] (oid (cursor-btree cursor))] 
		  :database con
		  ))
	 (len (length tuples)))
    ;; now we somehow have to load the keys into the array...
    ;; actually, this should be an adjustable vector...
    (setf (sql-crsr-ks cursor) (make-array (length tuples)))
    (do ((i 0 (1+ i))
	 (tup tuples (cdr tup)))
	((= i len) nil)
      (setf (aref (sql-crsr-ks cursor) i)
	    (deserialize-from-base64-string (caar tup) sc)))
    (sort (sql-crsr-ks cursor) #'my-generic-less-than)
    (setf (sql-crsr-ck cursor) 0)
    (setf (cursor-initialized-p cursor) t)
    ))

(clsql::restore-sql-reader-syntax-state) 

;; we're assuming here that nil is not a legitimate key.
(defmethod get-current-key ((cursor sql-cursor))
  (let ((x (sql-crsr-ck cursor)))
    (if (and (>= x 0) (< x (length (sql-crsr-ks cursor))))
	(svref (sql-crsr-ks cursor) x)
	'()
	))
  )

(defmethod get-current-value ((cursor sql-cursor))
  (let ((key (get-current-key cursor)))
    (if key
	(get-value key (cursor-btree cursor))
	'())))

(defmethod has-key-value ((cursor sql-cursor))
  (let ((key (get-current-key cursor)))
    (if key
	(values t key (get-value key (cursor-btree cursor)))
	(cursor-un-init cursor))))

 

(defmethod cursor-first ((cursor sql-cursor))
  (declare (optimize (speed 3)))
  ;; Read all of the keys...
  ;; We need to get the contoller db from the btree somehow...
  (cursor-init cursor)
  (has-key-value cursor)
  )

		 
;;A bit of a hack.....

;; If you run off the end, this can set cursor-initalized-p to nil.
(defmethod cursor-last ((cursor sql-cursor) )
  (unless (cursor-initialized-p cursor)
    (cursor-init cursor))
  (setf (sql-crsr-ck cursor) 
	(- (length (sql-crsr-ks cursor)) 1))
  (setf (cursor-initialized-p cursor) t)
  (has-key-value cursor))



(defmethod cursor-next ((cursor sql-cursor))
  (if (cursor-initialized-p cursor)
      (progn
	(incf (sql-crsr-ck cursor))
	(has-key-value cursor))
      (cursor-first cursor)))
	  
(defmethod cursor-prev ((cursor sql-cursor))
  (declare (optimize (speed 3)))
  (if (cursor-initialized-p cursor)
      (progn
	(decf (sql-crsr-ck cursor))
	(has-key-value cursor))
      (cursor-last cursor)))
	  
(defmethod cursor-set ((cursor sql-cursor) key)
  (declare (optimize (speed 3)))
  (if  (cursor-initialized-p cursor)
       (let ((p (position key (sql-crsr-ks cursor) :test #'equal)))
	 (if p
	     (progn
	       (setf (sql-crsr-ck cursor) p)
	       (setf (cursor-initialized-p cursor) t)	  
	       (has-key-value cursor)
	       )
	     (setf (cursor-initialized-p cursor) nil)))
       (progn
	 (cursor-init cursor)
	 (let ((p (position key (sql-crsr-ks cursor) :test #'equal)))
	   (if p
	       (progn
		 (setf (sql-crsr-ck cursor) p)
		 (has-key-value cursor)
		 )
	       (setf (cursor-initialized-p cursor) nil))))
       ))
  

(defmethod cursor-set-range ((cursor sql-cursor) key)
  (declare (optimize (speed 3)))
  ;; I'm a little fuzzy on when I should leave a cursor in
  ;; the initialized state...
  (unless (cursor-initialized-p cursor)
    (cursor-init cursor))
  (let ((len (length (sql-crsr-ks cursor)))
	(vs '()))
    (do ((i 0 (1+ i)))
	((or (= i len) 
	     vs)
	 vs)
      (progn
	(multiple-value-bind (h k v)
	    (cursor-next cursor)
	  (declare (ignore h v))
	  (when (my-generic-less-than key k)
	    (setf vs t))
	  )
	))
    (if vs
	(cursor-current cursor)
	(cursor-un-init cursor))))



(defmethod cursor-get-both ((cursor sql-cursor) key value)
  (declare (optimize (speed 3)))
  (let* ((bt (cursor-btree cursor))
	 (v (get-value key bt)))
    (if (equal v value)
;; We need to leave this cursor properly posistioned....
;; For a secondary cursor it's harder, but for this, it's simple
	(cursor-set cursor key)
	(cursor-un-init cursor))))

;; This needs to be rewritten!
(defmethod cursor-get-both-range ((cursor sql-cursor) key value)
  (declare (optimize (speed 3)))
  (let* ((bt (cursor-btree cursor))
	 (v (get-value key bt)))
    ;; Since we don't allow duplicates in primary cursors, I 
    ;; guess this is all that needs to be done!
    ;; If there were a test to cover this, the semantics would be clearer...
    (if (equal v value)
	(cursor-set cursor key)
	(cursor-un-init cursor))))



(defmethod cursor-delete ((cursor sql-cursor))
  (declare (optimize (speed 3)))
  (if (cursor-initialized-p cursor)
      (multiple-value-bind 
       (has k v) 
       (cursor-current cursor)
       (declare (ignore has v))
       ;; Now I need to suck the value out of the cursor, somehow....
       (remove-kv k (cursor-btree cursor)))
      (error "Can't delete with uninitialized cursor!")))


;; This needs to be changed!
(defmethod cursor-put ((cursor sql-cursor) value &key (key nil key-specified-p))
  "Put by cursor.  Not particularly useful since primaries
don't support duplicates.  Currently doesn't properly move
the cursor."
  (declare (optimize (speed 3))
	   (ignore key value key-specified-p))
  (error "Puts on sql-cursors are not yet implemented, because I can't get them to work on BDB cursors!"))

;; Secondary Cursors
(defclass sql-secondary-cursor (sql-cursor) 
  ((dup-number :accessor dp-nmbr :initarg :dup-number :initform 0 :type integer))
  (:documentation "Cursor for traversing bdb secondary indices."))


(defmethod make-cursor ((bt sql-btree-index))
  "Make a secondary-cursor from a secondary index."
  (declare (optimize (speed 3)))
  (make-instance 'sql-secondary-cursor 
		 :btree bt
		 :oid (oid bt)))



(defmethod has-key-value-scnd ((cursor sql-secondary-cursor) &key (returnpk nil))
  (let ((ck (sql-crsr-ck cursor)))
    (if (and (>= ck  0) (< ck  (length (sql-crsr-ks cursor))))
	(let* ((cur-pk (aref (sql-crsr-ks cursor)
			     (sql-crsr-ck cursor)))
	       (sc (get-con (cursor-btree cursor)))
	       (indexed-pk (sql-get-from-clcn-nth (cursor-oid cursor) cur-pk 
						  sc
						  (dp-nmbr cursor))))
	  (if indexed-pk
	      (let ((v (get-value indexed-pk (primary (cursor-btree cursor)))))
		(if v
		    (if returnpk
			(values t cur-pk v indexed-pk)
			(values t cur-pk v))
		    (cursor-un-init cursor :returnpk returnpk)))
	      (cursor-un-init cursor :returnpk returnpk)))
	(progn
	  (cursor-un-init cursor :returnpk returnpk)))))

(defmethod cursor-current ((cursor sql-secondary-cursor) )
  (cursor-current-x cursor))

(defmethod cursor-current-x ((cursor sql-secondary-cursor) &key (returnpk nil))
  (has-key-value-scnd cursor :returnpk returnpk)
)

(defmethod cursor-pcurrent ((cursor sql-secondary-cursor))
  (cursor-current-x cursor :returnpk t))

(defmethod cursor-pfirst ((cursor sql-secondary-cursor))
  (cursor-first-x cursor :returnpk t))

(defmethod cursor-plast ((cursor sql-secondary-cursor))
  (cursor-last-x cursor :returnpk t))

(defmethod cursor-pnext ((cursor sql-secondary-cursor))
  (cursor-next-x cursor :returnpk t))
	  
(defmethod cursor-pprev ((cursor sql-secondary-cursor))
  (cursor-prev-x cursor :returnpk t))
	  
(defmethod cursor-pset ((cursor sql-secondary-cursor) key)
  (declare (optimize (speed 3)))
  (unless (cursor-initialized-p cursor)
    (cursor-init cursor))
  (let ((idx (position key (sql-crsr-ks cursor) :test #'equal)))
    (if idx
        (progn
          (setf (sql-crsr-ck cursor) idx)
          (setf (dp-nmbr cursor) 0)
          (cursor-current-x cursor :returnpk t))
        (cursor-un-init cursor)
        )))

(defun array-index-if (p a)
  (do ((i 0 (1+ i)))
      ((or (not (array-in-bounds-p a i))
	(funcall p (aref a i)))
       (if (and (array-in-bounds-p a i) (funcall p (aref a i)))
	   i
	   -1)))
)

(defmethod cursor-pset-range ((cursor sql-secondary-cursor) key)
  (declare (optimize (speed 3)))
  (unless (cursor-initialized-p cursor)
    (cursor-init cursor))
  (let ((idx (array-index-if #'(lambda (x) (my-generic-at-most key x)) (sql-crsr-ks cursor))))
    (if (<= 0 idx)
	(progn
	  (setf (sql-crsr-ck cursor) idx)
	  (setf (dp-nmbr cursor) 0)
	  (cursor-current-x cursor :returnpk t)
	  )
	(cursor-un-init cursor :returnpk t)
    )))


;; Moves the cursor to a the first secondary key / primary key pair, 
;; with secondary key equal to the key argument, and primary key greater or equal to the pkey argument.
;; Returns has-tuple / secondary key / value / primary key.
(defmethod cursor-pget-both ((cursor sql-secondary-cursor) key pkey)
  (declare (optimize (speed 3)))
;; It's better to get the value by the primary key, 
;; as that is unique..
  (let* ((bt (primary (cursor-btree cursor)))
	 (v (get-value pkey bt)))
;; Now, bascially we set the cursor to the key and
;; andvance it until we get the value that we want...
    (if v
	(do ((vs 
	      (multiple-value-list (cursor-set cursor key))
	      (multiple-value-list (cursor-next cursor))))
	    ((or (null (car vs)) ;; We ran off the end..
		 (not (equal key (cadr vs))) ;; We ran out of values matching this key..
		 (equal v (caddr vs))) ;; we found what we are loodking for!
;; our return condition...
	     (if (equal v (caddr vs))
		 (cursor-current-x cursor :returnpk t)
		 (cursor-un-init cursor :returnpk t))
	      )
	  ;; Here's a body that's nice for debugging...
	  )
;; If we don't get a value, we have to un-init this cursor...
	(cursor-un-init cursor :returnpk t))))

(defmethod cursor-pget-both-range ((cursor sql-secondary-cursor) key pkey)
  (declare (optimize (speed 3)))
  ;; It's better to get the value by the primary key, 
  ;; as that is unique..
  (do ((vs 
	(append (multiple-value-list (cursor-set cursor key)) (list pkey))
	(multiple-value-list (cursor-next-x cursor :returnpk t))))
      ((or (null (car vs)) ;; We ran off the end..
	   (not (equal key (cadr vs))) ;; We ran out of values matching this key..
	   (equal pkey (caddr vs))	;; we found what we are loodking for!
	   (my-generic-less-than ;; we went beond the pkey
	    pkey
	    (cadddr vs)
	    )
	   ) 
       ;; our return condition...
       (if (or (equal pkey (caddr vs))
	       (my-generic-less-than ;; we went beond the pkey
		pkey
		(cadddr vs)
		))
	   (cursor-current-x cursor :returnpk t)
	   (cursor-un-init cursor :returnpk t))
       )
    ))


(defmethod cursor-delete ((cursor sql-secondary-cursor))
  "Delete by cursor: deletes ALL secondary indices."
  (declare (optimize (speed 3)))
  (if (cursor-initialized-p cursor)
      (multiple-value-bind 
	    (m k v p) 
	  (cursor-current-x cursor :returnpk t)
	(declare (ignore m k v))
	  (remove-kv p (primary (cursor-btree cursor)))
	  (let ((ck (sql-crsr-ck cursor))
		(dp (dp-nmbr cursor)))
	    (declare (ignorable dp))
	    (cursor-next cursor)
;; Now that we point to the old slot, remove the old slot from the array...
	    (setf (sql-crsr-ks cursor)
		  (remove-indexed-element-and-adjust 
		   ck
		   (sql-crsr-ks cursor)))
	    ;; now move us back to where we were
	    (cursor-prev cursor)
	  ))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-get-both ((cursor sql-secondary-cursor) key value)
  "cursor-get-both not implemented for secondary indices.
Use cursor-pget-both."
  (declare (ignore key value))
  (error "cursor-get-both not implemented on secondary
indices.  Use cursor-pget-both."))

(defmethod cursor-get-both-range ((cursor sql-secondary-cursor) key value)
  "cursor-get-both-range not implemented for secondary indices.
Use cursor-pget-both-range."
  (declare (ignore key value))
  (error "cursor-get-both-range not implemented on secondary indices.  Use cursor-pget-both-range."))

(defmethod cursor-put ((cursor sql-secondary-cursor) value &rest rest)
  "Puts are forbidden on secondary indices.  Try adding to
the primary."
  (declare (ignore rest value))
  (error "Puts are forbidden on secondary indices.  Try adding to the primary."))


(defmethod cursor-first ((cursor sql-secondary-cursor))
  (cursor-first-x cursor)
  )

(defmethod cursor-first-x ((cursor sql-secondary-cursor) &key (returnpk nil))
  (declare (optimize (speed 3)))
  (setf (dp-nmbr cursor) 0)
  (cursor-init cursor)
  (has-key-value-scnd cursor :returnpk returnpk)
  )

(defmethod cursor-next ((cursor sql-secondary-cursor))
  (cursor-next-x cursor)
)

(defmethod cursor-next-x ((cursor sql-secondary-cursor) &key (returnpk nil))
  (if (cursor-initialized-p cursor)
      (progn
	(let ((cur-pk (get-current-key cursor)))
	  (incf (sql-crsr-ck cursor))
	  (if (equal cur-pk (get-current-key cursor))
	      (incf (dp-nmbr cursor))
	      (setf (dp-nmbr cursor) 0))
	  (has-key-value-scnd cursor :returnpk returnpk)))
      (cursor-first-x cursor :returnpk returnpk)))
	  
(defmethod cursor-prev ((cursor sql-secondary-cursor))
  (cursor-prev-x cursor)
)
(defmethod cursor-prev-x ((cursor sql-secondary-cursor)  &key (returnpk nil))
  (declare (optimize (speed 3)))
  (if (cursor-initialized-p cursor)
      (progn
	(let ((prior-pk (get-current-key cursor)))
	  (decf (sql-crsr-ck cursor))
	  (if (eq prior-pk (get-current-key cursor))
	      (setf (dp-nmbr cursor) (max 0 (- (dp-nmbr cursor) 1)))
	      (setf (dp-nmbr cursor)
		    (1- (sql-get-from-clcn-cnt (cursor-oid cursor)
					       (get-current-key cursor)
					       (get-con (cursor-btree cursor))))
		    )))
	(has-key-value-scnd cursor :returnpk returnpk))
      (cursor-last-x cursor :returnpk returnpk)))

(defmethod cursor-next-dup ((cursor sql-secondary-cursor))
  (cursor-next-dup-x cursor)
)

(defmethod cursor-pnext-dup ((cursor sql-secondary-cursor))
  (cursor-next-dup-x cursor :returnpk t)
)

(defmethod cursor-next-dup-x ((cursor sql-secondary-cursor) &key (returnpk nil))
;;  (declare (optimize (speed 3)))
  (when (cursor-initialized-p cursor)
    (let* ((cur-pk (aref (sql-crsr-ks cursor)
			 (sql-crsr-ck cursor)))
	   (nint (+ 1 (sql-crsr-ck cursor)))
	   (nxt-pk (if (array-in-bounds-p (sql-crsr-ks cursor) nint) 
		       (aref (sql-crsr-ks cursor)
			     nint)
		       -1
		       ))
	   )
      (if (equal cur-pk nxt-pk)
	  (progn
	    (incf (dp-nmbr cursor))
	    (incf (sql-crsr-ck cursor))
	    (has-key-value-scnd cursor :returnpk returnpk))
	  (progn
	    (setf (dp-nmbr cursor) 0)
	    (cursor-un-init cursor :returnpk returnpk)
	    )))))

(defmethod cursor-next-nodup ((cursor sql-secondary-cursor))
  (cursor-next-nodup-x cursor)
)	  
(defmethod cursor-next-nodup-x ((cursor sql-secondary-cursor) &key (returnpk nil))
  (if (cursor-initialized-p cursor)
      (let ((n
	     (do ((i (sql-crsr-ck cursor) (1+ i)))
		 ((or 
		    (not (array-in-bounds-p (sql-crsr-ks cursor) (+ i 1)))
		    (not 
		     (equal (aref (sql-crsr-ks cursor) i)
			    (aref (sql-crsr-ks cursor) (+ 1 i)))))
		    (+ 1 i)))))
	(setf (sql-crsr-ck cursor) n)
	(setf (dp-nmbr cursor) 0)
	(has-key-value-scnd cursor :returnpk returnpk))
      (cursor-first-x cursor :returnpk returnpk)
      ))

(defmethod cursor-last ((cursor sql-secondary-cursor))
  (cursor-last-x cursor)
)
(defmethod cursor-last-x ((cursor sql-secondary-cursor) &key (returnpk nil))
  (unless (cursor-initialized-p cursor)
    (cursor-init cursor))
  (setf (sql-crsr-ck cursor) 
	(- (length (sql-crsr-ks cursor)) 1))
  (setf (dp-nmbr cursor) 
	(max 0
	(- (sql-get-from-clcn-cnt 
	    (cursor-oid cursor)
	    (get-current-key cursor)
	    (get-con (cursor-btree cursor))
	    )
	   1)))
  (assert (>= (dp-nmbr cursor) 0))
  (setf (cursor-initialized-p cursor) t)
  (has-key-value-scnd cursor :returnpk returnpk)
)



(defmethod cursor-prev-nodup ((cursor sql-secondary-cursor))
  (cursor-prev-nodup-x cursor)
)
(defmethod cursor-prev-nodup-x ((cursor sql-secondary-cursor) &key (returnpk nil))
  (declare (optimize (speed 3)))
  (if (cursor-initialized-p cursor)
      (progn
	(setf (sql-crsr-ck cursor) (- (sql-crsr-ck cursor) (+ 1 (dp-nmbr cursor))))
	(setf (dp-nmbr cursor) 
	      (max 0
	      (- (sql-get-from-clcn-cnt (cursor-oid cursor)
					(get-current-key cursor)
					(get-con (cursor-btree cursor))
		 ) 1)))
	(has-key-value-scnd cursor :returnpk returnpk))
      (cursor-last-x cursor :returnpk returnpk)))


(defmethod cursor-pnext-nodup ((cursor sql-secondary-cursor))
  (cursor-next-nodup-x cursor :returnpk t))

(defmethod cursor-pprev-nodup ((cursor sql-secondary-cursor))
  (cursor-prev-nodup-x cursor :returnpk t))

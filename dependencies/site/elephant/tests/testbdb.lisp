;;; testbdb.lisp
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


(defvar env)
(defvar db)

(defun prepare-bdb ()
  (ignore-errors 
    (delete-file (make-pathname :defaults (cdr *bdb-spec*)
				:name "testsbdb"))
    (delete-file (make-pathname :defaults (cdr *bdb-spec*)
				:name "log.0000000001")))
  (setq env (db-bdb::db-env-create))
  (db-bdb::db-env-open env (cdr *bdb-spec*) :create t :init-txn t :init-lock t 
	       :init-mpool t :init-log t :thread t
	       :recover-fatal t)
  
  (setq db (db-bdb::db-create env))
  (db-bdb::db-open db :file "testsbdb" :database "bar" :type DB-BDB::DB-BTREE
		      :auto-commit t :create t :thread t))

(deftest prepares-bdb
    (progn
      (setq db nil)
      (if (and (find-package :db-bdb)
	       (eq (first (elephant::controller-spec *store-controller*))
		   :BDB))
	  (finishes (prepare-bdb))
	  (progn 
	    (format t "Berkeley DB not loaded, so not runnning test prepares-bdb~%")
	    t)))
  t)

#|
(deftest put-alot
    (finishes
     (loop for key in keys
	   do
	   (db-bdb::db-put db key key :auto-commit t)))
  t)
  
(defun get-alot ()
  (loop for key in keys
	always (string= key (db-bdb::db-get db key))))

(deftest put-right (get-alot) t)

(deftest put-alot-b 
    (finishes
     (with-transaction (:environment env)
       (loop for key in keys
	     do
	     (db-bdb::db-put db key key))))
  t)

(deftest put-right-b (get-alot) t)
|#

(defun test-sequence1 ()
  (let ((seq (db-bdb::db-sequence-create db)))
    (db-bdb::db-sequence-set-cachesize seq 1000)
    (db-bdb::db-sequence-set-flags seq :seq-inc t :seq-wrap t)
    (db-bdb::db-sequence-set-range seq 0 most-positive-fixnum)
    (db-bdb::db-sequence-initial-value seq (- most-positive-fixnum 99))
    (db-bdb::db-sequence-open seq "testseq1" :create t :thread t)
    (loop for i = (db-bdb::db-sequence-get-fixnum seq 1 :txn-nosync t)
	  for j from (- most-positive-fixnum 99) to most-positive-fixnum
	  while (> i 0)
	  do
	  (assert (= i j))
	  finally (db-bdb::db-sequence-remove seq))))

(deftest test-seq1
    (if (not db)
	(progn 
	  (format t "Berkeley db not loaded, so not runnning test test-seq1~%")
	  t)
	(finishes (test-sequence1)))
  t)

(defun test-sequence2 ()
  (let ((seq (db-bdb::db-sequence-create db)))
    (db-bdb::db-sequence-set-cachesize seq 1000)
    (db-bdb::db-sequence-set-flags seq :seq-dec t :seq-wrap t)
    (db-bdb::db-sequence-set-range seq most-negative-fixnum 0)
    (db-bdb::db-sequence-initial-value seq (+ most-negative-fixnum 99))
    (db-bdb::db-sequence-open seq "testseq2" :create t :thread t)
    (loop for i = (db-bdb::db-sequence-get-fixnum seq 1 :txn-nosync t)
	  for j from (+ most-negative-fixnum 99) downto most-negative-fixnum
	  while (< i 0)
	  do
	  (assert (= i j))
	  finally (db-bdb::db-sequence-remove seq))))

(deftest test-seq2
    (if (not db)
	(progn 
	  (format t "BDB db not valid, so not runnning test test-seq2~%")
	  t)
	(finishes (test-sequence2)))
  t)

(defun cleanup-bdb ()
  (db-bdb::db-close db)
  (db-bdb::db-env-dbremove env "testsbdb" :database "bar")
  (db-bdb::db-env-close env)
  (setq env (db-bdb::db-env-create))
  (db-bdb::db-env-remove env "test"))

(deftest cleansup-bdb
    (if (not db)
	(progn 
	  (format t "Berkeley DB not open, so not runnning test cleanup-bdb~%")
	  t)
	(finishes (cleanup-bdb)))
  t)

;;(unuse-package "DB-BDB")
;;(use-package "ELE")

#|
(defun txn-alot (iters)
  (loop for i from 1 to iters
	do
	(with-transaction (:environment env)
	  (db-put db "mykey" "mydatum"))))	     

(defun get-alot-b (keys)
  (loop for key in keys
	do
	(db-get-buffered db key)))

(defun foreign-test (ln iters)
  (with-transaction (:environment env)
    (loop for i fixnum from 1 to iters
	  with write-buf of-type array-or-pointer-char = (uffi:allocate-foreign-object :char ln)
	  with str string = (make-string ln :initial-element #\c)
	  with key-buf of-type array-or-pointer-char = (uffi:allocate-foreign-object :char 2)
	  do
	  (copy-str-to-buf "fs" key-buf)
	  (copy-str-to-buf str write-buf)
	  (db-put-buffered db key-buf 2 write-buf ln)
	  finally 
	  (progn 
	    (uffi:free-foreign-object write-buf)
	    (uffi:free-foreign-object key-buf)))))

(defun cstring-test (ln iters)
  (with-transaction (:environment env)
    (loop for i fixnum from 1 to iters
	  with str string = (make-string ln :initial-element #\c)
	  do
	  (db-put db "fs" str))))
|#


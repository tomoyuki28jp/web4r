;;; testsorter.lisp
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

(in-package "ELE")

(def-function ("lisp_cmp_test" lisp-compare)
    ((a array-or-pointer-char)
     (as :unsigned-int)
     (b array-or-pointer-char)
     (bs :unsigned-int))
  :returning :int)

(defun lisp-cmp (a b)
  (with-buffer-streams (as bs)
    (buffer-write-int 0 as)
    (buffer-write-int 0 bs)
    (serialize a as)
    (serialize b bs)
    (< (lisp-compare (buffer-stream-buffer as) 
		     (db-bdb::buffer-stream-size as)
		     (buffer-stream-buffer bs) 
		     (db-bdb::buffer-stream-size bs)) 0)))

(defun lisp-cmp1 (a b)
  (with-buffer-streams (as bs)
    (buffer-write-int 0 as)
    (buffer-write-int 0 bs)
    (serialize a as)
    (serialize b bs)
    (lisp-compare (buffer-stream-buffer as) 
		  (db-bdb::buffer-stream-size as)
		  (buffer-stream-buffer bs) 
		  (db-bdb::buffer-stream-size bs))))

(defvar myvec)
(setq myvec (list 1 1/2 (- (expt 10 29)) (expt 10 29) most-positive-fixnum 
		  "aa" "ab" "AA" "AB" "aB" "Ba" "ba" "a test string"
		  'foo 'bar (make-symbol "BAZ") (make-symbol "quxx")
		  "Another string" "zzzzz" "zzzz2"
		  '(1 2 3) (make-hash-table) #p"a path" #p"another path"
		  -1.0d0 most-negative-double-float))

(def-function ("read_num" %read-num)
    ((buf array-or-pointer-char))
  :returning :double)

(defun read-num (num)
  (declare #-elephant-without-optimizations (optimize (speed 3))
	   (type integer num))
  (with-buffer-streams (nb)
    (serialize num nb)
    (%read-num (buffer-stream-buffer nb))))

(defun num-test (num) 
  (declare #-elephant-without-optimizations (optimize (speed 3))
	   (type integer num))
  (loop with i of-type double-float = 0.0d0
	for j fixnum from 0 below (ceiling (/ (integer-length num) 32))
	for bs = (byte 32 (* j 32))
	do
	(setq i (+ i (* (expt 2 (* j 32)) (coerce (ldb bs num) 'double-float))))
	finally
	(return (= i (coerce num 'double-float)))))


(defun find-bad-num (bot top)
  (declare #-elephant-without-optimizations (optimize (speed 3))
	   (type integer bot top))
  (cond ((= bot top) bot)
	((= bot (- top 1))
	 (if (num-test bot) top bot))
	(t
	 (let ((middle (ceiling (/ (+ top bot) 2))))
	   (if (num-test middle)
	       (find-bad-num middle top)
	       (find-bad-num bot middle))))))
	
(defun rfind-bad-num (bot top)
  (declare #-elephant-without-optimizations (optimize (speed 3))
	   (type integer bot top))
  (cond ((= bot top) bot)
	((= bot (- top 1))
	 (if (num-test bot) top bot))
	(t
	 (let ((middle (+ bot (random (- top bot)))))
	   (if (num-test middle)
	       (find-bad-num middle top)
	       (find-bad-num bot middle))))))

(defun loopit ()
  (loop for i from 2 to 63 do
	(loop for j from 1 to i do
	      (loop for k from 0 to j 
		    do (when (num-test
	   	
(defun loopit ()
  (loop for i from 2 to 63 
	for j =
	(loop for j from 1 to i
	      for k =
	      (loop for k from 0 to j 
		    unless (num-test (+ (expt 2 64) 
					(expt 2 i) 
					(expt 2 j) 
					(expt 2 k)))
		    do (return-from loopit (values i j k))))))


(defconstant lowest-bad-num? 18455751272964294657)
	   	

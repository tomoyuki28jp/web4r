;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; test.lisp -- Internal tests for elephant lisp backend
;;; 
;;; part of
;;;
;;; Elephant Object Oriented Database: Common Lisp Backend
;;;
;;; Copyright (c) 2007 by Ian Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant Lisp Backend users are granted the rights to distribute
;;; and use this software as governed by the terms of the Lisp Lesser
;;; GNU Public License (http://opensource.franz.com/preamble.html),
;;; also known as the LLGPL.

(in-package :db-lisp)

(defparameter *btree-file* "btree-test-file.db")

(defvar btree nil)
(defvar root nil)
(defvar idx1 nil)
(defvar idx2 nil)
(defvar leaf1 nil)
(defvar leaf2 nil)

(defun initialize-btree-tests ()
  (setf btree (open-lisp-btree *btree-file* :if-exists :overwrite))
  (setf root (btree-root btree))
  btree)

(defun cleanup-btree-tests ()
  (close-lisp-btree btree)
  (setf root nil
	idx1 nil
	idx2 nil
	leaf1 nil
	leaf2 nil))

(defun dump-keys (page)
  (with-buffer-streams (k)
    (scan-page-keys (k pointer position btree page)
      (format t "k: ~A  ptr: ~A  pos: ~A~%" 
	      (elephant-serializer2::deserialize k nil)
	      pointer position))))

(defmacro with-key ((bs num) &body body)
  `(with-buffer-stream (,bs)
     (elephant-serializer2::serialize ,num ,bs)
     ,@body))
     






    


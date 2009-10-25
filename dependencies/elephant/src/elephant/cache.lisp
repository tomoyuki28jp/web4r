;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; migrate.lisp -- Migrate between repositories
;;; 
;;; Initial version 8/26/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package "ELEPHANT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Portable value-weak hash-tables for the cache: when the
;;; values are collected, the entries (keys) should be
;;; flushed from the table too

(defun make-cache-table (&rest args)
  "Make a values-weak hash table: when a value has been
collected, so are the keys."
  #+(or cmu sbcl scl)
  (apply #'make-hash-table args)
  #+allegro
  (apply #'make-hash-table :values :weak args)
  #+lispworks
  (apply #'make-hash-table :weak-kind :value args)
  #+openmcl
  (apply #'make-hash-table :weak :value args)
  #-(or cmu sbcl scl allegro lispworks)
  (apply #'make-hash-table args)
  )

#+openmcl
(defclass cleanup-wrapper ()
  ((cleanup :accessor cleanup :initarg :cleanup)
   (value :accessor value :initarg :value)))

#+openmcl
(defmethod ccl:terminate ((c cleanup-wrapper))
  (funcall (cleanup c)))

(defun get-cache (key cache)
  "Get a value from a cache-table."
  #+(or cmu sbcl)
  (let ((val (gethash key cache)))
    (if val (values (weak-pointer-value val) t)
	(values nil nil)))
  #+openmcl 
  (let ((wrap (gethash key cache)))
    (if wrap (values (value wrap) t)
	(values nil nil)))
  #+(or allegro lispworks)
  (gethash key cache)
  )

(defun make-finalizer (key cache)
  #+(or cmu sbcl)
  (lambda () (remhash key cache))
  #+(or allegro openmcl)
  (lambda (obj) (declare (ignore obj)) (remhash key cache))
  )

(defun setf-cache (key cache value)
  "Set a value in a cache-table."
  #+(or cmu sbcl)
  (let ((w (make-weak-pointer value)))
    (finalize value (make-finalizer key cache))
    (setf (gethash key cache) w)
    value)
  #+openmcl
  (let ((w (make-instance 'cleanup-wrapper :value value
			  :cleanup (make-finalizer key cache))))
    (ccl:terminate-when-unreachable w)
    (setf (gethash key cache) w)
    value)
  #+allegro
  (progn
    (excl:schedule-finalization value (make-finalizer key cache))
    (setf (gethash key cache) value))
  #+lispworks
  (setf (gethash key cache) value)
  )

(defsetf get-cache setf-cache)

;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; elephant.asd -- ASDF system definition for elephant
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
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-user)

;; Forward def
(defpackage elephant-system
  (:use :cl :asdf)
  (:export :elephant-c-source :compiler-options :foreign-libraries-to-load-first :get-config-option))

(defpackage ele-bdb-system
  (:use :cl :asdf :elephant-system))

(in-package :ele-bdb-system)

;;
;; Compile bdb lib and load libraries
;;


#+(or windows mswindows)
(defun path-for-cygwin (path)
"DOS pathname -> cygwin pathname. Replace backslashes with slashes and drive letter with directory.  
e.g. \"C:\\dir\\\" -> \"/cygdrive/C/dir/\" "
  (let* ((result (namestring path))
         (colon-pos (position #\: result))
         (drive-letter (char result (1- colon-pos))))
    (setf (char result (1- colon-pos)) #\/)
    (setf (char result colon-pos) drive-letter)
    (setf result (concatenate 'string "/cygdrive" result))
    (substitute #\/ #\\ result)))

;; Forward def
(defclass elephant-c-source (c-source-file) ())

(defclass bdb-c-source (elephant-c-source) ())

(defmethod compiler-options ((compiler (eql :gcc)) (c bdb-c-source) &key &allow-other-keys)
  (append (library-directories c)
	  (call-next-method)
	  (list (format nil "-l~A" (get-db-name c)))))

(defun get-db-name (c)
  (subseq (pathname-name 
	   (make-pathname :defaults (get-config-option :berkeley-db-lib c)) )
	  3))

(defmethod compiler-options ((compiler (eql :cygwin)) (c bdb-c-source) &key &allow-other-keys)
  (append (library-directories c)
	  (list "-ldb45")
	  (call-next-method)))

(defun library-directories (c)
  (let ((include (make-pathname :defaults (get-config-option :berkeley-db-include-dir c)))
	(lib (make-pathname :defaults (get-config-option :berkeley-db-lib-dir c))))
    #+(or windows mswindows)
    (list (format nil "-L'~A'" (path-for-cygwin lib))
	  (format nil "-I'~A'" (path-for-cygwin include)))
    #-(or windows mswindows)
    (list (format nil "-L~A" lib) (format nil "-I~A" include))))

(defmethod foreign-libraries-to-load-first ((c bdb-c-source))
  (remove-if #'(lambda (x) (null (car x)))
	     (list (cons (get-config-option :pthread-lib c) "pthread")
	      (cons (get-config-option :berkeley-db-lib c)
		    (get-config-option :berkeley-db-lib c)))))

;;
;; System definition
;;

(defsystem ele-bdb
  :name "elephant"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.6.0"
  :maintainer "Ben Lee <blee@common-lisp.net>"
  :licence "LLGPL"
  :description "Object database for Common Lisp"
  :long-description "An object-oriented database based on Berkeley DB, for CMUCL/SBCL, OpenMCL, and Allegro."
  :components
  ((:module :src
	    :components
	    ((:module :db-bdb
		      :components
		      ((:file "package")
		       (:bdb-c-source "libberkeley-db")
		       (:file "berkeley-db")
		       (:file "bdb-controller")
		       (:file "bdb-slots")
		       (:file "bdb-collections")
		       (:file "bdb-transactions"))
		      :serial t))))
  :depends-on (:uffi :elephant))


;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          uffi-tests.asd
;;;; Purpose:       ASDF system definitionf for uffi testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; *************************************************************************

(defpackage #:uffi-tests-system
  (:use #:asdf #:cl))
(in-package #:uffi-tests-system)

(operate 'load-op 'uffi)

(defvar *library-file-dir* (append (pathname-directory *load-truename*)
				   (list "tests")))

(defclass uffi-test-source-file (c-source-file)
  ())

(defmethod output-files ((o compile-op) (c uffi-test-source-file))
  (let* ((library-file-type
	  (funcall (intern (symbol-name'#:default-foreign-library-type)
			   (symbol-name '#:uffi))))
	 (found
	  (some #'(lambda (dir)
		    (probe-file (make-pathname
				 :directory dir
				 :name (component-name c)
				 :type library-file-type)))
		'((:absolute "usr" "lib" "uffi")))))
    (list (if found
	      found
	      (make-pathname :name (component-name c)
			     :type library-file-type
			     :directory *library-file-dir*)))))

(defmethod perform ((o load-op) (c uffi-test-source-file))
  nil) ;;; library will be loaded by a loader file

(defmethod operation-done-p ((o load-op) (c uffi-test-source-file))
  (and (symbol-function (intern (symbol-name '#:cs-count-upper)
				(find-package '#:uffi-tests)))
       t))

(defmethod perform ((o compile-op) (c uffi-test-source-file))
  (unless (operation-done-p o c)
    #-(or win32 win64 windows mswindows)
    (unless (zerop (run-shell-command
		    #-freebsd "cd ~A; make"
		    #+freebsd "cd ~A; gmake"
		    (namestring (make-pathname :name nil
					       :type nil
					       :directory *library-file-dir*))))
      (error 'operation-error :component c :operation o))))

(defmethod operation-done-p ((o compile-op) (c uffi-test-source-file))
  (or (and (probe-file #p"/usr/lib/uffi/uffi-c-test.so") t)
      (let ((lib (make-pathname :defaults (component-pathname c)
				:type (uffi:default-foreign-library-type))))
	(and (probe-file lib)
	     (> (file-write-date lib) (file-write-date (component-pathname c)))))))

(defsystem uffi-tests
    :depends-on (:uffi)
    :components
    ((:module tests
	      :components
	      ((:file "rt")
	       (:file "package" :depends-on ("rt"))
	       (:uffi-test-source-file "uffi-c-test" :depends-on ("package"))
	       (:file "strtol" :depends-on ("package"))
	       (:file "atoifl" :depends-on ("package"))
	       (:file "getenv" :depends-on ("package"))
	       (:file "gethostname" :depends-on ("package"))
	       (:file "union" :depends-on ("package"))
	       (:file "arrays" :depends-on ("package"))
	       (:file "structs" :depends-on ("package"))
	       (:file "objects" :depends-on ("package"))
	       (:file "time" :depends-on ("package"))
	       (:file "foreign-loader" :depends-on ("package" "uffi-c-test"))
	       (:file "uffi-c-test-lib" :depends-on ("foreign-loader"))
	       (:file "compress" :depends-on ("foreign-loader"))
	       (:file "casts" :depends-on ("foreign-loader"))
	       (:file "foreign-var" :depends-on ("foreign-loader"))
	       ))))

(defmethod perform ((o test-op) (c (eql (find-system :uffi-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

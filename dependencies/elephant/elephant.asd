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

(defpackage elephant-system
  (:use :cl :asdf)
  (:export :elephant-c-source :compiler-options :foreign-libraries-to-load-first :get-config-option))

(in-package :elephant-system)

;;
;; Simple lisp/asdf-based make utility for elephant c files
;;

(defgeneric compiler-options (compiler c-source-file &key input-file output-file)
  (:documentation "Returns a list of options to pass to <compiler>"))

(defgeneric foreign-libraries-to-load-first (c-source-file)
  (:documentation "Provides an alist of foreign-libraries to load and the modules to load them into.  Similar to (input-files load-op), but much more specific"))

(defun uffi-funcall (fn &rest args)
  "Simplify uffi funcall, first ensure uffi is loaded"
  (unless (find-package :uffi)
    (asdf:operate 'asdf:load-op :uffi))
  (apply (find-symbol (symbol-name fn) (symbol-name :uffi)) args))

;;
;; User parameters (bdb root and pthread, if necessary)
;;

(defun get-config-option (option component)
  (let ((filespec (make-pathname :defaults (asdf:component-pathname (asdf:component-system component))
				 :name "my-config"
				 :type "sexp")))
    (unless (probe-file filespec)
      (error "Missing file. Copy config.sexp in elephant root
directory to my-config.sexp and edit it appropriately."))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))

;;
;; Supported C compilers
;;


(defvar *c-compilers*
  '((:gcc . "gcc")
    (:cygwin . "gcc")
    (:msvc . ""))
  "Associate compilers with platforms for compiling libmemutil/libsleepycat")

(defun c-compiler (comp)
  (get-config-option :compiler comp))

(defun c-compiler-path (comp)
  (let* ((compiler (get-config-option :compiler comp))
	 (entry (assoc compiler *c-compilers*)))
    (if entry
	(cdr entry)
	(error "Cannot find compiler path for config.sexp :compiler option: ~A" compiler))))

;;
;; Basic utilities for elephant c files
;;

(defclass elephant-c-source (c-source-file) ())

;; COMPILE

(defmethod output-files ((o compile-op) (c elephant-c-source))
  "Compute the output files (for dependency tracking), here we assume
   a library with the same name and a platform dependant extension"
  (list (make-pathname :name (component-name c)
		       :type (uffi-funcall :default-foreign-library-type)
		       :defaults (component-pathname c))))

(defmethod perform ((o compile-op) (c elephant-c-source))
  "Run the appropriate compiler for this platform on the source, getting
   the specific options from 'compiler-options method.  Default options 
   can be overridden or augmented by subclass methods"
  (unless (get-config-option :prebuilt-libraries c)
  #+(or mswindows windows)
  (progn
    (let* ((pathname (component-pathname c))
           (directory #+lispworks (make-pathname :host (pathname-host pathname) :directory (pathname-directory pathname))
                      #-lispworks (make-pathname :device (pathname-device pathname) :directory (pathname-directory pathname)))
           (stdout-lines) (stderr-lines) (exit-status))
      (let ((command (format nil "~A ~{~A ~}"
                       (c-compiler-path c)
                       (compiler-options (c-compiler c) c
                                         :input-file (format nil "\"~A\"" (namestring pathname))
                                         :output-file nil
                                         :library nil))))
        #+allegro (multiple-value-setq (stdout-lines stderr-lines exit-status) 
		    (excl.osi:command-output command :directory directory))
        #+lispworks (setf exit-status (system:call-system-showing-output command :current-directory directory))
        (unless (zerop exit-status)
          (error 'operation-error :component c :operation o)))

      (let ((command (format nil "dlltool -z ~A --export-all-symbols -e exports.o -l ~A ~A"
                       (format nil "\"~A\"" (namestring (make-pathname :type "def" :defaults pathname)))
                       (format nil "\"~A\"" (namestring (make-pathname :type "lib" :defaults pathname)))
                       (format nil "\"~A\"" (namestring (make-pathname :type "o" :defaults pathname))))))
        #+allegro (multiple-value-setq (stdout-lines stderr-lines exit-status) 
                      (excl.osi:command-output command :directory directory))
        #+lispworks (setf exit-status (system:call-system-showing-output command :current-directory directory))
        (unless (zerop exit-status)
          (error 'operation-error :component c :operation o)))

      (let ((command (format nil "~A ~{~A ~}"  ;; -I~A -L~A -l~A
                             (c-compiler-path c)
                             (compiler-options (c-compiler c) c
                                               :input-file
                                               (list (format nil "\"~A\"" (namestring 
                                                                           (make-pathname :type "o" :defaults pathname)))
                                                     "exports.o")
                                               :output-file (format nil "\"~A\"" (first (output-files o c)))
                                               :library t))))
        #+allegro (multiple-value-setq (stdout-lines stderr-lines exit-status)
                      (excl.osi:command-output command :directory directory))
        #+lispworks (setf exit-status (system:call-system-showing-output command :current-directory directory))
        (unless (zerop exit-status)
          (error 'operation-error :component c :operation o)))))

  #-(or mswindows windows)
  (unless (zerop (run-shell-command
		  "~A ~{~A ~}"
		  (c-compiler-path c)
		  (compiler-options (c-compiler c) c
				    :input-file (namestring (component-pathname c))
				    :output-file (namestring (first (output-files o c))))))
    (error 'operation-error :component c :operation o))))

#|
(defmethod perform ((o compile-op) (c elephant-c-source))
  "Run the appropriate compiler for this platform on the source, getting
   the specific options from 'compiler-options method.  Default options 
   can be overridden or augmented by subclass methods"
  #+(or mswindows windows)
  (progn
    (let ((pathname (component-pathname c)))
      (unless (zerop (run-shell-command
		      (format nil "~A ~{~A ~}"
			      (c-compiler-path c)
			      (compiler-options (c-compiler c) c
						:input-file (format nil "\"~A\"" (namestring pathname))
						:output-file nil
						:library nil))))
	(error 'operation-error :component c :operation o))
      (unless (zerop (run-shell-command
		      (format nil "dlltool -z ~A --export-all-symbols -e exports.o -l ~A ~A"
			      (format nil "\"~A\"" (namestring (make-pathname :type "def" :defaults pathname)))
			      (format nil "\"~A\"" (namestring (make-pathname :type "lib" :defaults pathname)))
			      (format nil "\"~A\"" (namestring (make-pathname :type "o" :defaults pathname))))))
	(error 'operation-error :component c :operation o))
      (unless (zerop (run-shell-command
		      (format nil "~A ~{~A ~} -I~A -L~A -l~A"
			      (c-compiler-path c)
			      (compiler-options (c-compiler c) c
						:input-files 
						(list (format nil "\"~A\"" (namestring 
						       (make-pathname :type "o" :defaults pathname)))
						      "exports.o")
						:output-file (format nil "\"~A\"" (first (output-files o c)))
						:library t))))
	(error 'operation-error :component c :operation o))))
  #-windows
  (unless (zerop (run-shell-command
		  "~A ~{~A ~}"
		  (c-compiler-path c)
		  (compiler-options (c-compiler c) c
				    :input-file (namestring (component-pathname c))
				    :output-file (namestring (first (output-files o c))))))
    (error 'operation-error :component c :operation o)))
|#

;;Cygwin compile script:
;;gcc -mno-cygwin -mwindows -std=c99 -c libmemutil.c
;;dlltool -z libmeutil.def --export-all-symbols -e exports.o -l libmemutil.lib libmemutil.o
;;gcc -shared -mno-cygwin -mwindows libmemutil.o exports.o -o libmemutil.dll

(defmethod operation-done-p ((o compile-op) (c elephant-c-source))
  "Is the first generated library more recent than the source file?"
  (let ((lib (first (output-files o c))))
    (and (probe-file (component-pathname c))
	 (probe-file lib)
	 (> (file-write-date lib) (file-write-date (component-pathname c))))))

(defmethod compiler-options ((compiler (eql :gcc)) (c elephant-c-source) &key input-file output-file &allow-other-keys)
  "Default compile and link options to create a library; no -L or -I options included; math lib as default"
  (unless (and input-file output-file)
    (error "Must specify both input and output files"))
  (list
   #-(or darwin macosx darwin-host) "-shared"
   #+(or darwin macosx darwin-host) "-bundle"
   #+(and X86-64 (or macosx darwin darwin-host)) "-arch x86_64"
   #+(and X86-64 linux) "-march=x86-64"
   "-fPIC"
   "-Wall"
   "-O2"
   "-g"
   input-file
   "-o" output-file
   "-lm"))

(defmethod compiler-options ((compiler (eql :cygwin)) (c elephant-c-source) &key input-file output-file library &allow-other-keys)
  (unless input-file
    (error "Must specify both input files"))
  (append
   (when library (list "-shared"))
   (list 
    "-mno-cygwin"
    "-mwindows"
    "-Wall")
    (unless library (list "-c -std=c99"))
    (if (listp input-file) input-file (list input-file))
    (when output-file (list "-o" output-file))))

(defmethod compiler-options ((compiler (eql :msvc)) (c elephant-c-source) &key input-file output-file)
  (declare (ignore input-file output-file))
  (error "MSVC compiler option not supported yet"))

;; LOAD

(defmethod perform ((o load-op) (c elephant-c-source))
  ;; Load any required external libraries
  (let ((libs (foreign-libraries-to-load-first c)))
    (dolist (file+module libs)
      (destructuring-bind (file . module) file+module
	(format t "Loading ~A~%" file)
	(or (uffi-funcall :load-foreign-library file :module module)
	    (error "Could not load ~A into ~A" file module)))))
  ;; Load the compiled libraries
  (dolist (file (output-files (make-instance 'compile-op) c))
    (format t "Attempting to load ~A...~%" (file-namestring file))
    (if (and (probe-file file)
	     (not (get-config-option :prebuilt-libraries c)))
	(progn
	  (or (uffi-funcall :load-foreign-library file :module (component-name c))
	      (error "Could not load ~A" file))
	  (format t "Loaded ~A~%" file))
	(let* ((root-dir (asdf:component-pathname (asdf:component-system c)))
	       (root-library-file (merge-pathnames (file-namestring file) root-dir)))
	  (or (uffi-funcall :load-foreign-library 
			    root-library-file
			    :module (component-name c))
	      (error "Output file ~A not found in elephant root" (namestring root-library-file)))
	  (format t "Loaded ~A~%" root-library-file)))))
	

(defmethod operation-done-p ((o load-op) (c elephant-c-source))
  nil)

(defmethod foreign-libraries-to-load-first ((c elephant-c-source))
  nil)

;;
;; System definition
;;

(defsystem elephant
  :name "elephant"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.9"
  :maintainer "Robert Read <read@robertlread.net>"
  :licence "LLGPL"
  :description "Object database for Common Lisp"
  :long-description "An object-oriented database based on Berkeley DB, for CMUCL/SBCL, OpenMCL, and Allegro."
  :components
  ((:module :src
	    :components
	    ((:module utils
		      :components
		      ((:file "package")
		       (:file "convenience")
		       (:file "locks")
		       (:file "os"))
		      :serial t)
	     (:module memutil
		      :components
		      ((:elephant-c-source "libmemutil")
		       (:file "memutil"))
		      :serial t
		      :depends-on (utils))
	     (:module elephant
		      :components
		      ((:file "package")
		       #+cmu (:file "cmu-mop-patches")
		       #+openmcl (:file "openmcl-mop-patches")
		       (:file "variables")
		       (:file "transactions")
		       (:file "metaclasses")
		       (:file "classes")
		       (:file "cache")
		       (:file "serializer")
		       (:file "controller")
		       (:file "collections")
		       (:file "pset")
		       (:file "classindex-utils")
		       (:file "classindex")
		       (:file "serializer1") ;; 0.6.0 db's
		       (:file "serializer2") ;; 0.6.1 db's
		       (:file "unicode2")
		       (:file "migrate")
;;		       (:file "query")
		       (:file "data-store-api"))
		      :serial t
		      :depends-on (memutil utils)))))
  :serial t
  :depends-on (:uffi :cl-base64))

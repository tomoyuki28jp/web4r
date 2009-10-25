;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          libraries.lisp
;;;; Purpose:       UFFI source to load foreign libraries
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)

(defvar *loaded-libraries* nil
  "List of foreign libraries loaded. Used to prevent reloading a library")

(defun default-foreign-library-type ()
  "Returns string naming default library type for platform"
  #+(or win32 win64 cygwin mswindows windows) "dll"
  #+(or macosx darwin ccl-5.0) "dylib"
  #-(or win32 win64 cygwin mswindows windows macosx darwin ccl-5.0) "so"
)

(defun foreign-library-types ()
  "Returns list of string naming possible library types for platform, sorted by preference"
  #+(or win32 win64 windows mswindows) '("dll" "lib")
  #+(or macosx darwin ccl-5.0) '("dylib" "bundle")
  #-(or win32 win64 windows mswindows macosx darwin ccl-5.0) '("so" "a" "o")
)

(defun find-foreign-library (names directories &key types drive-letters)
  "Looks for a foreign library. directories can be a single
string or a list of strings of candidate directories. Use default
library type if type is not specified."
  (unless types
    (setq types (foreign-library-types)))
  (unless (listp types)
    (setq types (list types)))
  (unless (listp names)
    (setq names (list names)))
  (unless (listp directories)
    (setq directories (list directories)))
  #+(or win32 win64 windows mswindows)
  (unless (listp drive-letters)
    (setq drive-letters (list drive-letters)))
  #-(or win32 win64 windows mswindows)
  (setq drive-letters '(nil))
  (dolist (drive-letter drive-letters)
    (dolist (name names)
      (dolist (dir directories)
        (dolist (type types)
          (let ((path (make-pathname
                       #+lispworks :host
                       #+lispworks (when drive-letter drive-letter)
                       #-lispworks :device
                       #-lispworks (when drive-letter drive-letter)
                       :name name
                       :type type
                       :directory
                       (etypecase dir
                         (pathname
                          (pathname-directory dir))
                         (list
                          dir)
                         (string
                          (pathname-directory
                           (parse-namestring dir)))))))
            (when (probe-file path)
              (return-from find-foreign-library path)))))))
   nil)


(defun load-foreign-library (filename &key module supporting-libraries
                                           force-load)
  #+(or allegro openmcl digitool sbcl) (declare (ignore module supporting-libraries))
  #+(or cmu scl) (declare (ignore module))
  #+lispworks (declare (ignore supporting-libraries))

  (flet ((load-failure ()
           (error "Unable to load foreign library \"~A\"." filename)))
    (when (and filename (or (null (pathname-directory filename))
                            (probe-file filename)))
      (if (pathnamep filename)    ;; ensure filename is a string to check if already loaded
          (setq filename (namestring (if (null (pathname-directory filename))
                                         filename
                                         ;; lispworks treats as UNC, so use truename
                                         #+(and lispworks mswindows) (truename filename)
                                         #-(and lispworks mswindows) filename))))

      (if (and (not force-load)
               (find filename *loaded-libraries* :test #'string-equal))
          t ;; return T, but don't reload library
      (progn
        #+cmu
        (let ((type (pathname-type (parse-namestring filename))))
          (if (string-equal type "so")
              (unless
                  (sys::load-object-file filename)
                (load-failure))
              (alien:load-foreign filename
                                  :libraries
                                  (convert-supporting-libraries-to-string
                                   supporting-libraries))))
        #+scl
        (let ((type (pathname-type (parse-namestring filename))))
          (alien:load-foreign filename
                              :libraries
                              (convert-supporting-libraries-to-string
                               supporting-libraries)))
        #+sbcl
        (handler-case (sb-alien::load-1-foreign filename)
          (sb-int:unsupported-operator (c)
            (if (fboundp (intern "LOAD-SHARED-OBJECT" :sb-alien))
                (funcall (intern "LOAD-SHARED-OBJECT" :sb-alien) filename)
                (error c))))

        #+lispworks (fli:register-module module :real-name filename
                                         :connection-style :immediate)
        #+allegro (load filename)
        #+openmcl (ccl:open-shared-library filename)
        #+digitool (ccl:add-to-shared-library-search-path filename t)

        (push filename *loaded-libraries*)
        t)))))

(defun convert-supporting-libraries-to-string (libs)
  (let (lib-load-list)
    (dolist (lib libs)
      (push (format nil "-l~A" lib) lib-load-list))
    (nreverse lib-load-list)))

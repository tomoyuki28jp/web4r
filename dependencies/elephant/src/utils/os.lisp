;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; os.lisp -- A set of convenience functions for cross-platform os functions 
;;;            that elephant requires
;;; 
;;; By Ian Eslick, <ieslick common-lisp net>
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

(in-package :elephant-utils)

(defmacro in-directory ((dir) &body body)
  `(progn 
     (#+sbcl sb-posix:chdir 
      #+cmu unix:unix-chdir
      #+allegro excl:chdir
      #+lispworks hcl:change-directory
      #+openmcl ccl:cwd
      ,dir)
     ,@body))

(defun launch-background-program (directory program &key (args nil))
  "Launch a program in a specified directory - not all shell interfaces
   or OS's support this"
  #+(and allegro (not mswindows))
  (multiple-value-bind (in out pid)
      (excl:run-shell-command (concat-separated-strings " " (list program) args)
			      :wait nil
			      :directory directory)
    (declare (ignore in out))
    pid)
  #+(and sbcl unix)
  (in-directory (directory)
    (sb-ext:run-program program args :wait nil))
  #+cmu 
  (in-directory (directory)
      (ext:run-program program args :wait nil))
  #+openmcl
  (in-directory (directory)
    (ccl:run-program program args :wait nil))
  #+lispworks
  (funcall #'sys::call-system
	 (format nil "~a~{ '~a'~} &" program args)
	 :current-directory directory
	 :wait nil)
  )

(defun kill-background-program (process-handle)
  #+(and allegro (not mswindows))
  (progn (excl.osi:kill process-handle 9)
	 (system:reap-os-subprocess :pid process-handle))
  #+(and sbcl unix)
  (sb-ext:process-kill process-handle 9)
  #+openmcl
  (ccl:signal-external-process process-handle 9)
;;  #+lispworks
;;  (apply #'sys::call-system
;;	 (format nil "kill ~A -9" process-handle)
;;	 :current-directory directory
;;	 :wait t)
  )


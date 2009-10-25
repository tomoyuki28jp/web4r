;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gethostname.cl
;;;; Purpose:       UFFI Example file to get hostname of system
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)


;;; This example is inspired by the example on the CL-Cookbook web site

(uffi:def-function ("gethostname" c-gethostname)
    ((name (* :unsigned-char))
     (len :int))
  :returning :int)

(defun gethostname ()
  "Returns the hostname"
  (let* ((name (uffi:allocate-foreign-string 256))
         (result-code (c-gethostname name 256))
         (hostname (when (zerop result-code)
                     (uffi:convert-from-foreign-string name))))
    (uffi:free-foreign-object name)
    (unless (zerop result-code)
      (error "gethostname() failed."))
    hostname))

(defun gethostname2 ()
  "Returns the hostname"
  (uffi:with-foreign-object (name '(:array :unsigned-char 256))
    (if (zerop (c-gethostname (uffi:char-array-to-pointer name) 256))
        (uffi:convert-from-foreign-string name)
        (error "gethostname() failed."))))

#+examples-uffi
(progn
  (format t "~&Hostname (technique 1): ~A" (gethostname))
  (format t "~&Hostname (technique 2): ~A" (gethostname2)))

#+test-uffi
(progn
  (let ((hostname1 (gethostname))
        (hostname2 (gethostname2)))

    (util.test:test (and (stringp hostname1) (stringp hostname2)) t
                    :fail-info "gethostname not string")
    (util.test:test (and (not (zerop (length hostname1)))
                         (not (zerop (length hostname2)))) t
                         :fail-info "gethostname length 0")
    (util.test:test (string= hostname1 hostname1) t
                    :fail-info "gethostname techniques don't match"))
  )



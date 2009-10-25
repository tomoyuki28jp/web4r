;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gethostname.lisp
;;;; Purpose:       UFFI Example file to get hostname of system
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)


;;; This example is inspired by the example on the CL-Cookbook web site

(eval-when (:compile-toplevel :load-toplevel :execute)
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
          (error "gethostname() failed.")))))

(deftest :gethostname.1 (stringp (gethostname)) t)
(deftest :gethostname.2 (stringp (gethostname2)) t)
(deftest :gethostname.3 (plusp (length (gethostname))) t)
(deftest :gethostname.4 (plusp (length (gethostname2))) t)
(deftest :gethostname.5 (string= (gethostname) (gethostname2)) t)




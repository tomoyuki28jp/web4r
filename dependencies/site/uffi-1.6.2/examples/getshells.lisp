;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getshells.cl
;;;; Purpose:       UFFI Example file to get lisp of legal shells
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)


(uffi:def-function "setusershell"
    nil
  :returning :void)

(uffi:def-function "endusershell"
    nil
  :returning :void)

(uffi:def-function "getusershell"
    nil
  :returning :cstring)

(defun getshells ()
  "Returns list of valid shells"
  (setusershell)
  (let (shells)
    (do ((shell (uffi:convert-from-cstring (getusershell))
                (uffi:convert-from-cstring (getusershell))))
        ((null shell))
      (push shell shells))
    (endusershell)
    (nreverse shells)))

#+examples-uffi
(format t "~&Shells: ~S" (getshells))


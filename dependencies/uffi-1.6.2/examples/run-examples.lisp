;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          run-examples.cl
;;;; Purpose:       Load and execute all examples for UFFI
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

#-uffi (asdf:oos 'asdf:load-op :uffi)

(pushnew :examples-uffi cl:*features*)

(flet ((load-test (name)
          (load (make-pathname :defaults *load-truename* :name name))))
  (load-test "c-test-fns")
  (load-test "arrays")
  (load-test "union")
  (load-test "strtol")
  (load-test "atoifl")
  (load-test "gettime")
  (load-test "getenv")
  (load-test "gethostname")
  (load-test "getshells")
  (load-test "compress"))

(setq cl:*features* (remove :examples-uffi cl:*features*))




;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; package.lisp -- package definition
;;; 
;;; Initial version 2/3/2007 by Ian Eslick
;;; <ieslick@common-lisp.net>
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

(in-package :cl-user)

(defpackage #:elephant-utils
  (:use common-lisp)
  (:export
   #:ele-make-lock
   #:ele-with-lock
   #:ele-make-fast-lock
   #:ele-with-fast-lock
   #:ele-thread-hash-key
   #:launch-background-program
   #:kill-background-program
   #:do-subsets
   #:subsets
   #:remove-keywords
   #:with-gensyms
   #:ifret
   #:aif
   #:awhen
   #:mklist
   #:it
   #:remove-indexed-element-and-adjust))

;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-lisp.asd -- ASDF system definition for elephant lisp backend
;;; 
;;; part of
;;;
;;; Elephant Object Oriented Database: Common Lisp Backend
;;;
;;; Copyright (c) 2007 by Ian Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant Lisp Backend users are granted the rights to distribute
;;; and use this software as governed by the terms of the Lisp Lesser
;;; GNU Public License (http://opensource.franz.com/preamble.html),
;;; also known as the LLGPL.

(in-package :cl-user)

(defpackage :db-lisp
  (:use :cl :elephant :elephant-backend :elephant-memutil :uffi))


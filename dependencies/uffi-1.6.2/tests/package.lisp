;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file uffi testing suite
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2003-2005 by Kevin M. Rosenberg
;;;;
;;;; $Id$
;;;; *************************************************************************

(defpackage #:uffi-tests
  (:use #:asdf #:cl #:uffi #:rtest)
  (:shadowing-import-from #:uffi #:run-shell-command))

(in-package #:uffi-tests)


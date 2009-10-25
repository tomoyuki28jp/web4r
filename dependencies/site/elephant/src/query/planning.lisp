;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; query.lisp -- Implement syntax for the elephant query engine
;;; 
;;; Copyright (c) 2007 by  Ian S. Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Limited General Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

;;
;; Planner
;;
;; Generate all possible access plans to O-relations
;; Generate all possible joins of O-relations
;; Organize into equiv. classes
;; Evaluate the cost of options in equiv classes
;; Select best in each class
;; Roll cost upto whole join tree
;; Iterate each join tree to repeat with other joins
;; Do best-first through equiv classes?


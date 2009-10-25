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
;; Relational algebra
;;

(defparameter *relation-algebra-grammar*
  '((select <op> <attr> <attr or value> <nset>)
    (project <op> <attr-list> <nset>)
    (rename <attr> <attr> <nset>)
    (union <nset> <nset>)
    (intersection <nset> <nset>)
    (difference <nset> <nset>)
    (divide <nset> <nset>)
    (natural-join <attr> <nset> <attr> <nset>)
    (theta-join op <attr> <nset> <attr> <nset>)
    (semi-join <attr> <nset> <attr> <nset>)
    (anti-join <attr> <nset> <attr> <nset>)))

;;
;; Theorems
;;

;; (select op <attr> <nset>) = (select op <attr> (select op <attr> <nset>)) ;; idempotence
;; (select (and op1 op2) <attr> <nset>) = (select op1 <attr> (select op2 <attr> <nset>)) ;; commutivity
;; (select (or op1 op2) <attr> <nset>) = (union (select op1 <attr> <nset>) (select op1 <attr> <nset>)) ;; commutivity
;; (select op <attr1> <value> (union <nset1> <nset2>)) = (union (select op <attr1> <value> <nset1>) (select ...)) ;; distributivity
;; (select opA (intersection <nset1> <nset2>)) = (intersection <nset1> (select opA <nset2>))) ;; distributivity
;; (select opA (intersection <nset1> <nset2>)) = (intersection (select opA <nset1>) <nset2>) 
;; (select opA (intersection <nset1> <nset2>)) = (intersection (select opA <nset1>) (select opA <nset1>)) 

;;
;; Optimize/Rewrite - reduce estimated set sizes
;;
;; Exercise theorems to perform certain heuristic optimizations (push selects through joins)



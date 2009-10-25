;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; convenience.lisp -- A set of convenience functions for Elephant
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

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro do-subsets ((subset subset-size list) &body body)
  "Look over subsets of the list"
  `(loop for ,subset in (subsets ,subset-size ,list) do
	,@body))

(defun subsets (size list)
  "Generate subsets of size n from the list; the last subset has 
   the remaining elements if size does not represent an equal division"
  (let ((subsets (cons nil nil)))
    (loop for elt in list
	  for i from 0 do
       (when (and (= 0 (mod i size)) (car subsets))
	 (setf (car subsets) (nreverse (car subsets)))
	 (push nil subsets))
       (push elt (car subsets)))
    (setf (car subsets) (nreverse (car subsets)))
    (nreverse subsets)))

(defun remove-keywords (keywords list)
  (cond ((null list) nil)
	((member (car list) keywords) (cddr list))
	(t (cons (car list) (remove-keywords keywords (cdr list))))))

(defun concat-separated-strings (separator &rest lists)
  (format nil (concatenate 'string "~{~A~^" (string separator) "~}")
	  (append-sublists lists)))

(defun append-sublists (list)
  "Takes a list of lists and appends all sublists"
  (let ((results (car list)))
    (dolist (elem (cdr list) results)
      (setq results (append results elem)))))

(defmacro ifret (pred alt)
  "If pred is non-null, return the value, otherwise return the alternate value"
  (with-gensyms (res)
  `(let ((,res ,pred))
     (if ,res ,res ,alt))))

(defmacro aif (pred default alt)
  "Anaphoric if"
  `(let ((it ,pred))
     (if it ,default ,alt)))

(defmacro awhen (pred &rest body)
  "Anaphoric when"
  `(let ((it ,pred))
     (declare (ignorable it))
     (when it
       ,@body)))

(defun mklist (elts)
  "Make sure the argument is a list or 
   make it a list if it's an atom"
  (if (listp elts) elts (list elts)))

(defun remove-indexed-element-and-adjust (idx array)
  "Remove element at idx and adjust the array to
   reduce array length by one"
  (let ((last (- (length array) 1)))
    (do ((i idx (1+ i)))
	((= i last) nil)
      (progn
	(setf (aref array i) (aref array (+ 1 i)))))
    (adjust-array array last)))

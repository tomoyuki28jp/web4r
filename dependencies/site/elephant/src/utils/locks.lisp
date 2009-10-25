;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; locks.lisp -- A generic interface to lisp/os locks
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

;; This is a quick portability hack to avoid external dependencies, if we get
;; too many of these do we need to import a standard library? do we need to import 
;; 'port' or some other thread layer to the elephant dependency list?

(defun ele-make-lock ()
  #+allegro (mp::make-process-lock)
  #+cmu (mp:make-lock)
  #+sbcl (sb-thread:make-mutex)
  #+mcl (ccl:make-lock)
  #+lispworks (mp:make-lock)
  #-(or allegro sbcl cmu lispworks mcl) nil )

(defmacro ele-with-lock ((lock &rest ignored) &body body)
  (declare (ignore ignored)
	   (ignorable lock))
  #+allegro `(mp:with-process-lock (,lock) ,@body)
  #+cmu `(mp:with-lock-held (,lock) ,@body)
  #+sbcl `(sb-thread:with-mutex (,lock) ,@body)
  #+lispworks `(mp:with-lock (,lock) ,@body)
  #+mcl `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or allegro sbcl cmu lispworks mcl) `(progn ,@body) )

;;
;; For tight loops we need a fast lock, for lisps that support this
;; with-interrupts or something similar this can help performance
;;

(defun ele-make-fast-lock ()
  #+allegro nil
  #-allegro (ele-make-lock))

(defmacro ele-with-fast-lock ((lock &rest ignored) &body body)
  (declare (ignorable lock ignored))
  #+allegro `(excl:without-interrupts ,@body)
  #-allegro `(ele-with-lock (,lock ,@ignored) ,@body))

(defun ele-thread-hash-key ()
"This routine has to return something unqiue about the thread which can serve as a hash key."
  #+sbcl  sb-thread::*current-thread*
  #+allegro mp:*current-process*
  #+cmu mp:*current-process*
  #+mcl ccl:*current-process*
  #+lispworks mp:*current-process*
  #-(or allegro sbcl cmu lispworks mcl) nil
  )

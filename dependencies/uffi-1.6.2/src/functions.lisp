;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function.lisp
;;;; Purpose:       UFFI source to C function definitions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)

(defun process-function-args (args)
  (if (null args)
      #+(or lispworks cmu sbcl scl cormanlisp digitool) nil
      #+allegro '(:void)
      #+openmcl (values nil nil)

      ;; args not null
      #+(or lispworks allegro cmu sbcl scl digitool cormanlisp)
      (let (processed)
        (dolist (arg args)
          (push (process-one-function-arg arg) processed))
        (nreverse processed))
      #+openmcl
      (let ((processed nil)
            (params nil))
        (dolist (arg args)
          (let ((name (car arg))
                (type (convert-from-uffi-type (cadr arg) :routine)))
            ;;(when (and (listp type) (eq (car type) :address))
            ;;(setf type :address))
            (push name params)
            (push type processed)
            (push name processed)))
        (values (nreverse params) (nreverse processed)))
    ))

(defun process-one-function-arg (arg)
  (let ((name (car arg))
        (type (convert-from-uffi-type (cadr arg) :routine)))
    #+(or cmu sbcl scl)
    ;(list name type :in)
    `(,name ,type ,@(if (= (length arg) 3) (list (third arg)) (values)))
    #+(or allegro lispworks digitool)
    (if (and (listp type) (listp (car type)))
        (append (list name) type)
      (list name type))
    #+openmcl
    (declare (ignore name type))
    ))


(defun allegro-convert-return-type (type)
  (if (and (listp type) (not (listp (car type))))
      (list type)
    type))

(defun funcallable-lambda-list (args)
  (let ((ll nil))
    (dolist (arg args)
      (push (car arg) ll))
    (nreverse ll)))

#|
(defmacro def-funcallable (name args &key returning)
  (let ((result-type (convert-from-uffi-type returning :return))
        (function-args (process-function-args args)))
    #+lispworks
    `(fli:define-foreign-funcallable ,name ,function-args
      :result-type ,result-type
      :language :ansi-c
      :calling-convention :cdecl)
    #+(or cmu scl sbcl)
    ;; requires the type of the function pointer be declared correctly!
    (let* ((ptrsym (gensym))
           (ll (funcallable-lambda-list args)))
      `(defun ,name ,(cons ptrsym ll)
        (alien::alien-funcall ,ptrsym ,@ll)))
    #+openmcl
    (multiple-value-bind (params args) (process-function-args args)
      (let ((ptrsym (gensym)))
        `(defun ,name ,(cons ptrsym params)
          (ccl::ff-call ,ptrsym ,@args ,result-type))))
    #+allegro
    ;; this is most definitely wrong
    (let* ((ptrsym (gensym))
           (ll (funcallable-lambda-list args)))
      `(defun ,name ,(cons ptrsym ll)
        (system::ff-funcall ,ptrsym ,@ll)))
    ))
|#

(defun convert-lispworks-args (args)
  (loop for arg in args
        with processed = nil
        do
        (if (and (= (length arg) 3) (eq (third arg) :out))
            (push (list (first arg)
                        (list :reference-return (second arg))) processed)
            (push (subseq arg 0 2) processed))
        finally (return (nreverse processed))))

(defun preprocess-names (names)
  (let ((fname (gensym)))
    (if (atom names)
        (values (list names fname) fname (uffi::make-lisp-name names))
        (values (list (first names) fname) fname (second names)))))

(defun preprocess-args (args)
  (loop for arg in args
        with lisp-args = nil and out = nil and processed = nil
        do
        (if (= (length arg) 3)
            (ecase (third arg)
              (:in
               (progn
                 (push (first arg) lisp-args)
                 (push (list (first arg) (second arg)) processed)))
              (:out
               (progn
                 (push (list (first arg) (second arg)) out)
                 (push (list (first arg) (list '* (second arg))) processed))))
            (progn
              (push (first arg) lisp-args)
              (push arg processed)))
        finally (return (values (nreverse lisp-args)
                                (nreverse out)
                                (nreverse processed)))))


(defmacro def-function (names args &key module returning)
  (multiple-value-bind (lisp-args out processed)
      (preprocess-args args)
    (declare (ignorable lisp-args processed))
    (if (= (length out) 0)
        `(%def-function ,names ,args
          ,@(if module (list :module module) (values))
          ,@(if returning (list :returning returning) (values)))

        #+(or cmu scl sbcl)
        `(%def-function ,names ,args
          ,@(if returning (list :returning returning) (values)))
        #+(and lispworks lispworks5)
        (multiple-value-bind (name-pair fname lisp-name)
            (preprocess-names names)
          `(progn
               (%def-function ,name-pair ,(convert-lispworks-args args)
                              ,@(if module (list :module module) (values))
                              ,@(if returning (list :returning returning) (values)))
               (defun ,lisp-name ,lisp-args
                 (,fname ,@(mapcar
                            #'(lambda (arg)
                                (cond ((member (first arg) lisp-args)
                                       (first arg))
                                      ((member (first arg) out :key #'first)
                                       t)))
                          args)))))
        #+(and lispworks (not lispworks5))
        `(%def-function ,names ,(convert-lispworks-args args)
          ,@(if module (list :module module) (values))
          ,@(if returning (list :returning returning) (values)))
        #-(or cmu scl sbcl lispworks)
        (multiple-value-bind (name-pair fname lisp-name)
            (preprocess-names names)
          `(progn
            (%def-function ,name-pair ,processed
             :module ,module :returning ,returning)
            ;(declaim (inline ,fname))
            (defun ,lisp-name ,lisp-args
              (with-foreign-objects ,out
                (values (,fname ,@(mapcar #'first args))
                        ,@(mapcar #'(lambda (arg)
                                      (list 'deref-pointer
                                            (first arg)
                                            (second arg))) out))))))
        )))


;; name is either a string representing foreign name, or a list
;; of foreign-name as a string and lisp name as a symbol
(defmacro %def-function (names args &key module returning)
  #+(or cmu sbcl scl allegro openmcl digitool cormanlisp) (declare (ignore module))

  (let* ((result-type (convert-from-uffi-type returning :return))
         (function-args (process-function-args args))
         (foreign-name (if (atom names) names (car names)))
         (lisp-name (if (atom names) (make-lisp-name names) (cadr names))))
    ;; todo: calling-convention :stdcall for cormanlisp
    #+allegro
    `(ff:def-foreign-call (,lisp-name ,foreign-name)
         ,function-args
       :returning ,(allegro-convert-return-type result-type)
       :call-direct t
       :strings-convert nil)
    #+(or cmu scl)
    `(alien:def-alien-routine (,foreign-name ,lisp-name)
         ,result-type
       ,@function-args)
    #+sbcl
    `(sb-alien:define-alien-routine (,foreign-name ,lisp-name)
         ,result-type
       ,@function-args)
    #+lispworks
    `(fli:define-foreign-function (,lisp-name ,foreign-name :source)
         ,function-args
       ,@(if module (list :module module) (values))
       :result-type ,result-type
      :language :ansi-c
       #+:mswindows :calling-convention #+:mswindows :cdecl)
    #+digitool
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (ccl:define-entry-point (,lisp-name ,foreign-name)
         ,function-args
         ,result-type))
    #+openmcl
    (declare (ignore function-args))
    #+(and openmcl darwinppc-target)
    (setf foreign-name (concatenate 'string "_" foreign-name))
    #+openmcl
    (multiple-value-bind (params args) (process-function-args args)
      `(defun ,lisp-name ,params
         (ccl::external-call ,foreign-name ,@args ,result-type)))
    #+cormanlisp
    `(ct:defun-dll ,lisp-name (,function-args)
       :return-type ,result-type
       ,@(if module (list :library-name module) (values))
       :entry-name ,foreign-name
       :linkage-type ,calling-convention) ; we need :pascal
    ))





;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives.lisp
;;;; Purpose:       UFFI source to handle immediate types
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)

#+(or openmcl digitool)
(defvar *keyword-package* (find-package "KEYWORD"))

#+(or openmcl digitool)
; MCL and OpenMCL expect a lot of FFI elements to be keywords (e.g. struct field names in OpenMCL)
; So this provides a function to convert any quoted symbols to keywords.
(defun keyword (obj)
  (cond ((keywordp obj)
         obj)
        ((null obj)
         nil)
        ((symbolp obj)
         (intern (symbol-name obj) *keyword-package*))
        ((and (listp obj) (eq (car obj) 'cl:quote))
         (keyword (cadr obj)))
        ((stringp obj)
         (intern obj *keyword-package*))
        (t
         obj)))

; Wrapper for unexported function we have to use
#+digitool
(defmacro def-mcl-type (name type)
  `(ccl::def-mactype ,(keyword name) (ccl:find-mactype ,type)))

(defmacro def-constant (name value &key (export nil))
  "Macro to define a constant and to export it"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,(when export (list 'export `(quote ,name)))
    ',name))

(defmacro def-type (name type)
  "Generates a (deftype) statement for CL. Currently, only CMUCL
supports takes advantage of this optimization."
  #+(or lispworks allegro openmcl digitool cormanlisp)  (declare (ignore type))
  #+(or lispworks allegro openmcl digitool cormanlisp) `(deftype ,name () t)
  #+(or cmu scl)
  `(deftype ,name () '(alien:alien ,(convert-from-uffi-type type :declare)))
  #+sbcl
  `(deftype ,name () '(sb-alien:alien ,(convert-from-uffi-type type :declare)))
  )

(defmacro null-char-p (val)
  "Returns T if character is NULL"
  `(zerop ,val))

(defmacro def-foreign-type (name type)
  #+lispworks `(fli:define-c-typedef ,name ,(convert-from-uffi-type type :type))
  #+allegro `(ff:def-foreign-type ,name ,(convert-from-uffi-type type :type))
  #+(or cmu scl) `(alien:def-alien-type ,name ,(convert-from-uffi-type type :type))
  #+sbcl `(sb-alien:define-alien-type ,name ,(convert-from-uffi-type type :type))
  #+cormanlisp `(ct:defctype ,name ,(convert-from-uffi-type type :type))
  #+(or openmcl digitool)
  (let ((mcl-type (convert-from-uffi-type type :type)))
    (unless (or (keywordp mcl-type) (consp mcl-type))
      (setf mcl-type `(quote ,mcl-type)))
    #+digitool
    `(def-mcl-type ,(keyword name) ,mcl-type)
    #+openmcl
    `(ccl::def-foreign-type ,(keyword name) ,mcl-type))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +type-conversion-hash+ (make-hash-table :size 20 :test #'eq))
  #+(or cmu sbcl scl) (defvar *cmu-def-type-hash*
                        (make-hash-table :size 20 :test #'eq))
  )

#+(or cmu scl)
(defvar *cmu-sbcl-def-type-list*
    '((:char . (alien:signed 8))
      (:unsigned-char . (alien:unsigned 8))
      (:byte . (alien:signed 8))
      (:unsigned-byte . (alien:unsigned 8))
      (:short . (alien:signed 16))
      (:unsigned-short . (alien:unsigned 16))
      (:int . (alien:signed 32))
      (:unsigned-int . (alien:unsigned 32))
      #-x86-64 (:long . (alien:signed 32))
      #-x86-64 (:unsigned-long . (alien:unsigned 32))
      #+x86-64 (:long . (alien:signed 64))
      #+x86-64 (:unsigned-long . (alien:unsigned 64))
      (:float . alien:single-float)
      (:double . alien:double-float)
      (:void . t)
      )
  "Conversions in CMUCL for def-foreign-type are different than in def-function")

#+sbcl
(defvar *cmu-sbcl-def-type-list*
    '((:char . (sb-alien:signed 8))
      (:unsigned-char . (sb-alien:unsigned 8))
      (:byte . (sb-alien:signed 8))
      (:unsigned-byte . (sb-alien:unsigned 8))
      (:short . (sb-alien:signed 16))
      (:unsigned-short . (sb-alien:unsigned 16))
      (:int . (sb-alien:signed 32))
      (:unsigned-int . (sb-alien:unsigned 32))
      #-x86-64 (:long . (sb-alien:signed 32))
      #-x86-64 (:unsigned-long . (sb-alien:unsigned 32))
      #+x86-64 (:long . (sb-alien:signed 64))
      #+x86-64 (:unsigned-long . (sb-alien:unsigned 64))
      (:float . sb-alien:single-float)
      (:double . sb-alien:double-float)
      (:void . t)
      )
  "Conversions in SBCL for def-foreign-type are different than in def-function")

(defvar *type-conversion-list* nil)

#+(or cmu scl)
(setq *type-conversion-list*
    '((* . *) (:void . c-call:void)
      (:pointer-void . (* t))
      (:cstring . c-call:c-string)
      (:char . c-call:char)
      (:unsigned-char . (alien:unsigned 8))
      (:byte . (alien:signed 8))
      (:unsigned-byte . (alien:unsigned 8))
      (:short . c-call:short)
      (:unsigned-short . c-call:unsigned-short)
      (:int . alien:integer) (:unsigned-int . c-call:unsigned-int)
      (:long . c-call:long) (:unsigned-long . c-call:unsigned-long)
      (:float . c-call:float) (:double . c-call:double)
      (:array . alien:array)))

#+sbcl
(setq *type-conversion-list*
    '((* . *) (:void . sb-alien:void)
      (:pointer-void . (* t))
      #-sb-unicode(:cstring . sb-alien:c-string)
      #+sb-unicode(:cstring . sb-alien:utf8-string)
      (:char . sb-alien:char)
      (:unsigned-char . (sb-alien:unsigned 8))
      (:byte . (sb-alien:signed 8))
      (:unsigned-byte . (sb-alien:unsigned 8))
      (:short . sb-alien:short)
      (:unsigned-short . sb-alien:unsigned-short)
      (:int . sb-alien:int) (:unsigned-int . sb-alien:unsigned-int)
      (:long . sb-alien:long) (:unsigned-long . sb-alien:unsigned-long)
      (:float . sb-alien:float) (:double . sb-alien:double)
      (:array . sb-alien:array)))

#+(or allegro cormanlisp)
(setq *type-conversion-list*
    '((* . *) (:void . :void)
      (:short . :short)
      (:pointer-void . (* :void))
      (:cstring . (* :unsigned-char))
      (:byte . :char)
      (:unsigned-byte . :unsigned-char)
      (:char . :char)
      (:unsigned-char . :unsigned-char)
      (:int . :int) (:unsigned-int . :unsigned-int)
      (:long . :long) (:unsigned-long . :unsigned-long)
      (:float . :float) (:double . :double)
      (:array . :array)))

#+lispworks
(setq *type-conversion-list*
    '((* . :pointer) (:void . :void)
      (:short . :short)
      (:pointer-void . (:pointer :void))
      (:cstring . (:reference-pass (:ef-mb-string :external-format
                                    (:latin-1 :eol-style :lf))
                   :allow-null t))
      (:cstring-returning . (:reference (:ef-mb-string :external-format
                                         (:latin-1 :eol-style :lf))
                             :allow-null t))
      (:byte . :byte)
      (:unsigned-byte . (:unsigned :byte))
      (:char . :char)
      (:unsigned-char . (:unsigned :char))
      (:int . :int) (:unsigned-int . (:unsigned :int))
      (:long . :long) (:unsigned-long . (:unsigned :long))
      (:float . :float) (:double . :double)
      (:array . :c-array)))

#+digitool
(setq *type-conversion-list*
     '((* . :pointer) (:void . :void)
       (:short . :short) (:unsigned-short . :unsigned-short)
       (:pointer-void . :pointer)
       (:cstring . :string)
       (:char . :character)
       (:unsigned-char . :unsigned-byte)
       (:byte . :signed-byte) (:unsigned-byte . :unsigned-byte)
       (:int . :long) (:unsigned-int . :unsigned-long)
       (:long . :long) (:unsigned-long . :unsigned-long)
       (:float . :single-float) (:double . :double-float)
       (:array . :array)))

#+openmcl
(setq *type-conversion-list*
     '((* . :address) (:void . :void)
       (:short . :short) (:unsigned-short . :unsigned-short)
       (:pointer-void . :address)
       (:cstring . :address)
       (:char . :signed-char)
       (:unsigned-char . :unsigned-char)
       (:byte . :signed-byte) (:unsigned-byte . :unsigned-byte)
       (:int . :int) (:unsigned-int . :unsigned-int)
       (:long . :long) (:unsigned-long . :unsigned-long)
       (:long-long . :signed-doubleword) (:unsigned-long-long . :unsigned-doubleword)
       (:float . :single-float) (:double . :double-float)
       (:array . :array)))

(dolist (type *type-conversion-list*)
  (setf (gethash (car type) +type-conversion-hash+) (cdr type)))

#+(or cmu sbcl scl)
(dolist (type *cmu-sbcl-def-type-list*)
  (setf (gethash (car type) *cmu-def-type-hash*) (cdr type)))

(defun basic-convert-from-uffi-type (type)
  (let ((found-type (gethash type +type-conversion-hash+)))
    (if found-type
        found-type
      #-(or openmcl digitool) type
      #+(or openmcl digitool) (keyword type))))

(defun %convert-from-uffi-type (type context)
  "Converts from a uffi type to an implementation specific type"
  (if (atom type)
      (cond
       #+(or allegro cormanlisp)
       ((and (or (eq context :routine) (eq context :return))
             (eq type :cstring))
        (setq type '((* :char) integer)))
       #+(or cmu sbcl scl)
       ((eq context :type)
        (let ((cmu-type (gethash type *cmu-def-type-hash*)))
          (if cmu-type
              cmu-type
              (basic-convert-from-uffi-type type))))
       #+lispworks
       ((and (eq context :return)
             (eq type :cstring))
        (basic-convert-from-uffi-type :cstring-returning))
       #+digitool
       ((and (eq type :void) (eq context :return)) nil)
       (t
        (basic-convert-from-uffi-type type)))
    (let ((sub-type (car type)))
      (case sub-type
        (cl:quote
         (convert-from-uffi-type (cadr type) context))
        (:struct-pointer
         #+(or openmcl digitool) `(:* (:struct ,(%convert-from-uffi-type (cadr type) :struct)))
         #-(or openmcl digitool) (%convert-from-uffi-type (list '* (cadr type)) :struct)
         )
        (:struct
         #+(or openmcl digitool) `(:struct ,(%convert-from-uffi-type (cadr type) :struct))
         #-(or openmcl digitool) (%convert-from-uffi-type (cadr type) :struct)
         )
       (:union
        #+(or openmcl digitool) `(:union ,(%convert-from-uffi-type (cadr type) :union))
        #-(or openmcl digitool) (%convert-from-uffi-type (cadr type) :union)
        )
       (t
        (cons (%convert-from-uffi-type (first type) context)
              (%convert-from-uffi-type (rest type) context)))))))

(defun convert-from-uffi-type (type context)
  (let ((result (%convert-from-uffi-type type context)))
    (cond
     ((atom result) result)
     #+openmcl
     ((eq (car result) :address)
      (if (eq context :struct)
          (append '(:*) (cdr result))
        :address))
     #+digitool
     ((and (eq (car result) :pointer) (eq context :allocation) :pointer))
     (t result))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (char= #\a (schar (symbol-name '#:a) 0))
    (pushnew :uffi-lowercase-reader *features*))
  (when (not (string= (symbol-name '#:a)
                      (symbol-name '#:A)))
    (pushnew :uffi-case-sensitive *features*)))

(defun make-lisp-name (name)
  (let ((converted (substitute #\- #\_ name)))
     (intern
      #+uffi-case-sensitive converted
      #+(and (not uffi-lowercase-reader) (not uffi-case-sensitive)) (string-upcase converted)
      #+(and uffi-lowercase-reader (not uffi-case-sensitive)) (string-downcase converted))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :uffi-lowercase-reader *features*))
  (setq cl:*features* (delete :uffi-case-sensitive *features*)))

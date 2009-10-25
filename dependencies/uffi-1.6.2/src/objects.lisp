;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          objects.lisp
;;;; Purpose:       UFFI source to handle objects and pointers
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun size-of-foreign-type (type)
    #+lispworks (fli:size-of type)
    #+allegro (ff:sizeof-fobject type)
    #+(or cmu scl)  (ash (eval `(alien:alien-size ,type)) -3) ;; convert from bits to bytes
    #+sbcl  (ash (eval `(sb-alien:alien-size ,type)) -3) ;; convert from bits to bytes
    #+clisp (values (ffi:size-of type))
    #+digitool
    (let ((mcl-type (ccl:find-mactype type nil t)))
      (if mcl-type
          (ccl::mactype-record-size mcl-type)
          (ccl::record-descriptor-length (ccl:find-record-descriptor type t t)))) ;error if not a record
    #+openmcl (ccl::%foreign-type-or-record-size type :bytes)
    ))

(defmacro allocate-foreign-object (type &optional (size :unspecified))
  "Allocates an instance of TYPE. If size is specified, then allocate
an array of TYPE with size SIZE. The TYPE parameter is evaluated."
  (if (eq size :unspecified)
      (progn
        #+(or cmu scl)
        `(alien:make-alien ,(convert-from-uffi-type (eval type) :allocation))
        #+sbcl
        `(sb-alien:make-alien ,(convert-from-uffi-type (eval type) :allocation))
        #+lispworks
        `(fli:allocate-foreign-object :type ',(convert-from-uffi-type type :allocate))
        #+allegro
        `(ff:allocate-fobject ',(convert-from-uffi-type type :allocate) :c)
        #+(or openmcl digitool)
        `(new-ptr ,(size-of-foreign-type (convert-from-uffi-type type :allocation)))
        )
      (progn
        #+(or cmu scl)
        `(alien:make-alien ,(convert-from-uffi-type (eval type) :allocation) ,size)
        #+sbcl
        `(sb-alien:make-alien ,(convert-from-uffi-type (eval type) :allocation) ,size)
        #+lispworks
        `(fli:allocate-foreign-object :type ',(convert-from-uffi-type type :allocate) :nelems ,size)
        #+allegro
        `(ff:allocate-fobject (list :array (quote ,(convert-from-uffi-type type :allocate)) ,size) :c)
        #+(or openmcl digitool)
        `(new-ptr (* ,size ,(size-of-foreign-type (convert-from-uffi-type type :allocation))))
        )))

(defmacro free-foreign-object (obj)
  #+(or cmu scl)
  `(alien:free-alien ,obj)
  #+sbcl
  `(sb-alien:free-alien ,obj)
  #+lispworks
  `(fli:free-foreign-object ,obj)
  #+allegro
  `(ff:free-fobject ,obj)
  #+(or openmcl digitool)
  `(dispose-ptr ,obj)
  )

(defmacro null-pointer-p (obj)
  #+lispworks `(fli:null-pointer-p ,obj)
  #+allegro `(zerop ,obj)
  #+(or cmu scl)   `(alien:null-alien ,obj)
  #+sbcl   `(sb-alien:null-alien ,obj)
  #+(or openmcl digitool)   `(ccl:%null-ptr-p ,obj)
  )

(defmacro make-null-pointer (type)
  #+(or allegro openmcl digitool) (declare (ignore type))
  #+(or cmu scl) `(alien:sap-alien (system:int-sap 0) (* ,(convert-from-uffi-type (eval type) :type)))
  #+sbcl `(sb-alien:sap-alien (sb-sys:int-sap 0) (* ,(convert-from-uffi-type (eval type) :type)))
  #+lispworks `(fli:make-pointer :address 0 :type (quote ,(convert-from-uffi-type (eval type) :type)))
  #+allegro 0
  #+(or openmcl digitool) `(ccl:%null-ptr)
  )

(defmacro make-pointer (addr type)
  #+(or allegro openmcl digitool) (declare (ignore type))
  #+(or cmu scl) `(alien:sap-alien (system:int-sap ,addr) (* ,(convert-from-uffi-type (eval type) :type)))
  #+sbcl `(sb-alien:sap-alien (sb-sys:int-sap ,addr) (* ,(convert-from-uffi-type (eval type) :type)))
  #+lispworks `(fli:make-pointer :address ,addr :type (quote ,(convert-from-uffi-type (eval type) :type)))
  #+allegro addr
  #+(or openmcl digitool) `(ccl:%int-to-ptr ,addr)
  )


(defmacro char-array-to-pointer (obj)
  #+(or cmu scl) `(alien:cast ,obj (* (alien:unsigned 8)))
  #+sbcl `(sb-alien:cast ,obj (* (sb-alien:unsigned 8)))
  #+lispworks `(fli:make-pointer :type '(:unsigned :char)
                                :address (fli:pointer-address ,obj))
  #+allegro obj
  #+(or openmcl digitool) obj
  )

(defmacro deref-pointer (ptr type)
  "Returns a object pointed"
  #+(or cmu sbcl lispworks scl) (declare (ignore type))
  #+(or cmu scl)  `(alien:deref ,ptr)
  #+sbcl  `(sb-alien:deref ,ptr)
  #+lispworks `(fli:dereference ,ptr)
  #+allegro `(ff:fslot-value-typed (quote ,(convert-from-uffi-type type :deref)) :c ,ptr)
  #+(or openmcl digitool) `(ccl:pref ,ptr ,(convert-from-uffi-type type :deref))
  )

#+digitool
(defmacro deref-pointer-set (ptr type value)
  `(setf (ccl:pref ,ptr ,(convert-from-uffi-type type :deref)) ,value))

#+digitool
(defsetf deref-pointer deref-pointer-set)

(defmacro ensure-char-character (obj)
  #+(or digitool) obj
  #+(or allegro cmu sbcl scl openmcl) `(code-char ,obj)
  ;; lispworks varies whether deref'ing array vs. slot access of a char
  #+lispworks `(if (characterp ,obj) ,obj (code-char ,obj)))

(defmacro ensure-char-integer (obj)
  #+(or digitool) `(char-code ,obj)
  #+(or allegro cmu sbcl scl openmcl) obj
  ;; lispworks varies whether deref'ing array vs. slot access of a char
  #+lispworks
  `(if (integerp ,obj) ,obj (char-code ,obj)))

(defmacro ensure-char-storable (obj)
  #+(or digitool (and lispworks (not lispworks5))) obj
  #+(or allegro cmu lispworks5 openmcl sbcl scl)
  `(char-code ,obj))

(defmacro pointer-address (obj)
  #+(or cmu scl)
  `(system:sap-int (alien:alien-sap ,obj))
  #+sbcl
  `(sb-sys:sap-int (sb-alien:alien-sap ,obj))
  #+lispworks
  `(fli:pointer-address ,obj)
  #+allegro
  obj
  #+(or openmcl digitool)
  `(ccl:%ptr-to-int ,obj)
  )

;; TYPE is evaluated.
#-(or openmcl digitool)
(defmacro with-foreign-object ((var type) &rest body)
  #-(or cmu sbcl lispworks scl) ; default version
  `(let ((,var (allocate-foreign-object ,type)))
    (unwind-protect
         (progn ,@body)
      (free-foreign-object ,var)))
  #+(or cmu scl)
  (let ((obj (gensym))
        (ctype (convert-from-uffi-type (eval type) :allocate)))
    (if (and (consp ctype) (eq 'array (car ctype)))
        `(alien:with-alien ((,obj ,ctype))
          (let* ((,var ,obj))
            ,@body))
        `(alien:with-alien ((,obj ,ctype))
          (let* ((,var (alien:addr ,obj)))
            ,@body))))
  #+sbcl
  (let ((obj (gensym))
        (ctype (convert-from-uffi-type (eval type) :allocate)))
    (if (and (consp ctype) (eq 'array (car ctype)))
        `(sb-alien:with-alien ((,obj ,ctype))
          (let* ((,var ,obj))
            ,@body))
        `(sb-alien:with-alien ((,obj ,ctype))
          (let* ((,var (sb-alien:addr ,obj)))
            ,@body))))
  #+lispworks
  `(fli:with-dynamic-foreign-objects ((,var ,(convert-from-uffi-type
                                              (eval type) :allocate)))
    ,@body)
  )

#-(or openmcl digitool)
(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
      `(with-foreign-object ,(car bindings)
        (with-foreign-objects ,(cdr bindings)
          ,@body))
      `(progn ,@body)))

#+(or openmcl digitool)
(defmacro with-foreign-objects (bindings &rest body)
  (let ((params nil) type count)
    (dolist (spec (reverse bindings)) ;keep order - macroexpands to let*
      (setf type (convert-from-uffi-type (eval (nth 1 spec)) :allocate))
      (setf count 1)
      (when (and (listp type) (eq (first type) :array))
        (setf count (nth 2 type))
        (unless (integerp count) (error "Invalid size for array: ~a" type))
        (setf type (nth 1 type)))
      (push (list (first spec) (* count (size-of-foreign-type type))) params))
    `(ccl:%stack-block ,params ,@body)))

#+(or openmcl digitool)
(defmacro with-foreign-object ((var type) &rest body)
  `(with-foreign-objects ((,var ,type))
     ,@body))

#+lispworks
(defmacro with-cast-pointer ((binding-name pointer type) &body body)
  `(fli:with-coerced-pointer (,binding-name
                          :type ',(convert-from-uffi-type (eval type) :type))
      ,pointer
    ,@body))

#+(or cmu scl sbcl)
(defmacro with-cast-pointer ((binding-name pointer type) &body body)
  `(let ((,binding-name
          (#+(or cmu scl) alien:cast
           #+sbcl sb-alien:cast
           ,pointer (* ,(convert-from-uffi-type (eval type) :type)))))
    ,@body))

#+(or allegro openmcl)
(defmacro with-cast-pointer ((binding-name pointer type) &body body)
  (declare (ignore type))
  `(let ((,binding-name ,pointer))
    ,@body))

#-(or lispworks cmu scl sbcl allegro openmcl)
(defmacro with-cast-pointer ((binding-name pointer type) &body body)
  (declare (ignore binding-name pointer type body))
  '(error "WITH-CAST-POINTER not (yet) implemented for ~A"
          (lisp-implementation-type)))

#+(or allegro openmcl)
(defun convert-external-name (name)
  "Add an underscore to NAME if necessary for the ABI."
  #+(or macosx darwinppc-target) (concatenate 'string "_" name)
  #-(or macosx darwinppc-target) name)

(defmacro def-foreign-var (names type module)
  #-lispworks (declare (ignore module))
  (let ((foreign-name (if (atom names) names (first names)))
        (lisp-name (if (atom names) (make-lisp-name names) (second names)))
        #-allegro
        (var-type (convert-from-uffi-type type :type)))
    #+(or cmu scl)
    `(alien:def-alien-variable (,foreign-name ,lisp-name) ,var-type)
    #+sbcl
    `(sb-alien:define-alien-variable (,foreign-name ,lisp-name) ,var-type)
    #+allegro
    `(define-symbol-macro ,lisp-name
      (ff:fslot-value-typed (quote ,(convert-from-uffi-type type :deref))
                            :c (ff:get-entry-point ,(convert-external-name foreign-name))))
    #+lispworks
    `(progn
      (fli:define-foreign-variable (,lisp-name ,foreign-name)
                                    :accessor :address-of
                                    :type ,var-type
                                    :module ,module)
      (define-symbol-macro ,lisp-name (fli:dereference (,lisp-name)
                                                        :copy-foreign-object nil)))
    #+openmcl
    `(define-symbol-macro ,lisp-name
       (deref-pointer (ccl:foreign-symbol-address
                       ,(convert-external-name foreign-name)) ,var-type))
    #-(or allegro cmu scl sbcl lispworks openmcl)
    `(define-symbol-macro ,lisp-name
      '(error "DEF-FOREIGN-VAR not (yet) defined for ~A"
        (lisp-implementation-type)))))


;;; Define a special variable, like DEFVAR, that will be initialized
;;; to a pointer which may need to be reset when a saved image is
;;; loaded.  This is needed for OpenMCL, which sets pointers to "dead
;;; macptrs" when a saved image is loaded.
;; This may possibly be needed for sbcl's SAVE-LISP-AND-DIE
(defmacro def-pointer-var (name value &optional doc)
  #-openmcl `(defvar ,name ,value ,@(if doc (list doc)))
  #+openmcl `(ccl::defloadvar ,name ,value ,doc))

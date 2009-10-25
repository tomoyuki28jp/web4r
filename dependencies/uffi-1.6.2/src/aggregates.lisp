;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          aggregates.lisp
;;;; Purpose:       UFFI source to handle aggregate types
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)

(defmacro def-enum (enum-name args &key (separator-string "#"))
  "Creates a constants for a C type enum list, symbols are created
in the created in the current package. The symbol is the concatenation
of the enum-name name, separator-string, and field-name"
  (let ((counter 0)
        (cmds nil)
        (constants nil))
    (declare (fixnum counter))
    (dolist (arg args)
      (let ((name (if (listp arg) (car arg) arg))
            (value (if (listp arg)
                       (prog1
                           (setq counter (cadr arg))
                         (incf counter))
                     (prog1
                         counter
                       (incf counter)))))
        (setq name (intern (concatenate 'string
                             (symbol-name enum-name)
                             separator-string
                             (symbol-name name))))
        (push `(uffi:def-constant ,name ,value) constants)))
    (setf cmds (append '(progn)
                       #+allegro `((ff:def-foreign-type ,enum-name :int))
                       #+lispworks `((fli:define-c-typedef ,enum-name :int))
                       #+(or cmu scl) `((alien:def-alien-type ,enum-name alien:signed))
                       #+sbcl `((sb-alien:define-alien-type ,enum-name sb-alien:signed))
                       #+digitool `((def-mcl-type ,enum-name :integer))
                       #+openmcl `((ccl::def-foreign-type ,enum-name :int))
                       (nreverse constants)))
    cmds))


(defmacro def-array-pointer (name-array type)
  #+allegro
  `(ff:def-foreign-type ,name-array
    (:array ,(convert-from-uffi-type type :array)))
  #+lispworks
  `(fli:define-c-typedef ,name-array
    (:c-array ,(convert-from-uffi-type type :array)))
  #+(or cmu scl)
  `(alien:def-alien-type ,name-array
    (* ,(convert-from-uffi-type type :array)))
  #+sbcl
  `(sb-alien:define-alien-type ,name-array
    (* ,(convert-from-uffi-type type :array)))
  #+digitool
  `(def-mcl-type ,name-array '(:array ,type))
  #+openmcl
  `(ccl::def-foreign-type ,name-array (:array ,(convert-from-uffi-type type :array)))
  )

(defun process-struct-fields (name fields &optional (variant nil))
  (let (processed)
    (dolist (field fields)
      (let* ((field-name (car field))
             (type (cadr field))
             (def (append (list field-name)
                          (if (eq type :pointer-self)
                              #+(or cmu scl) `((* (alien:struct ,name)))
                              #+sbcl `((* (sb-alien:struct ,name)))
                              #+(or openmcl digitool) `((:* (:struct ,name)))
                              #+lispworks `((:pointer ,name))
                              #-(or cmu sbcl scl openmcl digitool lispworks) `((* ,name))
                              `(,(convert-from-uffi-type type :struct))))))
        (if variant
            (push (list def) processed)
          (push def processed))))
    (nreverse processed)))


(defmacro def-struct (name &rest fields)
  #+(or cmu scl)
  `(alien:def-alien-type ,name (alien:struct ,name ,@(process-struct-fields name fields)))
  #+sbcl
  `(sb-alien:define-alien-type ,name (sb-alien:struct ,name ,@(process-struct-fields name fields)))
  #+allegro
  `(ff:def-foreign-type ,name (:struct ,@(process-struct-fields name fields)))
  #+lispworks
  `(fli:define-c-struct ,name ,@(process-struct-fields name fields))
  #+digitool
  `(ccl:defrecord ,name ,@(process-struct-fields name fields))
  #+openmcl
  `(ccl::def-foreign-type
    nil
    (:struct ,name ,@(process-struct-fields name fields)))
  )


(defmacro get-slot-value (obj type slot)
  #+(or lispworks cmu sbcl scl) (declare (ignore type))
  #+allegro
  `(ff:fslot-value-typed ,type :c ,obj ,slot)
  #+lispworks
  `(fli:foreign-slot-value ,obj ,slot)
  #+(or cmu scl)
  `(alien:slot ,obj ,slot)
  #+sbcl
  `(sb-alien:slot ,obj ,slot)
  #+(or openmcl digitool)
  `(ccl:pref ,obj ,(read-from-string (format nil ":~a.~a" (keyword type) (keyword slot))))
  )

#+(or openmcl digitool)
(defmacro set-slot-value (obj type slot value) ;use setf to set values
  `(setf (ccl:pref ,obj ,(read-from-string (format nil ":~a.~a" (keyword type) (keyword slot)))) ,value))

#+(or openmcl digitool)
(defsetf get-slot-value set-slot-value)


(defmacro get-slot-pointer (obj type slot)
  #+(or lispworks cmu sbcl scl) (declare (ignore type))
  #+allegro
  `(ff:fslot-value-typed ,type :c ,obj ,slot)
  #+lispworks
  `(fli:foreign-slot-pointer ,obj ,slot)
  #+(or cmu scl)
  `(alien:slot ,obj ,slot)
  #+sbcl
  `(sb-alien:slot ,obj ,slot)
  #+digitool
  `(ccl:%int-to-ptr (+ (ccl:%ptr-to-int ,obj) (the fixnum (ccl:field-info ,type ,slot))))
  #+openmcl
  `(let ((field (ccl::%find-foreign-record-type-field ,type ,slot)))
     (ccl:%int-to-ptr (+ (ccl:%ptr-to-int ,obj) (the fixnum (ccl::foreign-record-field-offset field)))))
)

;; necessary to eval at compile time for openmcl to compile convert-from-foreign-usb8
;; below
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; so we could allow '(:array :long) or deref with other type like :long only
  #+(or openmcl digitool)
  (defun array-type (type)
    (let ((result type))
      (when (listp type)
        (let ((type-list (if (eq (car type) 'quote) (nth 1 type) type)))
          (when (and (listp type-list) (eq (car type-list) :array))
            (setf result (cadr type-list)))))
      result))


  (defmacro deref-array (obj type i)
    "Returns a field from a row"
    #+(or lispworks cmu sbcl scl) (declare (ignore type))
    #+(or cmu scl)  `(alien:deref ,obj ,i)
    #+sbcl `(sb-alien:deref ,obj ,i)
    #+lispworks `(fli:dereference ,obj :index ,i :copy-foreign-object nil)
    #+allegro `(ff:fslot-value-typed (quote ,(convert-from-uffi-type type :type)) :c ,obj ,i)
    #+openmcl
    (let* ((array-type (array-type type))
           (local-type (convert-from-uffi-type array-type :allocation))
           (element-size-in-bits (ccl::%foreign-type-or-record-size local-type :bits)))
      (ccl::%foreign-access-form
       obj
       (ccl::%foreign-type-or-record local-type)
       `(* ,i ,element-size-in-bits)
       nil))
    #+digitool
    (let* ((array-type (array-type type))
           (local-type (convert-from-uffi-type array-type :allocation))
           (accessor (first (macroexpand `(ccl:pref obj ,local-type)))))
      `(,accessor
        ,obj
        (* (the fixnum ,i) ,(size-of-foreign-type local-type))))
    ))

; this expands to the %set-xx functions which has different params than %put-xx
#+digitool
(defmacro deref-array-set (obj type i value)
  (let* ((array-type (array-type type))
         (local-type (convert-from-uffi-type array-type :allocation))
         (accessor (first (macroexpand `(ccl:pref obj ,local-type))))
         (settor (first (macroexpand `(setf (,accessor obj ,local-type) value)))))
    `(,settor
      ,obj
      (* (the fixnum ,i) ,(size-of-foreign-type local-type))
      ,value)))

#+digitool
(defsetf deref-array deref-array-set)

(defmacro def-union (name &rest fields)
  #+allegro
  `(ff:def-foreign-type ,name (:union ,@(process-struct-fields name fields)))
  #+lispworks
  `(fli:define-c-union ,name ,@(process-struct-fields name fields))
  #+(or cmu scl)
  `(alien:def-alien-type ,name (alien:union ,name ,@(process-struct-fields name fields)))
  #+sbcl
  `(sb-alien:define-alien-type ,name (sb-alien:union ,name ,@(process-struct-fields name fields)))
  #+digitool
  `(ccl:defrecord ,name (:variant ,@(process-struct-fields name fields t)))
  #+openmcl
  `(ccl::def-foreign-type nil
                          (:union ,name ,@(process-struct-fields name fields)))
)


#-(or sbcl cmu)
(defun convert-from-foreign-usb8 (s len)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (fixnum len))
  (let ((a (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len a)
      (declare (fixnum i))
      (setf (aref a i) (uffi:deref-array s '(:array :unsigned-byte) i)))))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:without-package-locks
      (defvar *system-copy-fn* (if (fboundp (intern "COPY-FROM-SYSTEM-AREA" "SB-KERNEL"))
                                   (intern "COPY-FROM-SYSTEM-AREA" "SB-KERNEL")
                                   (intern "COPY-UB8-FROM-SYSTEM-AREA" "SB-KERNEL")))
    (defconstant +system-copy-offset+ (if (fboundp (intern "COPY-FROM-SYSTEM-AREA" "SB-KERNEL"))
                                          (* sb-vm:vector-data-offset sb-vm:n-word-bits)
                                          0))
    (defconstant +system-copy-multiplier+ (if (fboundp (intern "COPY-FROM-SYSTEM-AREA" "SB-KERNEL"))
                                              sb-vm:n-byte-bits
                                              1))))


#+sbcl
(defun convert-from-foreign-usb8 (s len)
  (let ((sap (sb-alien:alien-sap s)))
    (declare (type sb-sys:system-area-pointer sap))
    (locally
     (declare (optimize (speed 3) (safety 0)))
     (let ((result (make-array len :element-type '(unsigned-byte 8))))
       (funcall *system-copy-fn* sap 0 result +system-copy-offset+
                (* len +system-copy-multiplier+))
       result))))

#+cmu
(defun convert-from-foreign-usb8 (s len)
  (let ((sap (alien:alien-sap s)))
    (declare (type system:system-area-pointer sap))
    (locally
        (declare (optimize (speed 3) (safety 0)))
      (let ((result (make-array len :element-type '(unsigned-byte 8))))
        (kernel:copy-from-system-area sap 0
                                      result (* vm:vector-data-offset
                                                vm:word-bits)
                                      (* len vm:byte-bits))
        result))))

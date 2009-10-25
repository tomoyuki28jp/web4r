;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          union.cl
;;;; Purpose:       UFFI Example file to test unions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)

(uffi:def-union tunion1
    (char :char)
  (int :int)
  (uint :unsigned-int)
  (sf :float)
  (df :double))

(defun run-union-1 ()
  (let ((u (uffi:allocate-foreign-object 'tunion1)))
    (setf (uffi:get-slot-value u 'tunion1 'uint)
      ;; little endian
      #-(or sparc sparc-v9 powerpc ppc big-endian)
      (+ (* 1 (char-code #\A))
         (* 256 (char-code #\B))
         (* 65536 (char-code #\C))
         (* 16777216 255))
      ;; big endian
      #+(or sparc sparc-v9 powerpc ppc big-endian)
      (+ (* 16777216 (char-code #\A))
         (* 65536 (char-code #\B))
         (* 256 (char-code #\C))
         (* 1 255)))
    (format *standard-output* "~&Should be #\A: ~S"
            (uffi:ensure-char-character
             (uffi:get-slot-value u 'tunion1 'char)))
;;    (format *standard-output* "~&Should be negative number: ~D"
;;          (uffi:get-slot-value u 'tunion1 'int))
    (format *standard-output* "~&Should be positive number: ~D"
            (uffi:get-slot-value u 'tunion1 'uint))
    (uffi:free-foreign-object u))
  (values))

#+test-uffi
(defun test-union-1 ()
  (let ((u (uffi:allocate-foreign-object 'tunion1)))
    (setf (uffi:get-slot-value u 'tunion1 'uint)
          #-(or sparc sparc-v9 powerpc ppc)
          (+ (* 1 (char-code #\A))
             (* 256 (char-code #\B))
             (* 65536 (char-code #\C))
             (* 16777216 128))
          #+(or sparc sparc-v9 powerpc ppc)
          (+ (* 16777216 (char-code #\A))
             (* 65536 (char-code #\B))
             (* 256 (char-code #\C))
             (* 1 128))) ;set signed bit
    (util.test:test (uffi:ensure-char-character
                (uffi:get-slot-value u 'tunion1 'char))
               #\A
               :test #'eql
               :fail-info "Error with union character")
    #-(or sparc sparc-v9 openmcl digitool)
;;    (util.test:test (> 0 (uffi:get-slot-value u 'tunion1 'int))
;;             t
;;             :fail-info
;;             "Error with negative int in union")
    (util.test:test (plusp (uffi:get-slot-value u 'tunion1 'uint))
               t
               :fail-info
               "Error with unsigned int in union")
    (uffi:free-foreign-object u))
  (values))

#+examples-uffi
(run-union-1)


#+test-uffi
(test-union-1)

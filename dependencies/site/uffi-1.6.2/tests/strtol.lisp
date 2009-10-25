;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strtol.lisp
;;;; Purpose:       UFFI Example file to strtol, uses pointer arithmetic
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-foreign-type char-ptr (* :unsigned-char))

;; This example does not use :cstring to pass the input string since
;; the routine needs to do pointer arithmetic to see how many characters
;; were parsed

(uffi:def-function ("strtol" c-strtol)
    ((nptr char-ptr)
     (endptr (* char-ptr))
     (base :int))
  :returning :long)

(defun strtol (str &optional (base 10))
  "Returns a long int from a string. Returns number and condition flag.
Condition flag is T if all of string parses as a long, NIL if
their was no string at all, or an integer indicating position in string
of first non-valid character"
  (let* ((str-native (uffi:convert-to-foreign-string str))
         (endptr (uffi:allocate-foreign-object 'char-ptr))
         (value (c-strtol str-native endptr base))
         (endptr-value (uffi:deref-pointer endptr 'char-ptr)))

    (unwind-protect
         (if (uffi:null-pointer-p endptr-value)
             (values value t)
             (let ((next-char-value (uffi:deref-pointer endptr-value :unsigned-char))
                   (chars-parsed (- (uffi:pointer-address endptr-value)
                                    (uffi:pointer-address str-native))))
               (cond
                 ((zerop chars-parsed)
                  (values nil nil))
                 ((uffi:null-char-p next-char-value)
                  (values value t))
                 (t
                  (values value chars-parsed)))))
      (progn
        (uffi:free-foreign-object str-native)
        (uffi:free-foreign-object endptr)))))

(deftest :strtol.1 (strtol "123") 123 t)
(deftest :strtol.2 (strtol "0") 0 t)
(deftest :strtol.3 (strtol "55a") 55 2)
(deftest :strtol.4 (strtol "a") nil nil)





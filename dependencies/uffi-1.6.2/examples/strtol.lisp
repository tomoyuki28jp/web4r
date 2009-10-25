;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strtol.cl
;;;; Purpose:       UFFI Example file to strtol, uses pointer arithmetic
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)

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



#+examples-uffi
(progn
  (flet ((print-results (str)
           (multiple-value-bind (result flag) (strtol str)
             (format t "~&(strtol ~S) => ~S,~S" str result flag))))
    (print-results "55")
    (print-results "55.3")
    (print-results "a")))

#+test-uffi
(progn
  (flet ((test-strtol (str results)
           (util.test:test (multiple-value-list (strtol str)) results
                           :test #'equal
                           :fail-info "Error testing strtol")))
    (test-strtol "123" '(123 t))
    (test-strtol "0" '(0 t))
    (test-strtol "55a" '(55 2))
    (test-strtol "a" '(nil nil))))




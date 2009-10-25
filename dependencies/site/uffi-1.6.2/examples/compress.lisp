;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compress.cl
;;;; Purpose:       UFFI Example file for zlib compression
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (unless (uffi:load-foreign-library
           #-(or macosx darwin)
           (uffi:find-foreign-library
            "libz"
            '("/usr/local/lib/" "/usr/lib/" "/zlib/")
            :types '("so" "a"))
           #+(or macosx darwin)
           (uffi:find-foreign-library "z"
                                      `(,(pathname-directory *load-pathname*)))
           :module "zlib"
           :supporting-libraries '("c"))
    (warn "Unable to load zlib")))

(uffi:def-function ("compress" c-compress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
  :returning :int
  :module "zlib")

(defun compress (source)
  "Returns two values: array of bytes containing the compressed data
 and the numbe of compressed bytes"
  (let* ((sourcelen (length source))
         (destsize (+ 12 (ceiling (* sourcelen 1.01))))
         (dest (uffi:allocate-foreign-string destsize :unsigned t))
         (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (let ((result (c-compress dest destlen source-native sourcelen))
            (newdestlen (uffi:deref-pointer destlen :long)))
        (unwind-protect
            (if (zerop result)
                (values (uffi:convert-from-foreign-string
                         dest
                         :length newdestlen
                         :null-terminated-p nil)
                        newdestlen)
              (error "zlib error, code ~D" result))
          (progn
            (uffi:free-foreign-object destlen)
            (uffi:free-foreign-object dest)))))))

(uffi:def-function ("uncompress" c-uncompress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
  :returning :int
  :module "zlib")

(defun uncompress (source)
  (let* ((sourcelen (length source))
         (destsize 200000)  ;adjust as needed
         (dest (uffi:allocate-foreign-string destsize :unsigned t))
         (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (let ((result (c-uncompress dest destlen source-native sourcelen))
            (newdestlen (uffi:deref-pointer destlen :long)))
        (unwind-protect
             (if (zerop result)
                 (uffi:convert-from-foreign-string
                  dest
                  :length newdestlen
                  :null-terminated-p nil)
                 (error "zlib error, code ~D" result))
          (progn
            (uffi:free-foreign-object destlen)
            (uffi:free-foreign-object dest)))))))

#+examples-uffi
(progn
  (flet ((print-results (str)
           (multiple-value-bind (compressed len) (compress str)
             (let ((*print-length* nil))
               (format t "~&(compress ~S) => " str)
               (format t "~S~%" (map 'list #'char-code compressed))))))
    (print-results "")
    (print-results "test")
    (print-results "test2")))

#+test-uffi
(progn
  (flet ((test-compress (str)
           (multiple-value-bind (compressed len) (compress str)
             (multiple-value-bind (uncompressed len2) (uncompress compressed)
               (util.test:test str uncompressed :test #'string=
                               :fail-info "Error uncompressing a compressed string")))))
    (test-compress "")
    (test-compress "test")
    (test-compress "test2")))

;; Results of the above on my system:
;; (compress "") => 789c300001,8
;; (compress "test") => 789c2b492d2e1045d1c1,12
;; (compress "test2") => 789c2b492d2e31206501f3,13

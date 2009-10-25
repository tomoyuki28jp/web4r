;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gettime
;;;; Purpose:       UFFI Example file to get time, use C structures
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)

(uffi:def-foreign-type time-t :unsigned-long)

(uffi:def-struct tm
    (sec :int)
  (min :int)
  (hour :int)
  (mday :int)
  (mon :int)
  (year :int)
  (wday :int)
  (yday :int)
  (isdst :int))

(uffi:def-function ("time" c-time)
    ((time (* time-t)))
  :returning time-t)

(uffi:def-function ("localtime" c-localtime)
    ((time (* time-t)))
  :returning (* tm))

(uffi:def-type time-t :unsigned-long)
(uffi:def-type tm-pointer (* tm))

(defun gettime ()
   "Returns the local time"
   (uffi:with-foreign-object (time 'time-t)
;;     (declare (type time-t time))
     (c-time time)
     (let ((tm-ptr (the tm-pointer (c-localtime time))))
       (declare (type tm-pointer tm-ptr))
       (let ((time-string (format nil "~2d/~2,'0d/~d ~2d:~2,'0d:~2,'0d"
                                  (1+ (uffi:get-slot-value tm-ptr 'tm 'mon))
                                  (uffi:get-slot-value tm-ptr 'tm 'mday)
                                  (+ 1900 (uffi:get-slot-value tm-ptr 'tm 'year))
                                  (uffi:get-slot-value tm-ptr 'tm 'hour)
                                  (uffi:get-slot-value tm-ptr 'tm 'min)
                                  (uffi:get-slot-value tm-ptr 'tm 'sec)
                                  )))
         time-string))))




#+examples-uffi
(format t "~&~A" (gettime))

#+test-uffi
(progn
  (let ((time (gettime)))
    (util.test:test (stringp time) t :fail-info "Time is not a string")
    (util.test:test (plusp (parse-integer time :junk-allowed t))
                    t
                    :fail-info "time string does not start with a number")))



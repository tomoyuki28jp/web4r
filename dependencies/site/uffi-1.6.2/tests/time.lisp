;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          time.lisp
;;;; Purpose:       UFFI test file, time, use C structures
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

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
  (isdst :int)
  ;; gmoffset present on SusE SLES9
  (gmoffset :long))

(uffi:def-function ("time" c-time)
    ((time (* time-t)))
  :returning time-t)

(uffi:def-function "gmtime"
    ((time (* time-t)))
  :returning (:struct-pointer tm))

(uffi:def-function "asctime"
    ((time (:struct-pointer tm)))
  :returning :cstring)

(uffi:def-type time-t :unsigned-long)
(uffi:def-type tm-pointer (:struct-pointer tm))

(deftest :time.1
   (uffi:with-foreign-object (time 'time-t)
     (setf (uffi:deref-pointer time :unsigned-long) 7381)
     (uffi:deref-pointer time :unsigned-long))
  7381)

(deftest :time.2
  (uffi:with-foreign-object (time 'time-t)
    (setf (uffi:deref-pointer time :unsigned-long) 7381)
    (let ((tm-ptr (the tm-pointer (gmtime time))))
      (values (1+ (uffi:get-slot-value tm-ptr 'tm 'mon))
              (uffi:get-slot-value tm-ptr 'tm 'mday)
              (+ 1900 (uffi:get-slot-value tm-ptr 'tm 'year))
              (uffi:get-slot-value tm-ptr 'tm 'hour)
              (uffi:get-slot-value tm-ptr 'tm 'min)
              (uffi:get-slot-value tm-ptr 'tm 'sec)
              )))
  1 1 1970 2 3 1)


(uffi:def-struct timeval
    (secs :long)
  (usecs :long))

(uffi:def-struct timezone
    (minutes-west :int)
  (dsttime :int))

(uffi:def-function ("gettimeofday" c-gettimeofday)
    ((tv (* timeval))
     (tz (* timezone)))
  :returning :int)

(defun get-utime ()
  (uffi:with-foreign-object (tv 'timeval)
    (let ((res (c-gettimeofday tv (uffi:make-null-pointer 'timezone))))
      (values
       (+ (* 1000000 (uffi:get-slot-value tv 'timeval 'secs))
          (uffi:get-slot-value tv 'timeval 'usecs))
       res))))

(deftest :timeofday.1
    (multiple-value-bind (t1 res1) (get-utime)
      (multiple-value-bind (t2 res2) (get-utime)
        (and (or (= t2 t1) (> t2 t1))
             (> t1 1000000000)
             (> t2 1000000000)
             (zerop res1)
             (zerop res2))))
  t)

(defun posix-time-to-asctime (secs)
  "Converts number of seconds elapsed since 00:00:00 on January 1, 1970, Coordinated Universal Time (UTC)"
  (string-right-trim
   '(#\newline #\return)
   (uffi:convert-from-cstring
    (uffi:with-foreign-object (time 'time-t)
      (setf (uffi:deref-pointer time :unsigned-long) secs)
      (asctime (gmtime time))))))

(deftest :time.3
    (posix-time-to-asctime 0)
  "Thu Jan  1 00:00:00 1970")

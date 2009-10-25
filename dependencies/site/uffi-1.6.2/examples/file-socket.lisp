;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          file-socket.cl
;;;; Purpose:       UFFI Example file to get a socket on a file
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jul 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)

;; Values for linux
(uffi:def-constant PF_UNIX 1)
(uffi:def-constant SOCK_STREAM 1)

(uffi:def-function ("socket" c-socket)
    ((family :int)
     (type :int)
     (protocol :int))
    :returning :int)

(uffi:def-function ("connect" c-connect)
    ((sockfd :int)
     (serv-addr :void-pointer)
     (addr-len :int))
    :returning :int)

(defun connect-to-file-socket (filename)
  (let ((socket (c-socket PF_UNIX SOCK_STREAM 0)))
    (if (plusp socket)
        (let ((stream (c-connect socket filename (length filename))))
          stream)
      (error "Unable to create socket"))))

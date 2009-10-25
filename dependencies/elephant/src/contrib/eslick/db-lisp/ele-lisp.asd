;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-lisp.asd -- ASDF system definition for elephant lisp backend
;;; 
;;; part of
;;;
;;; Elephant Object Oriented Database: Common Lisp Backend
;;;
;;; Copyright (c) 2007 by Ian Eslick
;;; <ieslick@common-lisp.net>
;;;
;;; Elephant and Elephant Lisp Backend users are granted the rights to
;;; distribute and use this software as governed by the terms of the
;;; Lisp Lesser GNU Public License (http://opensource.franz.com/preamble.html),
;;; also known as the LLGPL.

(in-package :cl-user)

(defpackage ele-lisp-system
  (:use :cl :asdf :elephant-system))

(in-package :ele-lisp-system)

;;
;; System definition
;;

(defsystem ele-lisp
  :name "elephant-db-lisp"
  :author "Ian Eslick <ieslick@common-lisp.net>"
  :version "0.7.0"
  :maintainer "Ian Eslick <ieslick@common-lisp.net>"
  :licence "LLGPL"
  :description "Lisp backend for the Elephant persistent object database"
  :components
  ((:module :src
    :components
    ((:module :contrib
      :components
      ((:module :eslick
       :components
       ((:module :db-lisp
        :components
        ((:file "package")
	 (:file "file")
	 (:file "pages")
	 (:file "log")
	 (:file "btree")
	 (:file "transactions")
	 (:file "btree-ops")
	 (:file "lisp-transactions")
	 (:file "lisp-slots")
	 (:file "lisp-collections")
	 (:file "lisp-controller"))
	:serial t))))))))
   :depends-on (:elephant))


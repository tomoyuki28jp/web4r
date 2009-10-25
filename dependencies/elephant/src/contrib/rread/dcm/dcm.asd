;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; dcm.asd -- ASDF system definition for DCM
;;; 
;;; Copyright (c) 2005 Robert L. Read <read@robertlread.net>
;;; All rights reserverd.
(defsystem dcm
  :name "dcm"
  :author "Robert L. Read <read@robertlread.net>"
  :version "0.1"
  :maintainer "Robert L. Read <read@robertlread.net"
  :licence "All Rights Reserverd"
  :description "A simple object prevalence system with strategies"
  :long-description "An object prevalence system with strategies built on Elephant"
  :depends-on (:elephant)	
  :components
  ((:file "dcm-package")		
   (:file "dcm-macros")		
   (:file "dcm" :depends-on ("dcm-package" "dcm-macros"))
   (:file "gdcm" :depends-on ("dcm" "dcm-macros"))
   (:file "dcm-tests" :depends-on ("dcm" "gdcm" "dcm-macros"))
   )
   :serial t
)	


; To run:
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. go to http://localhost:8080/bbs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :bbs (:use :cl :web4r))
(in-package :bbs)

(ele:open-store '(:clsql (:postgresql "localhost" "test" "postgres" "pgpass")))
(defpclass bbs () ((title :length 256) (body  :length 3000)))
(defpages bbs)

(defparameter *srv* (start-server))
;(stop-server *srv*)

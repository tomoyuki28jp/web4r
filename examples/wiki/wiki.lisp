; To run:
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. go to http://localhost:8080/wiki

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :wiki (:use :cl :web4r))
(in-package :wiki)

(open-store *example-bdb*)

(defpclass wiki ()
  ((title :length 256 :index t)
   (body  :length 3000)))

(defpages wiki :edit-sml (example-path "wiki/sml/edit.sml")
               :show-sml (example-path "wiki/sml/show.sml"))

(defparameter *srv* (start-server))
;(stop-server *srv*)

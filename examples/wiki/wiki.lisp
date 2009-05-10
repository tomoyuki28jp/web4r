; To run:
; 1. Configure elephant
; 2. Replace the parameter of open-store if you don't have Elephant configured 
;    to work with the Berkeley DB
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 3. Compile and load this file
; 4. Go to http://localhost:8080/wiki

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :wiki (:use :cl :web4r))
(in-package :wiki)

(open-store *example-bdb*)

(defpclass wiki ()
  ((title :length 256 :index t)
   (body  :length 3000)))

(genpages wiki :edit-sml (example-path "wiki/sml/edit.sml")
               :show-sml (example-path "wiki/sml/show.sml"))

(defparameter *srv* (start-server))
;(stop-server *srv*)

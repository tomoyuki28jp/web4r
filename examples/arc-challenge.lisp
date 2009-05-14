; Take the Arc Challenge
; http://www.paulgraham.com/arcchallenge.html
; 
; To run:
; 1. Compile and load this file
; 2. Go to http://localhost:8080/said

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :arc (:use :cl :web4r :sml))
(in-package :arc)

; Arc:   codetree of 23 nodes: 15 leaves/tokens + 8 interior nodes
;(defop said req
;  (aform [w/link (pr "you said: " (arg _ "foo"))
;           (pr "click here")]
;    (input "foo") 
;    (submit)))

; web4r: codetree of 18 nodes: 11 leaves/tokens + 7 interior nodes
(defpage said ()
  (form/cont (a/cont [p "You said: " (last-post "foo")] "click here")
   (input-text "foo")))

(defparameter *srv* (start-server))
;(stop-server *srv*)

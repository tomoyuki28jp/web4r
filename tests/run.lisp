; run tests
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. run any test you want

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r-tests))

(in-package :web4r-tests)

(ele:open-store
 '(:clsql (:postgresql "localhost" "test" "postgres" "pgpass")))

; run each test
;(5am:run! 'server)

; run all tests
;(5am:run! 'web4r)

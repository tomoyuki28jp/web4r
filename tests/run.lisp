; run tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r-tests))

(in-package :web4r-tests)

; run each test
;(5am:run! 'server)

; run all tests
;(5am:run! 'web4r)

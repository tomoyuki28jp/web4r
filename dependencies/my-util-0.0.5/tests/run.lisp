(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :my-util-tests))

(in-package :my-util-tests)

; run all tests
;(5am:run! 'my-util)

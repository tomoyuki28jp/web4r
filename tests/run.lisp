(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r-tests))

(in-package :web4r-tests)

(ele:open-store *test-bdb*)
(defvar *srv* (start-server))
;(5am:run! 'web4r)

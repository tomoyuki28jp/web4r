(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r-tests))

(in-package :web4r-tests)

(ele:open-store
 `(:BDB ,(merge-pathnames "tests/bdb/" *web4r-dir*)))
(defvar *srv* (start-server))
;(run! 'web4r)

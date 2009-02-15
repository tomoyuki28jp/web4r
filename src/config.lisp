(in-package :web4r)

; Is there a way to automatically specify this path?
; I could not use *load-truename* for this.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *web4r-dir* "/home/tomo/src/lisp/web4r/"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *shtml-dir* (merge-pathnames "shtml/" *web4r-dir*)))

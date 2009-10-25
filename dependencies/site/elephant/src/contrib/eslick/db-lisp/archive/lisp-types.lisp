
(in-package :db-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map lisp types to binary types

(defparameter *lisp-binary-typemap*
  '((fixnum          . u32)
    (char            . iso-8859-1-char)
    (single-float    . single-float)
    (double-float    . double-float)
    (negative-bignum . bignum)
    (positive-bignum . bignum)
    (rational        . rational)
    (null            . +null+)

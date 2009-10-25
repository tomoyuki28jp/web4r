(in-package :cl-user)

(macrolet
    ((define-package ()
       `(defpackage :my-util
          (:use :cl :anaphora :cl-ppcre)
          (:export ; --- util ---
                   :pm
                   :pm1
                   :asdf-version
                   :asdf-version=
                   :asdf-version<=
                   :when-let
                   :with-gensyms
                   :->string
                   :->string-down
                   :->string-up
                   :->list
                   :->int
                   :->keyword
                   :->symbol
                   :hash->alist
                   :concat
                   :join
                   :run-hooks
                   :run-hook-with-args
                   :add-hook
                   :rem-hook
                   ; --- anaphora ---
                   ,@(loop for s being the external-symbol
                           in :anaphora collect s)
                   ; --- cl-ppcre ---
                   ,@(loop for s being the external-symbol
                           in :cl-ppcre collect s)
                   ))))
  (define-package))

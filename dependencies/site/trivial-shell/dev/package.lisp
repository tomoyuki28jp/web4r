(in-package #:common-lisp-user)

(defpackage #:trivial-shell
  (:use #:common-lisp)
  (:nicknames #:metashell)
  (:export 
   #:shell-command
   #:with-timeout
   #:*bourne-compatible-shell*
   #:*shell-search-paths*
   ;; conditions
   #:timeout-error
   #:timeout-error-command))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   #+allegro 
   '(mp:process-wait-with-timeout)
   #+clisp
   '()
   #+(and cmu mp)
   '(mp:process-wait-with-timeout)
   #+(and cmu (not mp))
   '()
   #+cormanlisp
   '()
   #+digitool
   '(ccl:process-wait-with-timeout)
   #+lispworks
   '(mp:process-wait-with-timeout)
   #+openmcl
   '(ccl:process-wait-with-timeout)
   #+(and sbcl sb-threads)
   '(sb-threads:make-semaphore
     sb-threads:signal-semaphore)
   #+(and sbcl (not sb-threads))
   '()
   '#:trivial-shell))

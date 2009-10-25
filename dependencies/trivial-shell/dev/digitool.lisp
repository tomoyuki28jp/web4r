(in-package #:trivial-shell)

(defun shell-command (command) 
  (ccl:do-shell-script command))

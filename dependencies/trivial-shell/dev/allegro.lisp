(in-package #:trivial-shell)

(defun %shell-command (command input)
  (multiple-value-bind (output error status)
      (excl.osi:command-output 
       command :whole t
       :input input)
    (values output error status)))
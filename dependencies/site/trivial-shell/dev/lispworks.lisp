(in-package #:trivial-shell)

(defun shell-command (command)
  ;; BUG: Lispworks combines output and error streams
  (let ((output (make-string-output-stream)))
    (unwind-protect
      (let ((status
             (system:call-system-showing-output
              command
              :prefix ""
              :show-cmd nil
              :output-stream output)))
        (values (get-output-stream-string output) nil status))
      (close output))))
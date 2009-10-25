(in-package #:trivial-shell)

(defun shell-command (command)
  (let* ((process (create-shell-process command t))
         (output (file-to-string-as-lines 
                  (ccl::external-process-output-stream process)))
         (error (file-to-string-as-lines
                 (ccl::external-process-error-stream process))))
    (close (ccl::external-process-output-stream process))
    (close (ccl::external-process-error-stream process))
    (values output
            error
            (process-exit-code process))))

(defun create-shell-process (command wait)
  (ccl:run-program
   *bourne-compatible-shell*
   (list "-c" command)
   :input nil :output :stream :error :stream
   :wait wait))

(defun process-alive-p (process)
  (eq (nth-value 0 (ccl:external-process-status process)) :running))

(defun process-exit-code (process)
  (nth-value 1 (ccl:external-process-status process)))
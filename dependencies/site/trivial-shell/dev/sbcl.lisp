(in-package #:trivial-shell)

(defun %shell-command (command input)
  (with-input (input-stream (or input :none))
    (let* ((process (sb-ext:run-program
                     *bourne-compatible-shell*
                     (list "-c" command)
		     :wait nil :input input-stream :output :stream :error :stream))
	   (output-thread (sb-thread:make-thread
                           #'(lambda ()
                               (file-to-string-as-lines
                                (sb-impl::process-output process)))))
	   (error-thread (sb-thread:make-thread
                          #'(lambda ()
                              (file-to-string-as-lines
                               (sb-impl::process-error process))))))
      (let ((error-code (sb-impl::process-exit-code (sb-impl::process-wait process)))
            (output-string (sb-thread:join-thread output-thread))
            (error-string (sb-thread:join-thread error-thread)))
        (close (sb-impl::process-output process))
        (close (sb-impl::process-error process))
        (values output-string error-string error-code)))))

(defun create-shell-process (command wait)
  (sb-ext:run-program
   *bourne-compatible-shell*
   (list "-c" command)
   :input nil :output :stream :error :stream :wait wait))

(defun process-alive-p (process)
  (sb-ext:process-alive-p process))

(defun process-exit-code (process)
  (sb-ext:process-exit-code process))

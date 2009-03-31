(in-package :web4r)

(defvar *debug-log-file* "/tmp/debug.log")

(defvar *debug-mode* nil)

; --- Util ------------------------------------------------------

(defmacro pm  (expr)
 "pretty print macroexpand"
  `(pprint (macroexpand   ',expr)))

(defmacro pm1 (expr)
 "pretty print macroexpand-1"
  `(pprint (macroexpand-1 ',expr)))

(defun ps (instance)
 "print each slot's name and value"
  (loop for slot in (ele::class-slots (class-of instance))
        as name = (ele::slot-definition-name slot)
        do (print  (list name
                         (or (ignore-errors
                               (slot-value instance name))
                             :unbound)))))

(defun hash->list (hash)
  (declare (hash-table hash))
  (loop for key being the hash-key of hash
     using (hash-value value)
     collect (cons key value)))

(defmacro with-post-parameters (parameters &rest body)
  `(let* ((*acceptor* (make-instance 'acceptor))
          (*reply*    (make-instance (acceptor-reply-class *acceptor*)))
          (*request*  (make-instance 'request :headers-in '())))
     (set-post-parameters ,parameters)
     ,@body))

(defmacro with-get-parameters (parameters &rest body)
  `(let* ((*acceptor* (make-instance 'acceptor))
          (*reply*    (make-instance (acceptor-reply-class *acceptor*)))
          (*request*  (make-instance 'request :headers-in '())))
     (set-get-parameters ,parameters)
     ,@body))

; --- Logging ---------------------------------------------------

(defun debug-log (&rest args)
  (let ((*message-log-pathname* *debug-log-file*))
    (log-message :debug (join " " args))))

(defun backtrace-log (error)
  (when *debug-mode*
    (with-open-file (s *debug-log-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (trivial-backtrace::print-backtrace error :output s))))

; --- Debug mode ------------------------------------------------

(defun debug-mode-on ()
  (setf *debug-mode* t
        *show-lisp-errors-p* t))

(defun debug-mode-off ()
  (setf *debug-mode* nil
        *show-lisp-errors-p* nil))

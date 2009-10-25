(in-package :my-util)

; --- Debugging -------------------------------------------------

(defmacro pm  (expr)  `(pprint (macroexpand   ',expr)))
(defmacro pm1 (expr)  `(pprint (macroexpand-1 ',expr)))

; --- ASDF versions ---------------------------------------------

(defun asdf-version (name)
  (asdf:component-version (asdf:find-system name)))

(defun asdf-version=  (name version)
  (or (string= (asdf-version name) version)
      (error "~S must be version ~A" name version)))

(defun asdf-version<= (name version)
  (flet ((int (x) (parse-integer (remove #\. x))))
    (or (<= (int version) (int (asdf-version name)))
        (error "~S must be version ~A or higher" name version))))

; --- Bindings --------------------------------------------------

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

; --- Type casting ----------------------------------------------

(defun ->string (x)
  (if (stringp x)
      x
      (with-output-to-string (s)
        (when x (princ x s)))))

(defun ->string-down (x) (string-downcase (->string x)))
(defun ->string-up   (x) (string-upcase   (->string x)))

(defun ->list (x)
  (if (listp x)
      x
      (list x)))

(defun ->int (x)
  (if (integerp x)
    x
    (ignore-errors (parse-integer x))))

(defun ->keyword (x)
  (if (keywordp x)
      x
      (intern (->string-up x) :keyword)))

(defun ->symbol (x)
  (intern (->string (->string-up x))))

(defun hash->alist (hash)
  (declare (hash-table hash))
  (loop for key being the hash-key of hash
     using (hash-value value)
     collect (cons key value)))

; --- Strings ---------------------------------------------------

(defun concat (&rest args)
  (apply #'concatenate 'string (mapcar #'->string args)))

(defun join (joiner &rest args)
  (format nil (concat "~{~A~^" (->string joiner) "~}")
          (remove nil args)))

; --- Hooks -----------------------------------------------------

(defvar *hooks* (make-hash-table))

(defun run-hooks (&rest hooks)
  (dolist (hook hooks)
    (dolist (function (gethash hook *hooks*))
      do (funcall function))))

(defun run-hook-with-args (hook &rest args)
  (dolist (function (gethash hook *hooks*))
    do (apply function args)))

(defun add-hook (hook function)
  (check-type function function)
  (let ((functions (gethash hook *hooks*)))
    (unless (member function functions)
      (setf (gethash hook *hooks*)
            (append functions (list function))))))

(defun rem-hook (hook function)
  (check-type function function)
  (setf (gethash hook *hooks*)
        (remove function (gethash hook *hooks*))))

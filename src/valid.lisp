(in-package :web4r)

; --- Util ------------------------------------------------------

(defun error-msg (type &rest args)
  (awhen (cdr (assoc type *error-formats*))
    (apply #'format (append (list nil it) args))))

(defun leap-year-p (year)
  (check-type year integer)
  (and (plusp year)
       (zerop (mod year 4))
       (or (plusp (mod year 100))
           (zerop (mod year 400)))))

(defun days-of (year month)
  (let ((feb (if (leap-year-p year) 29 28)))
    (nth (1- month) `(31 ,feb 31 30 31 30 31 31 30 31 30 31))))

(defun valid-date-p (y m d)
  (let ((y (->int y)) (m (->int m)) (d (->int d)))
    (when (and y m d)
      (aand (days-of y m) (<= d it) t))))

(defun valid-email-p (email)
  (scan-to-strings #.(regex-replace-all #\Newline
"^(?:(?:(?:(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+)
(?:\.(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+))*)|
(?:\"(?:\\[^\r\n]|[^\\\"])*\")))\@
(?:(?:(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+)
(?:\.(?:[a-zA-Z0-9_!#\$\%&'*+/=?\^`{}~|\-]+))*))$" "") email))

(defun empty (x)
  (let ((x (if (listp x) (remove nil x) x)))
    (when (or (null x) (equal x ""))
      t)))

; --- Validators ------------------------------------------------

(defmacro define-validator (name args &rest body)
  `(setf (gethash (->keyword ',name) *validators*)
         (lambda ,args ,@body)))

(defun get-validator (name)
  (gethash name *validators*))

(define-validator length (label value args)
  (unless (or (empty value) (listp value))
    (destructuring-bind (min &optional max)
        (if (aand args (listp it)) args (list nil args))
      (let ((len (if (pathnamep value)
                     (or (file-length* value) 0)
                     (length value))))
        (cond ((and max (> len max)) (error-msg :too-long  label max))
              ((and min (< len min)) (error-msg :too-short label min)))))))

(define-validator type (label value args)
  (unless (empty value)
    (macrolet ((is (fn error) `(unless ,fn (error-msg ,error label))))
      (let ((type (if (listp args) (car args) args)))
        (case type
          (:date    (is (apply #'valid-date-p value)  :invalid))
          (:alpha   (is (scan "^[a-zA-Z]+$" value)    :not-alpha))
          (:alnum   (is (scan "^[a-zA-Z0-9]+$" value) :not-alnum))
          (:integer (is (scan "^[0-9]+$" value)       :not-a-number))
          (:email   (is (valid-email-p value)         :invalid))
          (:regex   (is (scan (nth 1 args) value)     :invalid))
          (:image   (is (image-file-p value)          :not-a-image))
          (:member  (loop for v in (remove nil (->list value))
                          unless (member v (cadr args) :test #'equal)
                          return (error-msg :invalid label)))
          (otherwise (awhen type (error "invalid type: ~A" it))))))))

(define-validator nullable (label value args)
  (when (and (null args) (empty value))
    (error-msg :empty label)))

(define-validator unique (label value args)
  (destructuring-bind (class slot &optional ins) args
    (awhen (get-instances-by-value class slot value)
      (when (or (> (length it) 1) (not (eq (car it) ins)))
        (error-msg :not-a-unique label)))))

; --- Validations -----------------------------------------------

(defun validation-errors (label value validations)
  (loop for (type args) on validations by #'cddr
        as validator = (or (get-validator type)
                           (error "invalid validator: ~A" type))
        as error = (funcall validator label value args)
        when error collect error until error))

(defmacro with-validations (validations error-handler body)
  `(aif (append ,@(loop for v in validations
                        collect `(validation-errors ,@v)))
        (funcall ,error-handler it)
        ,body))

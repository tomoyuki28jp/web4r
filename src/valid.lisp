(in-package :web4r)

; --- Util ------------------------------------------------------

(defun error-msg (error &rest args)
  "Returns a string validation error message for the ERROR."
  (awhen (cdr (assoc error *error-formats*))
    (apply #'format (append (list nil it) args))))

(defun leap-year-p (year)
  "Returns true if the YEAR is a leap year and nil otherwise."
  (and (plusp year)
       (zerop (mod year 4))
       (or (plusp (mod year 100))
           (zerop (mod year 400)))))

(defun days-of (year month)
  "Returns the last day of the MONTH of the YEAR."
  (let ((feb (if (leap-year-p year) 29 28)))
    (nth (1- month) `(31 ,feb 31 30 31 30 31 31 30 31 30 31))))

(defun valid-date-p (year month day)
  "Returns true if the date if valid and nil otherwise."
  (let ((year (->int year)) (month (->int month)) (day (->int day)))
    (when (and year month day)
      (aand (days-of year month) (<= day it) t))))

(defun empty (x)
  "Returns true if X is nil, '' or a list contains only nil and nil otherwise."
  (let ((x (if (listp x) (remove nil x) x)))
    (when (or (null x) (equal x ""))
      t)))

(defun unique-p (class slot value &optional ins)
  "Returns nil if the same VALUE has been registered in the SLOT of the CLASS 
and true otherwise."
  (let* ((i (get-instances-by-value class slot value))
         (l (length i)))
    (or (= l 0)
        (and (= l 1) (eq (car i) ins)))))

; --- Validators ------------------------------------------------

(defmacro define-validator (name args &rest body)
  "Defines a new validator named NAME."
  `(setf (gethash (->keyword ',name) *validators*)
         (lambda ,args ,@body)))

(defun get-validator (name)
  "Returns a validator named NAME."
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

(define-validator format (label value args)
  (unless (empty value)
    (macrolet ((is (fn error) `(unless ,fn (error-msg ,error label))))
      (case args
        (:alpha   (is (scan "^[a-zA-Z]+$"        value) :not-alpha))
        (:alnum   (is (scan "^[a-zA-Z0-9]+$"     value) :not-alnum))
        (:integer (is (scan "^[0-9]+$"           value) :not-a-number))
        (:email   (is (scan *valid-email-format* value) :invalid))
        (:date    (is (apply #'valid-date-p value)      :invalid))
        (:image   (is (image-file-p value)              :not-a-image))
        (otherwise (cond ((stringp args) (is (scan args value) :invalid))
                         ((functionp args) (funcall args value))
                         (t (error "invalid format: ~A" args))))))))

(define-validator member (label value args)
  (unless (empty value)
    (loop for v in (remove nil (->list value))
          unless (member v args :test #'equal)
          return (error-msg :invalid label))))

(define-validator required (label value args)
  (when (and args (empty value))
    (error-msg :empty label)))

(define-validator unique (label value args)
  (destructuring-bind (class slot &optional ins) args
    (unless (unique-p class slot value ins)
      (error-msg :not-a-unique label))))

; --- Validations -----------------------------------------------


(defun validation-errors (label value validators)
  "Validates the VALUE with VALIDATORS and returns error messages if any. 
LABEL is used as the subject of the error messages.
Example:
 (validation-errors \"label\" \"12345\" '(:length 3))
 ;=> (\"label is too long (maximum is 3 characters)\")"
  (loop for (type args) on validators by #'cddr
        as validator = (or (get-validator type)
                           (error "invalid validator: ~A" type))
        as error = (funcall validator label value args)
        when error collect error until error))

(defmacro with-validations (validations error-handler body)
  "Executes the VALIDATIONS and the ERROR-HANDLER if any error 
and BODY otherwise. The ERROR-HANDLER takes one argument which is 
a list of validation error messages.
Example:
 (with-validations ((\"1\" \"v\" '(:required t))
                    (\"2\" nil '(:required t)))
   (lambda (e) e)
   \"ok\")
 ; => (\"2 can't be empty\")"
  `(aif (append ,@(loop for v in validations
                        collect `(validation-errors ,@v)))
        (funcall ,error-handler it)
        ,body))

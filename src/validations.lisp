; -*- Coding: utf-8; -*-
(in-package :web4r)

; --- Error Messages --------------------------------------------

(defvar *error-msgs-en*
  #.(hash
     :invalid      "~A is invalid"
     :confirmation "~A doesn't match confirmation"
     :empty        "~A can't be empty"
     :too-long     "~A is too long (maximum is ~D characters)"
     :too-short    "~A is too short (minimum is ~D characters)"
     :too-big      "~A is too big (maximum is ~D bytes)"
     :too-small    "~A is too small (minimum is ~D bytes)"
     :not-a-number "~A is not a number"
     :not-alpha    "~A must contains only alphabetic characters"
     :not-alnum    "~A must contains only alphabetic and digit characters"
     :not-unique   "The same ~A has already been registered"
     :not-a-image  "~A must be a jpeg, png or gif image file."
     ))

(defvar *error-msgs-ja*
  #.(hash
     :invalid      "~Aが不正です"
     :confirmation "~Aが一致しません"
     :empty        "~Aを入力して下さい"
     :too-long     "~Aを~D文字以内で入力して下さい"
     :too-short    "~Aを~D文字以上で入力して下さい"
     :too-big      "~Aを~Dバイト以内で入力して下さい"
     :too-small    "~Aを~Dバイト以上で入力して下さい"
     :not-a-number "~Aを数値で入力して下さい"
     :not-alpha    "~Aをアルファベットで入力して下さい"
     :not-alnum    "~Aを半角英数字で入力して下さい"
     :not-unique   "既に同じ~Aが登録されています"
     :not-a-image  "~Aをjpeg, gif, png形式の画像ファイルでアップロードして下さい"
     ))

(defvar *error-msgs* *error-msgs-en*)

(defun get-error-msg (key &rest args)
  (apply #'format
         (append (list nil (gethash key *error-msgs*))
                 args)))

; --- Validators ------------------------------------------------

(defvar *validators* (make-hash-table))

(defun set-validator (type validator)
  (sethash type *validators* validator))

(defun get-validator (type)
  (gethash type *validators*))

(set-validator :length
  (lambda (label val arg)
    (when arg
      (destructuring-bind (min &optional max)
          (if (listp arg) arg (list nil arg))
        (let* ((file-p (listp val))
               (len (if file-p
                        (aif (assoc-ref "size" val :test #'equalp) it 0)
                        (length val))))
          (unless (or (and file-p (eq len 0)) (null val))
            (if (and max (> len max))
                (get-error-msg (if file-p :too-big :too-long) label max)
                (when (and min (< len min))
                  (get-error-msg (if file-p :too-small :too-short) label min)))))))))

(set-validator :type
     (lambda (label val arg)
       (let ((type (if (listp arg) (car arg) arg)))
         (flet ((regex-validation (regex error-type)
                  (unless (preg-match regex val)
                    (get-error-msg error-type label))))
           (case type
             (:member
              (loop for v in (->list val)
                    unless (member v (nth 1 arg) :test #'equal)
                    return (get-error-msg :invalid label)))
             ; todo: better error checking on the date
             (:date
              (block date
                (flet ((validate-date (regex n)
                         (unless (preg-match regex (nth n val))
                           (return-from date
                             (get-error-msg :invalid label)))))
                  (validate-date "^\\d{4}$" 0)
                  (validate-date "^([1-9]|10|11|12)$" 1)
                  (validate-date "^([1-9]|[1|2][0-9]|30|31)$" 2))))
             (:alpha   (regex-validation "^[a-zA-Z]+$"    :not-alpha))
             (:alnum   (regex-validation "^[a-zA-Z0-9]+$" :not-alnum))
             (:integer (regex-validation "^\\d*$"         :not-a-number))
             (:email   (regex-validation "^[a-zA-Z0-9_\.\-]+?@[A-Za-z0-9_\.\-]+$"
                                                          :invalid))
             (:regex   (regex-validation arg              :invalid))
             (:image   (awhen (assoc-ref "save-name" val :test #'equalp)
                         (unless (image-file-p (upload-file-path it))
                           (get-error-msg :not-a-image label))))
             (otherwise
              (when arg (error "invalid type: ~A" type))))))))

(set-validator :nullable
     (lambda (label val arg)
       (when (and (not arg)
                  (or (null val)
                      (and (stringp val)
                           (string= val ""))
                      (and (listp val)
                           (listp (car val)) ; avoid date type
                           (awhen (assoc-ref "size" val :test #'equal)
                             (eq it 0)))))
         (get-error-msg :empty label))))

(set-validator :unique
     (lambda (label val arg)
       (when (and (listp arg) (< 2 (length arg)))
         (destructuring-bind (class slot &optional ins) arg
           (awhen (get-instances-by-value class slot val)
             (when (or (> (length it) 1)
                       (not (eq (car it) ins)))
               (get-error-msg :not-unique label)))))))

; --- Validations -----------------------------------------------

(defun validation-errors (label val validations)
  "returns error messages if there is any"
  (loop for (type arg) on validations by #'cddr
        as validator = (or (get-validator type)
                           (error "invalida validator: ~A" type))
        as error-msg = (funcall validator label val arg)
        when error-msg collect error-msg
        until error-msg))

(defmacro with-validations (validations error-handler body)
  `(aif (append ,@(loop for v in validations collect
                        `(validation-errors ,@v)))
        (funcall ,error-handler it)
        ,body))

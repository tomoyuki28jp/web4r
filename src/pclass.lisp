(in-package :web4r)

; --- Slots -----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass slot-options ()
     ((symbol   :accessor slot-symbol   :initarg :symbol   :type symbol
                :documentation "slot symbol")
      (id       :accessor slot-id       :initarg :id       :type string
                :documentation "form id")
      (label    :accessor slot-label    :initarg :label    :type string
                :documentation "form label")
      (unique   :accessor slot-unique   :initarg :unique   :initform nil
                :documentation "uniqueness of slot values")
      (required :accessor slot-required :initarg :required :initform nil
                :documentation "required")
      (rows     :accessor slot-rows     :initarg :rows     :initform nil
                :documentation "int rows for textarea")
      (cols     :accessor slot-cols     :initarg :cols     :initform nil
                :documentation "int cols for textarea")
      (size     :accessor slot-size     :initarg :size     :initform nil
                :documentation "int size for text input")
      (length   :accessor slot-length   :initarg :length   :initform nil
                :documentation "int max or list '(min max) for length validation")
      (hide-for :accessor slot-hide-for :initarg :hide-for :initform nil
                :documentation "where to hide the slot. :all for all or regexp to
hide it only on pages where the request uri matches to the regexp")
      (options  :accessor slot-options  :initarg :options  :initform '() :type list
                :documentation "form input options (also used for validation)")
      (comment  :accessor slot-comment  :initarg :comment  :initform ""  :type string
                :documentation "comment to display")
      (input    :accessor slot-input    :initarg :input    :initform nil
                :documentation "form input type - :text, :textarea, :radio, 
:checkbox, :select, :password or :file")
      (format   :accessor slot-format   :initarg :format   :initform nil
                :documentation "validation type - :alpha, :alnum, :integer, :email
:date, :image ,regexp in string or a function")
      (type     :accessor slot-type     :initarg :type    :initform nil :type symbol
                :documentation "slot definition type"))
    (:documentation "Extended slot options")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slot-type* (slot-def)
    "returns a type specifier from a slot definition"
    (acond ((member :type slot-def) (nth 1 it))
           ((aand (member :format slot-def)
                  (member (nth 1 it) '(:date :integer integer)))
            'integer)
           ((member :options slot-def) 'list)
           (t 'string))))

(defun get-slots (class)
  (gethash class *slots*))

(defun get-slot (class symbol)
  (find-if #'(lambda (s) (eq (slot-symbol s) symbol))
           (get-slots class)))

(defun get-slot-by-id (class id)
  (car (get-slots-if
        #'(lambda (s)
            (or (equal id (slot-id* class s))
                (equal id (slot-id s)))) ; for a inherited slot
        class)))

(defun get-excluded-slots (class)
  (if (eq *with-slots* :all)
      (get-slots class)
      (loop for s in (get-slots class)
            as symbol = (slot-symbol s)
            unless (or (aand (slot-hide-for s)
                             (or (eq it :all) (scan it (request-uri*))))
                       (member symbol *without-slots*)
                       (aand *with-slots* (not (member symbol it))))
            collect s)))

(defun get-slots-if (fn class)
  (remove-if-not fn (get-excluded-slots class)))

(defun file-slots (class)
  (get-slots-if #'(lambda (s) (eq (slot-input s)  :file)) class))

(defun date-slots (class)
  (get-slots-if #'(lambda (s) (eq (slot-format s) :date)) class))

(defun unique-slots (class) (get-slots-if #'slot-unique class))

(defun indexed-slots (class)
  (get-slots-if #'(lambda (s) (indexed-slot-p class (slot-symbol s)))
                class))

(defun indexed-slot-p (class slot)
  (map-indices #'(lambda (k v)
                   (declare (ignore v))
                   (when (eq k slot)
                     (return-from indexed-slot-p t)))
               (find-class-index class)))

(defun slot-id* (class slot)
  (concat (->string-down class) "_"
          (regex-replace-all "-" (->string-down (slot-symbol slot)) "_")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-slot (class slot)
    (setf slot (->list slot))
    (flet ((opt (x) (awhen (member x slot) (nth 1 it))))
      (let* ((id  (regex-replace-all "-" (->string-down (car slot)) "_")))
        (append
         (list :symbol   (car slot)
               :id       (concat (->string-down class) "_" id)
               :label    (or (opt :label)
                             (string-capitalize (regex-replace-all "_" id " ")))
               :input    (or (opt :input)
                             (aand (opt :length) (< 256 (if (atom it) it (car it)))
                                   :textarea))
               :format   (or (opt :format)
                             (aand (opt :type) (eq it 'integer) :integer))
               :type     (slot-type* slot)
               :hide-for (or (opt :hide-for)
                             (aand (opt :initform) (equal it '(ele:make-pset)) :all))
               :required (aif (member :required slot) (nth 1 it) t))
         (let ((fn (lambda (x) (awhen (opt x) (list x it))))
               (op '(:unique :length :size :rows :cols :comment :options)))
           (apply #'append (remove nil (mapcar fn op)))))))))

(defgeneric slot-display-value (instance slot &key nl->br))
(defmethod slot-display-value (instance (slot slot-options) &key (nl->br nil))
  (aif (ignore-errors (slot-value instance (slot-symbol slot)))
    (let ((input (slot-input slot)))
      (cond ((and nl->br (eq input :textarea)) (safe (nl->br (escape it))))
            ((eq (slot-format slot) :image)
             (load-sml-path "form/display/image.sml"))
            ((eq (slot-format slot) :date)
             (aand (split-date it) (apply #'join "-" it)))
            ((eq input :checkbox) (apply #'join (append '(", ") it)))
            (t it)))
    ""))

(defgeneric slot-save-value (slot &optional value))
(defmethod slot-save-value ((slot slot-options) &optional value)
  (with-slots (format id input options type) slot
    (let ((value (or value (post-parameter id))))
      (cond ((eq format :date)
             (destructuring-bind (&optional y m d) (posted-date id)
               (when y (->int (format nil "~4,'0d~2,'0d~2,'0d"
                                      (->int y) (->int m) (->int d))))))
            ((eq type 'integer) (->int value))
            ((eq input :file)
             (awhen (cont-session slot)
               (rem-cont-session slot)
               (pathname-name
                (save-file (image-path it "tmp") *upload-save-dir*))))
            ((eq input :checkbox) (posted-checkbox id))
            (t value)))))

(defun split-date (date)
  "split a date in 8 digits like 19830928 into a list like '(\"1983\" \"09\" \"28\")"
  (aand (->string date)
        (when (= (length it) 8)
          (list (subseq it 0 4) (subseq it 4 6) (subseq it 6 8)))))

; --- Forms for slots -------------------------------------------

(defgeneric form-valid-attr (class slot &optional ins))
(defmethod form-valid-attr (class (slot slot-options) &optional ins)
  "attributes of form input for jquery validation
http://docs.jquery.com/Plugins/Validation"
  (with-slots (required format length input unique) slot
    (append (awhen (append (when required '("required"))
                           (when (eq format :email)   '("email"))
                           (when (eq format :integer) '("number"))
                           (when (stringp format)     '("format")))
              `(:class ,(apply #'join " " it)))
            (awhen (and (not (eq input :file)) length)
              (append (aand (when (listp it) (car it))   `(:minlength ,it))
                      (aand (if (atom it) it (nth 1 it)) `(:maxlength ,it))))
            (when (stringp format) `(:format ,format))
            (when unique
              `(:remote ,(concat "/ajax/" (->string-down class)  "/unique/"
                                 (awhen ins (oid it))))))))

(defun form-value (slot-id slot-symbol &optional ins)
  (or (post-parameter slot-id)
      (aand ins (ignore-errors (slot-value it slot-symbol)))))

(defgeneric form-input (class slot &optional ins))
(defmethod form-input (class (slot slot-options) &optional ins)
  (with-slots (input format label id length symbol options size) slot
    (let* ((saved (aand ins (ignore-errors (slot-value it symbol))))
           (value (or (post-parameter id) saved)))
      (cond ((eq input :select)
             (select-form id options :selected value))
            (options
             (loop for o in options as oid = (concat id "_" o)
                   do (if (eq input :checkbox)
                          (load-sml-path "form/input/checkbox.sml")
                          (load-sml-path "form/input/radio.sml"))))
           ((eq format :date)
            (destructuring-bind (&optional y* m* d*) (split-date value)
              (destructuring-bind (&optional y m d) (posted-date id)
                (select-date id :y (or y y*) :m (or m m*) :d (or d d*)))))
           ((eq input :textarea)
            (with-slots (rows cols) slot
              (load-sml-path "form/input/textarea.sml")))
           ((eq input :file)
            (let* ((type (if (cont-session slot) "tmp" (when saved "upload")))
                   (file (aand (image-path (or (cont-session slot) saved) type)
                               (pathname-name it)))
                   (del-id (concat id "_delete")))
              (load-sml-path "form/input/file.sml")))
           (t (load-sml-path "form/input/text.sml"))))))

(defgeneric form-label (slot))
(defmethod form-label ((slot slot-options))
  (with-slots (format label id input) slot
    (load-sml-path "form/input/label.sml")))

(defgeneric required-mark (slot))
(defmethod required-mark ((slot slot-options))
  (when (slot-required slot)
    (load-sml-path "form/input/required_mark.sml")))

(defgeneric form-comment (slot))
(defmethod form-comment ((slot slot-options))
  (when-let (comment (aand (slot-comment slot) (not (equal it "")) it))
    (load-sml-path "form/input/comment.sml")))

(defmacro form-for/cont (cont &key class instance (submit "submit"))
  `(%form/cont (file-slots ,class) ,cont
     :id (concat (->string-down ,class) "_form")
     [table
      (loop for s in (get-excluded-slots ,class)
            do [tr [td (form-label s) (required-mark s) (form-comment s)]]
            do [tr [td (form-input ,class s ,instance)]])
       [tr [td (submit :value ,submit)]]]))

(defun select-date (name &key y m d (y-start 1900) (y-end 2030))
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (flet ((lst (from to) (loop for i from from to to collect i)))
      [span :class "change_date"
            (select-form (concat name "_y") (lst y-start y-end)
                         :selected (or (->int y) year)  :class "y")
            (select-form (concat name "_m") (lst 1 12)
                         :selected (or (->int m) month) :class "m")
            (select-form (concat name "_d") (lst 1 31)
                         :selected (or (->int d) date)  :class "d")])))

(defun posted-date (id)
  (flet ((date (x) (post-parameter (concat id "_" x))))
    (list (date "y") (date "m") (date "d"))))

(defun posted-checkbox (id)
  (mapcar #'cdr
          (remove-if-not #'(lambda (x) (equal (car x) (concat id "[]")))
                         (post-parameters*))))

; --- Validations -----------------------------------------------

(defun slot-validation-errors (class slot &optional ins)
  (with-slots (symbol id format required length unique options input) slot
    (validation-errors
     (slot-label slot)
     (cond ((eq format :date) (posted-date id))
           ((eq input :checkbox) (posted-checkbox id))
           ((eq input :file)
            (or (image-path (cont-session slot) "tmp")
                (awhen (aand ins (ignore-errors (slot-value it symbol)))
                  (image-path it "upload"))))
           (t (post-parameter id)))
     (append (list :required required :length length)
             (awhen format  `(:format ,it))
             (awhen options `(:member ,it))
             (when  unique  `(:unique ,(list class symbol ins)))))))

(defun class-validation-errors (class &optional ins)
  (edit-upload-file class ins)
  (loop for s in (get-excluded-slots class)
        as e = (slot-validation-errors class s ins)
        when (and e (eq (slot-input s) :file)) do (rem-cont-session s)
        when e collect (car e)))

; --- Elephant wrapper functions --------------------------------

(defun set-slots (name slots &optional parent)
  (setf (gethash name *slots*)
        (append (get-slots parent)
                (mapcar #'(lambda (s)
                            (apply #'make-instance 'slot-options
                                   (parse-slot name s)))
                        slots))))

(defmacro defpclass (name parent slot-defs &rest class-opts)
  (let ((parent* (when (listp parent) (car parent))))
    `(progn
       (set-slots ',name ',slot-defs ',parent*)
       ,(when (eq parent* 'user)
         `(setf *user* (make-instance 'user-class :class ',name)))
       (ele:defpclass ,name ,parent
         ,(append
           (loop for slot in slot-defs
                 as s = (->list slot) collect
                 `(,(car s)
                    :type ,(slot-type* s)
                    :accessor ,(aif (member :accessor s) (nth 1 it) (car s))
                    :initarg  ,(aif (member :initarg s)
                                    (nth 1 it) (->keyword (car s)))
                    ,@(awhen (member :allocation s)    `(:allocation ,(nth 1 it)))
                    ,@(awhen (member :documentation s) `(:documentation ,(nth 1 it)))
                    ,@(awhen (member :initform s)      `(:initform ,(nth 1 it)))
                    ,@(when  (or (aand (member :index  s) (nth 1 it))
                                 (aand (member :unique s) (nth 1 it)))
                             '(:index t))))
           '((created-at :accessor created-at :initform (get-universal-time) :index t)
             (updated-at :accessor updated-at :initform (get-universal-time) :index t)))
         ,@class-opts))))

(defun oid (instance)
  (ignore-errors (ele::oid instance)))

(defun get-instance-by-oid (class oid)
  (get-value (->int oid) (find-class-index class)))

(defun drop-instance (instance)
  (drop-instances (list instance)))

(defun drop-instance-by-oid (class oid)
  (aand (get-instance-by-oid class oid)
        (progn (drop-instance it)
               t)))

(defun drop-instances-by-class (class)
  (when (find-class class nil)
    (drop-instances (get-instances-by-class class))))

; --- Instance editing ------------------------------------------

(defun make-pinstance (class &optional slot-values)
  "makes an instance of pclass from posted parameters"
  (apply #'make-instance class
         (append (loop for s in (get-excluded-slots class)
                       collect (->keyword (slot-symbol s))
                       collect (slot-save-value s))
                 (when slot-values
                   (loop for s in slot-values
                         collect (->keyword (car s))
                         collect (slot-save-value
                                  (get-slot class (car s))
                                  (nth 1 s)))))))

(defun update-pinstance (class ins &optional slot-values)
  "updates the instance of pclass from posted parameters"
  (loop for s in (get-excluded-slots class)
        as value = (slot-save-value s)
        unless (and (eq (slot-input s) :file) (empty value))
        do (setf (slot-value ins (slot-symbol s)) value))
  (setf (slot-value ins 'updated-at) (get-universal-time))
  (when slot-values
    (loop for s in slot-values
          do (setf (slot-value ins (car s))
                   (slot-save-value (get-slot class (car s))
                                    (nth 1 s))))))

; --- File upload -----------------------------------------------

(defun edit-upload-file (class &optional ins)
  "Saves, changes or deletes an upload file"
  (when (= (random *tmp-files-gc-probability*) 0)
    (bordeaux-threads:make-thread
     (lambda () (tmp-files-gc))))
  (loop for s in (file-slots class)
        as new-file = (file-path (slot-id s))
        as tmp-file = (cont-session s)
        as saved-file = (aand ins (ignore-errors (slot-value it (slot-symbol s))))
        do (if (equal (post-parameter (concat (slot-id s) "_delete")) "t")
               (progn (awhen tmp-file   (delete-tmp-file it))
                      (awhen saved-file (delete-saved-file it))
                      (rem-cont-session s))
               (awhen (aand new-file (save-file it *tmp-save-dir*))
                 (awhen tmp-file   (delete-tmp-file it))
                 (awhen saved-file (delete-saved-file it))
                 (setf (cont-session s) (pathname-name it))))))

(defun save-file (file dir)
  (when (probe-file file)
    (awhen (uniqe-file-name dir)
      (and (rename-file file it) it))))

(defun uniqe-file-name (dir)
  (dotimes (x 5)
    (let ((file (merge-pathnames
                 (hunchentoot::create-random-string 10 36) dir)))
      (unless (probe-file file)
        (return-from uniqe-file-name file)))))

(defun tmp-files-gc ()
  (trivial-shell:shell-command
   (format nil "find ~A -maxdepth 1 -type f -cmin +~A -exec rm {} \\;"
           *tmp-save-dir* (floor (/ *tmp-files-gc-lifetime* 60)))))

(defun delete-saved-file (file)
  (ignore-errors
    (delete-file (merge-pathnames file *upload-save-dir*))))

(defun delete-tmp-file (file)
  (ignore-errors
    (delete-file (merge-pathnames file *tmp-save-dir*))))

(in-package :web4r)

; --- Slots -----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass slot-options ()
     ((symbol   :accessor slot-symbol   :initarg :symbol   :type symbol
                :documentation "The symbol of the slot.")
      (id       :accessor slot-id       :initarg :id       :type string
                :documentation "The string id of the slot.")
      (label    :accessor slot-label    :initarg :label    :type string
                :documentation "The label of the slot to display.")
      (unique   :accessor slot-unique   :initarg :unique   :initform nil
                :documentation "If this is non nil, the value of the slot is required.")
      (required :accessor slot-required :initarg :required :initform nil
                :documentation "If this is non nil, the value of the slot is required.")
      (rows     :accessor slot-rows     :initarg :rows     :initform nil
                :documentation "The size of rows for textarea input form.")
      (cols     :accessor slot-cols     :initarg :cols     :initform nil
                :documentation "The size of cols for textarea input form.")
      (size     :accessor slot-size     :initarg :size     :initform nil
                :documentation "The size of text input form.")
      (length   :accessor slot-length   :initarg :length   :initform nil
                :documentation "If this is an integer, this is a max length
 of the value otherwise a list of '(min max) length for the validation.")
      (hide-for :accessor slot-hide-for :initarg :hide-for :initform nil
                :documentation "This specifies where to hide the slot for.
 :all for all or a string regexp to hide it only on pages where the request
 uri matches to the regexp.")
      (options  :accessor slot-options  :initarg :options  :initform '() :type list
                :documentation "The select options for a select, radio or checkbox
 input forms.")
      (comment  :accessor slot-comment  :initarg :comment  :initform ""  :type string
                :documentation "The comment for the input form to display.")
      (input    :accessor slot-input    :initarg :input    :initform nil
                :documentation "The type of the input form which must be :text,
 :textarea, :radio, :checkbox, :select, :password or :file.")
      (format   :accessor slot-format   :initarg :format   :initform nil
                :documentation "The validation type which must be :alpha, :alnum,
 :integer, :email :date, :image ,regexp in string or a function.")
      (type     :accessor slot-type     :initarg :type    :initform nil :type symbol
                :documentation "The type specifier of the slot."))
    (:documentation "The persistent class extended slot options.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slot-type* (slot-definition)
    "Returns a type specifier of the slot from the SLOT-DEFINITION."
    (acond ((member :type slot-definition) (nth 1 it))
           ((aand (member :format slot-definition)
                  (member (nth 1 it) '(:date :integer integer)))
            'integer)
           ((member :options slot-definition) 'list)
           (t 'string))))

(defun get-slots (class)
  "Returns all the slots in the CLASS."
  (gethash class *slots*))

(defun get-slot (class slot-symbol)
  "Returns the slot specified by the CLASS and the SLOT-SYMBOL if any."
  (find-if #'(lambda (s) (eq (slot-symbol s) slot-symbol))
           (get-slots class)))

(defun get-slots-if (test class)
  "Returns a list of slots that satisfy the TEST. TEST is a designator
 for a function of one argument that returns a generalized boolean."
  (remove-if-not test (get-slots class)))

(defun get-excluded-slots-if (test class)
  "Returns a list of excluded slots that satisfy the TEST. TEST is a
 designator for a function of one argument that returns a generalized
 boolean."
  (remove-if-not test (get-excluded-slots class)))

(defun get-slot-by-id (class id)
  "Returns an instance of the slot-options specified by the CLASS ID
 if any."
  (car (get-slots-if
        #'(lambda (s)
            (or (equal id (slot-id* class s))
                (equal id (slot-id s)))) ; for a inherited slot
        class)))

(defun get-excluded-slots (class)
  "Returns a list of excluded slots in the CLASS. You can specify the
 excluded slots by the hide-for slot option or *without-slots*. The
 values of *with-slots* affects this."
  (if (eq *with-slots* :all)
      (get-slots class)
      (loop for s in (get-slots class)
            as symbol = (slot-symbol s)
            unless (or (aand (slot-hide-for s)
                             (or (eq it :all) (scan it (request-uri*))))
                       (member symbol *without-slots*)
                       (aand *with-slots* (not (member symbol it))))
            collect s)))

(defun get-file-slots (class)
  "Returns a list of the CLASS slots that the input type is :file."
  (get-slots-if #'(lambda (s) (eq (slot-input s) :file)) class))

(defun get-excluded-file-slots (class)
  "Returns a list of excluded file slots in the CLASS."
  (get-excluded-slots-if #'(lambda (s) (eq (slot-input s) :file)) class))

(defun indexed-slot-p (class slot)
  "Returns true if the SLOT in the CLASS is indexed and nil otherwise."
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

(defgeneric slot-display-value (instance slot &key nl->br)
  (:documentation "Returns the SLOT display values of the INSTANCE.
 If NL->BR is non nil, replace newlines with br html tags."))
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

(defgeneric slot-save-value (slot &optional value)
  (:documentation "Converts the VALUE for the SLOT into the saving
 format if needed and returns the converted value."))
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
  "Splits the 8 digits DATE like 19830928 into a list like 
 '(\"1983\" \"09\" \"28\") and returns the list."
  (aand (->string date)
        (when (= (length it) 8)
          (list (subseq it 0 4) (subseq it 4 6) (subseq it 6 8)))))

; --- Forms for slots -------------------------------------------

(defgeneric form-valid-attr (class slot &optional instance)
  (:documentation "Returns a list of attributes for input forms used for
 javascript validations. http://docs.jquery.com/Plugins/Validation"))
(defmethod form-valid-attr (class (slot slot-options) &optional ins)
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

(defgeneric form-input (class slot &optional instance)
  (:documentation "Displays an input form for the CLASS SLOT. INSTANCE
 is an instance of the CLASS to edit."))
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

(defgeneric form-label (slot)
  (:documentation "Displays the label of the SLOT."))
(defmethod form-label ((slot slot-options))
  (with-slots (format label id input) slot
    (load-sml-path "form/input/label.sml")))

(defgeneric required-mark (slot)
  (:documentation "Displays a required mark if the slot is required."))
(defmethod required-mark ((slot slot-options))
  (when (slot-required slot)
    (load-sml-path "form/input/required_mark.sml")))

(defgeneric form-comment (slot)
  (:documentation "Displays the comment of the SLOT if any."))
(defmethod form-comment ((slot slot-options))
  (when-let (comment (aand (slot-comment slot) (not (equal it "")) it))
    (load-sml-path "form/input/comment.sml")))

(defmacro form-for/cont (continuation &key class instance (submit "submit"))
  "Generates and displays a form for the CLASS with embedding the CONTINUATION
 within the form. INSTANCE is an instance to update. SUBMIT is the value of
 the submit button."
  `(%form/cont (get-excluded-file-slots ,class) ,continuation
     :id (concat (->string-down ,class) "_form")
     [table
      (loop for s in (get-excluded-slots ,class)
            do [tr [td (form-label s) (required-mark s) (form-comment s)]]
            do [tr [td (form-input ,class s ,instance)]])
       [tr [td (submit :value ,submit)]]]))

(defun select-date (name &key y m d (y-start 1900) (y-end 2030))
  "Generates and displays date select boxes named NAME. Y is the selected year,
 M is the selected month and D is the selected date. The Y-START is the oldest
 year and the Y-END is the newest year in the year select box."
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

(defun slot-validation-errors (class slot &optional instance)
  "Validates posted parameters to edit the SLOT value in the CLASS and
 returns a list of error messages if any."
  (with-slots (symbol id format required length unique options input) slot
    (validation-errors
     (slot-label slot)
     (cond ((eq format :date) (posted-date id))
           ((eq input :checkbox) (posted-checkbox id))
           ((eq input :file)
            (or (image-path (cont-session slot) "tmp")
                (awhen (aand instance (ignore-errors (slot-value it symbol)))
                  (image-path it "upload"))))
           (t (post-parameter id)))
     (append (list :required required :length length)
             (awhen format  `(:format ,it))
             (awhen options `(:member ,it))
             (when  unique  `(:unique ,(list class symbol instance)))))))

(defun class-validation-errors (class &optional instance)
  "Validates posted parameters to edit an instance of the CLASS and
 returns a lsit of error messages if any."
  (edit-upload-file class instance)
  (loop for s in (get-excluded-slots class)
        as e = (slot-validation-errors class s instance)
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
  "Defines a persistent object. You can use the extended slot options
 defined by the slot-options. See slot-options for the detail."
  (let ((parent* (when (listp parent) (car parent))))
    `(progn
       (set-slots ',name ',slot-defs ',parent*)
       ,(when (eq parent* 'user)
         `(setf *user* (make-instance 'user* :class ',name)))
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
  "Returns an oid (object id) of the INSTANCE."
  (ignore-errors (ele::oid instance)))

(defun get-instance-by-oid (class oid)
  "Returns an instance of the persistent CLASS specified by the OID."
  (get-value (->int oid) (find-class-index class)))

(defun drop-instance (instance)
  "Drops the INSTANCE."
  (drop-instances (list instance)))

(defun drop-instance-by-oid (class oid)
  "Drops an instance of the persistent CLASS specified by the OID."
  (aand (get-instance-by-oid class oid)
        (progn (delete-saved-files class it)
               (drop-instance it)
               t)))

(defun drop-instances-by-class (class)
  "Drops all the instances of the persistent CLASS."
  (when (find-class class nil)
    (drop-instances (get-instances-by-class class))))

; --- Instance editing ------------------------------------------

(defun make-pinstance (class &optional slot-values)
  "Makes an instance of the persistent CLASS from posted parameters.
 You can specify slot values by the SLOT-VALUES argument which must be
 an alist of a slot symbol -> a value."
  (apply #'make-instance class
         (append (loop for s in (get-excluded-slots class)
                       collect (->keyword (slot-symbol s))
                       collect (slot-save-value s))
                 (when slot-values
                   (loop for s in slot-values
                         collect (->keyword (car s))
                         collect (slot-save-value
                                  (get-slot class (car s))
                                  (cdr s)))))))

(defun update-pinstance (class instance &optional slot-values)
  "Updates the INSTANCE of the persistent CLASS from posted parameters.
 You can specify slot values by the SLOT-VALUES argument which must be
 an alist of a slot symbol -> a value."
  (loop for s in (get-excluded-slots class)
        as value = (slot-save-value s)
        unless (and (eq (slot-input s) :file) (empty value))
        do (setf (slot-value instance (slot-symbol s)) value))
  (setf (slot-value instance 'updated-at) (get-universal-time))
  (when slot-values
    (loop for s in slot-values
          do (setf (slot-value instance (car s))
                   (slot-save-value (get-slot class (car s))
                                    (cdr s))))))

; --- File upload -----------------------------------------------

(defun edit-upload-file (class &optional instance)
  "Saves, changes or deletes uploading/uploaded files for the CLASS
 from posted parameters. The INSTANCE is an instance of the CLASS
 which is needed to specify when you edit/delete an uploaded file of
 the instance."
  (when (= (random *tmp-files-gc-probability*) 0)
    (bordeaux-threads:make-thread
     (lambda () (tmp-files-gc))))
  (loop for s in (get-excluded-file-slots class)
        as new-file = (file-path (slot-id s))
        as tmp-file = (cont-session s)
        as saved-file = (aand instance
                              (ignore-errors (slot-value it (slot-symbol s))))
        do (if (equal (post-parameter (concat (slot-id s) "_delete")) "t")
               (progn (awhen tmp-file   (delete-tmp-file it))
                      (awhen saved-file (delete-saved-file it))
                      (rem-cont-session s))
               (awhen (aand new-file (save-file it *tmp-save-dir*))
                 (awhen tmp-file   (delete-tmp-file it))
                 (awhen saved-file (delete-saved-file it))
                 (setf (cont-session s) (pathname-name it))))))

(defun save-file (file directory)
  "Moves the FILE to the DIRECTORY."
  (when (probe-file file)
    (awhen (uniqe-file-name directory)
      (and (rename-file file it) it))))

(defun uniqe-file-name (directory)
  "Generates and returns a unique file name under the DIRECTORY."
  (dotimes (x 5)
    (let ((file (merge-pathnames
                 (hunchentoot::create-random-string 10 36) directory)))
      (unless (probe-file file)
        (return-from uniqe-file-name file)))))

(defun tmp-files-gc ()
  "Executes garbage collection for expired temporary saved files."
  (trivial-shell:shell-command
   (format nil "find ~A -maxdepth 1 -type f -cmin +~A -exec rm {} \\;"
           *tmp-save-dir* (floor (/ *tmp-files-gc-lifetime* 60)))))

(defun delete-saved-file (file)
  "Deletes the FILE under the directory specified by the *upload-save-dir*."
  (ignore-errors
    (delete-file (merge-pathnames file *upload-save-dir*))))

(defun delete-saved-files (class instance)
  "Deletes all the saved files for the CLASS INSTANCE."
  (loop for s in (get-file-slots class)
        as file = (ignore-errors (slot-value instance (slot-symbol s)))
        when file do (delete-saved-file file)))

(defun delete-tmp-file (file)
  "Deletes the FILE under the directory specified by the  *tmp-save-dir*."
  (ignore-errors
    (delete-file (merge-pathnames file *tmp-save-dir*))))

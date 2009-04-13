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
      (hide     :accessor slot-hide     :initarg :hide     :initform nil
                :documentation "treats the slot as excluded slots")
      (options  :accessor slot-options  :initarg :options  :initform '() :type list
                :documentation "form input options (also used for validation)")
      (comment  :accessor slot-comment  :initarg :comment  :initform ""  :type string
                :documentation "comment to display")
      (input    :accessor slot-input    :initarg :input    :initform nil
                :documentation "form input type - :text, :textarea, :radio, 
:checkbox, :select, :password or :file")
      (type     :accessor slot-type     :initarg :type     :initform nil
                :documentation "validation type - :integer, :date, :alpha, 
:alnum, :regex, :email, :image or :member"))
    (:documentation "Extended slot options")))

(defun set-slots (name slots &optional parent)
  (setf (gethash name *slots*)
        (append (get-slots parent)
                (loop for s in slots as s* = (parse-slot name s)
                      collect (apply #'make-instance 'slot-options s*)))))

(defun get-slots (class)
  (gethash class *slots*))

(defun get-slot (class symbol)
  (find-if #'(lambda (s) (eq (slot-symbol s) symbol))
           (get-slots class)))

(defun get-excluded-slots (class)
  (if (eq *with-slots* :all)
      (get-slots class)
      (loop for s in (get-slots class)
            as symbol = (slot-symbol s)
            unless (or (slot-hide s)
                       (member symbol *without-slots*)
                       (aand *with-slots* (not (member symbol it))))
            collect s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-slot (class slot)
    (setf slot (->list slot))
    (flet ((opt (x) (awhen (member x slot) (nth 1 it))))
      (append
       (let ((id (regex-replace-all "-" (->string-down (car slot)) "_")))
         (list :symbol   (car slot)
               :id       (concat (->string-down class) "_" id)
               :label    (or (opt :label)
                             (string-capitalize (regex-replace-all "_" id " ")))
               :input    (or (opt :input)
                             (aand (opt :length) (< 256 (if (atom it) it (car it)))
                                   :textarea))
               :type     (or (opt :type) (awhen (opt :options) (list :member it)))
               :hide     (or (opt :hide)
                             (aand (opt :initform) (equal it '(ele:make-pset)) t))
               :required (aif (member :required slot) (nth 1 it) t)))
       (let ((fn (lambda (x) (awhen (opt x) (list x it))))
             (op '(:unique :length :size :rows :cols :comment :options)))
         (apply #'append (remove nil (mapcar fn op))))))))

(defgeneric slot-display-value (instance slot &key nl->br))
(defmethod slot-display-value (instance (slot slot-options) &key (nl->br nil))
  (aif (ignore-errors (slot-value instance (slot-symbol slot)))
    (let ((input (slot-input slot)))
      (cond ((and nl->br (eq input :textarea)) (safe (nl->br (escape it))))
            ((eq (slot-type slot) :image)
             (load-sml-path "form/display/image.sml"))
            ((eq input :checkbox) (apply #'join (append '(", ") it)))
            (t it)))
    ""))

(defgeneric slot-save-value (slot &optional value))
(defmethod slot-save-value ((slot slot-options) &optional value)
  (with-slots (type id input options) slot
    (let ((value (or value (post-parameter id))))
      (cond ((eq type :date)
             (multiple-value-bind (y m d) (posted-date id)
               (unless (and (null y) (null m) (null d))
                 (join "-" y m d))))
            ((eq input :file)
             (awhen (cont-session slot)
               (rem-cont-session slot)
               (pathname-name
                (save-file (image-path it "tmp") *upload-save-dir*))))
            ((eq input :checkbox) (posted-checkbox id))
            (t value)))))

(defun file-slots (class)
  (remove-if-not #'(lambda (s) (eq (slot-input s) :file))
                 (get-excluded-slots class)))

; --- Forms for slots -------------------------------------------

(defgeneric form-valid-attr (slot))
(defmethod form-valid-attr ((slot slot-options))
  "attributes of form input for jquery validation
http://docs.jquery.com/Plugins/Validation"
  (with-slots (required type length input) slot
    (append (awhen (append (when required '("required"))
                           (when (eq type :email)   '("email"))
                           (when (eq type :integer) '("number")))
              `(:class ,(apply #'join " " it)))
            (awhen (and (not (eq input :file)) length)
              (append (aand (when (listp it) (car it))   `(:minlength ,it))
                      (aand (if (atom it) it (nth 1 it)) `(:maxlength ,it)))))))

(defgeneric form-input (slot &optional ins))
(defmethod form-input ((slot slot-options) &optional ins)
  (with-slots (input type label id length symbol options size) slot
    (let* ((saved (aand ins (ignore-errors (slot-value it symbol))))
           (value (or (post-parameter id) saved)))
      (cond ((eq input :select)
             (select-form id options value))
            (options
             (loop for o in options as oid = (concat id "_" o)
                   do (if (eq input :checkbox)
                          (load-sml-path "form/input/checkbox.sml")
                          (load-sml-path "form/input/radio.sml"))))
           ((eq type :date)
            (let ((date (when (stringp value) (split "-" value))))
              (multiple-value-bind (y m d) (posted-date id)
                (select-date id :y (or y (nth 0 date)) :m (or m (nth 1 date))
                                 :d (or d (nth 2 date))))))
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
  (with-slots (type label id input) slot
    (let ((type* (if (listp type) (car type) type)))
      (load-sml-path "form/input/label.sml"))))

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
            do [tr [td (form-input s ,instance)]])
       [tr [td (submit :value ,submit)]]]))

(defun select-date (name &key y m d (y-start 1900) (y-end 2030))
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (flet ((lst (from to) (loop for i from from to to collect i)))
      (select-form (concat name "_y") (lst y-start y-end) (or (->int y) year))
      (select-form (concat name "_m") (lst 1 12) (or (->int m) month))
      (select-form (concat name "_d") (lst 1 31) (or (->int d) date)))))

(defun posted-date (id)
  (flet ((date (x) (post-parameter (concat id "_" x))))
    (values (date "y") (date "m") (date "d"))))

(defun posted-checkbox (id)
  (mapcar #'cdr
          (remove-if-not #'(lambda (x) (equal (car x) (concat id "[]")))
                         (post-parameters*))))

; --- Validations -----------------------------------------------

(defun slot-validation-errors (class slot &optional ins)
  (with-slots (symbol id type required length unique options input) slot
    (validation-errors
     (slot-label slot)
     (cond ((eq type :date)
            (multiple-value-bind (y m d) (posted-date id)
              (list y m d)))
           ((eq input :checkbox) (posted-checkbox id))
           ((eq input :file)
            (or (image-path (cont-session slot) "tmp")
                (awhen (aand ins (ignore-errors (slot-value it symbol)))
                  (image-path it "upload"))))
           (t (post-parameter id)))
     (append (list :required required :length length :type type)
             (when unique `(:unique ,(list class symbol ins)))))))

(defun class-validation-errors (class &optional ins)
  (edit-upload-file class ins)
  (loop for s in (get-excluded-slots class)
        as e = (slot-validation-errors class s ins)
        when (and e (eq (slot-input s) :file)) do (rem-cont-session s)
        when e collect (car e)))

; --- Elephant wrapper functions --------------------------------

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
                    :accessor ,(aif (member :accessor s) (nth 1 it) (car s))
                    :initarg  ,(aif (member :initarg s)
                                    (nth 1 it) (->keyword (car s)))
                    ,@(awhen (member :allocation s) `(:allocation ,(nth 1 it)))
                    ,@(awhen (member :documentation s) `(:documentation ,(nth 1 it)))
                    ,@(awhen (member :initform s) `(:initform ,(nth 1 it)))
                    ,@(when  (or (aand (member :index  s) (nth 1 it))
                                 (aand (member :unique s) (nth 1 it)))
                             '(:index t))))
           '((created-at :accessor created-at :initform (get-universal-time))
             (updated-at :accessor updated-at :initform (get-universal-time)
              :index t)))
         ,@class-opts))))

(defun oid (instance)
  (handler-case (ele::oid instance)
    (error () nil)))

(defun get-instance-by-oid (class oid &optional (sc *store-controller*))
  (when-let (ins (ele::ele-with-fast-lock ((ele::instance-cache-lock sc))
                   (ele::get-cache (->int oid) (ele::instance-cache sc))))
    (when (typep ins class) ins)))

(defun drop-instance (instance)
  (drop-instances (list instance)))

(defun drop-class-instances (class)
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

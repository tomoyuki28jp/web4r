(in-package :web4r)

; --- Temporary uploaded files gc ----------------------------------

(defvar *tmp-files* '())
(defparameter *tmp-files-gc-lifetime*  1440)
(defvar *tmp-files-gc-probability* 100)

(defun set-tmp-file (file)
  (setf *tmp-files*
        (append *tmp-files*
                (list (cons (get-universal-time) file)))))

(defun rem-tmp-file (file)
  (setf *tmp-files*
        (remove-if #'(lambda (x) (string= (cdr x) file))
                   *tmp-files*)))

(defun tmp-file-gc ()
  (let ((expired-time (- (get-universal-time)
                         *tmp-files-gc-lifetime*)))
    (loop for f in *tmp-files*
          until (< expired-time (car f))
          do (awhen (cdr (pop *tmp-files*))
               (let ((file (upload-file-path it)))
                 (when (probe-file file)
                   (delete-file file)))))))

; --- Extended slot options -------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *slots* (make-hash-table)))

(defvar *with-slots* nil)

(defvar *without-slots* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-slots (class)
    (gethash class *slots*)))

(defun get-excluded-slots (class)
  (if (eq *with-slots* :all)
      (get-slots class)
      (loop for s in (get-slots class)
            as symbol = (slot-symbol s)
            unless (or (slot-hide s)
                       (member symbol *without-slots*)
                       (aand *with-slots* (not (member symbol it))))
            collect s)))

(defun get-slot (class symbol)
  (loop for s in (get-slots class)
        when (eq (slot-symbol s) symbol)
        do (return-from get-slot s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass slot-options ()
     ((symbol   :accessor slot-symbol   :initarg :symbol   :type symbol
                :documentation "slot symbol")
      (id       :accessor slot-id       :initarg :id       :type string
                :documentation "form id")
      (label    :accessor slot-label    :initarg :label    :type string
                :documentation "form label")
      (unique   :accessor slot-unique   :initarg :unique   :initform nil
                :documentation "Uniqueness of slot values")
      (nullable :accessor slot-nullable :initarg :nullable :initform nil
                :documentation "allow empty value")
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
      (input    :accessor slot-input    :initarg :input    :initform nil
                :documentation "form input type - :text, :textarea, :radio, 
:checkbox, :select, :password, :file or :member")
      (type     :accessor slot-type     :initarg :type     :initform nil
                :documentation "validation type - :integer, :date, :alpha, 
:alnum, :regex, :email or :image"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-slot (slot)
    (setf slot (->list slot))
    (flet ((opt (x) (awhen (member x slot) (nth 1 it))))
      (append
       (let ((id (symbol-name (car slot))))
         (list :symbol (car slot) :id id
               :label  (or (opt :label)
                           (string-capitalize
                            (replace-str "-" " " id)))
               :input  (or (opt :input)
                           (when (aand (opt :length)
                                       (< 256 (if (atom it) it (car it))))
                             :textarea))
               :hide   (or (opt :hide)
                           (when (aand (opt :initform)
                                       (equal it '(ele:make-pset)))
                             t))))
       (let ((fn (lambda (x) (awhen (opt x) (list x it))))
             (op '(:unique :nullable :length :type :size :rows :cols :options)))
         (apply #'append (remove nil (mapcar fn op))))))))

(defgeneric slot-display-value (instance slot &key nl->br))
(defmethod slot-display-value (instance (slot slot-options) &key (nl->br nil))
  (if (slot-boundp instance (slot-symbol slot))
    (let ((val   (slot-value instance (slot-symbol slot)))
          (type  (slot-type  slot))
          (input (slot-input slot)))
      (cond ((and nl->br (eq (slot-input slot) :textarea))
             (safe (nl->br (escape val))))
            ((eq type :image)
             (a/ :href (upload-file-uri val)
                 (img/ :src (safe (upload-file-thumbnail-uri val)) :alt (slot-id slot))))
            ((eq input :checkbox)
             (awhen val
               (join ", " val)))
            (t val)))
    ""))

(defgeneric slot-save-value (slot &optional value))
(defmethod slot-save-value ((slot slot-options) &optional value)
  (with-slots (type id input options) slot
    (let ((value (or value (post-param id))))
      (cond ((eq type :date)
             (mkstr (post-param (mkstr id "-Y")) "-"
                    (post-param (mkstr id "-M")) "-"
                    (post-param (mkstr id "-D"))))
            ((eq input :file)
             (rem-tmp-file (file-save-name id))
             (if (atom (last-post id))
                 (last-post id)
                 (file-save-name id)))
            ((eq input :checkbox)
             (loop for o in options
                   as v = (post-param (mkstr id "-" o))
                   when v collect o))
            (t value)))))

(defgeneric form-input (slot &optional ins))
(defmethod form-input ((slot slot-options) &optional ins)
  (with-slots (input type label id length symbol options) slot
    (let ((value (if (post-params)
                     (post-param id)
                     (when (and ins (slot-boundp ins symbol))
                       (slot-value ins symbol)))))
      (cond (options
             (if (eq input :select)
                 (select-form/ id options value)
                 (loop for o in options
                       as oid = (mkstr id "-" o)
                       do (progn
                            (if (eq input :checkbox)
                                (input-checked/ "checkbox" (or (post-param oid) value)
                                                :value o :id oid :name oid)
                                (input-checked/ "radio" value :value o
                                                :id o :name id))
                            (label/ :for o o)))))
            ((eq type :date)
             (let ((date (when (stringp value) (split "-" value))))
               (select-date/
                id :y (or (post-param (mkstr id "-Y")) (nth 0 date))
                   :m (or (post-param (mkstr id "-M")) (nth 1 date))
                   :d (or (post-param (mkstr id "-D")) (nth 2 date)))))
            ((eq input :textarea)
             (textarea/ :name id :rows (slot-rows slot)
                        :cols (slot-cols slot) :id id value))
            ((eq input :file)
             (flet ((upload-form (name)
                      (input/ :type :file :name name :id id :size (slot-size slot))))
               (aif (or (file-save-name id)
                        (and (listp value)
                             (awhen (assoc-ref "size" value :test #'equalp)
                               (when (> it 0)
                                 (assoc-ref "save-name" value :test #'equalp)))))
                    (progn 
                      (when (and ins (null (post-params)) value
                        (input/ :type "hidden" :name id :value value)))
                      (p/ "delete: "
                          (input-checked/ "checkbox" nil :value "delete"
                                          :name (concat id "-delete") :id id ))
                      (p/ "change: " (upload-form (concat id "-change")))
                      (img/ :src (upload-file-thumbnail-uri it) :alt id))
                    (upload-form id))))
            (t (input/ :type (if (eq input :password) "password" "text")
                       :name id :value value
                       :id id :size (slot-size slot)))))))

(defgeneric form-label (slot))
(defmethod form-label ((slot slot-options))
  (with-slots (type label id nullable) slot
    (cond ((eq type :date)
           (label/ :for (mkstr id "-Y") label))
      (t (when (and label id)
           (label/ :for id label))))))

(defgeneric must? (slot))
(defmethod must? ((slot slot-options))
  (unless (slot-nullable slot)
    (font/ :color "red" "*")))

(defun slot-validation-errors (class slot)
  (with-slots
        (symbol id label type nullable length unique options input) slot
    (when (and options (null type))
      (setf type (list :member options)))
    (validation-errors
     label
     (cond ((eq type :date)
            (list (post-param (mkstr id "-Y"))
                  (post-param (mkstr id "-M"))
                  (post-param (mkstr id "-D"))))
           ((eq input :file)
            (last-post id))
           ((eq input :checkbox)
            (loop for o in options
                  when (post-param (mkstr id "-" o))
                  collect o))
           (t (post-param id)))
     (list :nullable nullable :length length :type type
           :unique (when unique (list class symbol))))))

(defun class-validation-errors (class)
  "returns validation error messages if there is any"
  (loop for s in (get-excluded-slots class)
        as input = (slot-input s)
        as e = (unless (and (eq input :file)
                            (atom (last-post (slot-id s))))
                 (slot-validation-errors class s))
        when e collect (progn
                         (when (eq input :file)
                           (delete-uploading-file s))
                         (car e))))

(defun file-slots (class)
  (loop for s in (get-slots class)
        when (eq (slot-input s) :file)
        collect s))

(defun multipart-form-p (class)
  (not (null (file-slots class))))

; --- Wrapper functions -----------------------------------------

(defmacro defpclass (name parent slot-defs &rest class-opts)
  (let ((parent* (when (listp parent) (car parent))))
    `(progn
       (sethash ',name *slots*
                (append (gethash ',parent* *slots*)
                        (mapcar #'(lambda (s)
                                    (apply #'make-instance
                                             'slot-options (parse-slot s)))
                                ',slot-defs)))
       ,(when (eq parent* 'user)
          `(setf *user* (make-instance 'user-class :class ',name)))
       (ele:defpclass ,name ,parent
         ,(append
           (loop for slot in slot-defs
                 as  s = (->list slot) collect
                 `(,(car s)
                         :accessor ,(aif (member :accessor s) (nth 1 it) (car s))
                         :initarg  ,(aif (member :initarg s)
                                        (nth 1 it) (make-keyword (car s)))
                         ,@(awhen (member :allocation s) `(:allocation ,(nth 1 it)))
                         ,@(awhen (member :documentation s) `(:documentation ,(nth 1 it)))
                         ,@(awhen (member :initform s) `(:initform ,(nth 1 it)))
                         ,@(when (or (aand (member :index  s) (nth 1 it))
                                     (aand (member :unique s) (nth 1 it)))
                             '(:index t))))
           '((created-at :accessor created-at :initform (get-universal-time))
             (updated-at :accessor updated-at :initform (get-universal-time)
              :index t)))
         ,@class-opts))))

(defun oid (instance)
  (handler-case
      (ele::oid instance)
    (error () nil)))

(defun slot-values= (instance slot-values)
  (loop for s in slot-values
        unless (equal (slot-value instance (car s)) (nth 1 s))
        do (return-from slot-values= nil)
        finally (return t)))

(defun drop-instance (instance)
  (drop-instances (list instance)))

(defun get-instance-by-oid (class oid)
  (awhen (cond ((integerp oid) oid)
               ((stringp oid) (->int oid)))
    (awhen (ele::get-cached-instance *store-controller* it class)
      (when (typep it class)
        it))))

(defun per-page (ins &key (index 'updated-at))
  (let* ((total (length ins))
         (pager (make-instance 'pager :total-items total))
         (items (with-slots (total-pages current-page item-start item-end
                                         items-per-page) pager
                  (when (<= current-page total-pages)
                    (subseq (sort ins #'> :key index) item-start item-end)))))
    (values items pager)))

(defun drop-class-instances (class)
  (drop-instances (get-instances-by-class class)))

; --- Scaffold  -------------------------------------------------

(defun scaffold (class &key (index 'updated-at))
  (defpage default () (scaffold-index-page class :index index))
  (defpage show (oid) (scaffold-show-page  class oid))
  (defpage edit (oid) (scaffold-edit-page class :oid oid)))

(defun scaffold-index-page (class &key (index 'updated-at)
                            (max-strlen 20) (redirect-uri (req-uri)))
  (let* ((cname (string-downcase (symbol-name class)))
         (slots (get-excluded-slots class)))
    (multiple-value-bind (items pager)
        (per-page (get-instances-by-class class) :index index)
      (load-shtml (shtml-path "scaffold/index.shtml")))))

(defun scaffold-show-page (class oid)
  (let ((cname (string-downcase (symbol-name class)))
        (slots (get-excluded-slots class))
        (ins (get-instance-by-oid class oid)))
    (load-shtml (shtml-path "scaffold/show.shtml"))))

(defun scaffold-edit-page (class &key oid slot-values
                           (redirect-uri (host-uri)))
  (let* ((cname (string-downcase (symbol-name class)))
         (ins (when oid (get-instance-by-oid class oid)))
         (with-slots *with-slots*)
         (without-slots *without-slots*)
         (edit/cont* (lambda ()
                       (edit/cont class ins redirect-uri
                                  :with-slots    with-slots
                                  :without-slots without-slots
                                  :slot-values   slot-values))))
    (load-shtml (shtml-path "scaffold/edit.shtml"))))

(defun delete/cont (ins cname redirect-uri)
  (drop-instance ins)
  (redirect/msgs (rem-get-param redirect-uri "page")
                 (mkstr cname " was successfully deleted")))

(defun edit/cont (class ins page &key with-slots without-slots slot-values)
  (let ((*with-slots* (or with-slots *with-slots*))
        (*without-slots* (or without-slots *without-slots*)))
    (save-upload-file class)
    (aif (class-validation-errors class)
         (apply #'page/error-msgs
                (append (list (uri-path 1) it)
                        (when ins (list (oid ins)))))
         (progn
           (if ins
               (update-pinstance class ins slot-values)
               (make-pinstance class slot-values))
           (let ((msg (concat (string-downcase (symbol-name class))
                              " was successfully "
                              (if ins "updated" "created"))))
             (if (functionp page)
                 (funcall page msg)
                 (redirect/msgs page msg)))))))

(defun make-pinstance (class &optional slot-values)
  "makes a instance of pclass from posted parameters"
  (apply #'make-instance class
         (append (loop for s in (get-excluded-slots class)
                       collect (make-keyword (slot-id s))
                       collect (slot-save-value s))
                 (when slot-values
                   (loop for s in slot-values
                         collect (make-keyword (car s))
                         collect (slot-save-value
                                  (get-slot class (car s))
                                  (nth 1 s)))))))

(defun update-pinstance (class ins &optional slot-values)
  "updates a instance of pclass from posted parameters"
  (loop for s in (get-excluded-slots class)
        do (setf (slot-value ins (slot-symbol s))
                 (slot-save-value s)))
  (setf (slot-value ins 'updated-at) (get-universal-time))
  (when slot-values
    (loop for s in slot-values
          do (setf (slot-value ins (car s))
                   (slot-save-value (get-slot class (car s))
                                    (nth 1 s))))))

; --- File upload -----------------------------------------------

(defvar *upload-save-dir* "/tmp/web4r/public/upload/"
  "File upload directory path. This path should be under 
your server public directory")
(ensure-directories *upload-save-dir*)

(defun upload-file-path (file-name)
  (concat *upload-save-dir* file-name))

(defvar *upload-dir-uri* nil)

(defun upload-dir-uri ()
  (unless *upload-dir-uri*
    (setf *upload-dir-uri*
          (if (public-file-p *upload-save-dir*)
              (concat (host-uri)
                      (subseq *upload-save-dir*
                              (length (public-dir))))
              (error "*upload-save-dir* is not under the server public dir"))))
  *upload-dir-uri*)

(defun upload-file-uri (file-name)
  (if (probe-file (concat *upload-save-dir* file-name))
      (concat (upload-dir-uri) file-name)
      (noimage-uri)))

(defun upload-file-thumbnail-uri (file-name)
  (thumbnail-uri
   (concat (replace-str (public-dir) "" *upload-save-dir*)
           file-name)))

(defun save-upload-file (class)
  ; It may be better to start a new thread for a gc process.
  ; I don't do that right now because special variables
  ; won't be shared between threads.
  (when (= (random *tmp-files-gc-probability*) 0)
    (tmp-file-gc))
  (loop for s in (file-slots class)
     as id = (slot-id s)
     do (progn
          (let* ((ch  (concat id "-change"))
                 (ch? (aand (get-file-data ch "size" #'post-param)
                            (not (eq it 0)))))
            (when ch?
              (setf (request-post-params *request*)
                    (append (remove-if #'(lambda (x) (equal (car x) id))
                                       (post-params))
                            (list (cons id (post-param ch)))))
              (set-last-post))
            (cond ((equal "delete" (post-param (concat id "-delete")))
                   (delete-uploading-file s))
              ((and (aand (file-size id) (not (eq it 0)))
                    (or ch? (not (file-save-name id))))
               (awhen (uniq-file-name *upload-save-dir*)
                 (rename-file (file-tmp-name id) it)
                 (set-tmp-file (pathname-name it))
                 (set-last-post :name id :value
                                (append (list (cons "save-name"
                                                    (pathname-name it)))
                                        (last-post id))))))))))

(defun delete-uploading-file (slot)
  (let ((id (slot-id slot)))
    (awhen (file-save-name id)
      (delete-upload-file it))
    (awhen (aand (file-tmp-name id) (probe-file it))
      (delete-upload-file it))
    (rem-last-post :name id)
    (setf (request-post-params *request*)
          (remove-if #'(lambda (x) (string= (car x) id))
                     (post-params)))))

(defun delete-upload-file (file)
  (let ((file (upload-file-path file)))
    (when (probe-file file)
      (delete-file file))))

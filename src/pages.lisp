(in-package :web4r)

(defmacro genpages (class &key slot index-sml show-sml edit-sml
                    items-per-page links-per-page)
  `(progn
     (defpage ,class (:get (slot ,slot))
       (index-page ',class :slot slot :sml ,index-sml
                   :items-per-page ,items-per-page
                   :links-per-page ,links-per-page))
     (defpage ,(join "/" class 'show) (oid)
       (show-page ',class oid :sml ,show-sml))
     (defpage ,(join "/" class 'edit) (oid)
       (edit-page ',class :oid oid :sml ,edit-sml))
     (defpage ,(join "/" class 'delete) (oid)
       (delete-page ',class oid))
     (defpage ,(join "/" 'ajax class 'unique) (oid)
       (p (unique? ',class (get-parameters*) oid)))
     (defpage ,(join "/" 'ajax class 'list) (:get slot)
       (ajax-list ',class :slot slot))
     (defpage ,(join "/" 'ajax class 'delete) (oid)
       (p (drop-instance-by-oid* ',class oid)))))

(defmacro index-page (class &key slot (maxlength 20)
                      plural items-per-page links-per-page sml)
  `(let* ((class  ,class)
          (cname  (->string-down ,class))
          (plural (or ,plural (pluralize cname)))
          (maxlength ,maxlength)
          (per-page (param-per-page* ,items-per-page ,links-per-page)))
     (declare (ignorable web4r::maxlength web4r::per-page))
     (multiple-value-bind (items pager slots)
         (items-per-page ,class ,slot
                        :items-per-page ,items-per-page
                        :links-per-page ,links-per-page)
       (load-sml (or ,sml (sml-path "pages/index.sml"))
                 ,*web4r-package*))))

(defmacro show-page (class oid &key sml)
  `(let ((cname (->string-down ,class))
         (slots (get-excluded-slots ,class))
         (ins   (get-instance-by-oid ,class ,oid)))
     (load-sml (or ,sml (sml-path "pages/show.sml"))
               ,*web4r-package*)))

(defmacro edit-page (class &key oid slot-values redirect-uri sml)
  `(let* ((class ,class)
          (oid ,oid)
          (cname (->string-down class))
          (redirect-uri (or ,redirect-uri (page-uri cname)))
          (ins (awhen oid (get-instance-by-oid class it)))
          (with-slots *with-slots*)
          (without-slots *without-slots*)
          (edit/cont* (lambda ()
                        (edit/cont ,class ins redirect-uri
                                   :with-slots with-slots
                                   :without-slots without-slots
                                   :slot-values ,slot-values))))
     (declare (ignorable class))
     (load-sml (or ,sml (sml-path "pages/edit.sml"))
               ,*web4r-package*)))

(defun drop-instance-by-oid* (class oid)
  (aif (drop-instance-by-oid class oid)
       (concat (->string-down class) " was successfully deleted")
       "No such item"))

(defun delete-page (class oid &optional (redirect-uri
                                         (page-uri (->string-down class))))
  (redirect/msgs (rem-parameter redirect-uri "page")
                 (drop-instance-by-oid* class oid)))

(defun edit/cont (class ins page &key with-slots without-slots slot-values)
  (let ((*with-slots* (or with-slots *with-slots*))
        (*without-slots* (or without-slots *without-slots*)))
    (aif (class-validation-errors class ins)
         (apply #'page/error-msgs
                (append (list (request-uri*) it)
                        (when ins (list (oid ins)))))
         (progn
           (if ins
               (update-pinstance class ins slot-values)
               (make-pinstance class slot-values))
           (let ((msg (concat (->string-down class) " was successfully "
                              (if ins "updated" "created"))))
             (if (functionp page)
                 (funcall page msg)
                 (redirect/msgs page msg)))))))

(defun per-page (items &key (index 'updated-at) (order #'>)
                 items-per-page links-per-page)
  (let* ((total (length items))
         (pager (apply #'make-instance 'pager
                       (append `(:total-items ,total)
                               (awhen items-per-page `(:items-per-page ,it))
                               (awhen links-per-page `(:links-per-page ,it)))))
         (items (with-slots (item-start item-end items-per-page) pager
                  (when (<= (current-page pager) (total-pages pager))
                    (subseq (sort items order :key index) item-start item-end)))))
    (values items pager)))

(defun order-slot-id (class &optional slot)
  (or (awhen (member (get-parameter "slot")
                     '("updated-at" "created-at") :test #'equal)
        (car it))
      (aand (get-parameter "slot")
            (aand (get-slot-by-id class it)
                  (indexed-slot-p class (slot-symbol it)))
            it)
      slot
      "updated-at"))

(defun order-slot-symbol (class &optional slot)
  (let ((id (order-slot-id class slot)))
    (or (when (string= id "created-at") 'created-at)
        (aand (get-slot-by-id class id) (slot-symbol it))
        'updated-at)))

(defun list-order ()
  (if (string= "asc"
               (->string-down (get-parameter "order")))
      "asc" "desc"))

(defun order-param (slot-id)
  (if (string= slot-id (get-parameter "slot"))
      (if (string= (list-order) "desc")
          "asc" "desc")
      "desc"))

(defun order-fn (slot-type)
  (if (string= (list-order) "desc")
      (if (eq slot-type 'integer) #'> #'string>)
      (if (eq slot-type 'integer) #'< #'string<)))

(defun param-per-page (&optional items-per-page links-per-page)
  (values (or (aand (->int (get-parameter "items_per_page"))
                    (min *max-items-per-page* it))
              items-per-page
              *items-per-page*)
          (or (aand (->int (get-parameter "links_per_page"))
                    (min *max-links-per-page* it))
              links-per-page
              *links-per-page*)))

(defun param-per-page* (&optional items-per-page links-per-page)
  (multiple-value-bind (items-per-page links-per-page)
      (param-per-page items-per-page links-per-page)
    (concat "&items_per_page=" items-per-page
            "&links_per_page=" links-per-page)))

(defun items-per-page (class index &key items-per-page links-per-page)
  (let* ((index (order-slot-symbol class index))
         (slots (get-excluded-slots class))
         (type  (if (member index '(updated-at created-at))
                    'integer
                    (aand (get-slot class index) (slot-type it)))))
    (when type
      (multiple-value-bind (items-per-page links-per-page)
          (param-per-page items-per-page links-per-page)
        (multiple-value-bind (items pager)
            (per-page (get-instances-by-class class)
                      :index index :order (order-fn type)
                      :items-per-page items-per-page
                      :links-per-page links-per-page)
          (values items pager slots))))))

(defun unique? (class param oid)
  (if (aand (get-slot-by-id class (caar param))
            (slot-unique it)
            (unique-p class (slot-symbol it) (cdar param)
                      (get-instance-by-oid class oid)))
      "true" "false"))

(defmacro ajax-list (class &key slot sml)
  `(multiple-value-bind (items pager slots)
       (items-per-page ,class ,slot)
     (declare (ignorable web4r::pager))
     (let ((cname (->string-down ,class)))
       (load-sml (or ,sml (sml-path "pages/list.sml"))
                 ,*web4r-package*))))

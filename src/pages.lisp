(in-package :web4r)

(defmacro genpages (class &key index-slot index-sml show-sml edit-sml
                    items-per-page links-per-page)
  "Generates seven pages for the CLASS. The pages are index, show, edit,
 delete, uniquness validation, list and delete* pages. The last three pages
 are used via ajax. You can change the sml template files by the INDEX-SML,
 SHOW-SML or EDIT-SML argument. INDEX-SLOT is a symbol of the index slot
 used in the index page to sort instances. You can set the number to display
 items per page by the ITEMS-PER-PAGE and the number to display links per
 page by the LINKS-PER-PAGE argument."
  `(progn
     (defpage ,class (:get (slot ,index-slot))
       (index-page ',class :index-slot slot :sml ,index-sml
                   :items-per-page ,items-per-page
                   :links-per-page ,links-per-page))
     (defpage ,(join "/" class 'show) (oid)
       (show-page ',class oid :sml ,show-sml))
     (defpage ,(join "/" class 'edit) (oid)
       (edit-page ',class :oid oid :sml ,edit-sml))
     (defpage ,(join "/" class 'delete) (oid)
       (delete-page ',class oid))
     (defpage ,(join "/" 'ajax class 'unique) (oid)
       (p (unique-p* ',class (get-parameters*) oid)))
     (defpage ,(join "/" 'ajax class 'list) (:get slot)
       (item-list ',class :index-slot slot))
     (defpage ,(join "/" 'ajax class 'delete) (oid)
       (p (drop-instance-by-oid* ',class oid)))))

(defmacro index-page (class &key (index-slot 'updated-at) (maxlength 20)
                      (items-per-page *items-per-page*)
                      (links-per-page *links-per-page*) plural sml)
  "Displays a list of the CLASS instances for the current page as (x)html order
 by the INDEX-SLOT. You can set the number to display items per page by the
 ITEMS-PER-PAGE and the number to display links per page by the LINKS-PER-PAGE
 argument. SML is a pathname of a sml template file. The default title format
 of the page is like 'Listing pluralized-classname' where the 'pluralized-classname'
 part is created by (pluralize (->string-down class)), and you can change the
 part by the PLURAL argument. The MAXLENGTH is a max length of a slot value to
 display in a table cell, and the exceeded characters will be replaced by '...'."
  `(let* ((class  ,class)
          (cname  (->string-down ,class))
          (plural (or ,plural (pluralize cname)))
          (maxlength ,maxlength)
          (per-page (param-per-page* ,items-per-page ,links-per-page)))
     (declare (ignorable web4r::maxlength web4r::per-page))
     (multiple-value-bind (items pager slots)
         (items-per-page ,class ,index-slot
                        :items-per-page ,items-per-page
                        :links-per-page ,links-per-page)
       (load-sml (or ,sml (sml-path "pages/index.sml"))
                     ,*web4r-package*))))

(defmacro show-page (class oid &key sml)
  "Displays the values of the slots in the CLASS as (x)html for an instance
 specified by the OID. SML is a pathname of a sml template file."
  `(let ((cname (->string-down ,class))
         (slots (get-excluded-slots ,class))
         (ins   (get-instance-by-oid ,class ,oid)))
     (load-sml (or ,sml (sml-path "pages/show.sml"))
               ,*web4r-package*)))

(defmacro edit-page (class &key oid slot-values redirect-uri sml)
  "Displays an edit form for the instance specified by the OID in the CLASS.
 You can specify slot values by the SLOT-VALUES argument which must be an alist
 of a slot symbol/value paris. Users will be redirected to the REDIRECT-URI
 after successfully editing the instance. SML is a pathname of a sml template
 file."
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
  "Redirects the user to the REDIRECT-URI after deleting an instance
 specified by the OID in the CLASS."
  (redirect/msgs (rem-parameter redirect-uri "page")
                 (drop-instance-by-oid* class oid)))

(defun edit/cont (class instance page &key with-slots without-slots slot-values)
  "Validates posted parameters to edit the CLASS instance. If the INSTANCE is an
 instance of a persistent class, the instance is updated, otherwise a new instance
 is created. Users will be redirected to the PAGE after successfully editing the
 instance. The WITH-SLOTS and WITHOUT-SLOTS are lists of slot symbols used to
 specify included/excluded slots for the validations and editing. You can specify
 slot values by the SLOT-VALUES argument which must be an alist of slot a
 symbol/value pairs."
  (let ((*with-slots* (or with-slots *with-slots*))
        (*without-slots* (or without-slots *without-slots*)))
    (aif (class-validation-errors class instance)
         (apply #'page/error-msgs
                (append (list (request-uri*) it)
                        (when instance (list (oid instance)))))
         (progn
           (if instance
               (update-pinstance class instance slot-values)
               (make-pinstance class slot-values))
           (let ((msg (concat (->string-down class) " was successfully "
                              (if instance "updated" "created"))))
             (if (functionp page)
                 (funcall page msg)
                 (redirect/msgs page msg)))))))

(defun per-page (instances &key (index 'updated-at) (order #'>)
                 items-per-page links-per-page)
  "Returns two values, a list of instances and a pager instance. The 
 returned instances is a copy of the INSTANCES retrieved by the pager
 instance. INDEX is a symbol of the index slot used to sort the instances.
 ORDER is a predicate function to determine the order. You can set
 the number to display items per page by the ITEMS-PER-PAGE, and the
 number to display links per page by the LINKS-PER-PAGE argument."
  (let* ((total (length instances))
         (pager (apply #'make-instance 'pager
                       (append `(:total-items ,total)
                               (awhen items-per-page `(:items-per-page ,it))
                               (awhen links-per-page `(:links-per-page ,it)))))
         (ins (with-slots (item-start item-end items-per-page) pager
                (when (<= (current-page pager) (total-pages pager))
                  (subseq (sort instances order :key index) item-start item-end)))))
    (values ins pager)))

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

(defun unique-p* (class get-parameters oid)
  "GET-PARAMETERS must be an alist of a slot id/value pairs in the CLASS.
 Returns 'false' if the same value has been registered in the slot except
 the instance specified by the OID and 'true' otherwise."
  (if (aand (get-slot-by-id class (caar get-parameters))
            (slot-unique it)
            (unique-p class (slot-symbol it) (cdar get-parameters)
                      (get-instance-by-oid class oid)))
      "true" "false"))

(defmacro item-list (class &key index-slot sml)
  "Displays a list of the CLASS instances as a part of (x)html table order
 by the INDEX-SLOT. You can change the sml template file by the SML argument
 which must be a pathname of a valid file."
  `(multiple-value-bind (items pager slots)
       (items-per-page ,class ,index-slot)
     (declare (ignorable web4r::pager))
     (let ((cname (->string-down ,class)))
       (load-sml (or ,sml (sml-path "pages/list.sml"))
                 ,*web4r-package*))))

(in-package :web4r)

(defmacro defpages (class &key (index 'updated-at))
  `(progn
     (defpage ,class () (index-page ',class :index ',index))
     (defpage ,(join "/" class 'show) (oid)   (show-page ',class oid))
     (defpage ,(join "/" class 'edit) (oid)   (edit-page ',class :oid oid))
     (defpage ,(join "/" class 'delete) (oid) (delete-page ',class oid))
     (defpage ,(join "/" 'ajax class 'unique) (oid)
       (p (unique? ',class (get-parameters*) oid)))
     (defpage ,(join "/" 'ajax class 'list) (:get item order)
       (ajax-list ',class item order))
     (defpage ,(join "/" 'ajax class 'delete) (oid)
       (p (drop-instance-by-oid* ',class oid)))))

(defun index-page (class &key (index 'updated-at) (maxlength 20)
                   plural items-per-page links-per-page)
  (let* ((cname  (->string-down class))
         (plural (or plural (pluralize cname)))
         (slots  (get-excluded-slots class)))
    (multiple-value-bind (items pager)
        (per-page (get-instances-by-class class) :index index
                  :items-per-page items-per-page
                  :links-per-page links-per-page)
      (load-sml-path "pages/index.sml"))))

(defun show-page (class oid)
  (let ((cname (->string-down class))
        (slots (get-excluded-slots class))
        (ins   (get-instance-by-oid class oid)))
    (load-sml-path "pages/show.sml")))

(defun edit-page (class &key oid slot-values redirect-uri)
  (let* ((cname (->string-down class))
         (redirect-uri (or redirect-uri (page-uri cname)))
         (ins (awhen oid (get-instance-by-oid class it)))
         (with-slots *with-slots*)
         (without-slots *without-slots*)
         (edit/cont* (lambda ()
                       (edit/cont class ins redirect-uri
                            :with-slots with-slots
                            :without-slots without-slots
                            :slot-values slot-values))))
    (load-sml-path "pages/edit.sml")))

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

(defun per-page (items &key (index 'updated-at) (sort #'>)
                 items-per-page links-per-page)
  (let* ((total (length items))
         (pager (apply #'make-instance 'pager
                       (append `(:total-items ,total)
                               (awhen items-per-page `(:items-per-page ,it))
                               (awhen links-per-page `(:links-per-page ,it)))))
         (items (with-slots (item-start item-end items-per-page) pager
                  (when (<= (current-page pager) (total-pages pager))
                    (subseq (sort items sort :key index) item-start item-end)))))
    (values items pager)))

(defun unique? (class param oid)
  (if (aand (get-slot-by-id class (caar param))
            (slot-unique it)
            (unique-p class (slot-symbol it) (cdar param)
                      (get-instance-by-oid class oid)))
      "true" "false"))

(defun ajax-list (class id order)
  (let ((slots (get-excluded-slots class))
        (cname (->string-down class))
        (type  'integer)
        (items-per-page (get-parameter "items_per_page"))
        (links-per-page (get-parameter "links_per_page")))
    (aand (or (aand (get-slot-by-id class id)
                    (setf type (slot-type it))
                    (slot-symbol it))
              (aand (cadr (split "_" id))
                    (if (equal it "updated-at") 'updated-at
                         (when (equal it "created-at") 'created-at))))
          (let ((sort (if (string= "desc" (->string-down order))
                          (if (eq type 'integer) #'> #'string>)
                          (if (eq type 'integer) #'< #'string<))))
            (when-let (items (per-page (get-instances-by-class class)
                                  :index it :sort sort
                                  :items-per-page (->int items-per-page)
                                  :links-per-page (->int links-per-page)))
              (load-sml-path "ajax/list.sml"))))))

(in-package :web4r)

(defmacro defpages (class &key (index 'updated-at))
  `(progn
     (defpage ,class () (index-page ',class :index ',index))
     (defpage ,(join "/" class 'show) (oid) (show-page ',class oid))
     (defpage ,(join "/" class 'edit) (oid) (edit-page ',class :oid oid))
     (defpage ,(join "/" class 'delete) (oid) (delete-page ',class oid))))

(defun index-page (class &key (index 'updated-at) (maxlength 20) plural)
  (let* ((cname    (->string-down class))
         (plural   (or plural (pluralize cname)))
         (slots    (get-excluded-slots class)))
    (multiple-value-bind (items pager)
        (per-page (get-instances-by-class class) :index index)
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
         (edit/cont* (lambda () (edit/cont class ins redirect-uri
                                           :with-slots with-slots
                                           :without-slots without-slots
                                           :slot-values slot-values))))
    (load-sml-path "pages/edit.sml")))

(defun delete-page (class oid &optional
                        (redirect-uri
                         (page-uri (->string-down (join "/" class "index")))))
  (redirect/msgs (rem-parameter redirect-uri "page")
    (aif (aand oid (get-instance-by-oid class it))
         (progn (drop-instance it)
                (concat (->string-down class) " was successfully deleted"))
         "No such item")))

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

(defun per-page (items &key (index 'updated-at) (sort #'>))
  (let* ((total (length items))
         (pager (make-instance 'pager :total-items total))
         (items (with-slots (item-start item-end items-per-page) pager
                  (when (<= (current-page pager) (total-pages pager))
                    (subseq (sort items sort :key index) item-start item-end)))))
    (values items pager)))

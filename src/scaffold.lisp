(in-package :web4r)

(defmacro scaffold (class &key (index 'updated-at))
  `(progn
     (defpage ,class () (scaffold-index ',class :index ',index))
     (defpage ,(join "/" class 'show) (oid) (scaffold-show ',class oid))
     (defpage ,(join "/" class 'edit) (oid) (scaffold-edit ',class :oid oid))))

(defun scaffold-index (class &key (index 'updated-at)
                       (maxlength 20) (redirect-uri (request-uri*)))
  (let* ((cname (->string-down class))
         (slots (get-excluded-slots class)))
    (multiple-value-bind (items pager)
        (per-page (get-instances-by-class class) :index index)
      (load-sml (sml-file-path "scaffold/index.sml")))))

(defun scaffold-show (class oid)
  (let ((cname (->string-down class))
        (slots (get-excluded-slots class))
        (ins (get-instance-by-oid class oid)))
    (load-sml (sml-file-path "scaffold/show.sml"))))

(defun scaffold-edit (class &key oid slot-values redirect-uri)
  (let* ((cname (->string-down class))
         (redirect-uri (or redirect-uri (page-uri cname)))
         (ins (awhen oid (get-instance-by-oid class it)))
         (with-slots *with-slots*)
         (without-slots *without-slots*)
         (edit/cont* (lambda () (edit/cont class ins redirect-uri
                                           :with-slots with-slots
                                           :without-slots without-slots
                                           :slot-values slot-values))))
    (load-sml (sml-file-path "scaffold/edit.sml"))))

(defun per-page (items &key (index 'updated-at) (sort #'>))
  (let* ((total (length items))
         (pager (make-instance 'pager :total-items total))
         (items (with-slots (item-start item-end items-per-page) pager
                  (when (<= (current-page pager) (total-pages pager))
                    (subseq (sort items sort :key index) item-start item-end)))))
    (values items pager)))

(defun delete/cont (ins cname redirect-uri)
  (drop-instance ins)
  (redirect/msgs (rem-parameter redirect-uri "page")
                 (concat cname " was successfully deleted")))

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

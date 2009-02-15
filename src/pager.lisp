(in-package :web4r)

(defparameter *page-param* "page")

(defclass pager ()
  ((items-per-page :type     integer
                   :initform 10
                   :initarg  :items-per-page)
   (links-per-page :type     integer
                   :initform 10
                   :initarg  :links-per-page)
   (total-items    :type     integer
                   :initarg  :total-items
                   :initform (error "Must supply total-items"))
   (current-page   :type integer
                   :initarg  :current-page
                   :initform (get-current-page))
   (total-pages    :type integer :accessor total-pages)
   (item-start     :type integer :accessor item-start)
   (item-end       :type integer)
   (link-start     :type integer)
   (link-end       :type integer)))

(defun get-current-page ()
  (let ((page (awhen (get-param *page-param*) (->int it))))
    (if (aand page (plusp it)) page 1)))

(defmethod initialize-instance :after ((pager pager) &key)
  (with-slots (item-start item-end total-items total-pages
                          items-per-page current-page) pager
    (setf total-pages (if (= total-items 0)
                          0
                          (ceiling (/ total-items items-per-page))))
    (setf item-start  (* (1- current-page) items-per-page))
    (setf item-end    (let ((e (* current-page items-per-page)))
                        (if (> e total-items) total-items e)))
    (set-links pager)))

(defun set-links (pager)
  (with-slots (link-start link-end links-per-page
                          total-pages current-page) pager
    (flet ((set-link (s e) (setf link-start s link-end e)))
      (cond ((= total-pages 1)
             (set-link 0 0))
            ((>= links-per-page total-pages)
             (set-link 1 total-pages))
            (t (let* ((left  (round (/ links-per-page 2)))
                      (right (- links-per-page left)))
                 (cond ((<= current-page left)
                        (set-link 1 links-per-page))
                       ((> (+ current-page right) total-pages)
                        (set-link (- total-pages links-per-page)
                                   total-pages))
                       (t (set-link (- current-page left)
                                    (+ current-page right))))))))))

(defun page-links/ (pager)
  (with-slots (total-pages link-start link-end current-page) pager
    (unless (= total-pages 1)
      (load-shtml (shtml-path "common/page_links.shtml")))))

(defun page-summary/ (pager)
  (with-slots (total-items item-start item-end) pager
    (let ((item-start (1+ item-start)))
      (load-shtml (shtml-path "common/page_summary.shtml")))))

(defun w/p (link)
  "link with page parameter"
  (add-get-param link *page-param* (get-current-page)))

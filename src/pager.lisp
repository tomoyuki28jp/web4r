(in-package :web4r)

(defvar *page-param* "page")

(defclass pager ()
  ((items-per-page :type integer :initform 10 :initarg :items-per-page)
   (links-per-page :type integer :initform 10 :initarg :links-per-page)
   (total-items    :type integer :initarg :total-items
                   :initform (error "Must supply total-items"))
   (current-page   :type integer :initarg :current-page
                   :initform (get-current-page) :accessor current-page)
   (total-pages    :type integer :accessor total-pages)
   (item-start     :type integer :accessor item-start)
   (item-end       :type integer :accessor item-end)
   (link-start     :type integer :accessor link-start)
   (link-end       :type integer :accessor link-end)))

(defun get-current-page ()
  (let ((page (->int (get-parameter *page-param*))))
    (if (aand page (plusp it)) page 1)))

(defmethod initialize-instance :after ((p pager) &key)
  (with-slots (total-items total-pages items-per-page current-page) p
    (setf total-pages    (ceiling (/ total-items items-per-page))
          (item-start p) (* (1- current-page) items-per-page)
          (item-end   p) (let ((e (* current-page items-per-page)))
                             (if (> e total-items) total-items e)))
    (multiple-value-bind (link-start link-end) (link-limit p)
      (setf (link-start p) link-start
            (link-end   p) link-end))))

(defun link-limit (pager)
  (with-slots (links-per-page total-pages current-page) pager
    (cond ((= total-pages 1) (values 0 0))
          ((>= links-per-page total-pages) (values 1 total-pages))
          (t (let* ((left  (round (/ links-per-page 2)))
                    (right (- links-per-page left)))
               (cond ((<= current-page left) (values 1 links-per-page))
                     ((> (+ current-page right) total-pages)
                      (values (- total-pages links-per-page) total-pages))
                     (t (values (- current-page left)
                                (+ current-page right)))))))))

(defun page-links/ (pager)
  (with-slots (total-pages link-start link-end current-page) pager
    (when (> total-pages 1)
      (load-shtml (shtml-file-path "common/page_links.shtml")))))

(defun page-summary/ (pager)
  (with-slots (total-items item-start item-end) pager
    (let ((item-start (1+ item-start)))
      (load-shtml (shtml-file-path "common/page_summary.shtml")))))

(defun w/p (link)
  "returns the link with a page parameter"
  (add-parameter link *page-param* (get-current-page)))

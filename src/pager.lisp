(in-package :web4r)

(defclass pager ()
  ((items-per-page :type integer :initform *items-per-page* :initarg :items-per-page
                   :documentation "The number to display items per page.")
   (links-per-page :type integer :initform *links-per-page* :initarg :links-per-page
                   :documentation "The number to display page links per page.")
   (total-items    :type integer :initarg :total-items
                   :initform (error "Must supply total-items.")
                   :documentation "The number of total items.")
   (current-page   :type integer :initarg :current-page
                   :initform (get-current-page) :accessor current-page
                   :documentation "The current page number.")
   (total-pages    :type integer :accessor total-pages
                   :documentation "The number of total pages.")
   (item-start     :type integer :accessor item-start
                   :documentation "The start number to display items.")
   (item-end       :type integer :accessor item-end
                   :documentation "The end number to display items.")
   (link-start     :type integer :accessor link-start
                   :documentation "The start number to display page links.")
   (link-end       :type integer :accessor link-end
                   :documentation "The end number to display page links.")
   (next-link      :type string  :accessor next-link :initform ">>"
                   :documentation "The link to next page links.")
   (prev-link      :type string  :accessor prev-link :initform "<<"
                   :documentation "The link to previous page links."))
  (:documentation "A pagination class."))

(defun get-current-page ()
  "Returns the current page number."
  (let ((page (->int (get-parameter *page-param*))))
    (if (aand page (plusp it)) page 1)))

(defmethod initialize-instance :after ((p pager) &key)
  (with-slots (total-items total-pages items-per-page current-page) p
    (setf total-pages    (ceiling (/ total-items items-per-page))
          (item-start p) (* (1- current-page) items-per-page)
          (item-end   p) (min (* current-page items-per-page) total-items))
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
                      (values (1+ (- total-pages links-per-page)) total-pages))
                     (t (values (- current-page left)
                                (+ current-page right)))))))))

(defmacro prev-link* (pager &optional params)
  "Prints the link to previous page links. The default is '<<'."
  `(with-slots (links-per-page current-page prev-link) ,pager
     (let ((page   (max 1 (- current-page links-per-page)))
           (link   (prev-link ,pager))
           (params ,params))
       (when (< 1 (link-start ,pager))
         (load-sml-path "paging/page_link.sml" ,*web4r-package*)))))

(defmacro next-link* (pager &optional params)
  "Prints the link to next page links. The default is '>>'."
  `(with-slots (links-per-page current-page next-link total-pages) ,pager
     (let ((page   (min total-pages (+ current-page links-per-page)))
           (link   (next-link ,pager))
           (params ,params))
       (when (< (link-end ,pager) total-pages)
         (load-sml-path "paging/page_link.sml" ,*web4r-package*)))))

(defun page-links (pager &optional params)
  "Prints page links."
  (with-slots (total-pages link-start link-end current-page) pager
    (when (> total-pages 1)
      (load-sml-path "paging/page_links.sml"))))

(defun page-summary (pager)
  "Prints pagination summary. The default format is 'Results 1 - 10  of 100'."
  (with-slots
        (total-items item-start item-end items-per-page links-per-page) pager
    (let ((item-start (1+ item-start)))
      (load-sml-path "paging/page_summary.sml"))))

(defun w/p (link)
  "Returns the LINK with a get parameter denotate the current page number."
  (add-parameter link *page-param* (get-current-page)))

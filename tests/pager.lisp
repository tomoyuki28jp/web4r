(in-package :web4r-tests)
(in-suite web4r)

(test total-pages
  (with-post-parameters nil
    (let ((i (make-instance
              'pager :total-items 100 :items-per-page 10)))
      (is (eq 10 (total-pages i))))
    (let ((i (make-instance
              'pager :total-items 80  :items-per-page 10)))
      (is (eq  8 (total-pages i))))
    (let ((i (make-instance
              'pager :total-items 81  :items-per-page 10)))
      (is (eq  9 (total-pages i))))))

(test item-start
  (with-post-parameters nil
    (let ((i (make-instance
              'pager :total-items 12 :items-per-page 10 :current-page 1)))
      (is (eq 0  (web4r::item-start i))))
    (let ((i (make-instance
              'pager :total-items 12 :items-per-page 10 :current-page 2)))
      (is (eq 10 (web4r::item-start i))))))

(test item-end
  (with-post-parameters nil
    (let ((i (make-instance
              'pager :total-items 12 :items-per-page 10 :current-page 1)))
      (is (eq 10 (slot-value i 'web4r::item-end))))
    (let ((i (make-instance
              'pager :total-items 12 :items-per-page 10 :current-page 2)))
      (is (eq 12 (slot-value i 'web4r::item-end))))))

(test link-start
  (with-post-parameters nil
    (let ((i (make-instance
              'pager :total-items  80 :items-per-page 10
              :current-page  3 :links-per-page 10)))
      (is (eq 1 (slot-value i 'web4r::link-start))))
    (let ((i (make-instance
              'pager :total-items 130 :items-per-page 10
              :current-page 10 :links-per-page 10)))
      (is (eq 3 (slot-value i 'web4r::link-start))))
    (let ((i (make-instance
              'pager :total-items 200 :items-per-page 10
              :current-page 10 :links-per-page 10)))
      (is (eq 5 (slot-value i 'web4r::link-start))))))

(test link-end
  (with-post-parameters nil
    (let ((i (make-instance
              'pager :total-items 120 :items-per-page 10
              :current-page  3 :links-per-page 10)))
      (is (eq 10 (slot-value i 'web4r::link-end))))
    (let ((i (make-instance
              'pager :total-items 130 :items-per-page 10
              :current-page 10 :links-per-page 10)))
      (is (eq 13 (slot-value i 'web4r::link-end))))
    (let ((i (make-instance
              'pager :total-items 180 :items-per-page 10
              :current-page  3 :links-per-page 10)))
      (is (eq 10 (slot-value i 'web4r::link-end))))
    (let ((i (make-instance
              'pager :total-items 200 :items-per-page 10
              :current-page 10 :links-per-page 10)))
      (is (eq 15 (slot-value i 'web4r::link-end))))))

(test get-current-page
  (with-get-parameters (list (cons *page-param* "3"))
    (is (eq 3 (get-current-page))))
  (with-get-parameters (list (cons *page-param* "0"))
    (is (eq 1 (get-current-page)))))

(test w/p
  (with-get-parameters (list (cons *page-param* "3"))
    (is (equal (w/p "http://localhost:8080/")
               "http://localhost:8080/?page=3")))
  (with-get-parameters (list (cons *page-param* "0"))
    (is (equal (w/p "http://localhost:8080/")
               "http://localhost:8080/?page=1"))))

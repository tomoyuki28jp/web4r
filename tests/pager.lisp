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

(test get-current-page
  (let ((*page-param* "page"))
    (with-get-parameters `((,*page-param* . "1"))
      (is (eq (get-current-page) 1))))
  (let ((*page-param* "foo"))
    (with-get-parameters `((,*page-param* . "2"))
      (is (eq (get-current-page) 2)))))

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
              :current-page 3  :links-per-page 10)))
      (is (eq 1 (slot-value i 'web4r::link-start))))
    (let ((i (make-instance
              'pager :total-items 130 :items-per-page 10
              :current-page 10 :links-per-page 10)))
      (is (eq 4 (slot-value i 'web4r::link-start))))
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

(defmacro page-link= (page x)
  (with-gensyms (match regs)
    `(multiple-value-bind (,match ,regs)
         (scan-to-strings "\"\?page=([0-9]+)\"" (sml->ml ,x))
       (declare (ignore ,match))
       (eq ,page (aand ,regs (->int (elt it 0)))))))

(test prev-link
  (let ((i (make-instance
            'pager :total-items 90 :items-per-page 10
            :current-page  1 :links-per-page 10)))
    (is (page-link= nil (prev-link* i))))
  (let ((i (make-instance
            'pager :total-items 90 :items-per-page 10
            :current-page  6 :links-per-page 10)))
    (is (page-link= nil (prev-link* i))))

  (let ((i (make-instance
            'pager :total-items 120 :items-per-page 10
            :current-page  7 :links-per-page 10)))
    (is-true (page-link= 1 (prev-link* i))))
  (let ((i (make-instance
            'pager :total-items 240 :items-per-page 10
            :current-page  12 :links-per-page 10)))
    (is-true (page-link= 2 (prev-link* i)))))

(test next-link
  (let ((i (make-instance
            'pager :total-items 200 :items-per-page 10
            :current-page  15 :links-per-page 10)))
    (is (page-link= nil (next-link* i))))
  (let ((i (make-instance
            'pager :total-items 200 :items-per-page 10
            :current-page  14 :links-per-page 10)))
    (is-true (page-link= 20 (next-link* i))))
  (let ((i (make-instance
            'pager :total-items 120 :items-per-page 10
            :current-page  3 :links-per-page 10)))
    (is-true (page-link= 12 (next-link* i)))))

(test w/p
  (with-get-parameters (list (cons *page-param* "3"))
    (is (equal (w/p "http://localhost:8080/")
               "http://localhost:8080/?page=3")))
  (with-get-parameters (list (cons *page-param* "0"))
    (is (equal (w/p "http://localhost:8080/")
               "http://localhost:8080/?page=1"))))

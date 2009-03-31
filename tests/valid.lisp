(in-package :web4r-tests)
(in-suite web4r)

(defvar *label* "label")

(test leap-year-p
  (loop for y from 2000 to 2008
        for a in '(t nil nil nil t nil nil nil t)
        collect (is (eq (web4r::leap-year-p y) a))))

(test days-of
  (loop for m from 1 to 12
        as a in '(31 28 31 30 31 30 31 31 30 31 30 31)
        collect (is (eq (web4r::days-of 2009 m) a))
        finally (is (eq 29 (web4r::days-of 2008 2)))))

(test valid-date-p
  (is       (web4r::valid-date-p "1983" "9" "28"))
  (is-false (web4r::valid-date-p "1983" "13" "28"))
  (is-false (web4r::valid-date-p "1983" "9" "32"))
  (is-false (web4r::valid-date-p "2009" "2" "29"))
  (is       (web4r::valid-date-p "2008" "2" "29")))

(test valid-email-p
  (is       (web4r::valid-email-p "test+regexp@test.com"))
  (is-false (web4r::valid-email-p "da.me..@test.com"))
  (is-false (web4r::valid-email-p "invalid")))

(test empty
  (is       (web4r::empty nil))
  (is       (web4r::empty ""))
  (is       (web4r::empty '()))
  (is       (web4r::empty '(nil)))
  (is-false (web4r::empty 1))
  (is-false (web4r::empty "1")))

(test length
  (is (eq    (validation-errors *label* "123456789" '(:length 20)) nil))
  (is (eq    (validation-errors *label* "12345" '(:length (3 10))) nil))
  (is (equal (validation-errors *label* "123456789" '(:length 3))
             (list (web4r::error-msg :too-long *label* 3))))
  (is (equal (validation-errors *label* "123" '(:length (5 8)))
             (list (web4r::error-msg :too-short *label* 5))))
  (is (equal (validation-errors *label* "1234567890" '(:length (3 5)))
             (list (web4r::error-msg :too-long *label* 5)))))

(test type-date
  (is-false  (validation-errors *label* '("1983" "9" "28")  '(:type :date)))
  (is (equal (validation-errors *label* '("1983" "13" "28") '(:type :date))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* '("1983" "9" "32")  '(:type :date))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* '("2009" "2" "29")  '(:type :date))
             (list (web4r::error-msg :invalid *label*))))
  (is-false  (validation-errors *label* '("2008" "2" "29")  '(:type :date))))

(test type-alpha
  (is-false  (validation-errors *label* "ABCDEFGhijkl" '(:type :alpha)))
  (is (equal (validation-errors *label* "aiueo1" '(:type :alpha))
             (list (web4r::error-msg :not-alpha *label*))))
  (is (equal (validation-errors *label* "1aiueo" '(:type :alpha))
             (list (web4r::error-msg :not-alpha *label*))))
  (is (equal (validation-errors *label* "ai1ueo" '(:type :alpha))
             (list (web4r::error-msg :not-alpha *label*)))))

(test type-alnum
  (is-false (validation-errors *label* "aiueo1" '(:type :alnum)))
  (is (equal (validation-errors *label* "ai@ueo" '(:type :alnum))
             (list (web4r::error-msg :not-alnum *label*))))
  (is (equal (validation-errors *label* "!aiueo" '(:type :alnum))
             (list (web4r::error-msg :not-alnum *label*))))
  (is (equal (validation-errors *label* "aiueo-" '(:type :alnum))
             (list (web4r::error-msg :not-alnum *label*)))))

(test type-integer
  (is-false  (validation-errors *label* "1234567890" '(:type :integer)))
  (is (equal (validation-errors *label* "1234a56789" '(:type :integer))
             (list (web4r::error-msg :not-a-number *label*))))
  (is (equal (validation-errors *label* "a123456789" '(:type :integer))
             (list (web4r::error-msg :not-a-number *label*))))
  (is (equal (validation-errors *label* "123456789a" '(:type :integer))
             (list (web4r::error-msg :not-a-number *label*)))))

(test type-email
  (is-false  (validation-errors *label* "test@test.com" '(:type :email)))
  (is (equal (validation-errors *label* "invalid"       '(:type :email))
             (list (web4r::error-msg :invalid *label*)))))

(test type-regex
  (is-false (validation-errors *label* "408-644-1234"
                               '(:type (:regex "^\\d{3}-\\d{3}-\\d{4}$"))))
  (is (equal (validation-errors *label* "408-644-12340"
                                '(:type (:regex "^\\d{3}-\\d{3}-\\d{4}$")))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* "408-6440-1234"
                                '(:type (:regex "^\\d{3}-\\d{3}-\\d{4}$")))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* "4080-644-1234"
                                '(:type (:regex "^\\d{3}-\\d{3}-\\d{4}$")))
             (list (web4r::error-msg :invalid *label*)))))

(test type-image
  (is-false  (validation-errors *label* (test-file "test.gif")  '(:type :image)))
  (is-false  (validation-errors *label* (test-file "test.jpeg") '(:type :image)))
  (is-false  (validation-errors *label* (test-file "test.png")  '(:type :image)))
  (is (equal (validation-errors *label* (test-file "test.ico")  '(:type :image))
             (list (web4r::error-msg :not-a-image *label*))))
  (is (equal (validation-errors *label* (test-file "test")      '(:type :image))
             (list (web4r::error-msg :not-a-image *label*))))
  (is (equal (validation-errors *label* (test-file "test.ico")  '(:type :image))
             (list (web4r::error-msg :not-a-image *label*))))
  (is (equal (validation-errors *label* (test-file "test.zip")  '(:type :image))
             (list (web4r::error-msg :not-a-image *label*)))))

(test type-member
  (is (equal (validation-errors *label* "n" '(:type (:member ("a" "i" "u"))))
              (list (web4r::error-msg :invalid *label*))))
  (is-false  (validation-errors *label* "i" '(:type (:member ("a" "i" "u"))))))

(test nullable
  (is-false  (validation-errors *label* " " '(:nullable nil)))
  (is (equal (validation-errors *label* nil '(:nullable nil))
             (list (web4r::error-msg :empty *label*))))
  (is (equal (validation-errors *label* ""  '(:nullable nil))
             (list (web4r::error-msg :empty *label*))))
  (is (equal (validation-errors *label* '(nil nil nil) '(:nullable nil))
             (list (web4r::error-msg :empty *label*)))))

(test with-validations
  (is (equal (with-validations (("1" "v" '(:nullable nil))
                                ("2" "v" '(:nullable nil)))
               (lambda (e) e)
               "ok")
             "ok"))
  (is (equal (with-validations (("1" "v" '(:nullable nil))
                                ("2" nil '(:nullable nil)))
               (lambda (e) e)
               "ok")
             (list (web4r::error-msg :empty "2"))))
  (is (equal (with-validations (("1" nil '(:nullable nil))
                                ("2" nil '(:nullable nil)))
               (lambda (e) e)
               "ok")
             (list (web4r::error-msg :empty "1")
                   (web4r::error-msg :empty "2")))))

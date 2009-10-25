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

(test format-alpha
  (is-false  (validation-errors *label* "ABCDEFGhijkl" '(:format :alpha)))
  (is (equal (validation-errors *label* "aiueo1" '(:format :alpha))
             (list (web4r::error-msg :not-alpha *label*))))
  (is (equal (validation-errors *label* "1aiueo" '(:format :alpha))
             (list (web4r::error-msg :not-alpha *label*))))
  (is (equal (validation-errors *label* "ai1ueo" '(:format :alpha))
             (list (web4r::error-msg :not-alpha *label*)))))

(test format-alnum
  (is-false (validation-errors *label* "aiueo1" '(:format :alnum)))
  (is (equal (validation-errors *label* "ai@ueo" '(:format :alnum))
             (list (web4r::error-msg :not-alnum *label*))))
  (is (equal (validation-errors *label* "!aiueo" '(:format :alnum))
             (list (web4r::error-msg :not-alnum *label*))))
  (is (equal (validation-errors *label* "aiueo-" '(:format :alnum))
             (list (web4r::error-msg :not-alnum *label*)))))

(test format-integer
  (is-false  (validation-errors *label* "1234567890" '(:format :integer)))
  (is (equal (validation-errors *label* "1234a56789" '(:format :integer))
             (list (web4r::error-msg :not-a-number *label*))))
  (is (equal (validation-errors *label* "a123456789" '(:format :integer))
             (list (web4r::error-msg :not-a-number *label*))))
  (is (equal (validation-errors *label* "123456789a" '(:format :integer))
             (list (web4r::error-msg :not-a-number *label*)))))

(test format-email
  (is-false  (validation-errors *label* "test@test.com"        '(:format :email)))
  (is-false  (validation-errors *label* "test+regexp@test.com" '(:format :email)))
  (is (equal (validation-errors *label* "invalid"              '(:format :email))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* "da.me..@test.com"     '(:format :email))
             (list (web4r::error-msg :invalid *label*)))))

(test format-date
  (is-false  (validation-errors *label* '("1983" "9" "28")  '(:format :date)))
  (is (equal (validation-errors *label* '("1983" "13" "28") '(:format :date))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* '("1983" "9" "32")  '(:format :date))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* '("2009" "2" "29")  '(:format :date))
             (list (web4r::error-msg :invalid *label*))))
  (is-false  (validation-errors *label* '("2008" "2" "29")  '(:format :date))))

(test format-image
  (is-false  (validation-errors *label* (test-file "test.gif")  '(:format :image)))
  (is-false  (validation-errors *label* (test-file "test.jpeg") '(:format :image)))
  (is-false  (validation-errors *label* (test-file "test.png")  '(:format :image)))
  (is (equal (validation-errors *label* (test-file "test.ico")  '(:format :image))
             (list (web4r::error-msg :not-a-image *label*))))
  (is (equal (validation-errors *label* (test-file "test")      '(:format :image))
             (list (web4r::error-msg :not-a-image *label*))))
  (is (equal (validation-errors *label* (test-file "test.ico")  '(:format :image))
             (list (web4r::error-msg :not-a-image *label*))))
  (is (equal (validation-errors *label* (test-file "test.zip")  '(:format :image))
             (list (web4r::error-msg :not-a-image *label*)))))

(test format-regex
  (is-false (validation-errors *label* "408-644-1234"
                               '(:format "^\\d{3}-\\d{3}-\\d{4}$")))
  (is (equal (validation-errors *label* "408-644-12340"
                                '(:format "^\\d{3}-\\d{3}-\\d{4}$"))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* "408-6440-1234"
                                '(:format "^\\d{3}-\\d{3}-\\d{4}$"))
             (list (web4r::error-msg :invalid *label*))))
  (is (equal (validation-errors *label* "4080-644-1234"
                                '(:format "^\\d{3}-\\d{3}-\\d{4}$"))
             (list (web4r::error-msg :invalid *label*)))))

(test member
  (is (equal (validation-errors *label* "n" '(:member ("a" "i" "u")))
              (list (web4r::error-msg :invalid *label*))))
  (is-false  (validation-errors *label* "i" '(:member ("a" "i" "u")))))

(test required
  (is-false  (validation-errors *label* " " '(:required t)))
  (is (equal (validation-errors *label* nil '(:required t))
             (list (web4r::error-msg :empty *label*))))
  (is (equal (validation-errors *label* ""  '(:required t))
             (list (web4r::error-msg :empty *label*))))
  (is (equal (validation-errors *label* '(nil nil nil) '(:required t))
             (list (web4r::error-msg :empty *label*)))))

(test with-validations
  (is (equal (with-validations (("1" "v" '(:required t))
                                ("2" "v" '(:required t)))
               (lambda (e) e)
               "ok")
             "ok"))
  (is (equal (with-validations (("1" "v" '(:required t))
                                ("2" nil '(:required t)))
               (lambda (e) e)
               "ok")
             (list (web4r::error-msg :empty "2"))))
  (is (equal (with-validations (("1" nil '(:required t))
                                ("2" nil '(:required t)))
               (lambda (e) e)
               "ok")
             (list (web4r::error-msg :empty "1")
                   (web4r::error-msg :empty "2")))))

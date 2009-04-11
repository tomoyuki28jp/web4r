(in-package :web4r-tests)
(in-suite web4r)

(defun rem-newline (x)
  (remove #\newline x))

(defun string=* (str1 str2)
  (string= (rem-newline str1) (rem-newline str2)))

(defpclass testdb1 ()
    ((name         :length 50 :label "full name" :size 30)
     (password     :input :password :length (8 12) :hide t :comment "8-12 characters")
     (email        :type :email :unique t)
     (sex          :input :radio :options ("male" "female"))
     (marriage     :input :select :options ("single" "married" "divorced"))
     (hobbies      :input :checkbox :options ("sports" "music" "reading"))
     (birth-date   :type :date)
     (nickname     :length 50 :required nil)
     (phone-number :type (:regex "^\\d{3}-\\d{3}-\\d{4}$") :comment "xxx-xxx-xxxx")
     (zip-code     :type :integer :length 5 :comment "5 digit")
     (note         :length 300 :rows 5 :cols 30)
     (image        :input :file :type :image :length (1000 500000) :required nil)))

(defvar *test-slots*
  '(name password email sex marriage hobbies birth-date
    nickname phone-number zip-code note image))

(test get-slots
  (is (eq (length *test-slots*) (length (get-slots 'testdb1))))
  (loop for s in (get-slots 'testdb1)
        do (is (member (web4r::slot-symbol s) *test-slots*))))

(test get-excluded-slots
  (let ((sl (remove 'password *test-slots*)))
    (is (eq (length sl) (length (get-excluded-slots 'testdb1))))
    (loop for s in (get-excluded-slots 'testdb1)
          do (is (member (web4r::slot-symbol s) sl))))
  (let ((*with-slots* :all))
    (is (eq (length *test-slots*) (length (get-excluded-slots 'testdb1))))
    (loop for s in (get-excluded-slots 'testdb1)
          do (is (member (web4r::slot-symbol s) *test-slots*))))
  (let ((*without-slots* '(email))
        (sl (remove 'email (remove 'password *test-slots*))))
    (is (eq (length sl) (length (get-excluded-slots 'testdb1))))
    (loop for s in (get-excluded-slots 'testdb1)
          do (is (member (web4r::slot-symbol s) sl))))
  (let ((*with-slots* '(name password email))
        (sl '(name email)))
    (is (eq (length sl) (length (get-excluded-slots 'testdb1))))
    (loop for s in (get-excluded-slots 'testdb1)
          do (is (member (web4r::slot-symbol s) sl)))))

(test slot-symbol
  (loop for s in '(name password email sex marriage hobbies birth-date
                   nickname phone-number zip-code note image)
        do (is (eq s (web4r::slot-symbol (get-slot 'testdb1 s))))))

(test slot-id
  (is (equal "testdb1_name"         (web4r::slot-id (get-slot 'testdb1 'name))))
  (is (equal "testdb1_password"     (web4r::slot-id (get-slot 'testdb1 'password))))
  (is (equal "testdb1_email"        (web4r::slot-id (get-slot 'testdb1 'email))))
  (is (equal "testdb1_sex"          (web4r::slot-id (get-slot 'testdb1 'sex))))
  (is (equal "testdb1_marriage"     (web4r::slot-id (get-slot 'testdb1 'marriage))))
  (is (equal "testdb1_hobbies"      (web4r::slot-id (get-slot 'testdb1 'hobbies))))
  (is (equal "testdb1_birth-date"   (web4r::slot-id (get-slot 'testdb1 'birth-date))))
  (is (equal "testdb1_nickname"     (web4r::slot-id (get-slot 'testdb1 'nickname))))
  (is (equal "testdb1_phone-number" (web4r::slot-id (get-slot 'testdb1 'phone-number))))
  (is (equal "testdb1_zip-code"     (web4r::slot-id (get-slot 'testdb1 'zip-code))))
  (is (equal "testdb1_note"         (web4r::slot-id (get-slot 'testdb1 'note))))
  (is (equal "testdb1_image"        (web4r::slot-id (get-slot 'testdb1 'image)))))

(test slot-label
  (is (equal "full name"    (web4r::slot-label (get-slot 'testdb1 'name))))
  (is (equal "Password"     (web4r::slot-label (get-slot 'testdb1 'password))))
  (is (equal "Email"        (web4r::slot-label (get-slot 'testdb1 'email))))
  (is (equal "Sex"          (web4r::slot-label (get-slot 'testdb1 'sex))))
  (is (equal "Marriage"     (web4r::slot-label (get-slot 'testdb1 'marriage))))
  (is (equal "Hobbies"      (web4r::slot-label (get-slot 'testdb1 'hobbies))))
  (is (equal "Birth Date"   (web4r::slot-label (get-slot 'testdb1 'birth-date))))
  (is (equal "Nickname"     (web4r::slot-label (get-slot 'testdb1 'nickname))))
  (is (equal "Phone Number" (web4r::slot-label (get-slot 'testdb1 'phone-number))))
  (is (equal "Zip Code"     (web4r::slot-label (get-slot 'testdb1 'zip-code))))
  (is (equal "Note"         (web4r::slot-label (get-slot 'testdb1 'note))))
  (is (equal "Image"        (web4r::slot-label (get-slot 'testdb1 'image)))))

(test slot-unique
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'name))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'password))))
  (is (eq t   (web4r::slot-unique (get-slot 'testdb1 'email))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'sex))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'marriage))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'zip-code))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-unique (get-slot 'testdb1 'image)))))

(test slot-required
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'name))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'password))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'email))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'sex))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'marriage))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'hobbies))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-required (get-slot 'testdb1 'nickname))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'phone-number))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'zip-code))))
  (is (eq t   (web4r::slot-required (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-required (get-slot 'testdb1 'image)))))

(test slot-rows
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'name))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'password))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'email))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'sex))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'marriage))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'zip-code))))
  (is (eq 5   (web4r::slot-rows (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-rows (get-slot 'testdb1 'image)))))

(test slot-cols
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'name))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'password))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'email))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'sex))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'marriage))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'zip-code))))
  (is (eq 30  (web4r::slot-cols (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-cols (get-slot 'testdb1 'image)))))

(test slot-size
  (is (eq 30  (web4r::slot-size (get-slot 'testdb1 'name))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'password))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'email))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'sex))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'marriage))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'zip-code))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-size (get-slot 'testdb1 'image)))))

(test slot-length
  (is (eq 50                (web4r::slot-length (get-slot 'testdb1 'name))))
  (is (equal '(8 12)        (web4r::slot-length (get-slot 'testdb1 'password))))
  (is (eq nil               (web4r::slot-length (get-slot 'testdb1 'email))))
  (is (eq nil               (web4r::slot-length (get-slot 'testdb1 'sex))))
  (is (eq nil               (web4r::slot-length (get-slot 'testdb1 'marriage))))
  (is (eq nil               (web4r::slot-length (get-slot 'testdb1 'hobbies))))
  (is (eq nil               (web4r::slot-length (get-slot 'testdb1 'birth-date))))
  (is (eq 50                (web4r::slot-length (get-slot 'testdb1 'nickname))))
  (is (eq nil               (web4r::slot-length (get-slot 'testdb1 'phone-number))))
  (is (eq 5                 (web4r::slot-length (get-slot 'testdb1 'zip-code))))
  (is (eq 300               (web4r::slot-length (get-slot 'testdb1 'note))))
  (is (equal '(1000 500000) (web4r::slot-length (get-slot 'testdb1 'image)))))

(test slot-hide
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'name))))
  (is (eq t   (web4r::slot-hide (get-slot 'testdb1 'password))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'email))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'sex))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'marriage))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'zip-code))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-hide (get-slot 'testdb1 'image)))))

(test slot-options
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'name))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'password))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'email))))
  (is (equal '("male" "female")
                 (web4r::slot-options (get-slot 'testdb1 'sex))))
  (is (equal '("single" "married" "divorced")
                 (web4r::slot-options (get-slot 'testdb1 'marriage))))
  (is (equal '("sports" "music" "reading")
                 (web4r::slot-options (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'birth-date))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'zip-code))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'note))))
  (is (eq nil (web4r::slot-options (get-slot 'testdb1 'image)))))

(test slot-comment
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'name))))
  (is (equal "8-12 characters" (web4r::slot-comment (get-slot 'testdb1 'password))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'email))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'sex))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'marriage))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'hobbies))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'birth-date))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'nickname))))
  (is (equal "xxx-xxx-xxxx"    (web4r::slot-comment (get-slot 'testdb1 'phone-number))))
  (is (equal "5 digit"         (web4r::slot-comment (get-slot 'testdb1 'zip-code))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'note))))
  (is (equal ""                (web4r::slot-comment (get-slot 'testdb1 'image)))))

(test slot-input
  (is (eq nil       (web4r::slot-input (get-slot 'testdb1 'name))))
  (is (eq :password (web4r::slot-input (get-slot 'testdb1 'password))))
  (is (eq nil       (web4r::slot-input (get-slot 'testdb1 'email))))
  (is (eq :radio    (web4r::slot-input (get-slot 'testdb1 'sex))))
  (is (eq :select   (web4r::slot-input (get-slot 'testdb1 'marriage))))
  (is (eq :checkbox (web4r::slot-input (get-slot 'testdb1 'hobbies))))
  (is (eq nil       (web4r::slot-input (get-slot 'testdb1 'birth-date))))
  (is (eq nil       (web4r::slot-input (get-slot 'testdb1 'nickname))))
  (is (eq nil       (web4r::slot-input (get-slot 'testdb1 'phone-number))))
  (is (eq nil       (web4r::slot-input (get-slot 'testdb1 'zip-code))))
  (is (eq :textarea (web4r::slot-input (get-slot 'testdb1 'note))))
  (is (eq :file     (web4r::slot-input (get-slot 'testdb1 'image)))))

(test slot-type
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'name))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'password))))
  (is (eq :email   (web4r::slot-type (get-slot 'testdb1 'email))))
  (is (equal '(:member ("male" "female"))
                   (web4r::slot-type (get-slot 'testdb1 'sex))))
  (is (equal '(:member ("single" "married" "divorced"))
                   (web4r::slot-type (get-slot 'testdb1 'marriage))))
  (is (equal '(:member ("sports" "music" "reading"))
                   (web4r::slot-type (get-slot 'testdb1 'hobbies))))
  (is (eq :date    (web4r::slot-type (get-slot 'testdb1 'birth-date))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'nickname))))
  (is (equal '(:regex "^\\d{3}-\\d{3}-\\d{4}$")
                   (web4r::slot-type (get-slot 'testdb1 'phone-number))))
  (is (eq :integer (web4r::slot-type (get-slot 'testdb1 'zip-code))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'note))))
  (is (eq :image   (web4r::slot-type (get-slot 'testdb1 'image)))))

(defun safe= (html safe)
  (equal html (rem-newline (slot-value safe 'sml::obj))))

(test slot-display-value
  (let ((i (make-instance 'testdb1
             :name         "tomoyuki matsumoto"
             :password     "password"
             :email        "tomo@example.com"
             :sex          "male"
             :marriage     "single"
             :hobbies      '("sports" "reading")
             :birth-date   "1983-09-28"
             :nickname     "tomo"
             :phone-number "408-644-6198"
             :zip-code     "95129"
             :note         (format nil "hello~%world")
             :image        "test.gif")))
    (is (equal "tomoyuki matsumoto"
               (slot-display-value i (get-slot 'testdb1 'name))))
    (is (equal "password"
               (slot-display-value i (get-slot 'testdb1 'password))))
    (is (equal "tomo@example.com"
               (slot-display-value i (get-slot 'testdb1 'email))))
    (is (equal "male"
               (slot-display-value i (get-slot 'testdb1 'sex))))
    (is (equal "single"
               (slot-display-value i (get-slot 'testdb1 'marriage))))
    (is (equal "sports, reading"
               (slot-display-value i (get-slot 'testdb1 'hobbies))))
    (is (equal "1983-09-28"
               (slot-display-value i (get-slot 'testdb1 'birth-date))))
    (is (equal "tomo"
               (slot-display-value i (get-slot 'testdb1 'nickname))))
    (is (equal "408-644-6198"
               (slot-display-value i (get-slot 'testdb1 'phone-number))))
    (is (equal "95129"
               (slot-display-value i (get-slot 'testdb1 'zip-code))))
    (is (equal (format nil "hello~%world")
               (slot-display-value i (get-slot 'testdb1 'note))))
    (is (safe= "hello<br>world"
               (slot-display-value i (get-slot 'testdb1 'note) :nl->br t)))
    (is (string=* "<a href=\"http://localhost:8080/upload/test.gif\">
<img src=\"http://localhost:8080/thumbnail/?file=test.gif&type=upload&width=&height=\"
 alt=\"testdb1_image\" /></a>"
                  (sml->ml (slot-display-value i (get-slot 'testdb1 'image)))))))

(test slot-save-value
  (with-post-parameters
      '(("testdb1_name" . "tomoyuki matsumoto")
        ("testdb1_password" . "password")
        ("testdb1_email" . "tomo@tomo.com")
        ("testdb1_sex" . "male")
        ("testdb1_marriage" . "single")
        ("testdb1_hobbies_sports" . "sports")
        ("testdb1_hobbies_reading" . "reading")
        ("testdb1_birth-date-Y" . "1983")
        ("testdb1_birth-date-M" . "9")
        ("testdb1_birth-date-D" . "28")
        ("testdb1_nickname" . "tomo")
        ("testdb1_phone-number" . "408-644-6198")
        ("testdb1_zip-code" . "95129")
        ("testdb1_note" . "hello
world")
        ("image" ("name" . "test.gif")
         ("type" . "image/gif")
         ("tmp-name" . "/tmp/web4r/tmp/579198166b")
         ("size" . 1841)))
    (is (equal "tomoyuki matsumoto"
               (slot-save-value (get-slot 'testdb1 'name))))
    (is (equal "password"
               (slot-save-value (get-slot 'testdb1 'password))))
    (is (equal "tomo@tomo.com"
               (slot-save-value (get-slot 'testdb1 'email))))
    (is (equal "male"
               (slot-save-value (get-slot 'testdb1 'sex))))
    (is (equal "single"
               (slot-save-value (get-slot 'testdb1 'marriage))))
    (is (equal '("sports" "reading")
               (slot-save-value (get-slot 'testdb1 'hobbies))))
    (is (equal "1983-9-28"
               (slot-save-value (get-slot 'testdb1 'birth-date))))
    (is (equal "tomo"
               (slot-save-value (get-slot 'testdb1 'nickname))))
    (is (equal "408-644-6198"
               (slot-save-value (get-slot 'testdb1 'phone-number))))
    (is (equal "95129"
               (slot-save-value (get-slot 'testdb1 'zip-code))))
    (is (equal "hello
world"         (slot-save-value (get-slot 'testdb1 'note))))))

(defmacro sml= (sml ml)
  `(string=* (sml->ml ,sml) ,ml))

(test form-input
  (with-post-parameters '()
    (is-true (sml= (form-input (get-slot 'testdb1 'name))
"<input type=\"text\" class=\"required\" name=\"testdb1_name\" id=\"testdb1_name\" size=\"30\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'password))
"<input type=\"password\" class=\"required\" name=\"testdb1_password\" id=\"testdb1_password\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'email))
"<input type=\"text\" class=\"required\" name=\"testdb1_email\" id=\"testdb1_email\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'sex))
"<input type=\"radio\" value=\"male\" id=\"testdb1_sex_male\" name=\"testdb1_sex\" />
<label for=\"testdb1_sex_male\">male</label>
<input type=\"radio\" value=\"female\" id=\"testdb1_sex_female\" name=\"testdb1_sex\" />
<label for=\"testdb1_sex_female\">female</label>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'marriage))
"<select name=\"testdb1_marriage\" id=\"testdb1_marriage\">
<option value=\"single\">single</option>
<option value=\"married\">married</option>
<option value=\"divorced\">divorced</option>
</select>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'hobbies))
"<input type=\"checkbox\" value=\"sports\" id=\"testdb1_hobbies_sports\" name=\"testdb1_hobbies_sports\" />
<label for=\"testdb1_hobbies_sports\">sports</label>
<input type=\"checkbox\" value=\"music\" id=\"testdb1_hobbies_music\" name=\"testdb1_hobbies_music\" />
<label for=\"testdb1_hobbies_music\">music</label>
<input type=\"checkbox\" value=\"reading\" id=\"testdb1_hobbies_reading\" name=\"testdb1_hobbies_reading\" />
<label for=\"testdb1_hobbies_reading\">reading</label>"))
    (is-true (equalp (sml->ml (select-date "testdb1_birth-date"))
                     (sml->ml (form-input (get-slot 'testdb1 'birth-date)))))
    (is-true (sml= (form-input (get-slot 'testdb1 'nickname))
"<input type=\"text\" class=\"\" name=\"testdb1_nickname\" id=\"testdb1_nickname\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'phone-number))
"<input type=\"text\" class=\"required\" name=\"testdb1_phone-number\" id=\"testdb1_phone-number\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'zip-code))
"<input type=\"text\" class=\"required\" name=\"testdb1_zip-code\" id=\"testdb1_zip-code\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'note))
"<textarea name=\"testdb1_note\" rows=\"5\" cols=\"30\" id=\"testdb1_note\"></textarea>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'image))
"<input type=\"file\" name=\"testdb1_image\" id=\"testdb1_image\" />")))
  (with-post-parameters
      '(("testdb1_name" . "tomoyuki matsumoto")
        ("testdb1_password" . "password")
        ("testdb1_email" . "tomo@tomo.com")
        ("testdb1_sex" . "male")
        ("testdb1_marriage" . "single")
        ("testdb1_hobbies-sports" . "sports")
        ("testdb1_hobbies-reading" . "reading")
        ("testdb1_birth-date-Y". "1983")
        ("testdb1_birth-date-M". "9")
        ("testdb1_birth-date-D". "28")
        ("testdb1_nickname" . "tomo")
        ("testdb1_phone-number" . "408-644-6198")
        ("testdb1_zip-code" . "95129")
        ("testdb1_note" . "hello
world")
        ("image" ("name" . "test.gif")
         ("type" . "image/gif")
         ("tmp-name" . "/tmp/web4r/tmp/579198166b")
         ("size" . 1841)))
    (is-true (sml= (form-input (get-slot 'testdb1 'name))
"<input type=\"text\" class=\"required\" name=\"testdb1_name\" value=\"tomoyuki matsumoto\" id=\"testdb1_name\" size=\"30\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'password))
"<input type=\"password\" class=\"required\" name=\"testdb1_password\" value=\"password\" id=\"testdb1_password\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'email))
"<input type=\"text\" class=\"required\" name=\"testdb1_email\" value=\"tomo@tomo.com\" id=\"testdb1_email\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'sex))
"<input type=\"radio\" checked=\"checked\" value=\"male\" id=\"testdb1_sex_male\" name=\"testdb1_sex\" />
<label for=\"testdb1_sex_male\">male</label>
<input type=\"radio\" value=\"female\" id=\"testdb1_sex_female\" name=\"testdb1_sex\" />
<label for=\"testdb1_sex_female\">female</label>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'marriage))
"<select name=\"testdb1_marriage\" id=\"testdb1_marriage\">
<option value=\"single\" selected=\"selected\">single</option>
<option value=\"married\">married</option>
<option value=\"divorced\">divorced</option>
</select>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'hobbies))
"<input type=\"checkbox\" value=\"sports\" id=\"testdb1_hobbies_sports\" name=\"testdb1_hobbies_sports\" />
<label for=\"testdb1_hobbies_sports\">sports</label>
<input type=\"checkbox\" value=\"music\" id=\"testdb1_hobbies_music\" name=\"testdb1_hobbies_music\" />
<label for=\"testdb1_hobbies_music\">music</label>
<input type=\"checkbox\" value=\"reading\" id=\"testdb1_hobbies_reading\" name=\"testdb1_hobbies_reading\" />
<label for=\"testdb1_hobbies_reading\">reading</label>"))
    (is-true (equalp (sml->ml (select-date "testdb1_birth-date" :y 1983 :m 9 :d 28))
                     (sml->ml (form-input (get-slot 'testdb1 'birth-date)))))
    (is-true (sml= (form-input (get-slot 'testdb1 'nickname))
"<input type=\"text\" class=\"\" name=\"testdb1_nickname\" value=\"tomo\" id=\"testdb1_nickname\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'phone-number))
"<input type=\"text\" class=\"required\" name=\"testdb1_phone-number\" value=\"408-644-6198\" id=\"testdb1_phone-number\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'zip-code))
"<input type=\"text\" class=\"required\" name=\"testdb1_zip-code\" value=\"95129\" id=\"testdb1_zip-code\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'note))
"<textarea name=\"testdb1_note\" rows=\"5\" cols=\"30\" id=\"testdb1_note\">hello
world</textarea>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'image))
"<input type=\"file\" name=\"testdb1_image\" id=\"testdb1_image\" />")
             ))
  (with-post-parameters '()
    (let ((i (make-instance 'testdb1
                            :name         "tomoyuki matsumoto"
                            :password     "password"
                            :email        "tomo@tomo.com"
                            :sex          "male"
                            :marriage     "single"
                            :hobbies      '("sports" "reading")
                            :birth-date   "1983-09-28"
                            :nickname     "tomo"
                            :phone-number "408-644-6198"
                            :zip-code     "95129"
                            :note         (format nil "hello~%world")
                            :image        "test.gif")))
      (is-true (sml= (form-input (get-slot 'testdb1 'name) i)
"<input type=\"text\" class=\"required\" name=\"testdb1_name\" value=\"tomoyuki matsumoto\" id=\"testdb1_name\" size=\"30\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'password) i)
"<input type=\"password\" class=\"required\" name=\"testdb1_password\" value=\"password\" id=\"testdb1_password\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'email) i)
"<input type=\"text\" class=\"required\" name=\"testdb1_email\" value=\"tomo@tomo.com\" id=\"testdb1_email\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'sex) i)
"<input type=\"radio\" checked=\"checked\" value=\"male\" id=\"testdb1_sex_male\" name=\"testdb1_sex\" />
<label for=\"testdb1_sex_male\">male</label>
<input type=\"radio\" value=\"female\" id=\"testdb1_sex_female\" name=\"testdb1_sex\" />
<label for=\"testdb1_sex_female\">female</label>"))
      (is-true (sml= (form-input (get-slot 'testdb1 'marriage) i)
"<select name=\"testdb1_marriage\" id=\"testdb1_marriage\">
<option value=\"single\" selected=\"selected\">single</option>
<option value=\"married\">married</option>
<option value=\"divorced\">divorced</option>
</select>"))
      (is-true (sml= (form-input (get-slot 'testdb1 'hobbies) i)
"<input type=\"checkbox\" checked=\"checked\" value=\"sports\" id=\"testdb1_hobbies_sports\" name=\"testdb1_hobbies_sports\" />
<label for=\"testdb1_hobbies_sports\">sports</label>
<input type=\"checkbox\" value=\"music\" id=\"testdb1_hobbies_music\" name=\"testdb1_hobbies_music\" />
<label for=\"testdb1_hobbies_music\">music</label>
<input type=\"checkbox\" checked=\"checked\" value=\"reading\" id=\"testdb1_hobbies_reading\" name=\"testdb1_hobbies_reading\" />
<label for=\"testdb1_hobbies_reading\">reading</label>"))
      (is-true (equalp (sml->ml (select-date "testdb1_birth-date" :y 1983 :m 9 :d 28))
                       (sml->ml (form-input (get-slot 'testdb1 'birth-date) i))))
      (is-true (sml= (form-input (get-slot 'testdb1 'nickname) i)
"<input type=\"text\" class=\"\" name=\"testdb1_nickname\" value=\"tomo\" id=\"testdb1_nickname\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'phone-number) i)
"<input type=\"text\" class=\"required\" name=\"testdb1_phone-number\" value=\"408-644-6198\" id=\"testdb1_phone-number\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'zip-code) i)
"<input type=\"text\" class=\"required\" name=\"testdb1_zip-code\" value=\"95129\" id=\"testdb1_zip-code\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'note) i)
"<textarea name=\"testdb1_note\" rows=\"5\" cols=\"30\" id=\"testdb1_note\">hello
world</textarea>")))))

(test form-label
  (is (string=* "<label for=\"testdb1_name\">full name</label>"
                (sml->ml (form-label (get-slot 'testdb1 'name)))))
  (is (string=* "<label for=\"testdb1_password\">Password</label>"
                (sml->ml (form-label (get-slot 'testdb1 'password)))))
  (is (string=* "<label for=\"testdb1_email\">Email</label>"
                (sml->ml (form-label (get-slot 'testdb1 'email)))))
  (is (string=* "<label for=\"testdb1_sex_male\">Sex</label>"
                (sml->ml (form-label (get-slot 'testdb1 'sex)))))
  (is (string=* "<label for=\"testdb1_marriage\">Marriage</label>"
                (sml->ml (form-label (get-slot 'testdb1 'marriage)))))
  (is (string=* "<label for=\"testdb1_hobbies_sports\">Hobbies</label>"
                (sml->ml (form-label (get-slot 'testdb1 'hobbies)))))
  (is (string=* "<label for=\"testdb1_birth-date-Y\">Birth Date</label>"
                (sml->ml (form-label (get-slot 'testdb1 'birth-date)))))
  (is (string=* "<label for=\"testdb1_nickname\">Nickname</label>"
                (sml->ml (form-label (get-slot 'testdb1 'nickname)))))
  (is (string=* "<label for=\"testdb1_phone-number\">Phone Number</label>"
                (sml->ml (form-label (get-slot 'testdb1 'phone-number)))))
  (is (string=* "<label for=\"testdb1_zip-code\">Zip Code</label>"
                (sml->ml (form-label (get-slot 'testdb1 'zip-code)))))
  (is (string=* "<label for=\"testdb1_note\">Note</label>"
                (sml->ml (form-label (get-slot 'testdb1 'note)))))
  (is (string=* "<label for=\"testdb1_image\">Image</label>"
                (sml->ml (form-label (get-slot 'testdb1 'image))))))

(defun list= (x y)
  (and (null (set-difference x y :test #'equal))
       (null (set-difference y x :test #'equal))))

(test class-validation-errorsq
  (web4r::drop-class-instances 'testdb1)
  (with-post-parameters
      '(("testdb1_name" . "tomoyuki matsumoto")
        ("testdb1_password" . "password")
        ("testdb1_email" . "tomo@tomo.com")
        ("testdb1_sex" . "male")
        ("testdb1_marriage" . "single")
        ("testdb1_hobbies_sports" . "sports")
        ("testdb1_hobbies_reading" . "reading")
        ("testdb1_birth-date-Y" . "1983")
        ("testdb1_birth-date-M" . "9")
        ("testdb1_birth-date-D" . "28")
        ("testdb1_nickname" . "tomo")
        ("testdb1_phone-number" . "408-644-6198")
        ("testdb1_zip-code" . "95129")
        ("testdb1_note" . "hello world"))
    (is-false (class-validation-errors 'testdb1)))
  ; required
  (with-post-parameters '(("birth-date-Y" . "")
                          ("birth-date-M" . "")
                          ("birth-date-D" . ""))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'name)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'email)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'sex)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'marriage)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'hobbies)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'birth-date)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'phone-number)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'zip-code)))
                (web4r::error-msg :empty  (web4r::slot-label (get-slot 'testdb1 'note)))))))
  ; length
  (let ((*with-slots* :all))
    (with-post-parameters
        '(("testdb1_name" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("testdb1_password" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("testdb1_email" . "tomo@tomo.com")
          ("testdb1_sex" . "male")
          ("testdb1_marriage" . "single")
          ("testdb1_hobbies_sports" . "sports")
          ("testdb1_hobbies_reading" . "reading")
          ("testdb1_birth-date-Y" . "1983")
          ("testdb1_birth-date-M" . "9")
          ("testdb1_birth-date-D" . "28")
          ("testdb1_nickname" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("testdb1_phone-number" . "408-644-6198")
          ("testdb1_zip-code" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("testdb1_note" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("image" . #p"/tmp/web4r/tmp/579198166b"))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'name)) 50)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'password)) 12)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'nickname)) 50)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'zip-code)) 5)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'note)) 300)))))
  ; type
  (with-post-parameters
      '(("testdb1_name" . "tomoyuki matsumoto")
        ("testdb1_password" . "password")
        ("testdb1_email" . "invalid")
        ("testdb1_sex" . "male")
        ("testdb1_marriage" . "single")
        ("testdb1_hobbies_sports" . "sports")
        ("testdb1_hobbies_reading" . "reading")
        ("testdb1_birth-date-Y" . "1983")
        ("testdb1_birth-date-M" . "19")
        ("testdb1_birth-date-D" . "28")
        ("testdb1_nickname" . "tomo")
        ("testdb1_phone-number" . "408-644-61980")
        ("testdb1_zip-code" . "inval")
        ("testdb1_note" . "hello world")
        ("testdb1_image" . "/tmp/web4r/tmp/579198166b"))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'email)))
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'birth-date)))
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'phone-number)))
                (web4r::error-msg :not-a-number (web4r::slot-label (get-slot 'testdb1 'zip-code)))))))
  ; unique
  (let* ((i (make-instance 'testdb1 :email "uniquetest@uniquetest.com")))
    (with-post-parameters
        '(("testdb1_name" . "tomoyuki matsumoto")
          ("testdb1_password" . "password")
          ("testdb1_email" . "uniquetest@uniquetest.com")
          ("testdb1_sex" . "male")
          ("testdb1_marriage" . "single")
          ("testdb1_hobbies_sports" . "sports")
          ("testdb1_hobbies_reading" . "reading")
          ("testdb1_birth-date-Y" . "1983")
          ("testdb1_birth-date-M" . "9")
          ("testdb1_birth-date-D" . "28")
          ("testdb1_nickname" . "tomo")
          ("testdb1_phone-number" . "408-644-6198")
          ("testdb1_zip-code" . "95129")
          ("testdb1_note" . "hello world")
          ("testdb1_image" . #p"/tmp/web4r/tmp/579198166b"))
      (is (list= (class-validation-errors 'testdb1)
                 (list (web4r::error-msg :not-a-unique (web4r::slot-label (get-slot 'testdb1 'email))))))
      (is-false (class-validation-errors 'testdb1 i))))))

(test file-slots
  (is (equal (web4r::file-slots 'testdb1)
             (list (get-slot 'testdb1 'image)))))

(test oid
  (loop for i in '(1 2 3)
        as oid = (make-instance 'testdb1 :name i)
        do (is (eq i (slot-value (get-instance-by-oid 'testdb1 (oid oid)) 'name)))))

(test per-page
  (web4r::drop-class-instances 'testdb1)
  (loop for n from 1 to 26
        as i = (make-instance 'testdb1 :name n :updated-at n)
        do (setf (slot-value i 'updated-at) n))
  (with-get-parameters '(("page" . "1"))
    (loop for i in (per-page (ele:get-instances-by-class 'testdb1) :sort #'<)
          for n from 1 to 10
          do (is (eq n (slot-value i 'name)))))
  (with-get-parameters '(("page" . "2"))
    (loop for i in (per-page (ele:get-instances-by-class 'testdb1) :sort #'<)
          for n from 11 to 20
          do (is (eq n (slot-value i 'name)))))
  (with-get-parameters '(("page" . "3"))
    (loop for i in (per-page (ele:get-instances-by-class 'testdb1) :sort #'<)
          for n from 21 to 26
          do (is (eq n (slot-value i 'name)))))
  (with-get-parameters '(("page" . "4"))
    (is-false (per-page (ele:get-instances-by-class 'testdb1)))))

(test make-pinstance
  (web4r::drop-class-instances 'testdb1)
  (with-post-parameters 
      '(("testdb1_name" . "tomoyuki matsumoto")
        ("testdb1_email" . "tomo@tomo.com")
        ("testdb1_sex" . "male")
        ("testdb1_marriage" . "single")
        ("testdb1_hobbies_sports" . "sports")
        ("testdb1_hobbies_reading" . "reading")
        ("testdb1_birth-date-Y" . "1983")
        ("testdb1_birth-date-M" . "9")
        ("testdb1_birth-date-D" . "28")
        ("testdb1_nickname" . "tomo")
        ("testdb1_phone-number" . "408-644-6198")
        ("testdb1_zip-code" . "95129")
        ("testdb1_note" . "hello
world"))
    (let ((i (progn
               (make-pinstance 'testdb1 '((password "password")))
               (car (ele:get-instances-by-class 'testdb1)))))
      (is (equal "tomoyuki matsumoto"
                 (slot-display-value i (get-slot 'testdb1 'name))))
      (is (equal "password"
                 (slot-display-value i (get-slot 'testdb1 'password))))
      (is (equal "tomo@tomo.com"
                 (slot-display-value i (get-slot 'testdb1 'email))))
      (is (equal "male"
                 (slot-display-value i (get-slot 'testdb1 'sex))))
      (is (equal "single"
                 (slot-display-value i (get-slot 'testdb1 'marriage))))
      (is (equal "sports, reading"
                 (slot-display-value i (get-slot 'testdb1 'hobbies))))
      (is (equal "1983-9-28"
                 (slot-display-value i (get-slot 'testdb1 'birth-date))))
      (is (equal "tomo"
                 (slot-display-value i (get-slot 'testdb1 'nickname))))
      (is (equal "408-644-6198"
                 (slot-display-value i (get-slot 'testdb1 'phone-number))))
      (is (equal "95129"
                 (slot-display-value i (get-slot 'testdb1 'zip-code))))
      (is (equal (format nil "hello~%world")
                 (slot-display-value i (get-slot 'testdb1 'note))))
      (is (safe= "hello<br>world"
                 (slot-display-value i (get-slot 'testdb1 'note) :nl->br t))))))

(test update-pinstance
  (web4r::drop-class-instances 'testdb1)
  (with-post-parameters
      '(("testdb1_name" . "tomoyuki matsumoto")
        ("testdb1_email" . "tomo@tomo.com")
        ("testdb1_sex" . "male")
        ("testdb1_marriage" . "single")
        ("testdb1_birth-date-Y" . "1983")
        ("testdb1_birth-date-M" . "9")
        ("testdb1_birth-date-D" . "28")
        ("testdb1_nickname" . "tomo")
        ("testdb1_phone-number" . "408-644-6198")
        ("testdb1_zip-code" . "95129")
        ("testdb1_note" . "hello
world"))
    (let ((i (progn
               (make-pinstance 'testdb1 '((password "password")))
               (car (ele:get-instances-by-class 'testdb1)))))
      (with-post-parameters
          '(("testdb1_name" . "Tomoyuki Matsumoto2")
            ("testdb1_email" . "tomo@tomo.com2")
            ("testdb1_sex" . "Male2")
            ("testdb1_marriage" . "single2")
            ("testdb1_birth-date-Y" . "1982")
            ("testdb1_birth-date-M" . "8")
            ("testdb1_birth-date-D" . "27")
            ("testdb1_nickname" . "tomo2")
            ("testdb1_phone-number" . "408-644-6197")
            ("testdb1_zip-code" . "95128")
            ("testdb1_note" . "Hello
World2"))
        (update-pinstance 'testdb1 i '((password "password2"))))
      (is (equal "Tomoyuki Matsumoto2"
                 (slot-display-value i (get-slot 'testdb1 'name))))
      (is (equal "password2"
                 (slot-display-value i (get-slot 'testdb1 'password))))
      (is (equal "tomo@tomo.com2"
                 (slot-display-value i (get-slot 'testdb1 'email))))
      (is (equal "Male2"
                 (slot-display-value i (get-slot 'testdb1 'sex))))
      (is (equal "single2"
                 (slot-display-value i (get-slot 'testdb1 'marriage))))
      (is (equal "1982-8-27"
                 (slot-display-value i (get-slot 'testdb1 'birth-date))))
      (is (equal "tomo2"
                 (slot-display-value i (get-slot 'testdb1 'nickname))))
      (is (equal "408-644-6197"
                 (slot-display-value i (get-slot 'testdb1 'phone-number))))
      (is (equal "95128"
                 (slot-display-value i (get-slot 'testdb1 'zip-code))))
      (is (equal (format nil "Hello~%World2")
                 (slot-display-value i (get-slot 'testdb1 'note))))
      (is (safe= "Hello<br>World2"
                 (slot-display-value i (get-slot 'testdb1 'note) :nl->br t))))))

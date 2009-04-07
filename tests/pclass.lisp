(in-package :web4r-tests)
(in-suite web4r)

(defun rem-newline (x)
  (remove #\Newline x))

(defun string=* (str1 str2)
  (string= (rem-newline str1) (rem-newline str2)))

(defpclass testdb1 ()
    ((name         :length 50 :label "Full Name" :size 30)
     (password     :input :password :length (8 12) :hide t :comment "8-12 characters")
     (email        :type :email :unique t)
     (sex          :input :radio :options ("Male" "Female"))
     (marriage     :input :select :options ("single" "married" "divorced"))
     (hobbies      :input :checkbox :options ("sports" "music" "reading"))
     (birth-date   :type :date)
     (nickname     :length 50 :nullable t)
     (phone-number :type (:regex "^\\d{3}-\\d{3}-\\d{4}$") :comment "xxx-xxx-xxxx")
     (zip-code     :type :integer :length 5 :comment "5 digit")
     (note         :length 300 :rows 5 :cols 30)
     (image        :input :file :type :image :length (1000 500000) :nullable t)))

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
  (is (equal "NAME"         (web4r::slot-id (get-slot 'testdb1 'name))))
  (is (equal "PASSWORD"     (web4r::slot-id (get-slot 'testdb1 'password))))
  (is (equal "EMAIL"        (web4r::slot-id (get-slot 'testdb1 'email))))
  (is (equal "SEX"          (web4r::slot-id (get-slot 'testdb1 'sex))))
  (is (equal "MARRIAGE"     (web4r::slot-id (get-slot 'testdb1 'marriage))))
  (is (equal "HOBBIES"      (web4r::slot-id (get-slot 'testdb1 'hobbies))))
  (is (equal "BIRTH-DATE"   (web4r::slot-id (get-slot 'testdb1 'birth-date))))
  (is (equal "NICKNAME"     (web4r::slot-id (get-slot 'testdb1 'nickname))))
  (is (equal "PHONE-NUMBER" (web4r::slot-id (get-slot 'testdb1 'phone-number))))
  (is (equal "ZIP-CODE"     (web4r::slot-id (get-slot 'testdb1 'zip-code))))
  (is (equal "NOTE"         (web4r::slot-id (get-slot 'testdb1 'note))))
  (is (equal "IMAGE"        (web4r::slot-id (get-slot 'testdb1 'image)))))

(test slot-label
  (is (equal "Full Name"    (web4r::slot-label (get-slot 'testdb1 'name))))
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

(test slot-nullable
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'name))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'password))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'email))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'sex))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'marriage))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'hobbies))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'birth-date))))
  (is (eq t   (web4r::slot-nullable (get-slot 'testdb1 'nickname))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'phone-number))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'zip-code))))
  (is (eq nil (web4r::slot-nullable (get-slot 'testdb1 'note))))
  (is (eq t   (web4r::slot-nullable (get-slot 'testdb1 'image)))))

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
  (is (equal '("Male" "Female")
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
  (is (equal '(:MEMBER ("Male" "Female"))
                   (web4r::slot-type (get-slot 'testdb1 'sex))))
  (is (equal '(:MEMBER ("single" "married" "divorced"))
                   (web4r::slot-type (get-slot 'testdb1 'marriage))))
  (is (equal '(:MEMBER ("sports" "music" "reading"))
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
             :name         "Tomoyuki Matsumoto"
             :password     "password"
             :email        "tomo@example.com"
             :sex          "Male"
             :marriage     "single"
             :hobbies      '("sports" "reading")
             :birth-date   "1983-09-28"
             :nickname     "tomo"
             :phone-number "408-644-6198"
             :zip-code     "95129"
             :note         (format nil "Hello~%World")
             :image        "test.gif")))
    (is (equal "Tomoyuki Matsumoto"
               (slot-display-value i (get-slot 'testdb1 'name))))
    (is (equal "password"
               (slot-display-value i (get-slot 'testdb1 'password))))
    (is (equal "tomo@example.com"
               (slot-display-value i (get-slot 'testdb1 'email))))
    (is (equal "Male"
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
    (is (equal (format nil "Hello~%World")
               (slot-display-value i (get-slot 'testdb1 'note))))
    (is (safe= "Hello<br>World"
               (slot-display-value i (get-slot 'testdb1 'note) :nl->br t)))
    (is (string=* "<a href=\"http://localhost:8080/image/?file=test.gif&amp;type=upload\">
<img src=\"http://localhost:8080/thumbnail/?file=test&type=upload&width=&height=\" alt=\"IMAGE\" /></a>"
                  (sml->ml (slot-display-value i (get-slot 'testdb1 'image)))))))

(test slot-save-value
  (with-post-parameters
      '(("NAME" . "Tomoyuki Matsumoto")
        ("PASSWORD" . "password")
        ("EMAIL" . "tomo@tomo.com")
        ("SEX" . "Male")
        ("MARRIAGE" . "single")
        ("HOBBIES-sports" . "sports")
        ("HOBBIES-reading" . "reading")
        ("BIRTH-DATE-Y" . "1983")
        ("BIRTH-DATE-M" . "9")
        ("BIRTH-DATE-D" . "28")
        ("NICKNAME" . "tomo")
        ("PHONE-NUMBER" . "408-644-6198")
        ("ZIP-CODE" . "95129")
        ("NOTE" . "Hello
World")
        ("IMAGE" ("name" . "test.gif")
         ("type" . "image/gif")
         ("tmp-name" . "/tmp/web4r/tmp/579198166B")
         ("size" . 1841)))
    (is (equal "Tomoyuki Matsumoto"
               (slot-save-value (get-slot 'testdb1 'name))))
    (is (equal "password"
               (slot-save-value (get-slot 'testdb1 'password))))
    (is (equal "tomo@tomo.com"
               (slot-save-value (get-slot 'testdb1 'email))))
    (is (equal "Male"
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
    (is (equal "Hello
World"         (slot-save-value (get-slot 'testdb1 'note))))))

(defmacro sml= (sml ml)
  `(string=* (sml->ml ,sml) ,ml))

(test form-input
  (with-post-parameters '()
    (is-true (sml= (form-input (get-slot 'testdb1 'name))
                     "<input type=\"text\" name=\"NAME\" id=\"NAME\" size=\"30\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'password))
                     "<input type=\"password\" name=\"PASSWORD\" id=\"PASSWORD\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'email))
                     "<input type=\"text\" name=\"EMAIL\" id=\"EMAIL\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'sex))
                     "<input type=\"radio\" value=\"Male\" id=\"Male\" name=\"SEX\" />
<label for=\"Male\">Male</label>
<input type=\"radio\" value=\"Female\" id=\"Female\" name=\"SEX\" />
<label for=\"Female\">Female</label>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'marriage))
                     "<select name=\"MARRIAGE\" id=\"MARRIAGE\">
<option value=\"single\">single</option>
<option value=\"married\">married</option>
<option value=\"divorced\">divorced</option>
</select>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'hobbies))
                     "<input type=\"checkbox\" value=\"sports\" id=\"HOBBIES-sports\" name=\"HOBBIES-sports\" />
<label for=\"sports\">sports</label>
<input type=\"checkbox\" value=\"music\" id=\"HOBBIES-music\" name=\"HOBBIES-music\" />
<label for=\"music\">music</label>
<input type=\"checkbox\" value=\"reading\" id=\"HOBBIES-reading\" name=\"HOBBIES-reading\" />
<label for=\"reading\">reading</label>"))
    (is-true (equalp (sml->ml (select-date "BIRTH-DATE"))
                     (sml->ml (form-input (get-slot 'testdb1 'birth-date)))))
    (is-true (sml= (form-input (get-slot 'testdb1 'nickname))
                     "<input type=\"text\" name=\"NICKNAME\" id=\"NICKNAME\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'phone-number))
                     "<input type=\"text\" name=\"PHONE-NUMBER\" id=\"PHONE-NUMBER\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'zip-code))
                     "<input type=\"text\" name=\"ZIP-CODE\" id=\"ZIP-CODE\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'note))
                     "<textarea name=\"NOTE\" rows=\"5\" cols=\"30\" id=\"NOTE\"></textarea>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'image))
                     "<input type=\"file\" name=\"IMAGE\" id=\"IMAGE\" />")))
  (with-post-parameters
      '(("NAME" . "Tomoyuki Matsumoto")
        ("PASSWORD" . "password")
        ("EMAIL" . "tomo@tomo.com")
        ("SEX" . "Male")
        ("MARRIAGE" . "single")
        ("HOBBIES-sports" . "sports")
        ("HOBBIES-reading" . "reading")
        ("BIRTH-DATE-Y" . "1983")
        ("BIRTH-DATE-M" . "9")
        ("BIRTH-DATE-D" . "28")
        ("NICKNAME" . "tomo")
        ("PHONE-NUMBER" . "408-644-6198")
        ("ZIP-CODE" . "95129")
        ("NOTE" . "Hello
World")
        ("IMAGE" ("name" . "test.gif")
         ("type" . "image/gif")
         ("tmp-name" . "/tmp/web4r/tmp/579198166B")
         ("size" . 1841)))
    (is-true (sml= (form-input (get-slot 'testdb1 'name))
                     "<input type=\"text\" name=\"NAME\" value=\"Tomoyuki Matsumoto\" id=\"NAME\" size=\"30\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'password))
                     "<input type=\"password\" name=\"PASSWORD\" value=\"password\" id=\"PASSWORD\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'email))
                     "<input type=\"text\" name=\"EMAIL\" value=\"tomo@tomo.com\" id=\"EMAIL\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'sex))
                     "<input type=\"radio\" checked=\"checked\" value=\"Male\" id=\"Male\" name=\"SEX\" />
<label for=\"Male\">Male</label>
<input type=\"radio\" value=\"Female\" id=\"Female\" name=\"SEX\" />
<label for=\"Female\">Female</label>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'marriage))
                     "<select name=\"MARRIAGE\" id=\"MARRIAGE\">
<option value=\"single\" selected=\"selected\">single</option>
<option value=\"married\">married</option>
<option value=\"divorced\">divorced</option>
</select>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'hobbies))
                     "<input type=\"checkbox\" checked=\"checked\" value=\"sports\" id=\"HOBBIES-sports\" name=\"HOBBIES-sports\" />
<label for=\"sports\">sports</label>
<input type=\"checkbox\" value=\"music\" id=\"HOBBIES-music\" name=\"HOBBIES-music\" />
<label for=\"music\">music</label>
<input type=\"checkbox\" checked=\"checked\" value=\"reading\" id=\"HOBBIES-reading\" name=\"HOBBIES-reading\" />
<label for=\"reading\">reading</label>"))
    (is-true (equalp (sml->ml (select-date "BIRTH-DATE" :y 1983 :m 9 :d 28))
                     (sml->ml (form-input (get-slot 'testdb1 'birth-date)))))
    (is-true (sml= (form-input (get-slot 'testdb1 'nickname))
                     "<input type=\"text\" name=\"NICKNAME\" value=\"tomo\" id=\"NICKNAME\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'phone-number))
                     "<input type=\"text\" name=\"PHONE-NUMBER\" value=\"408-644-6198\" id=\"PHONE-NUMBER\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'zip-code))
                     "<input type=\"text\" name=\"ZIP-CODE\" value=\"95129\" id=\"ZIP-CODE\" />"))
    (is-true (sml= (form-input (get-slot 'testdb1 'note))
                     "<textarea name=\"NOTE\" rows=\"5\" cols=\"30\" id=\"NOTE\">Hello
World</textarea>"))
    (is-true (sml= (form-input (get-slot 'testdb1 'image))
                     "<input type=\"file\" name=\"IMAGE\" id=\"IMAGE\" />")
             ))
  (with-post-parameters '()
    (let ((i (make-instance 'testdb1
                            :name         "Tomoyuki Matsumoto"
                            :password     "password"
                            :email        "tomo@tomo.com"
                            :sex          "Male"
                            :marriage     "single"
                            :hobbies      '("sports" "reading")
                            :birth-date   "1983-09-28"
                            :nickname     "tomo"
                            :phone-number "408-644-6198"
                            :zip-code     "95129"
                            :note         (format nil "Hello~%World")
                            :image        "test.gif")))
      (is-true (sml= (form-input (get-slot 'testdb1 'name) i)
                       "<input type=\"text\" name=\"NAME\" value=\"Tomoyuki Matsumoto\" id=\"NAME\" size=\"30\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'password) i)
                       "<input type=\"password\" name=\"PASSWORD\" value=\"password\" id=\"PASSWORD\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'email) i)
                       "<input type=\"text\" name=\"EMAIL\" value=\"tomo@tomo.com\" id=\"EMAIL\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'sex) i)
                       "<input type=\"radio\" checked=\"checked\" value=\"Male\" id=\"Male\" name=\"SEX\" />
<label for=\"Male\">Male</label>
<input type=\"radio\" value=\"Female\" id=\"Female\" name=\"SEX\" />
<label for=\"Female\">Female</label>"))
      (is-true (sml= (form-input (get-slot 'testdb1 'marriage) i)
                       "<select name=\"MARRIAGE\" id=\"MARRIAGE\">
<option value=\"single\" selected=\"selected\">single</option>
<option value=\"married\">married</option>
<option value=\"divorced\">divorced</option>
</select>"))
      (is-true (sml= (form-input (get-slot 'testdb1 'hobbies) i)
                       "<input type=\"checkbox\" checked=\"checked\" value=\"sports\" id=\"HOBBIES-sports\" name=\"HOBBIES-sports\" />
<label for=\"sports\">sports</label>
<input type=\"checkbox\" value=\"music\" id=\"HOBBIES-music\" name=\"HOBBIES-music\" />
<label for=\"music\">music</label>
<input type=\"checkbox\" checked=\"checked\" value=\"reading\" id=\"HOBBIES-reading\" name=\"HOBBIES-reading\" />
<label for=\"reading\">reading</label>"))
      (is-true (equalp (sml->ml (select-date "BIRTH-DATE" :y 1983 :m 9 :d 28))
                       (sml->ml (form-input (get-slot 'testdb1 'birth-date) i))))
      (is-true (sml= (form-input (get-slot 'testdb1 'nickname) i)
                       "<input type=\"text\" name=\"NICKNAME\" value=\"tomo\" id=\"NICKNAME\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'phone-number) i)
                       "<input type=\"text\" name=\"PHONE-NUMBER\" value=\"408-644-6198\" id=\"PHONE-NUMBER\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'zip-code) i)
                       "<input type=\"text\" name=\"ZIP-CODE\" value=\"95129\" id=\"ZIP-CODE\" />"))
      (is-true (sml= (form-input (get-slot 'testdb1 'note) i)
                       "<textarea name=\"NOTE\" rows=\"5\" cols=\"30\" id=\"NOTE\">Hello
World</textarea>")))))

(test form-label
  (is (string=* "<label for=\"NAME\">Full Name</label>"
                (sml->ml (form-label (get-slot 'testdb1 'name)))))
  (is (string=* "<label for=\"PASSWORD\">Password</label>"
                (sml->ml (form-label (get-slot 'testdb1 'password)))))
  (is (string=* "<label for=\"EMAIL\">Email</label>"
                (sml->ml (form-label (get-slot 'testdb1 'email)))))
  (is (string=* "<label for=\"SEX\">Sex</label>"
                (sml->ml (form-label (get-slot 'testdb1 'sex)))))
  (is (string=* "<label for=\"MARRIAGE\">Marriage</label>"
                (sml->ml (form-label (get-slot 'testdb1 'marriage)))))
  (is (string=* "<label for=\"HOBBIES\">Hobbies</label>"
                (sml->ml (form-label (get-slot 'testdb1 'hobbies)))))
  (is (string=* "<label for=\"BIRTH-DATE-Y\">Birth Date</label>"
                (sml->ml (form-label (get-slot 'testdb1 'birth-date)))))
  (is (string=* "<label for=\"NICKNAME\">Nickname</label>"
                (sml->ml (form-label (get-slot 'testdb1 'nickname)))))
  (is (string=* "<label for=\"PHONE-NUMBER\">Phone Number</label>"
                (sml->ml (form-label (get-slot 'testdb1 'phone-number)))))
  (is (string=* "<label for=\"ZIP-CODE\">Zip Code</label>"
                (sml->ml (form-label (get-slot 'testdb1 'zip-code)))))
  (is (string=* "<label for=\"NOTE\">Note</label>"
                (sml->ml (form-label (get-slot 'testdb1 'note)))))
  (is (string=* "<label for=\"IMAGE\">Image</label>"
                (sml->ml (form-label (get-slot 'testdb1 'image))))))

(test must-mark
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'name)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'password)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'email)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'sex)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'marriage)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'hobbies)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'birth-date)))))
  (is (eq nil
          (must-mark (get-slot 'testdb1 'nickname))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'phone-number)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'zip-code)))))
  (is (string=* "<font color=\"red\">*</font>"
                (sml->ml (must-mark (get-slot 'testdb1 'note)))))
  (is (eq nil
          (must-mark (get-slot 'testdb1 'image)))))


(defun list= (x y)
  (and (null (set-difference x y :test #'equal))
       (null (set-difference y x :test #'equal))))

(test class-validation-errors
  (web4r::drop-class-instances 'testdb1)
  (with-post-parameters
      '(("NAME" . "Tomoyuki Matsumoto")
        ("PASSWORD" . "password")
        ("EMAIL" . "tomo@tomo.com")
        ("SEX" . "Male")
        ("MARRIAGE" . "single")
        ("HOBBIES-sports" . "sports")
        ("HOBBIES-reading" . "reading")
        ("BIRTH-DATE-Y" . "1983")
        ("BIRTH-DATE-M" . "9")
        ("BIRTH-DATE-D" . "28")
        ("NICKNAME" . "tomo")
        ("PHONE-NUMBER" . "408-644-6198")
        ("ZIP-CODE" . "95129")
        ("NOTE" . "Hello World"))
    (is-false (class-validation-errors 'testdb1)))
  ; nullable
  (with-post-parameters '(("BIRTH-DATE-Y" . "")
                          ("BIRTH-DATE-M" . "")
                          ("BIRTH-DATE-D" . ""))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'name)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'email)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'sex)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'marriage)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'hobbies)))
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'birth-date)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'phone-number)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'zip-code)))
                (web4r::error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'note)))))))
  ; length
  (let ((*with-slots* :all))
    (with-post-parameters
        '(("NAME" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("PASSWORD" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("EMAIL" . "tomo@tomo.com")
          ("SEX" . "Male")
          ("MARRIAGE" . "single")
          ("HOBBIES-sports" . "sports")
          ("HOBBIES-reading" . "reading")
          ("BIRTH-DATE-Y" . "1983")
          ("BIRTH-DATE-M" . "9")
          ("BIRTH-DATE-D" . "28")
          ("NICKNAME" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("PHONE-NUMBER" . "408-644-6198")
          ("ZIP-CODE" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("NOTE" . "toooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooolong")
          ("IMAGE" . #P"/tmp/web4r/tmp/579198166B"))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'name)) 50)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'password)) 12)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'nickname)) 50)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'zip-code)) 5)
                (web4r::error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'note)) 300)))))
  ; type
  (with-post-parameters
      '(("NAME" . "Tomoyuki Matsumoto")
        ("PASSWORD" . "password")
        ("EMAIL" . "invalid")
        ("SEX" . "Male")
        ("MARRIAGE" . "single")
        ("HOBBIES-sports" . "sports")
        ("HOBBIES-reading" . "reading")
        ("BIRTH-DATE-Y" . "1983")
        ("BIRTH-DATE-M" . "19")
        ("BIRTH-DATE-D" . "28")
        ("NICKNAME" . "tomo")
        ("PHONE-NUMBER" . "408-644-61980")
        ("ZIP-CODE" . "inval")
        ("NOTE" . "Hello World")
        ("IMAGE" . "/tmp/web4r/tmp/579198166B"))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'email)))
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'birth-date)))
                (web4r::error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'phone-number)))
                (web4r::error-msg :not-a-number (web4r::slot-label (get-slot 'testdb1 'zip-code)))))))
  ; unique
  (let* ((i (make-instance 'testdb1 :email "uniquetest@uniquetest.com")))
    (with-post-parameters
        '(("NAME" . "Tomoyuki Matsumoto")
          ("PASSWORD" . "password")
          ("EMAIL" . "uniquetest@uniquetest.com")
          ("SEX" . "Male")
          ("MARRIAGE" . "single")
          ("HOBBIES-sports" . "sports")
          ("HOBBIES-reading" . "reading")
          ("BIRTH-DATE-Y" . "1983")
          ("BIRTH-DATE-M" . "9")
          ("BIRTH-DATE-D" . "28")
          ("NICKNAME" . "tomo")
          ("PHONE-NUMBER" . "408-644-6198")
          ("ZIP-CODE" . "95129")
          ("NOTE" . "Hello World")
          ("IMAGE" . #P"/tmp/web4r/tmp/579198166B"))
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
      '(("NAME" . "Tomoyuki Matsumoto")
        ("EMAIL" . "tomo@tomo.com")
        ("SEX" . "Male")
        ("MARRIAGE" . "single")
        ("HOBBIES-sports" . "sports")
        ("HOBBIES-reading" . "reading")
        ("BIRTH-DATE-Y" . "1983")
        ("BIRTH-DATE-M" . "9")
        ("BIRTH-DATE-D" . "28")
        ("NICKNAME" . "tomo")
        ("PHONE-NUMBER" . "408-644-6198")
        ("ZIP-CODE" . "95129")
        ("NOTE" . "Hello
World"))
    (let ((i (progn
               (make-pinstance 'testdb1 '((password "password")))
               (car (ele:get-instances-by-class 'testdb1)))))
      (is (equal "Tomoyuki Matsumoto"
                 (slot-display-value i (get-slot 'testdb1 'name))))
      (is (equal "password"
                 (slot-display-value i (get-slot 'testdb1 'password))))
      (is (equal "tomo@tomo.com"
                 (slot-display-value i (get-slot 'testdb1 'email))))
      (is (equal "Male"
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
      (is (equal (format nil "Hello~%World")
                 (slot-display-value i (get-slot 'testdb1 'note))))
      (is (safe= "Hello<br>World"
                 (slot-display-value i (get-slot 'testdb1 'note) :nl->br t))))))

(test update-pinstance
  (web4r::drop-class-instances 'testdb1)
  (with-post-parameters
      '(("NAME" . "Tomoyuki Matsumoto")
        ("EMAIL" . "tomo@tomo.com")
        ("SEX" . "Male")
        ("MARRIAGE" . "single")
        ("BIRTH-DATE-Y" . "1983")
        ("BIRTH-DATE-M" . "9")
        ("BIRTH-DATE-D" . "28")
        ("NICKNAME" . "tomo")
        ("PHONE-NUMBER" . "408-644-6198")
        ("ZIP-CODE" . "95129")
        ("NOTE" . "Hello
World"))
    (let ((i (progn
               (make-pinstance 'testdb1 '((password "password")))
               (car (ele:get-instances-by-class 'testdb1)))))
      (with-post-parameters
          '(("NAME" . "Tomoyuki Matsumoto2")
            ("EMAIL" . "tomo@tomo.com2")
            ("SEX" . "Male2")
            ("MARRIAGE" . "single2")
            ("BIRTH-DATE-Y" . "1982")
            ("BIRTH-DATE-M" . "8")
            ("BIRTH-DATE-D" . "27")
            ("NICKNAME" . "tomo2")
            ("PHONE-NUMBER" . "408-644-6197")
            ("ZIP-CODE" . "95128")
            ("NOTE" . "Hello
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

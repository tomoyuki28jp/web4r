(in-package :web4r-tests)
(in-suite web4r)

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
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'sex))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'marriage))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'hobbies))))
  (is (eq :date    (web4r::slot-type (get-slot 'testdb1 'birth-date))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'nickname))))
  (is (equal '(:regex "^\\d{3}-\\d{3}-\\d{4}$")
                      (web4r::slot-type (get-slot 'testdb1 'phone-number))))
  (is (eq :integer (web4r::slot-type (get-slot 'testdb1 'zip-code))))
  (is (eq nil      (web4r::slot-type (get-slot 'testdb1 'note))))
  (is (eq :image   (web4r::slot-type (get-slot 'testdb1 'image)))))

(defun safe= (html safe)
  (equal html
         (replace-str *nl* "" (slot-value safe 'web4r::content))))

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
             :note         (concat "Hello" *nl* "World")
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
    (is (equal (concat "Hello" *nl* "World")
               (slot-display-value i (get-slot 'testdb1 'note))))
    (is (safe= "Hello<br>World"
               (slot-display-value i (get-slot 'testdb1 'note) :nl->br t)))
    (let ((*upload-save-dir* "/tmp/"))
      (is (string=* "<A HREF=\"http:///noimage\"><IMG SRC=\"http:///thumbnail/?file=/tmp/test.gif&width=100&height=100\" ALT=\"IMAGE\"></A>"
                    (shtml->html (slot-display-value i (get-slot 'testdb1 'image))))))))

(test slot-save-value
  (let ((*request*
         (web4r::make-request
          :post-params
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
                     ("size" . 1841))))))
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

(test form-input
  (is-true (shtml= (form-input (get-slot 'testdb1 'name))
                   "<INPUT TYPE=\"text\" NAME=\"NAME\" ID=\"NAME\" SIZE=\"30\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'password))
                   "<INPUT TYPE=\"password\" NAME=\"PASSWORD\" ID=\"PASSWORD\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'email))
                   "<INPUT TYPE=\"text\" NAME=\"EMAIL\" ID=\"EMAIL\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'sex))
                   "<INPUT TYPE=\"radio\" VALUE=\"Male\" ID=\"Male\" NAME=\"SEX\">
<LABEL FOR=\"Male\">Male</LABEL>
<INPUT TYPE=\"radio\" VALUE=\"Female\" ID=\"Female\" NAME=\"SEX\">
<LABEL FOR=\"Female\">Female</LABEL>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'marriage))
                   "<SELECT NAME=\"MARRIAGE\" ID=\"MARRIAGE\">
<OPTION VALUE=\"single\">single</OPTION>
<OPTION VALUE=\"married\">married</OPTION>
<OPTION VALUE=\"divorced\">divorced</OPTION>
</SELECT>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'hobbies))
                   "<INPUT TYPE=\"checkbox\" VALUE=\"sports\" ID=\"HOBBIES-sports\" NAME=\"HOBBIES-sports\">
<LABEL FOR=\"sports\">sports</LABEL>
<INPUT TYPE=\"checkbox\" VALUE=\"music\" ID=\"HOBBIES-music\" NAME=\"HOBBIES-music\">
<LABEL FOR=\"music\">music</LABEL>
<INPUT TYPE=\"checkbox\" VALUE=\"reading\" ID=\"HOBBIES-reading\" NAME=\"HOBBIES-reading\">
<LABEL FOR=\"reading\">reading</LABEL>"))
  (is-true (equalp (shtml->html (select-date/ "BIRTH-DATE"))
                   (shtml->html (form-input (get-slot 'testdb1 'birth-date)))))
  (is-true (shtml= (form-input (get-slot 'testdb1 'nickname))
                   "<INPUT TYPE=\"text\" NAME=\"NICKNAME\" ID=\"NICKNAME\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'phone-number))
                   "<INPUT TYPE=\"text\" NAME=\"PHONE-NUMBER\" ID=\"PHONE-NUMBER\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'zip-code))
                   "<INPUT TYPE=\"text\" NAME=\"ZIP-CODE\" ID=\"ZIP-CODE\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'note))
                   "<TEXTAREA NAME=\"NOTE\" ROWS=\"5\" COLS=\"30\" ID=\"NOTE\"></TEXTAREA>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'image))
                   "<INPUT TYPE=\"FILE\" NAME=\"IMAGE\" ID=\"IMAGE\">"))

  (let ((*request*
         (web4r::make-request
          :post-params
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
                     ("size" . 1841))))))
  (is-true (shtml= (form-input (get-slot 'testdb1 'name))
                   "<INPUT TYPE=\"text\" NAME=\"NAME\" VALUE=\"Tomoyuki Matsumoto\" ID=\"NAME\" SIZE=\"30\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'password))
                   "<INPUT TYPE=\"password\" NAME=\"PASSWORD\" VALUE=\"password\" ID=\"PASSWORD\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'email))
                   "<INPUT TYPE=\"text\" NAME=\"EMAIL\" VALUE=\"tomo@tomo.com\" ID=\"EMAIL\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'sex))
                   "<INPUT TYPE=\"radio\" CHECKED=\"checked\" VALUE=\"Male\" ID=\"Male\" NAME=\"SEX\">
<LABEL FOR=\"Male\">Male</LABEL>
<INPUT TYPE=\"radio\" VALUE=\"Female\" ID=\"Female\" NAME=\"SEX\">
<LABEL FOR=\"Female\">Female</LABEL>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'marriage))
                   "<SELECT NAME=\"MARRIAGE\" ID=\"MARRIAGE\">
<OPTION VALUE=\"single\" SELECTED=\"selected\">single</OPTION>
<OPTION VALUE=\"married\">married</OPTION>
<OPTION VALUE=\"divorced\">divorced</OPTION>
</SELECT>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'hobbies))
                   "<INPUT TYPE=\"checkbox\" CHECKED=\"checked\" VALUE=\"sports\" ID=\"HOBBIES-sports\" NAME=\"HOBBIES-sports\">
<LABEL FOR=\"sports\">sports</LABEL>
<INPUT TYPE=\"checkbox\" VALUE=\"music\" ID=\"HOBBIES-music\" NAME=\"HOBBIES-music\">
<LABEL FOR=\"music\">music</LABEL>
<INPUT TYPE=\"checkbox\" CHECKED=\"checked\" VALUE=\"reading\" ID=\"HOBBIES-reading\" NAME=\"HOBBIES-reading\">
<LABEL FOR=\"reading\">reading</LABEL>"))
  (is-true (equalp (shtml->html (select-date/ "BIRTH-DATE" :y 1983 :m 9 :d 28))
                   (shtml->html (form-input (get-slot 'testdb1 'birth-date)))))
  (is-true (shtml= (form-input (get-slot 'testdb1 'nickname))
                   "<INPUT TYPE=\"text\" NAME=\"NICKNAME\" VALUE=\"tomo\" ID=\"NICKNAME\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'phone-number))
                   "<INPUT TYPE=\"text\" NAME=\"PHONE-NUMBER\" VALUE=\"408-644-6198\" ID=\"PHONE-NUMBER\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'zip-code))
                   "<INPUT TYPE=\"text\" NAME=\"ZIP-CODE\" VALUE=\"95129\" ID=\"ZIP-CODE\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'note))
                   "<TEXTAREA NAME=\"NOTE\" ROWS=\"5\" COLS=\"30\" ID=\"NOTE\">Hello
World</TEXTAREA>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'image))
                   "<INPUT TYPE=\"FILE\" NAME=\"IMAGE\" ID=\"IMAGE\">")
                   ))
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
             :note         (concat "Hello" *nl* "World")
             :image        "test.gif")))
  (is-true (shtml= (form-input (get-slot 'testdb1 'name) i)
                   "<INPUT TYPE=\"text\" NAME=\"NAME\" VALUE=\"Tomoyuki Matsumoto\" ID=\"NAME\" SIZE=\"30\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'password) i)
                   "<INPUT TYPE=\"password\" NAME=\"PASSWORD\" VALUE=\"password\" ID=\"PASSWORD\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'email) i)
                   "<INPUT TYPE=\"text\" NAME=\"EMAIL\" VALUE=\"tomo@tomo.com\" ID=\"EMAIL\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'sex) i)
                   "<INPUT TYPE=\"radio\" CHECKED=\"checked\" VALUE=\"Male\" ID=\"Male\" NAME=\"SEX\">
<LABEL FOR=\"Male\">Male</LABEL>
<INPUT TYPE=\"radio\" VALUE=\"Female\" ID=\"Female\" NAME=\"SEX\">
<LABEL FOR=\"Female\">Female</LABEL>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'marriage) i)
                   "<SELECT NAME=\"MARRIAGE\" ID=\"MARRIAGE\">
<OPTION VALUE=\"single\" SELECTED=\"selected\">single</OPTION>
<OPTION VALUE=\"married\">married</OPTION>
<OPTION VALUE=\"divorced\">divorced</OPTION>
</SELECT>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'hobbies) i)
                   "<INPUT TYPE=\"checkbox\" CHECKED=\"checked\" VALUE=\"sports\" ID=\"HOBBIES-sports\" NAME=\"HOBBIES-sports\">
<LABEL FOR=\"sports\">sports</LABEL>
<INPUT TYPE=\"checkbox\" VALUE=\"music\" ID=\"HOBBIES-music\" NAME=\"HOBBIES-music\">
<LABEL FOR=\"music\">music</LABEL>
<INPUT TYPE=\"checkbox\" CHECKED=\"checked\" VALUE=\"reading\" ID=\"HOBBIES-reading\" NAME=\"HOBBIES-reading\">
<LABEL FOR=\"reading\">reading</LABEL>"))
  (is-true (equalp (shtml->html (select-date/ "BIRTH-DATE" :y 1983 :m 9 :d 28))
                   (shtml->html (form-input (get-slot 'testdb1 'birth-date) i))))
  (is-true (shtml= (form-input (get-slot 'testdb1 'nickname) i)
                   "<INPUT TYPE=\"text\" NAME=\"NICKNAME\" VALUE=\"tomo\" ID=\"NICKNAME\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'phone-number) i)
                   "<INPUT TYPE=\"text\" NAME=\"PHONE-NUMBER\" VALUE=\"408-644-6198\" ID=\"PHONE-NUMBER\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'zip-code) i)
                   "<INPUT TYPE=\"text\" NAME=\"ZIP-CODE\" VALUE=\"95129\" ID=\"ZIP-CODE\">"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'note) i)
                   "<TEXTAREA NAME=\"NOTE\" ROWS=\"5\" COLS=\"30\" ID=\"NOTE\">Hello
World</TEXTAREA>"))
  (is-true (shtml= (form-input (get-slot 'testdb1 'image) i)
                   "<INPUT TYPE=\"hidden\" NAME=\"IMAGE\" VALUE=\"test.gif\"><P>
delete: <INPUT TYPE=\"checkbox\" VALUE=\"delete\" NAME=\"IMAGE-delete\" ID=\"IMAGE\"></P>
<P>change: <INPUT TYPE=\"FILE\" NAME=\"IMAGE-change\" ID=\"IMAGE\"></P>
<IMG SRC=\"http:///thumbnail/?file=/tmp/web4r/public/upload/test.gif&amp;width=100&amp;height=100\" ALT=\"IMAGE\">"))))

(test form-label
  (is (string=* "<LABEL FOR=\"NAME\">Full Name</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'name)))))
  (is (string=* "<LABEL FOR=\"PASSWORD\">Password</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'password)))))
  (is (string=* "<LABEL FOR=\"EMAIL\">Email</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'email)))))
  (is (string=* "<LABEL FOR=\"SEX\">Sex</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'sex)))))
  (is (string=* "<LABEL FOR=\"MARRIAGE\">Marriage</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'marriage)))))
  (is (string=* "<LABEL FOR=\"HOBBIES\">Hobbies</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'hobbies)))))
  (is (string=* "<LABEL FOR=\"BIRTH-DATE-Y\">Birth Date</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'birth-date)))))
  (is (string=* "<LABEL FOR=\"NICKNAME\">Nickname</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'nickname)))))
  (is (string=* "<LABEL FOR=\"PHONE-NUMBER\">Phone Number</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'phone-number)))))
  (is (string=* "<LABEL FOR=\"ZIP-CODE\">Zip Code</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'zip-code)))))
  (is (string=* "<LABEL FOR=\"NOTE\">Note</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'note)))))
  (is (string=* "<LABEL FOR=\"IMAGE\">Image</LABEL>"
                (shtml->html (form-label (get-slot 'testdb1 'image))))))

(test must?
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'name)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'password)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'email)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'sex)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'marriage)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'hobbies)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'birth-date)))))
  (is (eq nil
          (must? (get-slot 'testdb1 'nickname))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'phone-number)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'zip-code)))))
  (is (string=* "<FONT COLOR=\"red\">*</FONT>"
                (shtml->html (must? (get-slot 'testdb1 'note)))))
  (is (eq nil
          (must? (get-slot 'testdb1 'image)))))


(defun list= (x y)
  (and (null (set-difference x y :test #'equal))
       (null (set-difference y x :test #'equal))))

(test class-validation-errors
  (web4r::drop-class-instances 'testdb1)
  (let ((*request*
         (web4r::make-request
          :post-params
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
            ("NOTE" . "Hello World")
            ("IMAGE" ("name" . "test.gif")
                     ("type" . "image/gif")
                     ("tmp-name" . "/tmp/web4r/tmp/579198166B")
                     ("size" . 1841))))))
    (is-false (class-validation-errors 'testdb1)))
  ; nullable
  (is (list= (class-validation-errors 'testdb1)
             (list
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'name)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'email)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'sex)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'marriage)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'hobbies)))
              (web4r::get-error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'birth-date)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'phone-number)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'zip-code)))
              (web4r::get-error-msg :empty   (web4r::slot-label (get-slot 'testdb1 'note))))))
  ; length
  (let ((*with-slots* :all)
        (*request*
         (web4r::make-request
          :post-params
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
            ("IMAGE" ("name" . "test.gif")
                     ("type" . "image/gif")
                     ("tmp-name" . "/tmp/web4r/tmp/579198166B")
                     ("size" . 1234))))))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::get-error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'name)) 50)
                (web4r::get-error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'password)) 12)
                (web4r::get-error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'nickname)) 50)
                (web4r::get-error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'zip-code)) 5)
                (web4r::get-error-msg :too-long (web4r::slot-label (get-slot 'testdb1 'note)) 300)))))
  ; type
  (let ((*request*
         (web4r::make-request
          :post-params
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
            ("IMAGE" ("name" . "test.txt")
                     ("type" . nil)
                     ("tmp-name" . "/tmp/web4r/tmp/579198166B")
                     ("size" . 1841))))))
    (is (list= (class-validation-errors 'testdb1)
               (list
                (web4r::get-error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'email)))
                (web4r::get-error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'birth-date)))
                (web4r::get-error-msg :invalid (web4r::slot-label (get-slot 'testdb1 'phone-number)))
                (web4r::get-error-msg :not-a-number (web4r::slot-label (get-slot 'testdb1 'zip-code)))))))
  ; unique
  (let* ((i (make-instance 'testdb1 :email "uniquetest@uniquetest.com"))
         (*request*
          (web4r::make-request
           :post-params
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
             ("IMAGE" ("name" . "test.gif")
              ("type" . "image/gif")
              ("tmp-name" . "/tmp/web4r/tmp/579198166B")
              ("size" . 1841))))))
    (is (list= (class-validation-errors 'testdb1)
               (list (web4r::get-error-msg :not-unique (web4r::slot-label (get-slot 'testdb1 'email))))))
    (is-false (class-validation-errors 'testdb1 i))))

(test file-slots
  (is (equal (web4r::file-slots 'testdb1)
             (list (get-slot 'testdb1 'image)))))

(test multipart-form-p
  (is-true (web4r::multipart-form-p 'testdb1))
  (let ((*without-slots* '(image)))
    (is-false (web4r::multipart-form-p 'testdb1))))

(test oid
  (loop for i in '(1 2 3)
        as oid = (make-instance 'testdb1 :name i)
        do (is (eq i (slot-value (get-instance-by-oid 'testdb1 (oid oid)) 'name)))))

(test per-page
  (web4r::drop-class-instances 'testdb1)
  (loop for n from 1 to 26
        as i = (make-instance 'testdb1 :name n :updated-at n)
        do (setf (slot-value i 'updated-at) n))
  (let ((*request* (web4r::make-request :get-params '(("page" . "1")))))
    (loop for i in (per-page (ele:get-instances-by-class 'testdb1) :sort #'<)
          for n from 1 to 10
          do (is (eq n (slot-value i 'name)))))
  (let ((*request* (web4r::make-request :get-params '(("page" . "2")))))
    (loop for i in (per-page (ele:get-instances-by-class 'testdb1) :sort #'<)
          for n from 11 to 20
          do (is (eq n (slot-value i 'name)))))
  (let ((*request* (web4r::make-request :get-params '(("page" . "3")))))
    (loop for i in (per-page (ele:get-instances-by-class 'testdb1) :sort #'<)
          for n from 21 to 26
          do (is (eq n (slot-value i 'name)))))
  (let ((*request* (web4r::make-request :get-params '(("page" . "4")))))
    (is-false (per-page (ele:get-instances-by-class 'testdb1)))))

(test make-pinstance
  (web4r::drop-class-instances 'testdb1)
  (let* ((*request*
          (web4r::make-request
           :post-params '(("NAME" . "Tomoyuki Matsumoto")
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
World"))))
         (i (progn
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
    (is (equal (concat "Hello" *nl* "World")
               (slot-display-value i (get-slot 'testdb1 'note))))
    (is (safe= "Hello<br>World"
               (slot-display-value i (get-slot 'testdb1 'note) :nl->br t)))))

(test update-pinstance
  (web4r::drop-class-instances 'testdb1)
  (let* ((*request*
          (web4r::make-request
           :post-params '(("NAME" . "Tomoyuki Matsumoto")
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
World"))))
         (i (progn
              (make-pinstance 'testdb1 '((password "password")))
              (car (ele:get-instances-by-class 'testdb1)))))
    (let* ((*request*
            (web4r::make-request
             :post-params '(("NAME" . "Tomoyuki Matsumoto2")
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
World2")))))
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
    (is (equal (concat "Hello" *nl* "World2")
               (slot-display-value i (get-slot 'testdb1 'note))))
    (is (safe= "Hello<br>World2"
               (slot-display-value i (get-slot 'testdb1 'note) :nl->br t)))))

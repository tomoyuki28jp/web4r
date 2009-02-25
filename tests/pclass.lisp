(in-package :web4r-tests)
(in-suite web4r)

(defpclass testdb1 ()
  ((name         :length 50 :label "Full Name" :size 30)
   (password     :input :password :length (8 12) :hide t)
   (email        :type :email :unique t)
   (sex          :input :radio :options ("Male" "Female"))
   (marriage     :input :select :options ("single" "married" "divorced"))
   (hobbies      :input :checkbox :options ("sports" "music" "reading"))
   (birth-date   :type :date)
   (nickname     :length 50 :nullable t)
   (phone-number :type (:regex "^\\d{3}-\\d{3}-\\d{4}$"))
   (zip-code     :type :integer :length 5)
   (note         :length 3000 :rows 5 :cols 30)
   (image        :input :file :type :image :length (1000 500000) :nullable t)))

(test get-slots
  (let ((sl '(name password email sex marriage hobbies birth-date
              nickname phone-number zip-code note image)))
    (is (eq (length sl) (length (get-slots 'testdb1))))
    (loop for s in (get-excluded-slots 'testdb1)
          do (is (member (web4r::slot-symbol s) sl)))))

(test get-excluded-slots
  (let ((sl '(name email sex marriage hobbies birth-date
              nickname phone-number zip-code note image)))
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
  (is (eq 3000              (web4r::slot-length (get-slot 'testdb1 'note))))
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
               (slot-display-value i (get-slot 'testdb1 'note) :nl->br t)))))

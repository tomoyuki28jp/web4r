Tutorial Part 6. Extended slot options and genpages macro
==========================================================
web4r provides extended slot options for a persistent class and [genpages macro](http://web4r.org/en/api#genpages) which generates pages to list, edit, show and delete instances of a persistent class.

Extended slot options
----------------------
- **label:**    A label of a slot.
- **unique:**   A value of a slot must be unique if this is non nil.
- **required:** A value of a slot is required if this is non nil.
- **rows:**     A row size of a textarea input field.
- **cols:**     A column size of a textarea input field.
- **size:**     A size of a text input field.
- **length:**   If this is non nil, validates a length of a value. An integer for a max length or a list of two elements for '(min max) length.
- **hide-for:** This specifies where to hide a slot for. :all for all or a string regexp to hide it only on pages where the request uri matches to the regexp.
- **options:**  Options for a select, radio or checkbox input forms.
- **comment:**  A comment of a slot.
- **input:**    A type of a input form which must be :text, :textarea, :radio, :checkbox, :select, :password or :file.
- **format:**   A validation type which must be :alpha, :alnum, :integer, :email :date, :image ,regexp in string, a function or nil.

Genpages macro
---------------
Examples:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)
    
    (in-package :cl-user)
    (defpackage :customer (:use :cl :web4r))
    (in-package :customer)
    
    (ele:open-store *example-bdb*)
    
    (defpclass customer ()
      ((name         :length 50 :label "Full Name" :size 30 :index t)
       (password     :input :password :length (8 12) :comment "8-12 characters"
                     :hide-for "^(?!/customer/edit/)")
       (email        :format :email :unique t)
       (sex          :input :radio :options ("Male" "Female"))
       (marriage     :input :select :options ("single" "married" "divorced"))
       (hobbies      :input :checkbox :options ("sports" "music" "reading"))
       (birth-date   :format :date :index t)
       (nickname     :length 50 :required nil)
       (phone-number :format "^\\d{3}-\\d{3}-\\d{4}$" :comment "xxx-xxx-xxxx" :index t)
       (zip-code     :type integer :length (5 5) :comment "5 digit" :index t)
       (note         :length 3000 :rows 5 :cols 30)
       (image        :input :file :format :image :length (1000 500000) :required nil)))
    (genpages customer)
    
    (start-server)

### index page
- Sorting a list with/without ajax
- Pagination with/without ajax 
- Deleting an instance with/without ajax

[![customer-index](http://web4r.org/customer-index-thumbnail.png)](http://web4r.org/customer-index.png)

### show page
- Showing slot values of an instance

[![customer-show](http://web4r.org/customer-show-thumbnail.png)](http://web4r.org/customer-show.png)

### edit page
- Creating an instance or editing an existing instance
- Validations
    - Client side validation with Javascript (uniqueness validation via ajax)
    - Server side validation

[![customer-new](http://web4r.org/customer-new-thumbnail.png)](http://web4r.org/customer-new.png)
[![customer-edit](http://web4r.org/customer-edit-thumbnail.png)](http://web4r.org/customer-edit.png)

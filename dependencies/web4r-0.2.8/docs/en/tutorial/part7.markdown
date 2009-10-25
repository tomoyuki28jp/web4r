Tutorial Part 7. Authentication
================================
A persistent class named user is defined in web4r/src/user.lisp.

    (defpclass user ()
      ((id   :format :alnum :length (4 12) :unique t)
       (pass :format :alnum :length (4 12) :input :password)))

When a developer extends the user persistent class, login, logout and regist pages are automatically generated.

    (require :web4r)
    (use-package :web4r)

    (ele:open-store '(:BDB "/path/to/db"))

    (defpclass blog-user (user)
      ((email :format :email :unique t)
       (blog-title :length 256)))

[![login-page](http://web4r.org/login-thumbnail.png)](http://web4r.org/login.png)
[![regist-page](http://web4r.org/regist-thumbnail.png)](http://web4r.org/regist.png)

When a user access to a page defined with the :auth argument and the user hasn't logged in yet, the user will be redirected to the login page.

    (defpage test (:auth)
      [p "For login users only"])

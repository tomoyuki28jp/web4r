Tutorial Part 7. Authentication
================================
The persistent class named user is defined in web4r/src/user.lisp.

    (defpclass user ()
      ((id   :format :alnum :length (4 12) :unique t)
       (pass :format :alnum :length (4 12) :input :password)))

When a user extends the user persistent class, login, logout and regist pages are automatically generated.

    (require :web4r)
    (use-package :web4r)

    (ele:open-store '(:BDB "/path/to/db"))

    (defpclass blog-user (user)
      ((email :format :email :unique t)
       (blog-title :length 256)))

[![login-page](http://web4r.org/login-thumbnail.png)](http://web4r.org/login.png)
[![regist-page](http://web4r.org/regist-thumbnail.png)](http://web4r.org/regist.png)

If a user supplies :auth as an argument of [defpage](http://web4r.org/en/api#defpage) and the user hasn't logged in yet, redirects the user to the login page.

    (defpage test (:auth)
      [p "For login users only"])

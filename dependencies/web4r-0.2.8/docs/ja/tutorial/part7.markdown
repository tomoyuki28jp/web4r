Tutorial Part 7. 認証管理
==========================
userという名前の永続化クラスがweb4r/src/user.lispで定義されています。

    (defpclass user ()
      ((id   :format :alnum :length (4 12) :unique t)
       (pass :format :alnum :length (4 12) :input :password)))

開発者がuserクラスを継承した際、ログイン、ログアウト、新規ユーザー登録のページが自動生成されます。

    (require :web4r)
    (use-package :web4r)

    (ele:open-store '(:BDB "/path/to/db"))

    (defpclass blog-user (user)
      ((email :format :email :unique t)
       (blog-title :length 256)))

[![login-page](http://web4r.org/login-thumbnail.png)](http://web4r.org/login.png)
[![regist-page](http://web4r.org/regist-thumbnail.png)](http://web4r.org/regist.png)

ログインしていないユーザーが:auth引数と共に定義されたページにアクセスした場合、そのユーザーはログインページにリダイレクトされます。

    (defpage test (:auth)
      [p "会員専用ページ"])

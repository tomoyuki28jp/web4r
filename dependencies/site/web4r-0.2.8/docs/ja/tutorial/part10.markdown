Tutorial Part 10. HTTP経由でアプリケーションをテスト
=====================================================

HTTP経由でアプリケーションをテストをする為の関数とマクロ
---------------------------------------------------------
- [with-new-cookie](http://web4r.org/en/api#with-new-cookie): 引数のフォームを新しいクッキーと共に実行
- [http-test-regist](http://web4r.org/en/api#http-test-regist): HTTP経由でユーザー登録をして、結果をテスト
- [http-test-login](http://web4r.org/en/api#http-test-login): HTTP経由でユーザーログインをして、結果をテスト
- [http-test-logout](http://web4r.org/en/api#http-test-logout): HTTP経由でユーザーログアウトをして、結果をテスト
- [http-test-make-instance](http://web4r.org/en/api#http-test-make-instance): HTTP経由でインスタンス作成をして、結果をテスト
- [http-test-update-instance](http://web4r.org/en/api#http-test-update-instance): HTTP経由でインスタンス更新をして、結果をテスト
- [http-test-get-instance-by-oid](http://web4r.org/en/api#http-test-get-instance-by-oid): HTTP経由でoidからインスタンスのスロットの値を取得して、結果をテスト
- [http-test-drop-instance-by-oid](http://web4r.org/en/api#http-test-drop-instance-by-oid): HTTP経由でoidからインスタンスを削除して、結果をテスト

*※ http-test-get-instance-by-oidを利用するには、id属性にslot idを指定したタグでslotの値を囲む必要があります。slot idは[slot-id](http://web4r.org/en/api#slot-id)にて取得可能です。*

genpagesマクロで作成したアプリケーションのテスト
---------------------------------------------------
### アプリケーションコード

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    
    (in-package :cl-user)
    (defpackage :wiki (:use :cl :web4r))
    (in-package :wiki)
    
    (ele:open-store *example-bdb*)
    
    (defpclass wiki ()
      ((title :length 256 :index t)
       (body  :length 3000)))
    (genpages wiki)
    
    (start-server)

### テストコード

    (let ((class 'wiki))
      (with-new-cookie
        (let ((oid (http-test-make-instance class :title "title1" :body "body1")))
          (http-test-get-instance-by-oid class oid)
          (http-test-update-instance class oid '((title . "title1c") (body . "body1c")))
          (http-test-drop-instance-by-oid class oid))))

[Tutorial Part 9](http://web4r.org/ja/tutorial9)で作成した認証付きblogアプリケーションのテスト
-----------------------------------------------------------------------------------
### テストコード

    (let ((class 'blog)
          (id    "user1")
          (pass  "pass1"))
      (with-new-cookie
        (http-test-regist `((id    . ,id)
                            (pass  . ,pass)
                            (email . "1@1.com")
                            (blog-title . "user1's blog")))
        (http-test-login :id id :pass pass)
        (let ((oid (http-test-make-instance class :title "title1" :body "body1")))
          (http-test-get-instance-by-oid class oid)
          (http-test-update-instance class oid '((title . "title1c") (body . "body1c")))
          (http-test-drop-instance-by-oid class oid))
        (http-test-logout)))

デバッグモード
---------------
[debug-mode-on](http://web4r.org/en/api#debug-mode-on)はデバッグモードを開始し、[debug-mode-off](http://web4r.org/en/api#debug-mode-off)はデバッグモードを終了します。

デバッグモードはエラー発生時に次の２つのことを行います。

1. Lispエラーの内容をHTTP経由でHTMLとして表示
2. Lispエラーとそのバックトレースを[\*debug-log-file\*](http://web4r.org/en/api#*debug-log-file*)に記録

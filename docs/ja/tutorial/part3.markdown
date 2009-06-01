Tutorial Part 3. ページの定義
==============================
[defpage](http://web4r.org/en/api#defpage)はページを定義する為のマクロです。ユーザーは'http://localhost:8080/ページ名'のようなURLにアクセスをすることにより、定義済みページを呼び出すことが出来ます。

文法:

    defpage page ([path ...] [:get garg ...] [:post parg ...]
                  :auth :redirect uri :default) body

引数と値:

    page---ページ名
    path---URIパス
    garg---getパラメーターのシンボル名
    parg---postパラメーターのシンボル名
    :auth---:authが引数で渡されて、ユーザーがログインしていない場合、ユーザーはログインページにリダイレクトされます
    uri---ユーザーのログイン後のリダイレクト先URI
    :default---:defaultが引数で渡された場合、現在のページ手続きが*default-handler*にセットされます
    body---フォーム

例1:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage hello (:get (name "world"))
      [p "Hello, " name])

    ; wget http://localhost:8080/hello => '<p>Hello, world</p>'
    ; wget http://localhost:8080/hello?name=tomo => '<p>Hello, tomo</p>'

例2:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage test (path1 path2 :get get1 get2)
      [p path1 " " path2 " " get1 " " get2])

    ; wget http://localhost:8080/test/1/2/ => '<p>1 2  </p>'
    ; wget http://localhost:8080/test/1/2/?get1=3&get2=4 => '<p>1 2 3 4</p>'

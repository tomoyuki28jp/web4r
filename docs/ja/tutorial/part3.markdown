Tutorial Part 3. ページの定義
==============================
[defpage](http://web4r.org/en/api#defpage)はページを定義する為のマクロです。ユーザーは'http://localhost:8080/ページパス'のようなURLにアクセスをすることにより、定義済みページを呼び出すことが出来ます。

文法:

    defpage paths ([parg ...] [:get garg ...] [:post parg ...]
                   :auth :redirect uri :default) body

引数と値:

    paths---ページのベースパス
    parg---ベースパスからの相対URIパス
    garg---getパラメーターのシンボル名
    parg---postパラメーターのシンボル名
    :auth---:authが引数で渡されて、ユーザーがログインしていない場合、ユーザーはログインページにリダイレクトされます
    uri---ユーザーのログイン後のリダイレクト先URI
    :default---:defaultが引数で渡された場合、現在のページ手続きが*default-handler*にセットされます
    body---フォーム

例1:  
*URIパスやget/postパラメーターを簡単に取得することが出来ます*

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage test (path1 path2 :get get1 get2)
      [p path1 " " path2 " " get1 " " get2])

    ; wget "http://localhost:8080/test/p1/p2/" => '<p>p1 p2  </p>'
    ; wget "http://localhost:8080/test/p1/p2/?get1=g1&get2=g2" => '<p>p1 p2 g1 g2</p>'

例2:  
*URIパスとget/postパラメーターはデフォルト値をセットすることが出来ます*

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage hello (:get (name "world"))
      [p "Hello, " name])

    ; wget "http://localhost:8080/hello" => '<p>Hello, world</p>'
    ; wget "http://localhost:8080/hello?name=tomo" => '<p>Hello, tomo</p>'

例3:  
*ベースパス(PATHS)は複数のパスで構成することが可能であり、複数のパスで指定した場合、PARGはベースパスからの相対パスになります*

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)

    (start-server)

    (defpage one/two (path1 path2) [p "two " path1 " " path2])
    (defpage one/two/three (path1 path2) [p "three " path1 " " path2])

    ; wget "http://localhost:8080/one/two/foo/bar/" => '<p>two foo bar</p>'
    ; wget "http://localhost:8080/one/two/three/foo/bar/" => '<p>three foo bar</p>'

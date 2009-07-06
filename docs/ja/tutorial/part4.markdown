Tutorial Part 4. 継続渡しスタイル
==================================
web4rは継続渡しスタイルによるwebプログラミングを可能にします。

CPSでwebプログラムを書くメリット
---------------------------------
- 制御を渡すのが簡単
- 状態保持の為に、クロージャに補足された自由変数をセッションオブジェクトの代わりに利用することができる

a/cont
-------
[a/cont](http://web4r.org/en/api#a/cont)は継続をリンクに埋め込みます。

例:

    (defpage test ()
      (a/cont [p "2nd page!"] "click here"))

![cps1](http://web4r.org/cps1.png)

form/cont
----------
[form/cont](http://web4r.org/en/api#form/cont)は継続をフォームに埋め込みます。  
*- ファイルをアップロードするには代わりに[multipart-form/cont](http://web4r.org/en/api#multipart-form/cont)を利用して下さい*  
*- form/contとmultipart-form/contはsubmitボタンが存在しない場合、submitボタンを挿入します*

例:

    (defpage test ()
      (form/cont (let ((foo (hunchentoot:post-parameter "foo")))
                   (a/cont [p "You said: " foo] "click here"))
        [input :type "text" :name "foo" /]))

![cps2](http://web4r.org/cps2.png)

page-lambda
------------
[page-lambda](http://web4r.org/en/api#page-lambda)はページ手続きを生成します。page-lambdaはパスやget/postパラメーターを束縛する為に利用します。

例:

    (defpage test ()
      (form/cont (page-lambda (:post foo)
                   (a/cont [p "You said: " foo] "click here"))
        [input :type "text" :name "foo" /]))

    ; => 生成ページはひとつ前の例と同じ

last-post
----------
[form/cont](http://web4r.org/en/api#form/cont)内の(last-post NAME)はコンパイル時にNAMEという名前のpostパラメーターを束縛するコードへと展開されます。

例:

    (defpage test ()
      (form/cont (a/cont [p "You said: " (last-post "foo")] "click here")
        [input :type "text" :name "foo" /]))

    ; 展開イメージ:
    ; (defpage test ()
    ;   (form/cont (let ((#:G1152 (hunchentoot:post-parameter "foo")))
    ;                (a/cont [p "You said: " #:G1152] "click here"))
    ;     [input :type "text" :name "foo" /]))

    ; => 生成ページはひとつ前の例と同じ

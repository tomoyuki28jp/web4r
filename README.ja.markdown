About
======
web4rは「世界一短いコードでwebアプリ作成ができるフレームワーク」を目指して開発されました。
（ここで言うコードの短さはトークンの数を意味します。）

#### "Hello World" アプリケーション:

    (require :web4r)
    (use-package :web4r)
    
    (defpage hello (:get (name "world"))
      [p "Hello, " name])
    
    (start-server)

    ; wget http://localhost:8080/hello => '<p>Hello, world</p>'
    ; wget http://localhost:8080/hello?name=tomo => '<p>Hello, tomo</p>'

#### Arc Challenge:

    (defpage said ()
      (form/cont (a/cont [p "You said: " (last-post "foo")] "click here")
       (input-text "foo")))

- [Take the Arc Challenge](http://www.paulgraham.com/arcchallenge.html) 
  ([和訳](http://practical-scheme.net/wiliki/wiliki.cgi?Arc%E3%81%8B%E3%82%89%E3%81%AE%E6%8C%91%E6%88%A6))
- [Arc Challenge デモ](http://demo.web4r.org/said)

#### Blog アプリケーション:

    (defpclass blog ()
      ((title :length 50 :index t)
       (body  :length 3000)))
    
    (genpages blog)

- [Blog デモ](http://demo.web4r.org/blog)

Documentation
==============
[Download](http://web4r.org/ja/download)  
[Install](http://web4r.org/ja/install)  
[Tutorial](http://web4r.org/ja/tutorial)  
[API Documentation](http://web4r.org/ja/api)  

*これらのドキュメントはソースのdocsディレクトリ以下に同梱されています*

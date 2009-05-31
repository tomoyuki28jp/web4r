About
======
web4rは「世界一短いコードでwebアプリ作成ができるフレームワーク」を目指して開発されました。

    (require :web4r)
    (use-package :web4r)
    
    (defpage hello (:get (name "world"))
      [p "Hello, " name])
    
    (start-server)

    ; wget http://localhost:8080/hello => '<p>Hello, world</p>'
    ; wget http://localhost:8080/hello?name=tom => '<p>Hello, tom</p>'

Documentation
==============
[Download](http://web4r.org/ja/download)  
[Install](http://web4r.org/ja/install)  
[Tutorial](http://web4r.org/ja/tutorial)  
[API Documentation](http://web4r.org/ja/api)  

*これらのドキュメントはソースのdocsディレクトリ以下に同梱されています*

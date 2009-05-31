About
======
The goal of web4r is enabling users to develop web applications with the shortest codes.

    (require :web4r)
    (use-package :web4r)
    
    (defpage hello (:get (name "world"))
      [p "Hello, " name])
    
    (start-server)

    ; wget http://localhost:8080/hello => '<p>Hello, world</p>'
    ; wget http://localhost:8080/hello?name=tomo => '<p>Hello, tomo</p>'

A complete web4r application.

Documentation
==============
[Download](http://web4r.org/en/download)  
[Install](http://web4r.org/en/install)  
[Tutorial](http://web4r.org/en/tutorial)  
[API Documentation](http://web4r.org/en/api)  

*Note: These documents are placed under the web4r/docs directory*

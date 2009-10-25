About
======
web4r is a continuations-based web application framework written in Common Lisp. The goal of web4r is enabling users to develop web applications with the shortest codes. (The shortness is about a number of tokens.)

#### The "Hello World" Application:

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
- [Arc Challenge Demo](http://demo.web4r.org/said)

#### Blog Application:

    (ele:open-store *example-bdb*)

    (defpclass blog ()
      ((title :length 50 :index t)
       (body  :length 3000)))
    
    (genpages blog)

- [Blog Demo](http://demo.web4r.org/blog)

Documentation
==============
[Download](http://web4r.org/en/download)  
[Install](http://web4r.org/en/install)  
[Tutorial](http://web4r.org/en/tutorial)  
[API Documentation](http://web4r.org/en/api)  

*Note: These documents are included under the web4r/docs directory*

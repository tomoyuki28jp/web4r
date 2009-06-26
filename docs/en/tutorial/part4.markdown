Tutorial Part 4. Continuation Passing Style (CPS)
==================================================
web4r provides a way to write web applications in continuation passing style.

a/cont
-------
[a/cont](http://web4r.org/en/api#a/cont) embeds continuation within a link node.

Examples:

    (defpage test ()
      (a/cont [p "2nd page!"] "click here"))

![cps1](http://web4r.org/cps1.png)

form/cont
----------
[form/cont](http://web4r.org/en/api#form/cont) embeds continuation within a form node.  
*- To upload files, use [multipart-form/cont](http://web4r.org/en/api#multipart-form/cont) instead.*  
*- form/cont and multipart-form/cont inserts a submit button if there isn't one.*

Examples:

    (defpage test ()
      (form/cont (let ((foo (hunchentoot:post-parameter "foo")))
                   (a/cont [p "You said: " foo] "click here"))
        [input :type "text" :name "foo" /]))

![cps2](http://web4r.org/cps2.png)

page-lambda
------------
[page-lambda](http://web4r.org/en/api#page-lambda) creates a page precedence which is used to bound paths and get/post parameters.

Examples:

    (defpage test ()
      (form/cont (page-lambda (:post foo)
                   (a/cont [p "You said: " foo] "click here"))
        [input :type "text" :name "foo" /]))

    ; => Generated pages are the same with the previous example.

last-post
----------
(last-post NAME) inside [form/cont](http://web4r.org/en/api#form/cont) is expanded to bound the last post parameter named NAME at compile time.

Examples:

    (defpage test ()
      (form/cont (a/cont [p "You said: " (last-post "foo")] "click here")
        [input :type "text" :name "foo" /]))

    ; Expanded code (image):
    ; (defpage test ()
    ;   (form/cont (let ((#:G1152 (hunchentoot:post-parameter "foo")))
    ;                (a/cont [p "You said: " #:G1152] "click here"))
    ;     [input :type "text" :name "foo" /]))

    ; => Generated pages are the same with the previous example.

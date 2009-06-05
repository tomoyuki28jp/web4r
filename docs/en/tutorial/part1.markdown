Tutorial Part 1. S-expression Markup language
==============================================
This part of a tutorial will show how to use [sml](http://github.com/tomoyuki28jp/sml/) (s-expression markup language). If you prefer to use normal HTML templates to work with designers, you can use Edi's [HTML-TEMPLATE](http://www.weitz.de/html-template/) instead.

Download, Install and Use
--------------------------
web4r depends on sml, so if you have already installed web4r, you should already have installed sml. Otherwise download sml from [this page](https://github.com/tomoyuki28jp/sml) and install it.

To use sml, run this codes:

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :sml))
    (use-package :sml)

Syntax
-------
[tag attributes... values...]

Examples:

    [html [body "Hello, world!"]]

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html>
    ;       <body>Hello, world!</body>
    ;   </html>

Attributes
-----------
Keyword parameters or any parameter inside (attr ...) appears before values become attributes.

Examples:

    ; All of the codes below generate this (x)html:
    ;=> <p id="id1" class="class1">value</p>
    
    [p :id "id1" :class "class1" "value"]
    [p (attr :id "id1") (attr :class "class1") "value"]
    [p (attr "id" "id1") (attr "class" "class1") "value"]
    [p (attr :id "id1" :class "class1") "value"]
    
Markup language
----------------
You can change the markup language to generate by \*markup-lang\* which must be either one of :xhtml, :html or :xml. The default markup language is xhtml.

Examples:

    (let ((*markup-lang* :xhtml))
      [html [body "xhtml"]])

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html>
    ;       <body>xhtml</body>
    ;   </html>


    (let ((*markup-lang* :html))
      [html [body "html"]])      

    ;=> <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
    ;   "http://www.w3.org/TR/html4/loose.dtd">
    ;   <html>
    ;       <body>html</body>
    ;   </html>


    (let ((*markup-lang* :xml))
      [html [body "xml"]])

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <html>
    ;       <body>xml</body>
    ;   </html>

Closed Parenthesis
-------------------
If you don't need a closed parenthesis, supplies / as a last parameter.

Examples:

    [br] ;=> <br></br>

    (let ((*markup-lang* :xhtml))
      [br /]) ;=> <br />

    (let ((*markup-lang* :html))
      [br /]) ;=> <br>

    (let ((*markup-lang* :xml))
      [br /]) ;=> <br />

Escape
-------
By default, sml escapes all attributes and values.

    [p "<>"] ;=> <p>&lt;&gt;</p>

You can manually escape an object with the escape function, and escaped objects won't be double escaped.

    [p (escape"<>")] ;=> <p>&lt;&gt;</p>
    [p (escape (escape"<>"))] ;=> <p>&lt;&gt;</p>

If you don't want to escape an object, use the safe macro.

    [p (safe"<>")] ;=> <p><></p>

Template
---------
You can use sml as a template markup language. Sml codes written in another template file are expanded inside a lisp code at compile time, so you don't need to assign any variable.

Examples:  
*/tmp/template.sml*

    (print x)

*/tmp/use.lisp*

    (let ((x "ok!"))
      (load-sml "/tmp/template.sml")) ;=> "ok!"

Manipulate Template Elements
-----------------------------
You can select a template element by a selector and manipulate it with a manipulator. The syntax of selector is tag, #id and .class. Manipulators are append, replace and remove.

Examples:

    (define-template :template1
        [html [head [title "Default title"]]
              [body [p :id "id1" "p1"]
                    [p :class "class1" "p2"]]])
    
    (with-template (:template1)
      (replace title [title "new title"]) ; replace the title
      (remove  "#id1") ; remove the element whose id is 'id1'
      (remove  ".class1") ; remove the element whose class is 'class1'
      (append  body [p "ok!"])) ; append a new element to the body

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html>
    ;       <head>
    ;           <title>new title</title>
    ;       </head>
    ;       <body>
    ;           <p>ok!</p>
    ;       </body>
    ;   </html>

FAQ
----
### Q. Why did you reinvent yet another markup language with reader macros? [cl-who](http://www.weitz.de/cl-who/) does the similar things with a regular macro.

A. Because it's shorter. For example, if you rewrite the following sml code with cl-who, I think it's gonna be longer.

    [html [body [table (dotimes (x 3) [tr [td x]])]]]
    
    ; =>
    ;<?xml version="1.0" encoding="UTF-8"?>
    ;<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;<html>
    ;    <body>
    ;        <table>
    ;            <tr><td>0</td></tr>
    ;            <tr><td>1</td></tr>
    ;            <tr><td>2</td></tr>
    ;        </table>
    ;    </body>
    ;</html>


If you prefer [cl-who](http://www.weitz.de/cl-who/), you can use it instead of sml.

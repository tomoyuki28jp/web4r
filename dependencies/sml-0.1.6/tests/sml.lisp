(in-package :cl-user)

(defpackage :sml-tests
  (:use :cl :sml :5am :my-util))

(in-package :sml-tests)

(def-suite sml)
(in-suite  sml)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *shtml-dir*
    (when-let (p (load-time-value #.*compile-file-pathname*))
      (truename (merge-pathnames "shtml/" (directory-namestring p))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shtml-path (file)
    (namestring (merge-pathnames file *shtml-dir*))))

(defun string=* (str1 str2)
  (string= (remove #\Newline str1)
           (remove #\Newline str2)))

(defmacro sml= (sml ml)
  `(string=* (sml->ml ,sml) ,ml))

(setf *indent-mode* nil)

(test doctype
  (let ((*markup-lang* :xhtml))
    (let ((*doctype* :strict))
      (is-true (string= (doctype)
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")))
    (let ((*doctype* :transitional))
      (is-true (string= (doctype)
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
")))
    (let ((*doctype* :frameset))
      (is-true (string= (doctype)
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
"))))
  (let ((*markup-lang* :html))
    (let ((*doctype* :strict))
      (is-true (string= (doctype)
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">
")))
    (let ((*doctype* :transitional))
      (is-true (string= (doctype)
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">
")))
    (let ((*doctype* :frameset))
      (is-true (string= (doctype)
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
\"http://www.w3.org/TR/html4/frameset.dtd\">
"))))
  (let ((*markup-lang* :xml))
    (is-true (string= (doctype)
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
"))))

(test escape
  (is (string= (escape* :<>) "&lt;&gt;"))
  (is (string= (escape* '<>) "&lt;&gt;"))
  (is (string= (escape* '("<>")) "(&lt;&gt;)"))
  (is (string= (escape* 123) "123"))
  (is (string= (escape* "&<>'\"")
               "&amp;&lt;&gt;&#039;&quot;"))
  (is (string= (escape* "&1<2>3'4\"")
               "&amp;1&lt;2&gt;3&#039;4&quot;"))
  (is (string= (escape* "1&2<3>4'5\"6")
               "1&amp;2&lt;3&gt;4&#039;5&quot;6"))
  (is (string= (escape* (safe "&amp;&lt;&gt;&#039;&quot;"))
               "&amp;&lt;&gt;&#039;&quot;"))
  (is (string= (escape* (safe (safe "&amp;&lt;&gt;&#039;&quot;")))
               "&amp;&lt;&gt;&#039;&quot;"))
  (is (string= (escape* (escape "<>")) "&lt;&gt;")))

(test nl->br
  (let ((*markup-lang* :xhtml))
    (string= (nl->br 
"1
2
3")
"1<br />
2<br />
3"))
  (let ((*markup-lang* :html))
    (string= (nl->br 
"1
2
3")
"1<br>
2<br>
3")))

(test attr
  (is-true (string= (attr :k1 "v1")
                    " k1=\"v1\""))
  (is-true (string= (attr :k1 "v1" :k2 "v2")
                    " k1=\"v1\" k2=\"v2\""))
  (is-true (string= (attr :k1 "v1" :k2 "v2" :k3 "v3")
                    " k1=\"v1\" k2=\"v2\" k3=\"v3\""))
  (is-true (string= (attr '(:k1 "v1" :k2 "v2" :k3 "v3"))
                    " k1=\"v1\" k2=\"v2\" k3=\"v3\""))
  (is-true (sml= [p :k1 "k1" :k2 "v2" :k3 "v3" "ok"]
                 "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>"))
  (is-true (sml= [p (attr :k1 "k1") :k2 "v2" :k3 "v3" "ok"]
                 "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>"))
  (is-true (sml= [p :k1 "k1" (attr :k2 "v2") :k3 "v3" "ok"]
                 "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>"))
  (is-true (sml= [p :k1 "k1" :k2 "v2" (attr :k3 "v3") "ok"]
                 "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>"))
  (let ((k :k3) (v "v3"))
    (is-true (sml= [p :k1 "k1" :k2 "v2" (attr k v) "ok"]
                   "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>")))
  (is-true (sml= [p (attr :k1 "k1" :k2 "v2") :k3 "v3" "ok"]
                 "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>"))
  (is-true (sml= [p (attr '(:k1 "k1" :k2 "v2")) :k3 "v3" "ok"]
                 "<p k1=\"k1\" k2=\"v2\" k3=\"v3\">ok</p>")))

(test tags
  (is-true (sml= [p "ok"] "<p>ok</p>"))
  (is-true (sml= [p '(1 2 3)] "<p>123</p>"))
  (is-true (sml= [p '(1 . 2)] "<p>(1 . 2)</p>"))
  (is-true (sml= [p :id "p-id" "ok"] "<p id=\"p-id\">ok</p>"))
  (is-true (sml= [body [p "ok"]] "<body><p>ok</p></body>"))
  (let ((*markup-lang* :xhtml))
    (is-true (sml= [html [body "ok"]]
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html><body>ok</body></html>")))
  (is-true (sml= [a :href "http://localhost:8080/" "link"]
                 "<a href=\"http://localhost:8080/\">link</a>"))
  (is-true (sml= [img :src "http://localhost:8080/img.png" /]
                 "<img src=\"http://localhost:8080/img.png\" />"))
  (is-true (sml= [form :name "form-name" :method "post" :action ""
                       [input :type "text" :name "text-name" /]
                       [input :type "submit" :value "submit-button" /]]
"<form name=\"form-name\" method=\"post\" action=\"\">
<input type=\"text\" name=\"text-name\" />
<input type=\"submit\" value=\"submit-button\" />
</form>"))
  (is-true (sml= [table (dotimes (i 3) [th [td i]])]
"<table>
<th><td>0</td></th>
<th><td>1</td></th>
<th><td>2</td></th>
</table>"))
  (is-true (sml= [p "1" "2" "3" "4" "5"] "<p>12345</p>")))

(test form
  (is-true (sml= (form)
                 "<form><input type=\"submit\" /></form>"))
  (is-true (sml= (form [input :type "submit" /])
                 "<form><input type=\"submit\" /></form>"))
  (is-true (sml= (form (submit))
                 "<form><input type=\"submit\" /></form>"))

  (is-true (sml= (form [table [tr [td "submit"]
                                  [td [input :type "submit" /]]]])
"<form>
<table>
<tr>
<td>submit</td>
<td><input type=\"submit\" /></td>
</tr>
</table>
</form>"))
  (is-true (sml= (form [table [tr [td "submit"]
                                  [td (submit)]]])
"<form>
<table>
<tr>
<td>submit</td>
<td><input type=\"submit\" /></td>
</tr>
</table>
</form>")))

(define-template :test
    [html :lang "ja"
          [head [title "Default title"]]
          [body [div :id "div-id1" "div1"]
                [div :id "div-id2" "div2"]]])

(test multipart-form
  (is-true (sml= (multipart-form)
"<form enctype=\"multipart/form-data\">
<input type=\"submit\" />
</form>"))
  (is-true (sml= (multipart-form (submit))
"<form enctype=\"multipart/form-data\">
<input type=\"submit\" />
</form>"))
  (is-true (sml= (multipart-form [input :type "submit" /])
"<form enctype=\"multipart/form-data\">
<input type=\"submit\" />
</form>")))

(test input-text
  (is-true (sml= (input-text "foo")
                 "<input type=\"text\" name=\"foo\" />"))
  (is-true (sml= (input-text "foo" :value "v1")
                 "<input type=\"text\" name=\"foo\" value=\"v1\" />")))

(test input-file
  (is-true (sml= (input-file "foo")
                 "<input type=\"file\" name=\"foo\" />")))

(test submit
  (is-true (sml= (submit) "<input type=\"submit\" />"))
  (is-true (sml= (submit :name "name1" :value "value1")
                 "<input type=\"submit\" name=\"name1\" value=\"value1\" />")))

(test input-checked
  (is-true (sml= (input-checked "checkbox" "val")
                 "<input type=\"checkbox\" />"))
  (is-true (sml= (input-checked "checkbox" "val" :value "val")
                 "<input type=\"checkbox\" checked=\"checked\" value=\"val\" />"))
  (is-true (sml= (input-checked "radio" "val" :name "name1" :value "val")
                 "<input type=\"radio\" checked=\"checked\" name=\"name1\" value=\"val\" />")))

(test select-form
  (is-true (sml= (select-form "name1" '(1 2 3))
"<select name=\"name1\" id=\"name1\">
<option value=\"1\">1</option>
<option value=\"2\">2</option>
<option value=\"3\">3</option>
</select>"))
  (is-true (sml= (select-form "name1" '(1 2 3) :selected 2)
"<select name=\"name1\" id=\"name1\">
<option value=\"1\">1</option>
<option value=\"2\" selected=\"selected\">2</option>
<option value=\"3\">3</option>
</select>")))

(test load-shtml
  (let ((x "ok"))
    (sml= (load-sml (shtml-path "test3.sml")) "<p>ok</p>"))
  (let ((lst '(1 2 3)))
    (sml= (load-sml (shtml-path "test4.sml"))
          "<ul><li>1</li><li>2</li><li>3</li></ul>"))
  (is-true (sml= (load-sml (shtml-path "test5.sml"))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>new title</title></head>
<body><p>Default body</p></body>
</html>"))
  (is-true (sml= (load-sml (shtml-path "test6.sml"))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>Default title</title></head>
<body>new body</body>
</html>")))

(test with-template
  (is-true (sml= (with-template (:test))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>Default title</title></head>
<body>
<div id=\"div-id1\">div1</div>
<div id=\"div-id2\">div2</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test) (replace div [div "new"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>Default title</title></head>
<body>
<div>new</div>
<div>new</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test) (replace "#div-id1" [div "new div id1"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>Default title</title></head>
<body>
<div>new div id1</div>
<div id=\"div-id2\">div2</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test) (remove head))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<body>
<div id=\"div-id1\">div1</div>
<div id=\"div-id2\">div2</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test)
                   (append head [meta :http-equiv "Content-Type"
                                      :content "text/html; charset=UTF-8" /]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head>
<title>Default title</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
</head>
<body>
<div id=\"div-id1\">div1</div>
<div id=\"div-id2\">div2</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test)
                   (append head [p "test1"])
                   (append head [p "test2"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head>
<title>Default title</title>
<p>test1</p>
<p>test2</p>

</head>
<body>
<div id=\"div-id1\">
div1
</div>
<div id=\"div-id2\">
div2
</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test)
                   (append head ([p "new1"] [p "new2"])))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head>
<title>Default title</title>
<p>new1</p>
<p>new2</p>

</head>
<body>
<div id=\"div-id1\">
div1
</div>
<div id=\"div-id2\">
div2
</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test)
                   (append  head [p "ok"])
                   (replace title [title "new"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head>
<title>new</title>
<p>ok</p>
</head>
<body>
<div id=\"div-id1\">
div1
</div>
<div id=\"div-id2\">
div2
</div>
</body>
</html>"))
  (is-true (sml= (with-template (:test)
                   (replace "#div-id2"
                            [div :id "a"]
                            [div :id "b"]
                            [div :id "c"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\"><head><title>Default title</title></head>
<body>
<div id=\"div-id1\">div1</div>
<div id=\"a\"></div>
<div id=\"b\"></div>
<div id=\"c\"></div>
</body></html>")))

(test with-sml-file
  (let ((x "ok"))
    (is-true (sml= (with-sml-file (shtml-path "test1.sml"))
                   "<p>ok</p>")))
  (let ((lst '(1 2 3)))
    (is-true (sml= (with-sml-file (shtml-path "test2.sml"))
                    "<ul><li>1</li><li>2</li><li>3</li></ul>")))
  (is-true (sml= (with-sml-file (shtml-path "template1.sml")
                   (replace title [title "new title"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>new title</title></head>
<body><p>Default body</p></body>
</html>"))
  (is-true (sml= (with-sml-file (shtml-path "template1.sml")
                   (replace body [body "new body"]))
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html lang=\"ja\">
<head><title>Default title</title></head>
<body>new body</body>
</html>")))

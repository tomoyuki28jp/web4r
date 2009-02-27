(in-package :web4r-tests)
(in-suite web4r)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *test-dir*
    (namestring (merge-pathnames "tests/" *web4r-dir*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *test-shtml-dir*
    (namestring (merge-pathnames "shtml/" *test-dir*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun test-shtml-path (file)
    (namestring (merge-pathnames file *test-shtml-dir*))))

(defun string=* (str1 str2)
  (string= (replace-str *nl* "" str1)
           (replace-str *nl* "" str2)))

(defmacro shtml= (shtml html)
  `(string=* (shtml->html ,shtml) ,html))

(test doctype
  (let ((*doctype* *doctype-strict*))
    (is (equal (doctype)
               (format nil "~A~%"
                       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN
http://www.w3.org/TR/html4/strict.dtd\">"))))
  (let ((*doctype* *doctype-transitional*))
    (is (equal (doctype)
               (format nil "~A~%"
                       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">"))))
  (let ((*doctype* *doctype-frameset*))
    (is (equal (doctype)
               (format nil "~A~%"
                       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN
http://www.w3.org/TR/html4/frameset.dtd\">")))))

(test escape
  (is (string= (escape "&<>'\"") "&amp;&lt;&gt;&#039;&quot;"))
  (is (string= (escape "&1<2>3'4\"") "&amp;1&lt;2&gt;3&#039;4&quot;"))
  (is (string= (escape "1&2<3>4'5\"6") "1&amp;2&lt;3&gt;4&#039;5&quot;6"))
  (is (string= (escape (safe "&amp;&lt;&gt;&#039;&quot;")) "&amp;&lt;&gt;&#039;&quot;"))
  (is (string= (escape (safe (safe "&amp;&lt;&gt;&#039;&quot;"))) "&amp;&lt;&gt;&#039;&quot;")))

(test tags
  (is-true (shtml= (p/ "ok") "<P>ok</P>"))
  (is-true (shtml= (p/ :id "p-id" "ok") "<P ID=\"p-id\">ok</P>"))
  (is-true (shtml= (body/ (p/ "ok")) "<BODY><P>ok</P></BODY>"))
  (let ((*doctype* *doctype-transitional*))
    (is-true (shtml= (html/ (body/ "ok"))
                     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\"><HTML><BODY>ok</BODY></HTML>")))
  (is-true (shtml= (a/ :href "http://localhost:8080/" "link")
                   "<A HREF=\"http://localhost:8080/\">link</A>"))
  (is-true (shtml= (img/ :src "http://localhost:8080/img.png")
                   "<IMG SRC=\"http://localhost:8080/img.png\">"))
  (is-true (shtml= (form/ :name "form-name" :method "post" :action ""
                          (input/ :type "text" :name "text-name")
                          (input/ :type "submit" :value "submit-button"))
                   "<FORM NAME=\"form-name\" METHOD=\"post\" ACTION=\"\">
<INPUT TYPE=\"text\" NAME=\"text-name\">
<INPUT TYPE=\"submit\" VALUE=\"submit-button\">
</FORM>"))
  (is-true (shtml= (table/
                    (loop for i in '(1 2 3)
                          do (th/ (td/ i))))
                   "<TABLE><TH><TD>1</TD></TH><TH><TD>2</TD></TH><TH><TD>3</TD></TH></TABLE>"))
  (is-true (shtml= (p/ "1" "2" "3" "4" "5") "<P>12345</P>")))

(define-shtml :test
    (html/ :lang "ja"
           (head/ (title/ "Default title"))
           (body/ (p/ "Default body"))))

(test shtml
  (let ((*doctype* *doctype-transitional*))
    (is-true (shtml= (with-shtml (:test))
                     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>Default title</TITLE></HEAD>
<BODY><P>Default body</P></BODY>
</HTML>"))
    (is-true (shtml= (with-shtml (:test)
                       :title (title/ "New title"))
                     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>New title</TITLE></HEAD>
<BODY><P>Default body</P></BODY>
</HTML>"))
    (is-true (shtml= (with-shtml (:test)
                       :body (body/ (p/ "New body")))
                     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>Default title</TITLE></HEAD>
<BODY><P>New body</P></BODY>
</HTML>"))))

(test shtml-file
  (let ((x "x"))
    (is-true (shtml= (with-shtml-file (test-shtml-path "test1.shtml"))
                     "<P>x</P>")))
  (let ((lst '(1 2 3)))
    (is-true (shtml= (with-shtml-file (test-shtml-path "test2.shtml"))
                     "<UL><LI>1</LI><LI>2</LI><LI>3</LI></UL>")))
  (is-true (shtml= (with-shtml-file (test-shtml-path "template1.shtml"))
                   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>Default title</TITLE></HEAD>
<BODY><P>Default body</P></BODY></HTML>"))
  (is-true (shtml= (with-shtml-file (test-shtml-path "template1.shtml")
                     :title (title/ "New title"))
                   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>New title</TITLE></HEAD>
<BODY><P>Default body</P></BODY></HTML>"))
  (is-true (shtml= (with-shtml-file (test-shtml-path "template1.shtml")
                     :body (body/ (p/ "New body")))
                   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>Default title</TITLE></HEAD>
<BODY><P>New body</P></BODY></HTML>")))

(test load-shtml
  (let ((x "x"))
    (is-true (shtml= (load-shtml (test-shtml-path "test3.shtml"))
                     "<P>x</P>")))
  (let ((lst '(1 2 3)))
    (is-true (shtml= (load-shtml (test-shtml-path "test4.shtml"))
                     "<UL><LI>1</LI><LI>2</LI><LI>3</LI></UL>")))
  (is-true (shtml= (load-shtml (test-shtml-path "test5.shtml"))
                   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>New title</TITLE></HEAD>
<BODY><P>Default body</P></BODY></HTML>"))
  (is-true (shtml= (load-shtml (test-shtml-path "test6.shtml"))
                   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"ja\">
<HEAD><TITLE>Default title</TITLE></HEAD>
<BODY><P>New body</P></BODY></HTML>")))

(test input-text/
  (is-true (shtml= (input-text/ "foo")
                   "<INPUT TYPE=\"text\" NAME=\"foo\">"))
  (is-true (shtml= (input-text/ "foo" :value "v1")
                   "<INPUT TYPE=\"text\" NAME=\"foo\" VALUE=\"v1\">")))

(test input-checked/
  (is-true (shtml= (input-checked/ "checkbox" "val")
                   "<INPUT TYPE=\"checkbox\">"))
  (is-true (shtml= (input-checked/ "checkbox" "val" :value "val")
                   "<INPUT TYPE=\"checkbox\" CHECKED=\"checked\" VALUE=\"val\">"))
  (is-true (shtml= (input-checked/ "radio" "val" :name "name1" :value "val")
                   "<INPUT TYPE=\"radio\" CHECKED=\"checked\" NAME=\"name1\" VALUE=\"val\">")))

(test submit/
  (is-true (shtml= (submit/) "<INPUT TYPE=\"submit\">"))
  (is-true (shtml= (submit/ :name "name1" :value "value1")
                   "<INPUT TYPE=\"submit\" NAME=\"name1\" VALUE=\"value1\">")))

(test select-form/
  (is-true (shtml= (select-form/ "name1" '(1 2 3))
                   "<SELECT NAME=\"name1\" ID=\"name1\">
<OPTION VALUE=\"1\">1</OPTION>
<OPTION VALUE=\"2\">2</OPTION>
<OPTION VALUE=\"3\">3</OPTION>
</SELECT>"))
  (is-true (shtml= (select-form/ "name1" '(1 2 3) 2)
                   "<SELECT NAME=\"name1\" ID=\"name1\">
<OPTION VALUE=\"1\">1</OPTION>
<OPTION VALUE=\"2\" SELECTED=\"selected\">2</OPTION>
<OPTION VALUE=\"3\">3</OPTION>
</SELECT>")))

(test select-date/
  (is-true (shtml= (select-date/ "name1" :y-start 2000 :y-end 2002 :y 2001 :m 5 :d 10)
                   "<SELECT NAME=\"name1-Y\" ID=\"name1-Y\">
<OPTION VALUE=\"2000\">2000</OPTION>
<OPTION VALUE=\"2001\" SELECTED=\"selected\">2001</OPTION>
<OPTION VALUE=\"2002\">2002</OPTION>
</SELECT>
<SELECT NAME=\"name1-M\" ID=\"name1-M\">
<OPTION VALUE=\"1\">1</OPTION>
<OPTION VALUE=\"2\">2</OPTION>
<OPTION VALUE=\"3\">3</OPTION>
<OPTION VALUE=\"4\">4</OPTION>
<OPTION VALUE=\"5\" SELECTED=\"selected\">5</OPTION>
<OPTION VALUE=\"6\">6</OPTION>
<OPTION VALUE=\"7\">7</OPTION>
<OPTION VALUE=\"8\">8</OPTION>
<OPTION VALUE=\"9\">9</OPTION>
<OPTION VALUE=\"10\">10</OPTION>
<OPTION VALUE=\"11\">11</OPTION>
<OPTION VALUE=\"12\">12</OPTION>
</SELECT>
<SELECT NAME=\"name1-D\" ID=\"name1-D\">
<OPTION VALUE=\"1\">1</OPTION>
<OPTION VALUE=\"2\">2</OPTION>
<OPTION VALUE=\"3\">3</OPTION>
<OPTION VALUE=\"4\">4</OPTION>
<OPTION VALUE=\"5\">5</OPTION>
<OPTION VALUE=\"6\">6</OPTION>
<OPTION VALUE=\"7\">7</OPTION>
<OPTION VALUE=\"8\">8</OPTION>
<OPTION VALUE=\"9\">9</OPTION>
<OPTION VALUE=\"10\" SELECTED=\"selected\">10</OPTION>
<OPTION VALUE=\"11\">11</OPTION>
<OPTION VALUE=\"12\">12</OPTION>
<OPTION VALUE=\"13\">13</OPTION>
<OPTION VALUE=\"14\">14</OPTION>
<OPTION VALUE=\"15\">15</OPTION>
<OPTION VALUE=\"16\">16</OPTION>
<OPTION VALUE=\"17\">17</OPTION>
<OPTION VALUE=\"18\">18</OPTION>
<OPTION VALUE=\"19\">19</OPTION>
<OPTION VALUE=\"20\">20</OPTION>
<OPTION VALUE=\"21\">21</OPTION>
<OPTION VALUE=\"22\">22</OPTION>
<OPTION VALUE=\"23\">23</OPTION>
<OPTION VALUE=\"24\">24</OPTION>
<OPTION VALUE=\"25\">25</OPTION>
<OPTION VALUE=\"26\">26</OPTION>
<OPTION VALUE=\"27\">27</OPTION>
<OPTION VALUE=\"28\">28</OPTION>
<OPTION VALUE=\"29\">29</OPTION>
<OPTION VALUE=\"30\">30</OPTION>
<OPTION VALUE=\"31\">31</OPTION>
</SELECT>")))

(in-package :web4r-tests)
(in-suite web4r-tests)

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
  (string= (escape "&<>'\"") "&amp;&lt;&gt;&#039;&quot;")
  (string= (escape "&1<2>3'4\"") "&amp;1&lt;2&gt;3&#039;4&quot;")
  (string= (escape "1&2<3>4'5\"6") "1&amp;2&lt;3&gt;4&#039;5&quot;6")
  (string= (escape (safe "&amp;&lt;&gt;&#039;&quot;")) "&amp;&lt;&gt;&#039;&quot;")
  (string= (escape (safe (safe "&amp;&lt;&gt;&#039;&quot;"))) "&amp;&lt;&gt;&#039;&quot;"))

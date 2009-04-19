(with-sml-file (sml-path "template.sml")
  (replace title [title (if oid "Editing " "New ") cname])
  (append  head  [script :type "text/javascript" :src "/js/jquery-1.3.2.min.js"])
  (append  head  [script :type "text/javascript" :src "/js/jquery.validate.js"])
  (append  head  [script :type "text/javascript" :src "/js/jquery.date.js"])
  (append  head  [script :type "text/javascript" :src "/js/edit-page.js"])
  (replace body  [body
                  (if (and oid (not ins))
                      [p "That page doesn't exist!"]
                      (progn
                        [h1 :id "title" :class cname (if oid "Editing " "New ") cname]
                        (msgs)
                        (form-for/cont (funcall edit/cont*)
                          :class class :instance ins
                          :submit (if oid "Update" "Create"))))]))

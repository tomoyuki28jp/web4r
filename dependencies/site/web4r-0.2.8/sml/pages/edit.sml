(with-sml-file (sml-path "template.sml")
  (replace title [title (if oid "Editing " "New ") cname])
  (replace body  [body
                  (if (and oid (not ins))
                      [p "That page doesn't exist!"]
                      (progn
                        [h1 :id "title" :class cname (if oid "Editing " "New ") cname]
                        (msgs)
                        (form-for/cont (funcall edit/cont*)
                          :class class :instance ins
                          :submit (if oid "Update" "Create"))))]))

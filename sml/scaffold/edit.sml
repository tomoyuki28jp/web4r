(with-sml-file (sml-path "common/template.sml")
  :title [title (if oid "Editing " "New ") cname]
  :body [body
         (if (and oid (not ins))
             [p "That page doesn't exist!"]
             (progn
               [h1 (if oid "Editing " "New ") cname]
               (msgs)
               (form-for/cont (funcall edit/cont*)
                 :class class :instance ins
                 :submit (if oid "Update" "Create"))))])

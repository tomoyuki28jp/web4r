(if items
    (mapcar
     #'(lambda (x)
         [tr
          (loop for s in slots do
                [td (omit (slot-display-value x s) 20)]
                finally
                (progn
                  [td [a :href (w/p (page-uri cname "show" (oid x))) "Show"]]
                  [td [a :href (w/p (page-uri cname "edit" (oid x))) "Edit"]]
                  [td [a :href (page-uri cname "delete" (oid x))
                         :class "delete" "Delete"]]))])
     items)
    [p "There is no " cname])

(if items
    (dolist (i items)
      [tr (dolist (s slots) [td (omit (slot-display-value i s) 20)])
          [td [a :href (w/p (page-uri cname "show" (oid i))) "Show"]]
          [td [a :href (w/p (page-uri cname "edit" (oid i))) "Edit"]]
          [td [a :href (page-uri cname "delete" (oid i)) "Delete"]]])
    [p "There is no " cname])

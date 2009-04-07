(with-sml-file (sml-file-path "common/template.sml")
  :title [title "Listing " cname]
  :body [body
         (msgs)
         [h1 "Listing " cname]
         (aif items
              (progn
                (page-summary pager)
                (page-links pager)
                [table
                 [tr (loop for s in slots do [th (slot-label s)]
                           finally (dotimes (x 3) [th (safe "&nbsp;")]))]
                 (mapcar
                  #'(lambda (x)
                      [tr
                       (loop for s in slots do
                            [td (omit (slot-display-value x s) maxlength)]
                          finally
                            (progn
                              [td [a :href (w/p (page-uri cname "show" (oid x))) "Show"]]
                              [td [a :href (w/p (page-uri cname "edit" (oid x))) "Edit"]]
                              [td [a :href (page-uri cname "delete" (oid x)) "Delete"]]))])
                  it)])
              [p "There are no " cname])
         [p [a :href (w/p (page-uri cname "edit")) "New " cname]]])

(with-sml-file (sml-file-path "common/template.sml")
  :title [title "Listing blog posts"]
  :body [body
         (msgs)
         [h1 "Listing blog posts"]
         (aif items
              (progn
                (page-summary pager)
                (page-links pager)
                [table
                 [tr (mapcar #'(lambda (s) [th (slot-label s)]) slots)
                     [th (safe "&nbsp;")]
                     (when owner-p
                       (dotimes (x 2) [th (safe "&nbsp;")]))]
                 (mapcar
                  #'(lambda (x)
                      [tr
                       (loop for s in slots do
                            [td (omit (slot-display-value x s) 20)]
                             finally
                             (progn
                               [td [a :href (page-uri "blog" "show" (oid x)) "Show"]]
                               (when owner-p
                                 [td [a :href (w/p (page-uri "blog" "edit" (oid x))) "Edit"]]
                                 [td (a/cont (delete/cont x "Blog post" uri) "Delete")])))])
                  it)])
              [p "There are no blog post yet"])
         (when owner-p
           [p [a :href (w/p (page-uri "blog" "edit")) "New post"]])
         [p [a :href (page-uri "blog") "Top"]]])

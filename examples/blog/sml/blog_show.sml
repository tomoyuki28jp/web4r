(with-sml-file (sml-path "template.sml")
  (replace title [title "Show blog"])
  (replace body  [body
                  (aif ins
                      (progn
                        (msgs)
                        ; blog post
                        [div [p (time-format "~y/~m/~d" (updated-at it))]
                             [h3 (title it)]
                             [p (safe (nl->br (escape (body it))))]]
                        ; comment list
                        [hr /]
                        [div [h3 "Comments"]
                             (awhen comments
                               (page-summary pager)
                               (page-links pager)
                               (loop for c in comments
                                     do [p (name c) " said:"[br /]
                                           (safe (nl->br (escape (comment c))))[br /]
                                           (time-format "~y/~m/~d ~h:~i" (created-at c))]))]
                                        ; comment form
                        [h3 "Leave your comment"]
                        [div (form-for/cont (post-comment/cont oid)
                               :class 'comment :submit "Post")])
                    [p "That page doesn't exist!"])]))

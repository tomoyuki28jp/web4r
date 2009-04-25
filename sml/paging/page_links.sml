[div :class "page_links"
     [ul (prev-link* pager params)
         (loop for page from link-start to link-end
               do (if (= page current-page)
                      [li [span page]]
                      [li [a :href (safe (concat "?" *page-param* "=" page params))
                             page]]))
         (next-link* pager params)]]

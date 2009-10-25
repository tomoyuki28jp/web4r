[div :class "page_links"
     [ul (prev-link* pager parameters)
         (loop for page from link-start to link-end
               do (if (= page current-page)
                      [li [span page]]
                      [li [a :href (concat "?" *page-param* "=" page parameters)
                             page]]))
         (next-link* pager parameters)]]

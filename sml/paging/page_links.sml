[div :class "page_links"
     [ul (loop for page from link-start to link-end
               do (if (= page current-page)
                      [li [span page]]
                      [li [a :href (concat "?page=" page) page]]))]]

(with-sml-file (sml-path "template.sml")
  (replace title [title "Show " cname])
  (append  head  [script :type "text/javascript" :src "/js/examples/showdown.js"])
  (append  head  [script :type "text/javascript" (safe "
window.onload = function() {
    var body = document.getElementById('wiki_body');
    var converter = new Attacklab.showdown.converter();
    body.innerHTML = converter.makeHtml(body.innerHTML);
}
")])
  (replace body
           [body (aif ins
                     [table :id "table_show"
                            (loop for s in slots do
                                  [tr [th (slot-label s)]
                                      [td :id (slot-id s)
                                          (slot-display-value it s)]])]
                   [p "That page doesn't exist"])]))

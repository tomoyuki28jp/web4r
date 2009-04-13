[input :type "checkbox" :value o :id oid :name (concat id "[]")
       (attr (form-valid-attr slot))
       (attr (when (equal o (or (post-parameter oid)
                                (when (member o saved :test #'equal) o)))
               '(:checked "checked"))) /]
[label :for oid o]

[input :type "checkbox" :value o :id oid :name (concat id "[]")
       (attr (form-valid-attr class slot ins))
       (attr (when (aand (or (posted-checkbox id) saved)
                         (member o it :test #'equal))
               '(:checked "checked"))) /]
[label :for oid o]

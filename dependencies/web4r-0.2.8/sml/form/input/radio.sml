[input :type "radio" :value o :id oid :name id
       (attr (form-valid-attr class slot ins))
       (attr (when (equal o value) '(:checked "checked"))) /]
[label :for oid o]

[input :type (if (eq input :password) "password" "text")
       :class (if (slot-required slot) "required" "")
       :name id :value value :id id :size size /]

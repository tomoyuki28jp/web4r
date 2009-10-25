(in-package #:lift-documentation)

#+(or)
(defmethod additional-markdown-extensions-for-system 
    append ((system (eql (asdf:find-system 'lift-documentation))))
  '(clcl))

(defmethod search-locations-for-system 
    append ((system (eql (asdf:find-system 'lift-documentation))))
  (list (asdf:system-relative-pathname 
	 'lift-documentation "website/source/resources/")
	(asdf:system-relative-pathname 
	 'lift-documentation "website/source/")
	(asdf:system-relative-pathname 
	 'lift-documentation "../shared//")
	))


(defsystem #:stw-xml-parse
  :depends-on ("closer-mop"
	       "stw-meta"
	       "stw-utils")
  :description ""
  :serial t
  :components ((:file "package")
	       (:file "meta")
	       (:file "condition")
	       (:file "entities")
	       (:file "model")
	       (:file "parse")
	       (:file "print")))

(defsystem #:stw-xml-parse
  :depends-on ("closer-mop"
	       "cl-comp"
	       "stw-utils")
  :description "An XML to DOM tree parser, evaluating elements and attributes into CLOS objects."
  :serial t
  :components ((:file "package")
	       (:file "meta")
	       (:file "condition")
	       (:file "entities")
	       (:file "model")
	       (:file "parse")
	       (:file "query")
	       (:file "print"))
    :long-description
    #.(uiop:read-file-string
       (uiop:subpathname *load-pathname* "docs/README.org"))
    :in-order-to ((test-op (load-op :stw-xml-test))))

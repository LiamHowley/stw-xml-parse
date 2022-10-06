(defsystem #:stw-xml-parse
  :author "Liam Howley <liam.howley@thespanningtreeweb.ie>"
  :license "MIT"
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
	       (:file "print")
	       (:file "read")
	       (:file "query"))
    :long-description
    #.(uiop:read-file-string
       (uiop:subpathname *load-pathname* "docs/README.org"))
    :in-order-to ((test-op (load-op :stw-xml-test))))

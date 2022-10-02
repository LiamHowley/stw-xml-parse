(defsystem #:stw-xml-test
  :description "Test suite for stw-xml-parse."
  :depends-on ("parachute" "stw-xml-parse")
  :serial t
  :components ((:file "package")
	       (:file "tests"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :xml.test)))

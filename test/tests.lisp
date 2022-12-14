(in-package xml.test)

(define-element-node name (attribute-node)
  ((first-name :initarg :first-name :type first-name :reader first-name)
   (second-name :initarg :second-name :type second-name :reader second-name)))

(define-element-node first-name (attribute-node)
  ())

(define-element-node second-name (attribute-node)
  ())

(define-element-node DOB (attribute-node)
  ((day :initarg :day :type day :reader day)
   (month :initarg :month :type month :reader month)
   (year :initarg :year :type year :reader year)))

(define-element-node day (attribute-node)
  ())

(define-element-node month (attribute-node)
  ())

(define-element-node year (attribute-node)
  ())

(define-element-node account (branch-node)
  (id
   (name :initarg :name :type name :reader name)
   (DOB :initarg :dob :type dob :reader dob)))

(defvar *account*
  "<account id=\"1\">
<name>
<first-name>Liam</first-name>
<second-name>Howley</second-name>
</name>
<DOB>
<day>1</day>
<month>Jan</month>
<year>2000</year>
</DOB>
</account>")

;; attribute nodes where child-nodes are also mapped to slots.

(defvar *parsed-account* (parse-document (make-instance 'xml-document-node :document *account*)))

(define-test attribute-nodes...
  :parent test-parse
  (is string= "Liam" (text (car (retrieve-text-nodes-from-parents *parsed-account* 'first-name))))
  (is string= "Howley" (text (car (retrieve-text-nodes-from-parents *parsed-account* 'second-name))))
  (is string= "1" (text (car (retrieve-text-nodes-from-parents *parsed-account* 'day))))
  (is string= "Jan" (text (car (retrieve-text-nodes-from-parents *parsed-account* 'month))))
  (is string= "2000" (text (car (retrieve-text-nodes-from-parents *parsed-account* 'year))))
  (is string= "1" (write-to-string (car (get-elements-by-tagname *parsed-account* "day"))))
  (is string= "Jan" (write-to-string (car (get-elements-by-tagname *parsed-account* "month"))))
  (is string= "2000" (write-to-string (car (get-elements-by-tagname *parsed-account* "year")))))


;; non attribute nodes - slots map to attributes

(define-element-node div ()
  (id
   (html-class :attribute "class" :initarg :class :initform nil :reader html-class)))

(define-element-node a ()
  (href target))

(define-element-node span ()
  ((html-class :attribute "class" :initarg :class :initform nil :reader html-class)))

(define-element-node img (leaf-node)
  (src))

(defvar *markup* "<div id='container1' class='container square'><a href=\"/test\" target=\"_blank\"><span class='square'>caption</span><img src='/my-img-server'/>><</a></div>")

(define-test parse-markup...
  :parent test-parse
  (fail (parse-document (make-instance 'xml-document-node :document *markup*)) 'multiple-value-error)
  (let ((parsed-markup (handler-bind ((multiple-value-error #'use-first-found-value))
			 (parse-document (make-instance 'xml-document-node :document *markup*)))))
    (of-type 'xml-document-node parsed-markup)
    (of-type 'element-node (car (slot-value parsed-markup 'child-nodes)))
    (of-type 'div (get-element-with-attribute parsed-markup "id"))
    (of-type 'div (get-element-with-attribute-value parsed-markup "id" "container1"))
    ;; this is xml where attribute values can only be a string. This should be false
    (let ((element (get-element-with-attribute-value parsed-markup "class" "container")))
      (true (typep element 'div)))
    (let ((element (get-element-with-attribute-value parsed-markup "class" "square")))
      (of-type 'span element))
    (let ((text-nodes (retrieve-text-nodes parsed-markup)))
      (is string= "caption" (text (car text-nodes)))
      (is string= ">" (text (cadr text-nodes)))
      (is string= "<" (text (caddr text-nodes)))
      (of-type 'span (parent-node (car text-nodes)))
      (of-type 'img (get-next-sibling (parent-node (car text-nodes)))))
    (is string= "caption" (text (car (retrieve-text-nodes-with-token parsed-markup "cap"))))
    (is string= "caption" (text (car (retrieve-text-nodes-with-tokens parsed-markup "cap" "tion"))))))



(define-test serialize...
  :parent test-parse
  (let ((parsed-markup (handler-bind ((multiple-value-error #'use-first-found-value))
			 (parse-document (make-instance 'xml-document-node :document *markup*)))))
    (is string= "<div id='container1' class='container'><a href='/test' target='_blank'><span class='square'>caption</span><img src='/my-img-server' />&gt;&lt;</a></div>" (serialize parsed-markup))
    (is string=  
	"<div id='container1' class='container'>
   <a href='/test' target='_blank'>
      <span class='square'>caption
      </span>
      <img src='/my-img-server' />&gt;&lt;
   </a>
</div>"
	(serialize parsed-markup t))
    (is string= "<account id='1'><name><first-name>Liam</first-name><second-name>Howley</second-name></name><dob><day>1</day><month>Jan</month><year>2000</year></dob></account>"
	(serialize *parsed-account*))
    (is string= 
	"<account id='1'>
   <name>
      <first-name>Liam
      </first-name>
      <second-name>Howley
      </second-name>
   </name>
   <dob>
      <day>1
      </day>
      <month>Jan
      </month>
      <year>2000
      </year>
   </dob>
</account>"
	(serialize *parsed-account* t))))


(define-test reader...
  :parent test-parse
  (when (readerp)
    (remove-reader))
  (true (set-reader))
  (let* ((document-node (read-from-string "<a href='/some-url'>url</a>"))
	 (child-node (car (slot-value document-node 'child-nodes))))
    (true (slot-exists-p child-node 'href))
    (is string= "/some-url" (slot-value child-node 'href))
    (is string= "url" (text (car (slot-value child-node 'child-nodes))))
    (of-type 'readtable (remove-reader))))

(define-test errors-and-generic-nodes...
  :parent test-parse
  (setf *mode* :strict)
  (fail (parse-document "<custom-node custom-slot='value'>a custom node</custom-node>") 'class-not-found-error)
  (setf *mode* :silent)
  (let* ((document-node (parse-document "<custom-node custom-slot='value'>a custom node</custom-node>"))
	 (child-node (car (slot-value document-node 'child-nodes))))
    (of-type 'generic-node child-node)
    (is string= "a custom node" (text (car (retrieve-text-nodes-with-token document-node "a custom node"))))
    (of-type 'generic-node (get-element-with-attribute document-node "custom-slot"))))

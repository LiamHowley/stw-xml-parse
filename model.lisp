(in-package xml.parse)

;;; ELEMENT-CLASS slots map element attributes, and the ELEMENT-CLASS class name maps the
;;; element name. Thus slots have a type of STW-DIRECT-ATTRIBUTE-DEFINITION
;;; unless overridden.  This allows for convenient caching of slot
;;; definitions.

;;; DOM-NODE and its immediate descendants remain instances of STANDARD-CLASS,
;;; and the slots those of STANDARD-DIRECT-SLOT-DEFINITION. They are also cached.

(defclass document-node ()
  ((file :initarg :file
	 :initform nil
	 :accessor file)
   (child-nodes :initarg :child-nodes
		:initform nil
		:type list
		:accessor child-nodes)
   (document :initarg :document
	     :type simple-string
	     :documentation "Document to be parsed."
	     :accessor document))
  (:documentation "For whole documents or document fragments, (e.g. template files)."))

(defclass xml-document-node (document-node)
  ())

(defclass dom-node ()
  ((parent-node :initarg :parent-node
		:initform nil
		:accessor parent-node))
  (:documentation "Element class for dom nodes."))

(defclass standard-element-node (dom-node)
  ())

(defclass element-node (standard-element-node)
  ())

(defclass branch-node (element-node)
  ((child-nodes :initarg :child-nodes
		:initform nil
		:type list
		:accessor child-nodes))
  (:documentation "Element that may contain child elements"))

(defclass leaf-node (element-node)
  ()
  (:documentation "Void element that contains only attributes and no content"))

(defclass content-node (element-node)
  ((the-content :initarg :the-content :initform nil :reader the-content)
   (closing-tag :reader closing-tag
		:initform (error "Closing tag requires value, as content-node elements are not evaluated")))
  (:documentation "Element that has attributes, and whose only child is a string."))

(defclass attribute-node (branch-node)
  ()
  (:documentation "If the element being parsed is an attribute of another class, 
setf the slot value of the relevant slot with the current node, and push to child-node
of parent-node. Attribute nodes are also branch-nodes to allow them to be extended in 
turn"))

(defclass text-node (dom-node)
  ((text :initarg :text
	 :initform ""
	 :type vector
	 :reader text)))

(defclass whitespace-node (text-node)
  ((text :initarg :text
	 :initform ""
	 :type vector
	 :reader text)))

(define-sgml-node sgml-node (standard-element-node)
  ((the-content :initarg :the-content :reader the-content)
   (closing-tag :initform ">")))




;;;;;;;;

(defmacro define-element-node (&whole form name &body parts)
  "Defines an element node. A default representation of a slot is offered, (including initarg, and accessor, 
which defaults to the slot name), so that a list of slots can be created via a list of symbols. Initform is
not set. The object will be initialized during parsing and leaving valueless slots unbound will reduce memory
requirements. Otherwise, slots can be manually defined. 

A note on inheritance: By default nodes are assumed to be branch-nodes. If not a BRANCH-NODE add LEAF-NODE or CONTENT-NODE
to the list of supers."
  (macrolet ((set-attr (attribute &optional value)
	       `(unless (member ,attribute slot)
		  (setf slot (append slot `(,,attribute ,,value))))))
    (let* ((parts (if (not (listp (car parts)))
		      (error "illegal option ~s in ~s."
			     (car parts) form)
		      parts))
	   (supers (let ((supers (car parts)))
		     (if (and supers
			      (loop for class in supers
				      thereis (map-filtered-precedents
					       (find-class class)
					       (lambda (class)
						 (or (typep class 'dom-node)
						     (subtypep class 'dom-node))))))
			 supers
			 (nconc supers (list 'branch-node)))))
	   (instance-slots (cadr parts))
	   (class-slots (cddr parts))
	   (slots (mapcar #'(lambda (slot)
			      (setf slot (ensure-list slot))
			      (set-attr :type 'simple-string)
			      (set-attr :initarg (intern (string-upcase (symbol-name (car slot))) 'keyword))
			      (set-attr :reader (car slot))
			      slot)
			  instance-slots)))
      `(progn
	 (defclass ,name ,supers
	   ,slots
	   ,@class-slots
	   ,@(unless (assoc :metaclass class-slots)
	       `((:metaclass element-class))))
	 (export ',name)
	 ,@(loop
	     for slot in slots
	     collect `(export ',(car slot)))))))


;;;; generic nodes

(defclass generic-node (branch-node)
  ((element-name :initform nil
		 :type string
		 :initarg :element-name
		 :reader class->element)
   (attributes :initform (make-hash-table :test #'equal)
	       :type hash-table
	       :reader attributes))
  (:documentation "When no other shoe fits, GENERIC-NODE is available. Not set by default,
but can be assigned and used by binding to the ASSIGN-GENERIC-NODE restart when CLASS-NOT-FOUND-ERROR
is invoked. GENERIC-NODE inherits from BRANCH-NODE, but changes class to GENERIC-LEAF-NODE if 
element is self-closing. CHANGE-CLASS is invoked in order to dispatch correctly when printing."))

(defclass generic-leaf-node (generic-node leaf-node)
  ())

(defmethod class->element ((node standard-class))
  nil)


(defmethod slot-unbound (class (node dom-node) slot-name)
  (setf (slot-value node slot-name) nil))


;; headers

(define-sgml-node ?xml (leaf-node)
  (version
   (encoding :initform nil)
   (standalone :initform nil)))


;; sgml / comments / DTDS etc

(define-sgml-node !-- (sgml-node)
  ((closing-tag :initarg :closing-tag :initform "-->"))
  (:documentation "comment string"))


(define-sgml-node ![CDATA[ (sgml-node)
  ((closing-tag :initarg :closing-tag :initform "]]>"))
  (:case . :upper)
  (:documentation "unparsed data"))

(define-sgml-node !DOCTYPE (sgml-node)
  ()
  (:case . :upper))

(in-package xml.parse)


;; elements

(defgeneric class->element (class)
  (:documentation "Element name associated with class"))

(defclass element-class (standard-class)
  ((element :initarg :element
	    :type string
	    :documentation "Name of mapped element."
	    :reader class->element)
   (closing-tag :initarg :closing-tag
		:initform nil
		:documentation "Specifically for content-nodes. Otherwise ignored.")
   (accepted-case :initarg :case
		  :type :keyword
		  :initform :any
		  :documentation "Case sensitivity. Accepts :upper, :lower and :any. Defaults to :any."
		  :reader accepted-case)
   (slot-index :type (or null trie)
		:documentation "Trie that maps attribute names to slots."
	       :initform nil
	       :reader slot-index)
   (permitted-content :initform nil
		      :type list
		      :initarg :permitted-content
		      :reader permitted-content)
   (permitted-parent :initform nil
		     :type list 
		     :initarg :permitted-parent
		     :reader permitted-parent)))



(defmethod (setf class->element) ((element string) (class element-class))
  (setf (slot-value class 'element) element
	(gethash element *element-class-map*) (class-name class)))

(defmethod (setf class->element) (element (class element-class))
  (error "element name must be a string"))

(defmethod attribute->slot ((attribute string) (class element-class))
  (with-slots (slot-index) class
    (car (find-word attribute slot-index))))

(defmethod attribute->slot (attribute (class element-class))
  (error "attribute must be a string"))


(defmethod validate-superclass
    ((class element-class)
     (superclass standard-class))
  t)

(defmethod validate-superclass
    ((class standard-class)
     (superclass element-class))
  t)


(defmethod shared-initialize :after ((class element-class) slot-names &key)
  (declare (ignore slot-names))
  (with-slots (element slot-index) class
    (unless (stringp element)
      (error "the element ~s is not a string." element))
    (setf slot-index (make-trie)
	  ;; map element to class
	  (gethash element *element-class-map*) class)
    (loop
      for slot in (filter-slots-by-type class 'xml-direct-slot-definition)
      for attribute = (slot-value slot 'attribute)
      when attribute
	do (setf (slot-index attribute class) slot))))


(defmethod slot-unbound (class (instance element-class) (slot-name (eql 'element)))
  (with-slots (accepted-case) instance
    (let ((element (symbol-name (class-name instance))))
      (setf (slot-value instance 'element)
	    (if (eq accepted-case :upper)
		(string-upcase element)
		(string-downcase element))))))



(defvar *element-class-map* (make-hash-table :test #'equal))

;; attributes

(deftype multiple-attributes ()
  "Type for slots that contain more than one potential attribute."
  '(or null cons))

;;; definition

(defclass xml-direct-slot-definition (standard-direct-slot-definition)
  ((attribute :initarg :attribute
	      :initform nil
	      :type string
	      :documentation "find attribute given slot name"
	      :reader slot-definition-attribute)
   (expected-value :initarg :expected-value
		   :initform nil
		   :type list
		   :documentation "reference values to match against"
		   :reader expected-value)
   (status :initform :active
	   :initarg :status
	   :type keyword
	   :documentation "catch or flag deprecated or obsolete elements"
	   :reader status)))

(defmethod direct-slot-definition-class
    ((class element-class) &key &allow-other-keys)
  (find-class 'xml-direct-slot-definition))

(defclass xml-effective-slot-definition (standard-effective-slot-definition)
  ())

(defvar *effective-slot-definition*)

(defmethod effective-slot-definition-class
    ((class element-class) &key &allow-other-keys)
  (if *effective-slot-definition*
      *effective-slot-definition*
      (call-next-method)))


(defmethod compute-effective-slot-definition
    ((class element-class) name direct-slot-definitions)
  (declare (ignore name))
  (flet ((directp (slot) (typep slot 'xml-direct-slot-definition))) 
    (let ((*effective-slot-definition*
	   (when (find-if #'directp direct-slot-definitions)
	     (find-class 'xml-effective-slot-definition))))
      (call-next-method))))


(defmethod initialize-instance :after ((slot xml-direct-slot-definition) &key)
  (with-slots (attribute) slot
    (let ((slot-name (slot-definition-name slot)))
      (unless attribute
	(setf attribute (string-downcase slot-name)))
      (unless (stringp attribute)
	(error "the attribute ~s is not a string." attribute)))))


(defmethod (setf slot-index) ((slot xml-direct-slot-definition) (attribute string) (class element-class))
  "Writer for binding a slot to an attribute. Invoked when a slot-missing-error is handled."
  (unless (slot-value slot 'attribute)
    (setf (slot-value slot 'attribute) attribute))
  (insert-word attribute (slot-index class)
	       (list slot
		     (slot-definition-name slot)
		     (slot-definition-type slot)
		     attribute
		     (length attribute))))


;;; sgml-class

(defvar *element-class-trie* (make-trie))

(defclass sgml-class (element-class)
  ((closing-tag :initform ">" :initarg :closing-tag)))

(defmethod shared-initialize :after ((class sgml-class) slot-names &key)
  (declare (ignore slot-names))
  (with-slots (element accepted-case) class
    (unless (stringp element)
      (error "the element ~s is not a string." element))
      (insert-word element *element-class-trie* class)))

(defmacro define-sgml-node (name &body body)
  `(defclass ,name
       ,@body
     (:metaclass sgml-class)))

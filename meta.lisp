(in-package xml.parse)


;; elements

(defgeneric class->element (class)
  (:documentation "Element name associated with class"))

(defclass element-class (standard-class)
  ((element :initarg :element
	    :type string
	    :initform nil
	    :documentation "Name of mapped element."
	    :reader class->element)
   (accepted-case :initarg :case
		  :type :keyword
		  :initform :any
		  :documentation "Case sensitivity. Accepts :upper, :lower and :any. Defaults to :any."
		  :reader accepted-case)
   (slot-index :type (or null trie)
		:documentation "Trie that maps attribute names to slots."
		:initform nil)
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
    ((superclass standard-class)
     (class element-class))
  t)


(defmethod shared-initialize :after ((class element-class) slot-names &key)
  (declare (ignore slot-names))
  (with-slots (element slot-index accepted-case) class
    (setf slot-index (make-trie))
    (loop for slot in (filter-slots-by-type class 'xml-direct-slot-definition)
	  for attribute = (slot-value slot 'attribute)
	  when attribute
	    do (insert-word attribute
			    slot-index
			    (list slot
				  (slot-definition-name slot)
				  (slot-definition-type slot)
				  attribute
				  (length attribute))
			    nil))
    (when element
      (unless (stringp element)
	(error "the element ~s is not a string." element)))
    (setf element (if element
		      element
		      (let ((element (symbol-name (class-name class))))
			(if (eq accepted-case :upper)
			    (string-upcase element)
			    (string-downcase element))))
	  ;; map element to class
	  (gethash element *element-class-map*) class)))



;; attributes

(defvar *element-class-map* (make-hash-table :test #'equal))

;;; definition

(defclass xml-direct-slot-definition (standard-direct-slot-definition)
  ((package :initarg :package
	    :initform :attribute
	    :type string
	    :documentation "for package names such xmlns."
	    :reader print-packagep)
   (attribute :initarg :attribute
	      :initform nil
	      :type string
	      :documentation "find attribute given slot name"
	      :reader slot-definition-attribute)
   (expected-value :initarg :expected-value
		   :initform nil
		   :type cons
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
	(setf attribute (string-downcase slot-name))
	(unless (stringp attribute)
	  (error "the attribute ~s is not a string." attribute))))))


;;; sgml-class

(defvar *element-class-trie* (make-trie))

(defclass sgml-class (element-class)
  ())

(defmethod shared-initialize :after ((class sgml-class) slot-names &key)
  (declare (ignore slot-names))
  (with-slots (element accepted-case) class
    (when element
      (unless (stringp element)
	(error "the element ~s is not a string." element)))
      (setf element
	    (cond (element
		   (string-upcase element))
		  ((eq accepted-case :lower)
		   (string-downcase (symbol-name (class-name class))))
		  (t (symbol-name (class-name class)))))
      (insert-word element *element-class-trie* class nil)))

(defmacro define-sgml-node (name &body body)
  `(defclass ,name
       ,@body
     (:metaclass sgml-class)))

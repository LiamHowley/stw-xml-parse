(in-package xml.parse)

(defvar *element-name*)

(defvar *next-attribute*)

(defvar *next-element*)

(defvar *element-tags*)

(defvar *preserve-whitespace* nil
  "New line and indentation? t or nil. Don't set directly.")

;;; reader functions

(defgeneric read-content (node)
  (:documentation "Read text-node, content-node content, etc."))

(defgeneric read-attribute-value (slot attribute slot-type)
  (:documentation "Read attribute value."))

(defgeneric read-attribute (node)
  (:documentation "Read an attribute."))

(defgeneric read-element-attributes (node)
  (:documentation "Read attributes one at a time."))

(defgeneric read-into (output predicate)

  (:documentation "READ-INTO is called to read a portion of document
designated by predicate and specialised output on (EQL <type>).")

  (:method 
      (output predicate)
    (parse-value output (funcall (read-and-decode predicate))))

  (:method
      ((output (eql 'cons)) predicate)
    (let ((list)
	  (reader (read-and-decode #'(lambda (char)
				       (or (funcall predicate char)
					   (char= char #\space))))))
      (loop
	(multiple-value-bind (token char)
	    (funcall reader)
	  (case char
	    (#\space
	     (next)
	     (push token list))
	    (t (push token list)
	     (return)))))
      (nreverse list)))

  (:method
      ((output (eql 'array)) predicate)
    (let ((list (read-into 'cons predicate)))
      (the array (make-array (length list) :element-type 'simple-array :initial-contents list)))))

(defgeneric read-subelements (node)
  (:documentation "Read sub-elements and add to dom tree."))

(defgeneric read-element (node)
  (:documentation "READ-ELEMENT is context specific. Whilst similar, XML reads 
differently to HTML and wildly so to JSON and other serialization formats."))

(defgeneric read-element-name ()
  (:documentation "Names are context specific.")
  (:method () nil))


(defgeneric prepare-slot (class slot)
  (:documentation "Use this function to setup any objects / tables / arrays etc. 
in which multiple values are stored"))

(defgeneric parse-value (output value)
  (:documentation "Return value according to type"))

(defgeneric get-element-name ())


(defgeneric parse-document (document &key)
  (:documentation "Requires a document node and an optional parsing function 
that accepts a document node. Makes best effort to maintain coherence and 
avoid trailing/malformed tags/text.")

  (:method
      ((document string) &key (parser #'read-element) preserve-whitespace (overwrite t))
    (parse-document (make-instance 'document-node :document document)
		    :parser parser
		    :overwrite overwrite
		    :preserve-whitespace preserve-whitespace))

  (:method
      ((file pathname) &key (parser #'read-element) preserve-whitespace (overwrite t))
    (let ((seq (sequence-from-file file)))
      (parse-document (make-instance 'xml-document-node :document seq :file file)
		      :parser parser
		      :overwrite overwrite
		      :preserve-whitespace preserve-whitespace)))

  (:method
      ((document-node xml-document-node) &key (parser #'read-element) preserve-whitespace (overwrite t))
    (let ((*element-name* (read-until (match-character #\> #\space #\/ #\!)))
	  (*next-attribute* (read-until (match-character #\space #\= #\> #\/)))
	  (*next-element* (read-and-decode (match-character #\<)))
	  (*element-tags* (read-until (match-character #\< #\>)))
	  (*end-sgml* (read-until (match-character #\>)))
	  (*end-comment* (read-until (match-string "-->")))
	  (*end-cdata* (read-until (match-string "]]>")))
	  (*consume-whitespace* (consume-while #'whitespacep)))
      (with-slots (file) document-node
	(unless (slot-boundp document-node 'document)
	  (setf (slot-value document-node 'document) (sequence-from-file file))))
      (call-next-method document-node
			:parser parser
			:preserve-whitespace preserve-whitespace
			:overwrite overwrite)))

  (:method
      ((document document-node) &key (parser #'read-element) preserve-whitespace (overwrite t))
    (declare (inline parse%))
    (when overwrite
      (setf (slot-value document 'child-nodes) nil))
    (let ((*preserve-whitespace* preserve-whitespace))
      (parse% parser document))))


(defgeneric bind-child-node (parent-node child-node)

  (:documentation "Controls the binding of element nodes. At its most basic
parent-node is set as the child-node's parent-node, and vice versa. More complex
interactions can be devised with method specialization.")

  (:method (parent-node child-node)
    nil)

  (:method 
      :around ((parent-node branch-node) (child-node dom-node))
    (call-next-method)
    parent-node)

  (:method 
      ((parent-node branch-node) (child-node dom-node))
    (push child-node (slot-value parent-node 'child-nodes))
    (setf (slot-value child-node 'parent-node) parent-node))

  (:method 
      ((parent-node document-node) (child-node dom-node))
    (push child-node (slot-value parent-node 'child-nodes))
    (setf (slot-value child-node 'parent-node) parent-node))

  (:method 
      ((parent-node branch-node) (child-node attribute-node))
    (let ((child-name (class-name (class-of child-node))))
      (when (slot-exists-p parent-node child-name)
	(setf (slot-value parent-node child-name) child-node))
      (call-next-method))))


(defun parse% (fn node)
  (let* ((*char-index* 0)
	 (*line-number* 1)
	 (*document* (slot-value node 'document))
	 (*length* (array-total-size *document*))
	 (*decoder* (get-decoder *document*)))
    (declare (fixnum *char-index* *line-number* *length*)
	     (simple-string *document*))
    (loop
      while (and (< *char-index* *length*)
		 (funcall fn node)))
    (with-slots (child-nodes) node
      (setf child-nodes (nreverse child-nodes)))
    node))


(defmethod tag-open-char ((node xml-document-node))
  #\<)


(defun read-into-object ()
  "Retrieve element-class or string and find appropriate node"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (class name)
      (read-element-name)
    (cond (class
	   (make-instance class :stw-reader t))
	  (name
	   (restart-case
	       (class-not-found-error "There is no class matching the element name ~a" name)
	     (assign-generic-node ()
	       :report "Use GENERIC-NODE"
	       (make-generic-node name))))
	  (t
	   ;; as read-into-object was invoked due to encountering
	   ;; an opening character.
	   (prog1
	       (make-instance 'text-node
			      :text (make-string 1 :initial-element (tag-open-char)))
	     (next))))))



;;; initialization

(defmethod initialize-instance ((declaration ?xml) &key stw-reader)
  (when stw-reader
    (call-next-method))
  (unless (slot-value declaration 'version)
    (error "xml version required with declaration."))
  declaration)


(defmethod initialize-instance ((node sgml-node) &key stw-reader)
  (call-next-method)
  (when stw-reader
    (read-content node)
    (when (eq (stw-read-char) #\>)
      (next)))
  node)


(defmethod initialize-instance ((node element-node) &key stw-reader ignore-attributes)
  (call-next-method)
  (when stw-reader
    (unless ignore-attributes
      (read-element-attributes node)))
    ;;(when (char= (stw-read-char) #\>)
    ;;  (next)))
  node)


(defmethod initialize-instance :after ((node branch-node) &key stw-reader)
  (when stw-reader
    (read-subelements node))
  node)


(defmethod initialize-instance :after ((node content-node) &key stw-reader)
  (when stw-reader
    (read-content node))
  node)


(defun make-generic-node (name)
  (let* ((node (make-instance 'generic-node :element-name name :stw-reader nil))
	 ;; using throw and catch instead of a more straight forward return to catch
	 ;; the semantic meaning of the return value.
	 (self-closing (catch 'self-closing
			 (read-element-attributes node))))
    (if self-closing
	(change-class node 'generic-leaf-node)
	(read-subelements node))
    node))


(defgeneric read-element (node)
  (:documentation "READ-ELEMENT is context specific. Whilst similar, XML reads 
differently to HTML and wildly so to JSON and other serialization formats.")

  (:method
      :around (node)
    (handler-bind ((class-not-found-error
		     (take-action c (assign-generic-node))))
      (call-next-method)))

  (:method (node)
    (error "No elements found in global context."))

  (:method ((node document-node))
    ;; first skip newlines tabs etc.
    (funcall *consume-whitespace*)
    ;; now read
    (let ((char (stw-read-char)))
      (cond ((eq char (tag-open-char node))
	     (next)
	     (bind-child-node node (read-into-object)))
	    ((eq :eof char) nil)
	    (t
	     (awhen (action-p)
	       (funcall self "Invalid character at beginning of xml document"))
	     nil)))))


(defmethod read-element-name ()
  (multiple-value-bind (name char)
      (funcall *element-name*)
    (case char
      (#\!
       (get-element-name))
      (t
       (let ((name (string-downcase name)))
	 (aif (gethash name *element-class-map*)
	      (values self name)
	      (values nil name)))))))


(defmethod get-element-name ()
  (declare (optimize (safety 0) (speed 3)))
  (let ((result)
	(foundp (walk-branch *element-class-trie*))
	(end nil)
	(depth 0))
    (declare (function foundp)
	     (boolean end)
	     (fixnum depth))
    (loop
      for index of-type fixnum from *char-index* to *length*
      for char = (the character (aref *document* index))
      for next = (unless end
		   (funcall foundp char))
      when next
	do (setf result (trie-leaf next))
      when result
	do (setf depth index)
      unless next 
	do (setf end t) 
      when end
	do (cond ((eql depth index)
		  (setf *char-index* index)
		  (return result))
		 (t 
		  (return (values nil (funcall *element-name*))))))))


(defmethod read-subelements ((node branch-node))
  (declare (inline action-p stw-read-char))
  (loop
    for char = (stw-read-char)
    while char
    do (case char
	 (:eof
	  (return))
	 (#\> 
	  (next)
	  (read-whitespace node)
	  (case (stw-read-char)
	    (#\<
	     nil)
	    (t 
	     (read-content node))))
	 (#\<
	  (case (stw-peek-next-char)
	    (#\/
	     ;; Closing tag found.
	     ;; Consume #\< and #\/ characters and compare with opening tag
	     (next 2)
	     (let ((closing-tag (funcall *element-tags*)))
	       (awhen (action-p *mode*)
		 (let ((opening-tag (or (class->element (class-of node))
					(class->element node))))
		   ;; closing-tag is consumed.
		   (unless (string= opening-tag closing-tag)
		     (funcall self "mismatch between opening tag: ~a and closing tag: ~a."
			      opening-tag closing-tag))))
	       (return)))
	    (#\<
	     ;; Stray tag, render as text and encode when printed.
	     (bind-child-node node (make-instance 'text-node :text "<"))
	     (next))
	    (t
	     (next)
	     (read-whitespace node)
	     ;; bind child-nodes at this point.
	     (bind-child-node node (read-into-object)))))
	 (t 
	  (read-whitespace node)
	  (read-content node)))))


(defmethod read-subelements :around ((node branch-node))
  (call-next-method)
  (with-slots (child-nodes) node
    (when child-nodes
      (setf child-nodes
	    (nreverse child-nodes)))
    node))


(defmethod read-into
    ((output (eql 'boolean)) predicate)
  (let ((value (funcall (read-and-decode #'(lambda (char)
					     (funcall predicate char))))))
    (cond ((string-equal "true" value)
	   (the boolean t))
	  ((string-equal "false" value)
	   (the boolean nil))
	  (t (error "The value ~a is not a boolean" value)))))


(defmethod read-attribute-value
    ((slot xml-direct-slot-definition) attribute slot-type)
  (let ((char (stw-read-char))
	(type slot-type))
    (case char
      (#\=
       (next)
       (read-attribute-value slot attribute slot-type))
      ((#\" #\')
       (next)
       (read-into type #'(lambda (test-char)
			   (char= char test-char))))
      (t
       (error "xml attributes missing quotes")))))


(defmethod assign-value
    ((class element-node) (slot xml-direct-slot-definition) slot-name attribute value)
  (declare (ignore attribute))
  (setf (slot-value class slot-name) value))


(defmethod read-attribute ((class element-node))
  (multiple-value-bind (slot slot-name slot-type attribute)
      (map-attribute-to-slot class)
    (if slot
	(assign-value class slot slot-name attribute
		      (read-attribute-value slot attribute slot-type))
	(let ((start *char-index*)
	      (attribute (funcall *attribute-name*)))
	  (restart-case
	      (slot-not-found-error "There are no slots representing the attribute ~a"
				    attribute)
	    (assign-slot-to-attribute (slot)
	      :report "Use a different slot to permanently represent the attribute."
	      :interactive (lambda () 
			     (princ "Please specify a slot to use: ")
			     (list (read)))
	      (setf (slot-index attribute (class-of class))
		    (find-slot-definition (class-of class) slot 'xml-direct-slot-definition)
		    *char-index* start)
	      (read-attribute class))
	    (ignore-missing-slot ()
	      :report "Ignore attribute."
	      (funcall *skip-attribute*)
	      nil))))))


(defmethod read-element-attributes ((node element-node))
  (loop
    (let ((char (stw-read-char)))
      (case char
	((#\> nil)
	 (return))
	((#\" #\' #\/ #\newline #\space #\tab)
	 (next))
	(t 
	 (read-attribute node))))))



;;; attributes for generic-node

(defmethod read-attribute-value (slot attribute slot-type)
  (let ((char (stw-read-char)))
    (case char
      (#\=
       (next)
       (read-attribute-value slot attribute slot-type))
      ((#\" #\')
       (funcall (read-and-decode #'(lambda (test-char)
				     (char= char test-char)))))
      (t
       (error "xml attributes missing quotes")))))


(defmethod assign-value
    ((class generic-node) slot slot-name attribute value)
  (declare (ignore slot))
  (setf (gethash attribute (slot-value class 'attributes)) value))


(defmethod read-attribute ((class generic-node))
  (let ((attribute (funcall *next-attribute*))
	(slot (find-slot-definition (find-class 'generic-node) 'attributes)))
    (assign-value class slot nil attribute (read-attribute-value slot attribute (slot-definition-type slot)))))


(defmethod read-element-attributes ((node generic-node))
  (loop
    (let ((char (stw-read-char)))
      (case char
	((#\> nil)
	 (return))
	((#\" #\' #\newline #\space)
	 (stw-read-char))
	(#\/ 
	 (stw-read-char)
	 (throw 'self-closing t))
	(t 
	 (read-attribute node))))))


(defmethod map-attribute-to-slot ((node element-node))
  (declare (optimize (safety 0) (speed 3))
	   (inline walk-branch trie-leaf next))
  (let ((result)
	(foundp (walk-branch (slot-value (class-of node) 'slot-index))))
    (declare (function foundp) (list result))
    (loop
      for index of-type fixnum from *char-index* below *length*
      for char = (the character (aref *document* index))
      for next = (or (funcall foundp char)
		     (funcall foundp #\*))
      when next
	do (setf result (trie-leaf next))
      unless next
	do (if result
	       (destructuring-bind (slot slot-name slot-type attribute length)
		   result
		 (declare (fixnum length))
		 (return (values slot slot-name slot-type (map-attribute slot-name attribute length))))
	       (return (values nil nil nil attribute))))))


(defmethod map-attribute (result attribute length)
  (declare (fixnum length)
	   (ignore resuilt))
  (next length)
  attribute)



;;; read non jvaluated content and sub-elements

(defmethod read-content ((node sgml-node))
  (with-slots (the-content closing-tag) node
    (setf the-content (string-downcase (funcall (read-until (match-string closing-tag)))))))

(defmethod read-content ((node !--))
  (with-slots (the-content) node
    (setf the-content (funcall *end-comment*))))

(defmethod read-content ((node ![CDATA[))
  (with-slots (the-content) node
    (setf the-content (funcall *end-cdata*))))

(defmethod read-content ((node content-node))
  (with-slots (the-content closing-tag) node
    (next)
    (setf the-content (funcall closing-tag))))

(defmethod read-content (node)
  (awhen (funcall *next-element*)
    (bind-child-node node (make-instance 'text-node :text self))))



(defmethod read-whitespace (node)
  (multiple-value-bind (start end)
      (funcall *consume-whitespace*)
    (when (and *preserve-whitespace*
	       (> end start))
      (let ((whitespace (subseq *document* start end)))
	(bind-child-node node (make-instance 'whitespace-node :text whitespace))))))



;;; read attributes values

(declaim (ftype (function (string &optional float) float) float%)
	 (inline float%))

(defun float% (string &optional prototype)
  (declare (optimize (speed 3)(safety 0)))
  (let ((num (read-from-string value)))
    (the float 
	 (etypecase num
	   (float num)
	   (number (aif prototype
			(float num self)
			(float num)))))))


(defmethod parse-value (output value)
  value)

(defmethod parse-value (output (value string))
  (the string value))

(defmethod parse-value ((output (eql 'fixnum)) (value string))
  (the fixnum (parse-integer value :junk-allowed t)))

(defmethod parse-value ((output (eql 'float)) (value string))
  (float% num))

(defmethod parse-value ((output (eql 'short-float)) (value string))
  (float% num 0s0))

(defmethod parse-value ((output (eql 'single-float)) (value string))
  (float% num 0s0))

(defmethod parse-value ((output (eql 'double-float)) (value string))
  (float num 0d0))

(defmethod parse-value ((output (eql 'symbol)) (value string))
  (the symbol (intern value)))

(defmethod parse-value ((output (eql 'keyword)) (value string))
  (the keyword (intern value 'keyword)))



(defgeneric write-to-file (file object &key)

  (:method
      (file (object string) &key (if-does-not-exist :create) (if-exists :supersede))
    (sequence-to-file file object if-exists if-does-not-exist))

  (:method
      (file (object document-node) &key (if-does-not-exist :create) (if-exists :supersede))
    (sequence-to-file file (write-to-string object) if-exists if-does-not-exist)))

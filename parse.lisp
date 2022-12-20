(in-package xml.parse)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *opening-char* #\<)

  (defvar *xml-env-functions*
    (let ((table (make-hash-table :test #'eq :size 16)))
      (flet ((set-reader (sym func)
	       (setf (gethash sym table) func)))
	(set-reader :element-delimiter (read-until (match-character #\> #\space #\/ #\!)))
	(set-reader :attribute-delimiter (read-until (match-character #\> #\space #\/)))
	(set-reader :attribute-name (read-until (match-character #\= #\space #\>)))
	(set-reader :next-attribute (read-until (match-character #\space #\= #\> #\/)))
	(set-reader :next-element (read-and-decode (match-character *opening-char*)))
	(set-reader :element-tags (read-until (match-character #\< #\>)))
	(set-reader :end-sgml (read-until (match-character #\>)))
	(set-reader :end-comment (read-until (match-string "-->")))
	(set-reader :end-cdata (read-until (match-string "]]>")))
	(set-reader :skip-attribute (consume-until (match-character #\space #\>)))
	(set-reader :read-until-single-quote (read-and-decode (match-character #\space #\')))
	(set-reader :read-until-double-quote (read-and-decode (match-character #\space #\"))))
      table)))
  
(defun set-reader-function (sym func)
  (setf (gethash sym *xml-env-functions*) func))

(defmacro call-reader (sym)
  `(funcall (gethash ,sym *xml-env-functions*)))

(defvar *preserve-whitespace* nil
  "New line and indentation? Boolean. Don't set directly.")

;;; reader functions

(defgeneric read-content (node)
  (:documentation "Read text-node, content-node content, etc."))

(defgeneric read-attribute-value (slot attribute slot-type)
  (:documentation "Read attribute value."))

(defgeneric read-attribute (node)
  (:documentation "Read an attribute."))

(defgeneric read-element-attributes (node)
  (:documentation "Read attributes one at a time."))

(defgeneric read-into (output reader)

  (:documentation "READ-INTO is called to read a portion of document
designated by predicate and specialised output on (EQL <type>).")

  (:method 
      (output (reader symbol))
    (parse-value output (call-reader reader)))

  (:method
    ((output (eql 'boolean)) (reader symbol))
  (let ((value (call-reader reader)))
    (cond ((string-equal "true" value)
	   (the boolean t))
	  ((string-equal "false" value)
	   (the boolean nil))
	  (t (error "The value ~a is not a boolean" value)))))

  (:method
      ((output (eql 'cons)) (reader symbol))
    (let ((list))
      (loop
	(multiple-value-bind (token char)
	    (call-reader reader)
	  (case char
	    (#\space
	     (next)
	     (push token list))
	    (t (push token list)
	     (return)))))
      (nreverse list)))

  (:method
      ((output (eql 'list)) (reader symbol))
    (read-into 'cons (call-reader reader)))
  
  (:method
      ((output (eql 'array)) (reader symbol))
    (let ((list (read-into 'cons (call-reader reader))))
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

(defgeneric assign-value (class slot-name attribute value)
  (:documentation "Assign value to slot according to type"))

(defgeneric parse-value (output value)
  (:documentation "Return value according to type"))

(defgeneric get-element-name ())

(defgeneric parse-document (document &key)
  (:documentation "Requires a document node and an optional parsing function 
that accepts a document node. Makes best effort to maintain coherence and 
avoid trailing/malformed tags/text.")

  (:method
      ((document string) &key (parser #'read-element) preserve-whitespace)
    (parse-document (make-instance 'xml-document-node :document document)
		    :parser parser
		    :preserve-whitespace preserve-whitespace))

  (:method
      ((file pathname) &key (parser #'read-element) preserve-whitespace)
    (parse-document (make-instance 'xml-document-node :file file)
		    :parser parser
		    :preserve-whitespace preserve-whitespace))

  (:method
      ((document-node xml-document-node) &key (parser #'read-element) preserve-whitespace)
    (declare (ignore parser preserve-whitespace)
	     (inline parse-stream))
    (with-slots (file) document-node
      (unless (slot-boundp document-node 'document)
	(setf (slot-value document-node 'document) 
	      (with-open-file (in file :direction :input)
		(with-output-to-string (out)
		  (parse-stream in out)))))
      (call-next-method)))

  (:method
      ((document document-node) &key (parser #'read-element) preserve-whitespace)
    (declare (inline parse%))
    (let ((*preserve-whitespace* preserve-whitespace))
      (parse% parser document))))


(defgeneric bind-child-node (parent-node child-node)

  (:documentation "Controls the binding of element nodes. At its most basic
parent-node is set as the child-node's parent-node, and vice versa. More complex
interactions can be devised with method specialization.")

  (:method (parent-node child-node)
    nil)

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
	     (assign-generic-node (c)
	       :report "Use GENERIC-NODE"
	       (declare (ignore c))
	       (make-generic-node name))))
	  (t
	   ;; as read-into-object was invoked due to encountering
	   ;; an opening character.
	   (prog1
	       (make-instance 'text-node
			      :text (make-string 1 :initial-element *opening-char*))
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
  node)


(defmethod initialize-instance :after ((node branch-node) &key stw-reader)
  (when stw-reader
    (unless (char= (stw-peek-last-char) #\/)
      (read-subelements node)))
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
		     (take-action c (assign-generic-node c))))
      (call-next-method)))

  (:method (node)
    (error "No elements found in global context."))

  (:method ((node document-node))
    (declare (inline consume-whitespace))
    ;; first skip newlines tabs etc.
    (consume-whitespace)
    ;; now read
    (let ((char (stw-read-char)))
      (cond ((eq char *opening-char*)
	     (next)
	     (bind-child-node node (read-into-object)))
	    ((eq :eof char) nil)
	    (t
	     (awhen (action-p)
	       (funcall self "Invalid character at beginning of xml document"))
	     nil)))))


(defmethod read-element-name ()
  (multiple-value-bind (name char)
      (call-reader :element-delimiter)
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
		  (return (values nil (call-reader :element-delimiter))))))))


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
	     (let ((closing-tag (call-reader :element-tags)))
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


(declaim (ftype (function (character) keyword) attribute-value-reader)
	 (inline attribute-value-reader))

(defun attribute-value-reader (char)
  (declare (optimize (speed 3) (safety 0)))
  (ecase char
    (#\' :read-until-single-quote)
    (#\" :read-until-double-quote)))


(defmethod read-attribute-value
    ((slot xml-direct-slot-definition) attribute slot-type)
  (let ((char (stw-read-char)))
    (case char
      (#\=
       (next)
       (read-attribute-value slot attribute slot-type))
      ((#\" #\')
       (next)
       (multiple-value-bind (value char%)
	   (call-reader (attribute-value-reader char))
	 (cond ((char= char% char)
		value)
	       ((char= char% #\space)
		(restart-case
		    (multiple-value-error "XML does not support attributes with multiple values." attribute)
		  (use-value (user-supplied)
		    user-supplied)
		  (use-first-found-value (c)
		    :report "Use first found value and skip the rest"
		    (declare (ignore c))
		    (funcall (consume-until (match-character char)))
		    value)
		  (ignore-attribute (c)
		    :report "Ignore all values."
		    (declare (ignore c))
		    (funcall (consume-until (match-character char)))
		    nil))))))
      (t
       (error "xml attributes missing quotes")))))


(defmethod assign-value
    ((class element-node) slot-name attribute value)
  (declare (ignore attribute))
  (when value
    (setf (slot-value class slot-name) value)))


(defmethod read-attribute ((class element-node))
  (multiple-value-bind (slot slot-name slot-type attribute length)
      (map-attribute-to-slot class)
    (if slot
	(assign-value class
		      slot-name
		      (map-attribute slot-name attribute length)
		      (read-attribute-value slot attribute slot-type))
	(let ((start *char-index*)
	      (attribute (call-reader :attribute-name)))
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
	    (ignore-missing-slot (c)
	      :report "Ignore attribute."
	      (declare (ignore c))
	      (call-reader :skip-attribute)
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
       (next)
       (multiple-value-bind (value char%)
	   (call-reader (attribute-value-reader char))
	 (cond ((char= char% char)
		value)
	       ((char= char% #\space)
		(restart-case (multiple-value-error attribute value)
		  (use-first-found-value (c)
		    :report "Use first found value and skip the rest"
		    (declare (ignore c))
		    (funcall (consume-until (match-character char)))
		    value)
		  (ignore-attribute (c)
		    :report "Ignore all values."
		    (declare (ignore c))
		    (funcall (consume-until (match-character char)))
		    nil))))))
      (t
       (error "xml attributes missing quotes")))))


(defmethod assign-value
    ((class generic-node) slot-name attribute value)
  (declare (ignore slot-name))
  (when value
    (setf (gethash attribute (slot-value class 'attributes)) value)))


(defmethod read-attribute ((class generic-node))
  (let ((attribute (call-reader :next-attribute))
	(slot (find-slot-definition (find-class 'generic-node) 'attributes 'standard-direct-slot-definition)))
    (assign-value class nil attribute (read-attribute-value slot attribute (slot-definition-type slot)))))


(defmethod read-element-attributes ((node generic-node))
  (loop
    (let ((char (stw-read-char)))
      (case char
	((#\> nil)
	 (return))
	((#\" #\' #\newline #\space #\tab)
	 (next))
	(#\/ 
	 (next)
	 (throw 'self-closing t))
	(t 
	 (read-attribute node))))))

(declaim (inline map-attribute-to-slot))

(defun map-attribute-to-slot (node)
  (declare (optimize (safety 0) (speed 3))
	   (element-node node)
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
	       (return (values-list result))
	       (return)))))


(defmethod map-attribute (result attribute length)
  (declare (fixnum length)
	   (ignore result))
  (next length)
  attribute)



;;; read non evaluated content and sub-elements

(defmethod read-content ((node sgml-node))
  (with-slots (the-content closing-tag) node
    (setf the-content (string-downcase (call-reader closing-tag)))))

(defmethod read-content ((node content-node))
  (with-slots (the-content closing-tag) node
    (next)
    (setf the-content (call-reader closing-tag))))

(defmethod read-content (node)
  (awhen (call-reader :next-element)
    (bind-child-node node (make-instance 'text-node :text self))))



(defmethod read-whitespace (node)
  (declare (inline consume-whitespace))
  (multiple-value-bind (start end)
      (consume-whitespace)
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

(defmethod parse-value ((output (eql 'simple-string)) (value number))
  (the simple-string (write-to-string value)))

(defmethod parse-value (output (value string))
  (the string value))

(defmethod parse-value ((output (eql 'real)) (value string))
  (the fixnum (parse-integer value :junk-allowed t)))

(defmethod parse-value ((output (eql 'integer)) (value string))
  (the fixnum (parse-integer value :junk-allowed t)))

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

(in-package xml.parse)

(defvar *opening-char* #\<)

(defvar *stray-tags*)

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

(defgeneric read-into (output predicate)

  (:documentation "READ-INTO is called to read a portion of document
designated by predicate and specialised output on (EQL <type>).")

  (:method 
      (output (predicate function))
    (parse-value output (read-and-decode predicate)))

  (:method
      ((output (eql 'boolean)) (predicate function))
    (let ((value (read-and-decode predicate)))
      (cond ((string-equal "true" value)
	     (the boolean t))
	    ((string-equal "false" value)
	     (the boolean nil))
	    (t (error "The value ~a is not a boolean" value)))))

  (:method
      ((output (eql 'cons)) (predicate function))
    (let ((list))
      (loop
	(multiple-value-bind (token char)
	    (read-and-decode predicate)
	  (case char
	    (#\space
	     (next)
	     (push token list))
	    (t (push token list)
	     (return)))))
      (nreverse list)))

  (:method
      ((output (eql 'list)) (predicate function))
    (read-into 'cons (read-and-decode predicate)))
  
  (:method
      ((output (eql 'array)) (predicate function))
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
      ((document string)
       &key (parser #'read-element) preserve-whitespace (element-class-map *element-class-map*) file)
    (declare (inline parse%))
    (let ((*preserve-whitespace* preserve-whitespace)
	  (*element-class-map* element-class-map))
      (parse% parser
	      (make-instance 'document-node
			     :document document
			     :file file)
	      document)))

  (:method
      ((file pathname)
       &key (parser #'read-element) preserve-whitespace (element-class-map *element-class-map*))
    (declare (ignore parser preserve-whitespace)
	     (inline parse-stream))
    (parse-document
     (with-open-file (in file :direction :input)
       (with-output-to-string (out)
	 (parse-stream in out)))
     :element-class-map element-class-map
     :file file)))


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


(defun parse% (fn node document)
  (let* ((*char-index* 0)
	 (*document* document)
	 (*length* (array-total-size *document*))
	 (*decoder* (get-decoder *document*))
	 (*stray-tags*))
    (declare (fixnum *char-index* *length*)
	     (simple-string *document*))
    (loop
      while (and (< *char-index* *length*)
		 (funcall fn node)))
    (with-slots (child-nodes) node
      (setf child-nodes (nreverse child-nodes)))
    node))



(defun read-into-object (&optional node)
  "Retrieve element-class or string and find appropriate node"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (class name)
      (read-element-name)
    (cond (class
	   (make-instance class :stw-reader t :parent-node node))
	  (name
	   (restart-case
	       (class-not-found-error "There is no class matching the element name ~a" name)
	     (assign-generic-node (c)
	       :report "Use GENERIC-NODE"
	       (declare (ignore c))
	       (make-generic-node name))
	     (ignore-node (c)
	       :report "Skip opening and closing tags."
	       (declare (ignore c))
	       (let ((*mode* :strict))
		 (consume-until (match-character #\/ #\>))
		 (ecase (stw-read-char)
		   (#\/ (next))
		   (#\> (push name *stray-tags*)))))))
	  (text
	   (make-instance 'text-node :text text))
	  (t
	   ;; as read-into-object was invoked due to encountering
	   ;; an opening character.
	   (prog1
	       (make-instance 'text-node :text "<")
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
      (read-until (match-character #\> #\space #\/ #\!))
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
		  (return (values nil (read-until (match-character #\> #\space #\/ #\!)))))))))


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
	     (let ((closing-tag (read-until (match-character #\< #\>))))
	       (aif (action-p *mode*)
		    (let ((opening-tag (or (class->element (class-of node))
					   (class->element node))))
		      ;; closing-tag is consumed.
		      (unless (string= opening-tag closing-tag)
			(cond ((and *stray-tags* (string= closing-tag (car *stray-tags*)))
			       (pop *stray-tags*)
			       (return))
			      (t
			       (funcall self "mismatch between opening tag: ~a and closing tag: ~a."
					opening-tag closing-tag)))))
		    (return))))
	    ((#\< #\space)
	     ;; Stray tag, render as text and encode when printed.
	     (bind-child-node node (make-instance 'text-node :text "<"))
	     (next))
	    (t
	     (next)
	     (read-whitespace node)
	     ;; bind child-nodes at this point.
	     (bind-child-node node (read-into-object node)))))
	 (t 
	  (read-whitespace node)))))


(defmethod read-subelements :around ((node branch-node))
  (call-next-method)
  (with-slots (child-nodes) node
    (when child-nodes
      (setf child-nodes
	    (nreverse child-nodes)))
    node))


(declaim (ftype (function (character) function) attribute-value-reader)
	 (inline attribute-value-reader))

(defun attribute-value-reader (char)
  (declare (optimize (speed 3) (safety 0)))
  (ecase char
    (#\' (match-character #\' #\space))
    (#\" (match-character #\" #\space))))


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
	   (read-and-decode (attribute-value-reader char))
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
		    (consume-until (match-character char))
		    value)
		  (ignore-attribute (c)
		    :report "Ignore all values."
		    (declare (ignore c))
		    (consume-until (match-character char))
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
	      (attribute (read-until (match-character #\= #\space #\>))))
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
	      (consume-until (match-character #\space #\>))
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
	   (read-and-decode (attribute-value-reader char))
	 (cond ((char= char% char)
		value)
	       ((char= char% #\space)
		(restart-case (multiple-value-error attribute value)
		  (use-first-found-value (c)
		    :report "Use first found value and skip the rest"
		    (declare (ignore c))
		    (consume-until (match-character char))
		    value)
		  (ignore-attribute (c)
		    :report "Ignore all values."
		    (declare (ignore c))
		    (consume-until (match-character char))
		    nil))))))
      (t
       (error "xml attributes missing quotes")))))


(defmethod assign-value
    ((class generic-node) slot-name attribute value)
  (declare (ignore slot-name))
  (when value
    (setf (gethash attribute (slot-value class 'attributes)) value)))


(defmethod read-attribute ((class generic-node))
  (let ((attribute (read-until (match-character #\space #\= #\> #\/)))
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
    (setf the-content (read-until (match-string closing-tag)))))

(defmethod read-content ((node content-node))
  (with-slots (the-content closing-tag) node
    (next)
    (setf the-content (read-until (match-string closing-tag)))))

(defmethod read-content (node)
  (awhen (read-and-decode (match-character *opening-char*))
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

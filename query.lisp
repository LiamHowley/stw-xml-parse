(in-package xml.parse)


(defgeneric clone-node (node)
  (:method
       ((node dom-node))
      (read-from-string (write-to-string node))))


(defgeneric find-ancestor-node (node ancestor &optional limiting-node)
  (:documentation "Traverse up through the document tree from node to find a specified ancestor. Returns ancestor node if found; otherwise nil.")

  (:method
      ((node document-node) ancestor &optional limiting-node)
    (declare (ignore ancestor limiting-node))
    node)

  (:method
      ((node dom-node) ancestor &optional (limiting-node 'document-node))
    (let ((parent-node (slot-value node 'parent-node)))
      (when parent-node
	(if (typep parent-node ancestor)
	    parent-node
	    (cond ((and (eq limiting-node ancestor)
			(typep parent-node ancestor))
		   parent-node)
		  ((typep parent-node limiting-node)
		   nil)
		  (t (find-ancestor-node parent-node ancestor))))))))


(defun walk-tree (node predicate &key from-end)
  (labels ((walk (node acc)
	     (cond ((null node)
		    acc)
		   ((consp node)
		    (walk (car node) (walk (cdr node) acc)))
		   (t
		    (let ((result (funcall predicate node)))
		      (typecase node
			((or document-node branch-node)
			 (with-slots (child-nodes) node
			   (walk (if from-end
				     child-nodes
				     (reverse child-nodes))
				 (if result
				     (cons node acc)
				     acc))))
			(t (if result
			       (cons node acc)
			       acc))))))))
    (nreverse
     (walk node nil))))


(defgeneric attribute-value (node attribute)

  (:documentation "For reading and writing attribute values. This is the canonical method
to write attribute values as it also forces rerendering of node in document when printing.")

  (:method
       ((node element-node) (attribute string))
      (declare (inline attribute->slot))
      (let ((slot (attribute->slot attribute-name (class-of node))))
	(when slot
	  (let ((slot-name (slot-definition-name slot)))
	    (when (slot-boundp node slot-name)
	      (slot-value node slot-name)))))))


(defmethod (setf attribute-value)
   (value (node element-node) (attribute string))
  (declare (inline attribute->slot))
  (let ((slot (attribute->slot attribute-name (class-of node))))
    (when slot
      (let ((slot-name (slot-definition-name slot)))
	(setf (slot-value node slot-name) value
	      (slot-value node 'coordinates) nil)))))
		       


  ;; slot-not-found-error

(defmethod ensure-string (value)
  (ensure-string value))

(defmethod ensure-string ((value string))
  (make-displaced-array value 0 (array-total-size value)))


(defgeneric get-elements-by-tagname (node tagname)
  (:documentation "Return all elements with the specified tagname")
  (:method
       (node tagname)
      (let ((symbol (gethash tagname *element-class-map*)))
	(query-select-all node
			  #'(lambda (node)
			      (typep node symbol))))))


(defun all (values)
  #'(lambda (node slot)
      (let ((test-value (slot-value node (slot-definition-name slot))))
	(typecase test-value
	  (atom
	   (member test-value values :test #'equal))
	  (cons
	   (loop for value in values
		   always (member value test-value :test #'equal)))
	  (array
	   (loop for value in values
		   always (loop for value% across test-value
				 when (equal value value%)
				   do (return t))))))))
      

(defun any (values)
  #'(lambda (node slot)
      (let ((test-value (slot-value node (slot-definition-name slot))))
	(typecase test-value
	  (atom
	   (member test-value values :test #'equal))
	  (cons
	   (loop for value in values
		   thereis (member value test-value :test #'equal)))
	  (array
	   (loop for value in values
		   thereis (loop for value% across test-value
				 when (equal value value%)
				   do (return t))))))))


(defun attribute-valuep (name fn)
  #'(lambda (node)
      (when (typep node 'element-node)
	(let ((slot (attribute->slot name (class-of node))))
	  (when (and slot (slot-boundp node (slot-definition-name slot)))
	    (funcall fn node slot))))))


(defun attributes-p (fn &rest attributes)
  "Requires a function that accepts a boolean, (e.g. every / some), 
and returns a closure that accepts a element-node."
  #'(lambda (node)
      (and (typep node 'element-node)
	   (funcall fn #'(lambda (attribute)
			   (let ((slot (attribute->slot attribute (class-of node))))
			     (when slot
			       (slot-boundp node (slot-definition-name slot)))))
		    attributes))))


(defun attribute-p (name)
  #'(lambda (node)
      (and (typep node 'element-node)
	   (let ((slot (attribute->slot name (class-of node))))
	     (slot-boundp node (slot-definition-name slot))))))



(defun tokens-p (fn &rest tokens)
  "Requires a function that accepts a boolean, (e.g. every / some), 
and returns a closure that accepts a sequence as an argument."
  #'(lambda (seq)
      (funcall fn #'(lambda (token)
		      (search token seq :test #'string-equal))
	       tokens)))


(defgeneric get-element-with-attributes (node &rest attributes)
  (:documentation "Return the first element that contains all attributes.")
  (:method
       (node &rest attributes)
      (query-select node (apply #'attributes-p #'some attributes))))


(defgeneric get-elements-with-attributes (node &rest attributes)
  (:documentation "Return all elements that contain all attributes.")
  (:method
       (node &rest attributes)
      (query-select-all node (apply #'attributes-p #'every attributes))))


(defgeneric get-elements-with-attribute (node attribute)
  (:documentation "Return all elements that contain attribute.")
  (:method
       (node attribute)
      (query-select-all node (attribute-p attribute))))


(defgeneric get-element-with-attribute (node attribute)
  (:documentation "Return the first element that contains attribute.")
  (:method
       (node attribute)
      (query-select node (attribute-p attribute))))


(defgeneric get-element-with-attribute-values (node attribute-name &rest attribute-values)
  (:documentation "Return the first element that contains the attribute and each of attribute-values")
  (:method
       (node attribute-name &rest attribute-values)
      (query-select node (attribute-valuep attribute-name (all attribute-values)))))


(defgeneric get-element-with-attribute-value (node attribute-name &rest attribute-values)
  (:documentation "Return the first element that contains the attribute and one-of of attribute-values")
  (:method
       (node attribute-name &rest attribute-values)
      (query-select node (attribute-valuep attribute-name (any attribute-values)))))


(defgeneric get-elements-with-attribute-value (node attribute-name &rest attribute-values)
  (:documentation "Return all elements that contain the attribute and any of attribute-values")
  (:method
       (node attribute-name &rest attribute-values)
      (query-select-all node (attribute-valuep attribute-name (any attribute-values)))))


(defgeneric get-elements-with-attribute-values (node attribute-name &rest attribute-values)
  (:documentation "Return all elements that contain the attribute and all of attribute-values")
  (:method
       (node attribute-name &rest attribute-values)
      (query-select-all node (attribute-valuep attribute-name (all attribute-values)))))


(defgeneric get-next-sibling (node &optional index)
  (:documentation "Return the next element in nodelist")

  (:method
      (node &optional (index 1))
    (let* ((parent-node (slot-value node 'parent-node))
	   (nodelist (slot-value parent-node 'child-nodes))
	   (position (position node nodelist)))
      (nth (+ position index) nodelist))))


(defgeneric get-previous-sibling (node &optional index)
  (:documentation "Return the previous element in nodelist")

  (:method
       (node &optional (index 1))
    (let* ((parent-node (slot-value node 'parent-node))
	   (nodelist (slot-value parent-node 'child-nodes))
	   (position (position node nodelist)))
      (nth (- position index) nodelist))))


(defgeneric query-select (node predicate &optional from-end)
  (:documentation "Given a starting-node and a predicate, 
query-select returns the first node that matches.")
  (:method
       (node predicate &optional from-end)
      (walk-tree node
		 #'(lambda (node)
		     (when (funcall predicate node)
		       (return-from query-select node)))
		 :from-end from-end)))


(defgeneric query-select-all (node predicate)
  (:documentation "Given a starting-node and a predicate, 
query-select-all returns all matching nodes.")
  (:method
       (node predicate)
      (walk-tree node
		 #'(lambda (node)
		     (funcall predicate node)))))


(defgeneric retrieve-text-nodes (node &optional filter)
  (:documentation "Return all text nodes for node, with optional filter. Filter if supplied
must be a function that accepts one text-node as an argument.")
  (:method
       (node &optional filter)
      (walk-tree node
		 #'(lambda (node)
		     (let ((res (typep node 'text-node)))
		       (cond ((and res filter)
			      (funcall filter node))
			     (res
			      t)))))))


(defgeneric retrieve-text-nodes-with-token (node token)
  (:documentation "Return all text nodes containing token.")
  (:method
       (node token)
      (retrieve-text-nodes node #'(lambda (node)
				    (search token (slot-value node 'text))))))
  

(defgeneric retrieve-text-nodes-with-tokens (node &rest tokens)
  (:documentation "Return all text nodes containing any of the specified tokens.")
  (:method
       (node &rest tokens)
      (retrieve-text-nodes node
			   #'(lambda (node)
			       (funcall (apply #'tokens-p #'some tokens)
					(slot-value node 'text))))))


(defgeneric retrieve-text-nodes-with-all-tokens (node &rest tokens)
  (:documentation "Return all text nodes containing all tokens.")
  (:method
       (node &rest tokens)
      (retrieve-text-nodes node
			   #'(lambda (node)
			       (funcall (apply #'tokens-p #'every tokens)
					(slot-value node 'text))))))



(defgeneric remove-node (node)
  (:method
       (node)
      (with-slots (parent-node) node
	(remove node (slot-value parent-node 'child-nodes)))
      (setf parent-node nil)
      node))


(defgeneric add-node (node parent-node)
  (:method
       (node parent-node)
      (setf (slot-value parent-node 'child-nodes) (nconc (slot-value parent-node 'child-nodes) node)
	    (slot-value node 'parent-node) parent-node)
      node))


(defgeneric insert-before (node node-to-insert)
  (:method
       (node node-to-insert)
      (with-slots (child-nodes) (slot-value node 'parent-node)
	(push to-insert
	      (cdr
	       (nthcdr
		(1- (position node child-nodes :test #'eq))
		child-nodes)))
	to-insert)))


(defgeneric insert-after (node node-to-insert)
  (:method
       (node node-to-insert)
      (with-slots (child-nodes) (slot-value node 'parent-node)
	(push to-insert
	      (cdr
	       (nthcdr
		(position node child-nodes :test #'equal)
		child-nodes)))
	to-insert)))


(defgeneric first-child (node)
  (:method
       (node)
      (car (slot-value node 'child-nodes))))


(defgeneric last-child (node)
  (:method
       (node)
      (car (last (slot-value node 'child-nodes)))))


(defgeneric last-of-type (node type)
  (:method
       (node type)
      (walk-tree node
		 #'(lambda (node)
		     (typep node type))
		 :from-end t)))


(defgeneric first-of-type (node type)
  (:method
       (node type)
      (walk-tree node
		    #'(lambda (node)
			(typep node type)))))


(defgeneric retrieve-comments (node)
  (:documentation "Return all comments.")
  (:method (node)
    (walk-tree node
	       #'(lambda (node)
		   (typep node '!--)))))

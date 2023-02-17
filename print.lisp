(in-package xml.parse)

(defvar *indent* nil)

(defvar *print-childnodes* t)


(defmethod print-object ((object document-node) stream)
  ;; use get-macro-character as a predicate for
  ;; how to print.
  (if (get-macro-character #\<)
      (let ((*encoder* #'(lambda (char)
			   (cdr (assoc char *special-chars* :test #'char=)))))
	(serialize-object object stream))
      (call-next-method)))

(defmethod print-object ((object dom-node) stream)
  ;; use get-macro-character as a predicate for
  ;; how to print.
  (if (get-macro-character #\<)
      (let ((*encoder* #'(lambda (char)
			   (cdr (assoc char *special-chars* :test #'char=)))))
	(serialize-object object stream))
      (call-next-method)))

(defmethod print-object ((node attribute-node) stream)
  (let ((nodes (retrieve-text-nodes node)))
    (loop
      for node% in nodes
      collect (print-object node% stream)
      unless (eq node% (car (last nodes)))
	collect (write-char #\space stream))))

(defmethod print-object ((node text-node) stream)
  (let* ((*document* (text node))
	 (*decoder* (get-decoder *document*))
	 (*char-index* 0)
	 (*length* (length *document*)))
    (princ (read-and-decode) stream)))


;;; serializing 

(declaim (inline indent-string)
	 (ftype (function ((or null fixnum) stream) (or null simple-string)) indent-string))

(defun indent-string (num stream)
  "Indent string by number of chars. Takes two args: num and stream."
  (declare (optimize (speed 3) (safety 0)))
  (when *indent*
    (fresh-line stream)
    (write-string (make-string num :initial-element #\space) stream)))


(defun serialize (object &key (indent *indent*) (stream (make-string-output-stream)) (serializer #'serialize-object))
  (let ((*encoder* #'(lambda (char)
		       (cdr (assoc char *special-chars* :test #'char=))))
	(*indent* indent))
    (funcall serializer object stream *indent* t)
    (get-output-stream-string stream)))


(defgeneric serialize-object (object stream &optional indent include-children)
  (:documentation "Print object to stream in xml format."))


(defmethod serialize-object ((object document-node) (stream stream) &optional (indent *indent*) include-children)
  (declare (ignore include-children))
  (loop
    for node in (slot-value object 'child-nodes)
    do (serialize-object node stream (when indent 0))))


(defmethod serialize-object ((node generic-node) (stream stream) &optional indent include-children)
  (if include-children
      (loop
	for child in (slot-value node 'child-nodes)
	do (serialize-object child stream (when indent (+ 3 indent)) t))
      (write-string " ... " stream)))


(defmethod serialize-object ((object text-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (with-encoder
      (slot-value object 'text)
      *encoder*
    (let ((*decoder* (get-decoder *document*)))
      (princ (read-and-encode) stream))))


(defmethod serialize-object ((declaration ?xml) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (with-slots (version encoding standalone) declaration
    (write-string "<?xml version='" stream)
    (write-string version stream)
    (when encoding
      (write-string " encoding='" stream)
      (write-string encoding stream)
      (write-char #\' stream))
    (when standalone
      (write-string " standalone='" stream)
      (write-string standalone stream)
      (write-char #\' stream))
    (write-string " ?>" stream)))


(defmethod serialize-object :before ((object element-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (dolist (slot (filter-slots-by-type (class-of object) 'xml-direct-slot-definition))
    (awhen (slot-value object (slot-definition-name slot))
      (unless (typep self 'dom-node)
	(print-slot object slot (slot-definition-type slot) stream)))))


(defmethod print-slot (object slot-definition type stream)
  (declare (ignore stream))
  (let ((slot-name (slot-definition-name slot-definition)))
    (print object)
    (print slot-name)
    (terpri)
    (when (slot-boundp object slot-name)
      (call-next-method))))


(defmethod serialize-object ((node sgml-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (indent-string indent stream)
  (write-char #\< stream)
  (write-string (class->element (class-of node)) stream)
  (write-string (slot-value node 'the-content) stream)
  (write-string (slot-value (class-of node) 'closing-tag) stream))


(defun print-opening-tag (element stream indent)
  (indent-string indent stream)
  (write-char #\< stream)
  (write-string element stream))

(defun print-closing-tag (element stream indent)
  (indent-string indent stream)
  (write-string "</" stream)
  (write-string element stream)
  (write-char #\> stream))


(defmethod serialize-object :around ((object element-node) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (print-opening-tag (or (class->element (class-of object))
			 (class->element object))
		     stream indent)
  (call-next-method))

(defmethod serialize-object :before ((object generic-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (maphash #'(lambda (attribute value)
	       (with-encoder value
		   *encoder*
		 (let ((*decoder* (get-decoder *document*)))
		   (write-char #\space stream)
		   (write-string attribute stream)
		   (write-char #\= stream)
		   (write-char #\' stream)
		   (write-string (read-and-encode) stream)
		   (write-char #\' stream))))
	   (slot-value object 'attributes)))

(defmethod serialize-object ((object generic-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (call-next-method))

(defmethod serialize-object ((object branch-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (write-char #\> stream)
  (if include-children
      (loop
	for child in (slot-value object 'child-nodes)
	do (serialize-object child stream (when indent (+ 3 indent)) t))
      (write-string " ... " stream)))

(defmethod serialize-object :after ((object branch-node) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (print-closing-tag (or (class->element (class-of object))
			 (class->element object))
		     stream indent))

(defmethod serialize-object ((object leaf-node) (stream stream) &optional indent include-children)
  (declare (ignore indent include-children))
  (write-string " />" stream))

(defmethod serialize-object ((object content-node) (stream stream) &optional indent include-children)
  (declare (ignore include-children))
  (write-char #\> stream)
  (let ((text (slot-value object 'the-content)))
    (when text
      (write-string text stream)))
  (print-closing-tag (class->element (class-of object)) stream indent))


;;print attributes

(declaim (inline print-attribute-handle)
	 (ftype (function (xml-direct-slot-definition stream) string) print-attribute-handle))

(defun print-attribute-handle (slot stream)
  (write-char #\space stream)
  (write-string (slot-definition-attribute slot) stream)
  (write-string "='" stream))

(defmethod print-slot ((object dom-node) slot type (stream stream))
  (awhen (slot-value object (slot-definition-name slot))
    (if (numberp self)
	(print-slot object slot 'number stream)
	(with-encoder self *encoder*
	  (let ((*decoder* (get-decoder *document*)))
	    (print-attribute-handle slot stream)
	    (write-string (read-and-encode) stream)
	    (write-char #\' stream))))))

(defmethod print-slot ((object dom-node) slot (type (eql 'number)) (stream stream))
  (awhen (slot-value object (slot-definition-name slot))
    (print-attribute-handle slot stream)
    (write-string (ensure-string self) stream)
    (write-char #\' stream)))


(defmethod print-slot ((object dom-node) slot (type (eql 'array)) (stream stream))
  (awhen (slot-value object (slot-definition-name slot))
    (with-encoder
	(concat-string self t)
	*encoder*
      (let ((*decoder* (get-decoder *document*)))
	(print-attribute-handle slot stream)
	(write-string (read-and-encode) stream)
	(write-char #\' stream)))))

(defmethod print-slot ((object dom-node) slot (type (eql 'cons)) (stream stream))
  (awhen (slot-value object (slot-definition-name slot))
    (with-encoder
	(concat-string self t)
	*encoder*
      (let ((*decoder* (get-decoder *document*)))
	(print-attribute-handle slot stream)
	(write-string (read-and-encode) stream)
	(write-char #\' stream)))))

(defmethod print-slot ((object dom-node) slot (type (eql 'boolean)) (stream stream))
  (print-attribute-handle slot stream)
  (write-string (if (slot-value object (slot-definition-name slot))
		    "true"
		    "false")
		stream)
  (write-char #\' stream))


(defmethod print-slot ((object dom-node) slot (type (eql 'multiple-attributes)) (stream stream))
  (loop
    for (attribute . value) in (slot-value object (slot-definition-name slot))
    do (write-string " " stream)
    do (write-string (string-downcase attribute) stream)
    when value
      do (write-string "='" stream)
      and do (write-string value stream)
      and do (write-string "'" stream)))



(defgeneric write-to-file (file object &key)

  (:method
      (file (object string) &key (if-does-not-exist :create) (if-exists :supersede))
    (sequence-to-file file object if-exists if-does-not-exist))

  (:method
      (file (object document-node) &key (if-does-not-exist :create) (if-exists :supersede))
    (sequence-to-file file (if (readerp)
			       (write-to-string object)
			       (serialize object))
		      if-exists if-does-not-exist)))

(in-package xml.parse)

(defvar *indent* nil)



(defmacro if-attribute-value (object slot &body body)
  "if attribute has value then ... Anaphora: slot-value is available for capture."
  `(let ((slot-value (slot-value ,object (slot-definition-name ,slot))))
     (when slot-value
       ,@body)))


(declaim (inline indent-string)
	 (ftype (function (fixnum stream) simple-string) indent-string))

(defun indent-string (num stream)
  "Indent string by number of chars. Takes two args: num and stream."
  (declare (optimize (speed 3) (safety 0)))
  (fresh-line stream)
  (write-string (make-string num :initial-element #\space) stream))


(defun serialize (object &optional indent (stream (make-string-output-stream)))
  (let ((*encoder* #'(lambda (char)
		       (cdr (assoc char *special-chars* :test #'char=))))
	(*indent* indent))
    (serialize-object object stream)
    (get-output-stream-string stream)))


(defmethod serialize-object ((object document-node) (stream stream) &optional indent)
  (loop
    for node in (slot-value object 'child-nodes)
    do (serialize-object node stream 0)))


(defmethod serialize-object ((node dom-node) (stream stream) &optional indent)
  (loop
    for child in (slot-value node 'child-nodes)
    do (serialize-object child stream 0)))


(defmethod serialize-object ((node generic-node) (stream stream) &optional indent)
  (loop
    for child in (slot-value node 'child-nodes)
    do (serialize-object child stream indent)))


(defmethod serialize-object ((object text-node) (stream stream) &optional indent)
  (declare (ignore indent))
  (with-encoder
      (slot-value object 'text)
    *encoder*
    (let ((*decoder* (get-decoder *document*))
	  (reader (read-and-encode)))
      (princ (funcall reader) stream))))


(defmethod serialize-object ((declaration ?xml) (stream stream) &optional indent)
  (declare (ignore indent))
  (with-slots (version encoding standalone) declaration
    (write-string "<?xml version='" stream)
    (write-string version stream)
    (when encoding
      (write-string " encoding='" stream)
      (write-string encoding stream)
      (write-string "'" stream))
    (when standalone
      (write-string " standalone='" stream)
      (write-string standalone stream)
      (write-string "'"))
    (write-string " ?>" stream)))


(defmethod serialize-object :before ((object element-node) (stream stream) &optional indent)
  (declare (ignore indent))
  (dolist (slot (filter-slots-by-type (class-of object) 'xml-direct-slot-definition))
    (print-slot object slot (slot-definition-type slot) stream)))


(defmethod print-slot (object slot-definition type stream)
  (declare (ignore stream))
  (let ((slot-name (slot-definition-name slot-definition)))
    (when (slot-boundp object slot-name)
      (call-next-method))))


(defmethod serialize-object ((node sgml-node) (stream stream) &optional indent)
  (when *indent*
    (indent-string indent stream))
  (write-string "<" stream)
  (write-string (class->element (class-of node)) stream)
  (write-string (slot-value node 'the-content) stream)
  (write-string (slot-value node 'closing-tag) stream))


(defun print-opening-tag (object stream indent)
  (when *indent*
    (indent-string indent stream))
  (write-string "<" stream)
  (write-string (class->element object) stream))

(defun print-closing-tag (object stream indent)
  (when *indent*
    (indent-string indent stream))
  (write-string "</" stream)
  (write-string (class->element object) stream)
  (write-string ">" stream))



(defmethod serialize-object :around ((object element-node) (stream stream) &optional indent)
  (print-opening-tag (class-of object) stream indent)
  (call-next-method))

(defmethod serialize-object :before ((object generic-node) (stream stream) &optional indent)
  (with-encoder
      (slot-value object 'attributes)
    *encoder*
    (let ((*decoder* (get-decoder *document*))
	  (reader (read-and-encode)))
      (maphash #'(lambda (attribute value)
		   (write-string attribute stream)
		   (write-string "=" stream))
	       (write-string (funcall reader) stream)))))

(defmethod serialize-object :after ((object generic-node) (stream stream) &optional indent)
  (print-closing-tag object stream indent))

(defmethod serialize-object ((object branch-node) (stream stream) &optional indent)
  (write-string ">" stream)
  (loop
    for node in (slot-value object 'child-nodes)
    do (serialize-object node stream (+ 3 indent))))

(defmethod serialize-object :after ((object branch-node) (stream stream) &optional indent)
  (print-closing-tag (class-of object) stream indent))

(defmethod serialize-object ((object leaf-node) (stream stream) &optional indent)
  (write-string " />" stream))

(defmethod serialize-object ((object content-node) (stream stream) &optional indent)
  (write-string ">" stream)
  (let ((text (slot-value object 'the-content)))
    (when text
      (write-string text stream)))
  (print-closing-tag (class-of object) stream indent))


;;print attributes

(declaim (inline print-attribute-handle)
	 (ftype (function (xml-direct-slot-definition stream) string) print-attribute-handle))

(defun print-attribute-handle (slot stream)
  (write-string " " stream)
  (write-string (slot-definition-attribute slot) stream)
  (write-string "='" stream))
      
(defmethod print-slot ((object dom-node) slot type (stream stream))
  (if-attribute-value
   object slot
   (with-encoder
     slot-value
     *encoder*
     (let ((*decoder* (get-decoder *document*))
	   (reader (read-and-encode)))
       (print-attribute-handle slot stream)
       (write-string (funcall reader) stream)
       (write-string "'" stream)))))

(defmethod print-slot ((object dom-node) slot (type (eql 'real)) (stream stream))
  (if-attribute-value
   object slot
   (print-attribute-handle slot stream)
   (write-string (ensure-string slot-value) stream)
   (write-string "'" stream)))

(defmethod print-slot ((object dom-node) slot (type (eql 'array)) (stream stream))
  (if-attribute-value
   object slot
   (with-encoder
       (concat-string slot-value)
     *encoder*
     (let ((*decoder* (get-decoder *document*))
	   (reader (read-and-encode)))
       (print-attribute-handle slot stream)
       (write-string (funcall reader) stream)
       (write-string "'" stream)))))

(defmethod print-slot ((object dom-node) slot (type (eql 'cons)) (stream stream))
  (if-attribute-value
   object slot
   (with-encoder
       (concat-string slot-value)
     *encoder*
     (let ((*decoder* (get-decoder *document*))
	   (reader (read-and-encode)))
       (print-attribute-handle slot stream)
       (write-string (funcall reader) stream)
       (write-string "'" stream)))))

(defmethod print-slot ((object dom-node) slot (type (eql 'boolean)) (stream stream))
  (print-attribute-handle slot stream)
  (write-string (if (slot-value object (slot-definition-name slot))
		    "true"
		    "false")
		stream)
  (write-string "'" stream))

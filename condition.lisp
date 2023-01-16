(in-package xml.parse)

;;; error handling mode

(declaim (keyword *mode*)
	 (inline action-p)
	 (ftype (function (&optional keyword symbol)
			  (or symbol null))
		action-p))

(defparameter *mode* :silent 
  ":verbose -> WARN; :strict -> ERROR; :silent -> do nothing at all.")


(defun action-p (&optional (action *mode*) (e 'error))
  (declare (optimize (safety 0) (speed 3)))
  (case action
    (:verbose 'warn)
    (:strict e)
    (t nil)))


;;; conditions and restarts

(defmacro define-restart (name args &body body)
  (let ((restart (gensym)))
    `(defun ,name ,args
       (let ((,restart (find-restart ',name)))
	 ,@body
	 (when ,restart
	   (invoke-restart ,restart ,@args))))))



;; managing slot attribution

(define-condition slot-not-found-error (simple-error)
  ((node :initarg :node :reader node)
   (attribute :initarg :attribute :reader attribute)))

(defun slot-not-found-error (format-control &rest format-args)
  (cerror "Ignore error and continue."
	  'slot-not-found-error
	  :format-control format-control
	  :format-arguments format-args))

(define-restart assign-slot-to-attribute (slot))

(define-restart ignore-missing-slot (c))


;; managing class attribution

(define-condition class-not-found-error (simple-error)
  ((node-name :initarg :node-name :reader node-name)))

(defun class-not-found-error (format-control name)
  (error 'class-not-found-error
	  :node-name name
	  :format-control format-control
	  :format-arguments (list name)))

(define-restart assign-generic-node (c))

(define-restart ignore-node (c)
  "Skip over the tag as if it does not exist. Read child-nodes in.
When node is a branch node, watch out for any stray tags.")

(define-restart assign-text-node (c))

;; multiple attribute values are verbotten in XML
;; and selectively used in HTML.

(define-condition multiple-value-error (simple-error)
  ((attribute :initarg :attribute :reader attribute)
   (value :initarg :value :reader value)))

(defun multiple-value-error (format-control &rest format-args)
  (cerror "Choose an attribute value or ignore the attribute."
	  'multiple-value-error
	  :format-control "Multiple values assigned to attribute ~s"
	  :format-arguments format-args))

(define-restart use-first-found-value (c))

(define-restart ignore-attribute (c))


(define-condition tag-mismatch-error (simple-error)
  ((tag :initarg :tag :reader tag)))

(defun tag-mismatch-error (expected received) 
  (error 'tag-mismatch-error
	 :tag received
	 :format-control "The tag ~a is not ~a"
	 :format-arguments (list received expected)))


(define-condition stray-closing-tag-error (simple-error)
  ((tag :initarg :tag :reader tag))
  (:documentation "When the function SKIP-NODE is invoked on a BRANCH-NODE, 
or CONTENT-NODE, a closing element tag is left in the document. When *MODE*
is :SILENT, this is ignored, but when *MODE* is :STRICT, a STRAY-CLOSING-TAG-ERROR
is invoked."))

(defun stray-closing-tag-error (tag) 
  (cerror "Ignore tag and move on"
	  'stray-closing-tag-error
	  :tag tag
	  :format-control "Closing tag ~a found at toplevel."
	  :format-arguments (list tag)))

;; entity/character conditions

(define-condition invalid-xml-character-error (simple-error) ())

(defun invalid-xml-character-error (format-control &rest rest)
  (error 'invalid-xml-character-error
	  :format-control format-control
	  :format-arguments rest))


(define-condition restricted-xml-character-warning (simple-warning) ())

(defun restricted-xml-character-warning (format-control &rest rest)
  (warn 'restricted-xml-character-warning
	:format-control format-control
	:format-arguments rest))


(define-condition restricted-xml-character-error (simple-warning) ())

(defun restricted-xml-character-error (format-control &rest rest)
  (cerror "Ignore error and continue"
	  'restricted-xml-character-error
	  :format-control format-control
	  :format-arguments rest))

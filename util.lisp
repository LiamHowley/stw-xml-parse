(in-package xml.parse)


(defmacro defclass-with-initargs (name &body parts)
  "Defines a class and ensures slots have initargs."
  (macrolet ((set-attr (attribute &optional value)
	       `(unless (member ,attribute slot)
		  (setf slot (append slot `(,,attribute ,,value))))))
    (setf (cadr parts)
	  (mapcar #'(lambda (slot)
		      (setf slot (ensure-list slot))
		      (set-attr :type 'simple-string)
		      (set-attr :initarg (intern (string-upcase (symbol-name (car slot))) 'keyword))
		      slot)
		  (cadr parts)))
    `(defclass ,name ,@parts)))


(defun make-reader (sym)
  (let ((package (stw.util:find-and-replace (package-name *package*) '((#\. . #\-)))))
    (intern (concatenate 'string package "-" (symbol-name sym)) *package*)))

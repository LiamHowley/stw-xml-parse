(in-package xml.parse)

;;; error handling mode

(eval-when (:compile-toplevel :load-toplevel :execute)

  (declaim (keyword *mode*)
	   (ftype (function (&optional keyword)
			    (or symbol null))
		  action-p))

  (defparameter *mode* :silent 
    ":verbose -> WARN; :strict -> ERROR; :silent -> do nothing at all.")

  (defun action-p (&optional (action *mode*))
    (declare (optimize (safety 0) (speed 3)))
    (case action
      (:verbose 'warn)
      (:strict 'error)
      (t nil))))


(defmacro take-action (condition &body body)
  "Unless *MODE* is strict, take action specified in body."
  (let ((action (gensym)))
    `(lambda (c)
       (let ((,action (action-p)))
	 (when (eq ,action 'warn)
	   (warn (apply #'format nil (simple-condition-format-control ,condition)
			(simple-condition-format-arguments ,condition))))
	 (unless (eq ,action 'error)
	   ,@body)))))


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
  (cerror "Slot not found."
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



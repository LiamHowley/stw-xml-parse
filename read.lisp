(in-package xml.parse) 

(defvar *initial-readtable* nil)

(defun read-xml (stream char)
  (declare (ignore char))
  (let ((next-char (read-char stream nil nil)))
    (case next-char
      (#\space
       (unread-char next-char stream)
       (return-from read-xml (values (intern "<"))))
      (#\=
       (return-from read-xml (values (intern "<="))))
      (t
       (unread-char next-char stream)
       (let ((output (with-output-to-string (out)
		       (write-char #\< out)
		       (parse-stream stream out))))
	 (parse-document output))))))

(defun set-reader (&optional (reader #'read-xml))
  (setf *initial-readtable* (copy-readtable *readtable*))
  (unless (get-macro-character #\<)
    (set-macro-character #\< reader t)))

(defun remove-reader ()
  (setf *readtable* *initial-readtable*))

(defun readerp ()
  (get-macro-character #\<))

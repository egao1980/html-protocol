(in-package #:html-backend-plump)

(defclass plump-backend (html-backend) ())

(defun make-plump-backend ()
  (make-instance 'plump-backend))

(defun use-plump-backend ()
  (setf *html-backend* (make-plump-backend)))

(defun %source-string (source)
  (etypecase source
    (string source)
    (pathname (uiop:read-file-string source))
    (stream (with-output-to-string (out)
              (loop with buf = (make-string 4096)
                    for n = (read-sequence buf source)
                    do (write-string buf out :end n)
                    until (< n (length buf)))))))

(defun %attr-alist (table)
  (let ((out nil))
    (maphash (lambda (k v)
               (push (cons (string-downcase (string k)) (if (stringp v) v (princ-to-string v)))
                     out))
             table)
    (nreverse out)))

(defun %from-plump (node)
  (cond
    ((plump:root-p node)
     (make-html-document
      :children (loop for child across (plump:children node)
                      for converted = (%from-plump child)
                      when converted collect converted)))
    ((plump:doctype-p node)
     (make-instance 'html-doctype :name (or (plump:doctype node) "html")))
    ((plump:comment-p node)
     (make-instance 'html-comment :data (plump:text node)))
    ((plump:textual-node-p node)
     (make-instance 'html-text :data (plump:text node)))
    ((plump:element-p node)
     (make-html-element (plump:tag-name node)
                        :attributes (%attr-alist (plump:attributes node))
                        :children (loop for child across (plump:children node)
                                        for converted = (%from-plump child)
                                        when converted collect converted)))
    (t nil)))

(defmethod backend-parse ((backend plump-backend) source &key)
  (handler-case
      (let ((root (plump:parse (%source-string source))))
        (%from-plump root))
    (error (e)
      (error 'html-parse-error :message (princ-to-string e)))))

(use-plump-backend)

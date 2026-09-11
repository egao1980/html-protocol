(in-package #:html-protocol)

(defun %escape-text (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (t (write-char ch out))))))

(defun %escape-attr (string)
  (with-output-to-string (out)
    (loop for ch across (if (stringp string) string (princ-to-string string))
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (#\< (write-string "&lt;" out))
               (t (write-char ch out))))))

(defun %write-attrs (element stream)
  (dolist (pair (reverse (html-element-attributes element)))
    (format stream " ~A=\"~A\""
            (car pair)
            (%escape-attr (cdr pair)))))

(defun %serialize-node (node stream)
  (etypecase node
    (html-document
     (dolist (child (html-document-children node))
       (%serialize-node child stream)))
    (html-doctype
     (format stream "<!DOCTYPE ~A>" (html-doctype-name node)))
    (html-comment
     (format stream "<!--~A-->" (html-comment-data node)))
    (html-text
     (write-string (html-text-data node) stream))
    (html-element
     (let ((name (html-element-name node))
           (opaque (opaque-text-element-p node)))
       (write-char #\< stream)
       (write-string name stream)
       (%write-attrs node stream)
       (cond
         ((html-element-void-p node)
          (write-string ">" stream))
         (t
          (write-char #\> stream)
          (dolist (child (html-element-children node))
            (cond
              ((and opaque (html-text-p child))
               (write-string (html-text-data child) stream))
              ((html-text-p child)
               (write-string (%escape-text (html-text-data child)) stream))
              (t
               (%serialize-node child stream))))
          (format stream "</~A>" name)))))))

(defun serialize-document (document &key stream)
  "Serialize DOCUMENT to a string, or STREAM."
  (unless (html-document-p document)
    (error 'html-encode-error :message "serialize expects an html-document"))
  (if stream
      (progn (%serialize-node document stream) stream)
      (with-output-to-string (s)
        (%serialize-node document s))))

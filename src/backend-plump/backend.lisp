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

;;; Attribute order.
;;;
;;; Plump keeps an element's attributes in an EQUALP hash table, and the
;;; iteration order of a hash table is unspecified (SBCL happens to iterate
;;; in insertion order; ECL does not).  The protocol promises source order,
;;; so while BACKEND-PARSE runs we bind plump's dispatcher list
;;; (PLUMP:*TAG-DISPATCHERS*) to copies whose parsers note the lexer index
;;; right after the tag name.  For each element they return, the attribute
;;; names are read again from that index with plump's own READ-ATTRIBUTE,
;;; under a local binding of PLUMP-LEXER:*INDEX*, so the names are
;;; tokenised the same way plump tokenised them.  Nothing global in plump is
;;; changed; the binding ends with the parse.

(defvar *attribute-order* nil
  "While BACKEND-PARSE runs: an EQ hash table that maps each plump element to
its attribute names in source order.")

(defparameter *tag-whitespace*
  '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page)
  "The whitespace set of plump's READ-ATTRIBUTES loop.")

(defun %scan-attribute-names (start)
  "Attribute names of the start tag whose name ends at lexer index START, in
source order.  Mirrors plump's READ-ATTRIBUTES loop, without moving the lexer."
  (let ((plump-lexer:*index* start))
    (loop for char = (plump-lexer:peek)
          until (member char '(#\/ #\> nil))
          if (member char *tag-whitespace*)
            do (plump-lexer:advance)
          else
            collect (car (plump-parser:read-attribute)))))

(defun %ordering-dispatcher (dispatcher)
  ;; COPY-TAG-DISPATCHER and TAG-DISPATCHER-PARSER are the accessors of
  ;; plump's TAG-DISPATCHER struct.  Plump does not export them, but its
  ;; exported DEFINE-TAG-PARSER macro expands into them.
  (let ((copy (plump-parser::copy-tag-dispatcher dispatcher))
        (parse (plump-parser::tag-dispatcher-parser dispatcher)))
    (setf (plump-parser::tag-dispatcher-parser copy)
          (lambda (name)
            (let* ((start plump-lexer:*index*)
                   (node (funcall parse name)))
              (when (plump:element-p node)
                (setf (gethash node *attribute-order*)
                      (%scan-attribute-names start)))
              node)))
    copy))

(defun %parse-ordered (string)
  "PLUMP:PARSE STRING, recording each element's attribute order in
*ATTRIBUTE-ORDER*."
  (let ((plump:*tag-dispatchers*
          (mapcar #'%ordering-dispatcher plump:*tag-dispatchers*)))
    (plump:parse string)))

(defun %attr-value (v)
  (cond ((stringp v) v)
        ((null v) "")
        (t (princ-to-string v))))

(defun %attr-alist (element)
  "ELEMENT's attributes as a fresh alist, in source order.  A name repeated in
the source keeps its first position and plump's (last) value."
  (let* ((table (plump:attributes element))
         (out nil))
    (flet ((emit (name)
             (multiple-value-bind (value presentp) (gethash name table)
               (let ((key (canonical-attribute-name name)))
                 (when (and presentp (not (assoc key out :test #'string=)))
                   (push (cons key (%attr-value value)) out))))))
      (dolist (name (and *attribute-order* (gethash element *attribute-order*)))
        (emit name))
      ;; Anything the scan did not see (an element built outside the parser):
      ;; append it sorted by name, so the result never depends on hash order.
      (let ((rest nil))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (unless (assoc (canonical-attribute-name k) out :test #'string=)
                     (push k rest)))
                 table)
        (dolist (name (sort rest #'string< :key #'string-downcase))
          (emit name))))
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
                        :attributes (%attr-alist node)
                        :children (loop for child across (plump:children node)
                                        for converted = (%from-plump child)
                                        when converted collect converted)))
    (t nil)))

(defmethod backend-parse ((backend plump-backend) source &key)
  (handler-case
      (let* ((*attribute-order* (make-hash-table :test 'eq))
             (root (%parse-ordered (%source-string source))))
        (%from-plump root))
    (error (e)
      (error 'html-parse-error :message (princ-to-string e)))))

(use-plump-backend)

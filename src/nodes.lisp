(in-package #:html-protocol)

(defparameter *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta"
    "param" "source" "track" "wbr"))

(defparameter *opaque-text-elements*
  '("script" "style"))

(defclass html-node () ())

(defclass html-document (html-node)
  ((children :initarg :children :accessor html-document-children :initform nil)))

(defun html-document-p (object)
  (typep object 'html-document))

(defclass html-element (html-node)
  ((name :initarg :name :accessor html-element-name :initform "")
   (attributes :initarg :attributes :accessor html-element-attributes :initform nil)
   (children :initarg :children :accessor html-element-children :initform nil)))

(defun html-element-p (object)
  (typep object 'html-element))

(defclass html-text (html-node)
  ((data :initarg :data :accessor html-text-data :initform "")))

(defun html-text-p (object)
  (typep object 'html-text))

(defclass html-comment (html-node)
  ((data :initarg :data :accessor html-comment-data :initform "")))

(defun html-comment-p (object)
  (typep object 'html-comment))

(defclass html-doctype (html-node)
  ((name :initarg :name :accessor html-doctype-name :initform "html")))

(defun html-doctype-p (object)
  (typep object 'html-doctype))

(defun make-html-document (&key children)
  (make-instance 'html-document :children children))

(defun make-html-element (name &key attributes children)
  (make-instance 'html-element
                 :name (string-downcase name)
                 :attributes attributes
                 :children children))

(defun void-element-p (name)
  (and (stringp name)
       (find name *void-elements* :test #'string-equal)))

(defun html-element-void-p (element)
  (void-element-p (html-element-name element)))

(defun opaque-text-element-p (name-or-element)
  (let ((name (if (html-element-p name-or-element)
                  (html-element-name name-or-element)
                  name-or-element)))
    (and (stringp name)
         (find name *opaque-text-elements* :test #'string-equal))))

(defun document-root (document)
  "First html-element child of DOCUMENT (usually html)."
  (find-if #'html-element-p (html-document-children document)))

(defun element-attr (element name)
  (cdr (assoc name (html-element-attributes element) :test #'string-equal)))

(defun (setf element-attr) (value element name)
  (let* ((key (string-downcase name))
         (cell (assoc key (html-element-attributes element) :test #'string-equal)))
    (if cell
        (setf (cdr cell) value)
        (push (cons key value) (html-element-attributes element)))
    value))

(defun element-text (node)
  "Concatenated descendant text. script/style stay opaque (their text children only)."
  (labels ((walk (n acc)
             (cond
               ((html-text-p n)
                (cons (html-text-data n) acc))
               ((html-element-p n)
                (if (opaque-text-element-p n)
                    (append (reverse (mapcar #'html-text-data
                                             (remove-if-not #'html-text-p
                                                            (html-element-children n))))
                            acc)
                    (reduce #'walk (html-element-children n)
                            :initial-value acc :from-end t)))
               ((html-document-p n)
                (reduce #'walk (html-document-children n)
                        :initial-value acc :from-end t))
               (t acc))))
    (apply #'concatenate 'string (nreverse (walk node nil)))))

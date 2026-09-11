(defpackage #:html-protocol
  (:use #:cl)
  (:nicknames #:stack-html)
  (:export #:html-error
           #:html-parse-error
           #:html-encode-error
           #:html-error-message

           #:*html-backend*
           #:html-backend
           #:backend-parse
           #:backend-serialize
           #:parse
           #:serialize

           #:html-node
           #:html-document
           #:html-document-p
           #:html-document-children
           #:html-element
           #:html-element-p
           #:html-element-name
           #:html-element-attributes
           #:html-element-children
           #:html-element-void-p
           #:html-text
           #:html-text-p
           #:html-text-data
           #:html-comment
           #:html-comment-p
           #:html-comment-data
           #:html-doctype
           #:html-doctype-p
           #:html-doctype-name

           #:make-html-document
           #:make-html-element
           #:document-root
           #:element-attr
           #:element-text
           #:opaque-text-element-p
           #:void-element-p

           #:select))

(in-package #:html-protocol)

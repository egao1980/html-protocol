(in-package #:html-protocol)

(defvar *html-backend* nil
  "Current HTML backend object.")

(defgeneric backend-parse (backend source &key)
  (:documentation "Parse SOURCE (string, octets, stream, pathname) to html-document."))

(defgeneric backend-serialize (backend document &key stream)
  (:documentation "Serialize DOCUMENT. Default uses the protocol writer."))

(defmethod backend-serialize ((backend html-backend) document &key stream)
  (serialize-document document :stream stream))

(defun parse (source &key)
  "Parse SOURCE via *HTML-BACKEND* → html-document."
  (unless *html-backend*
    (error 'html-parse-error :message "*html-backend* is unbound — load html-backend-plump"))
  (backend-parse *html-backend* source))

(defun serialize (document &key stream)
  "Serialize DOCUMENT via *HTML-BACKEND*, or the protocol writer if no backend."
  (if *html-backend*
      (backend-serialize *html-backend* document :stream stream)
      (serialize-document document :stream stream)))

(in-package #:html-protocol)

(define-condition html-error (error)
  ((message :initarg :message :reader html-error-message :initform nil))
  (:report (lambda (c s)
             (format s "HTML error~@[: ~A~]" (html-error-message c)))))

(define-condition html-parse-error (html-error) ())
(define-condition html-encode-error (html-error) ())

(defclass html-backend () ()
  (:documentation "Base class for html-protocol backends."))

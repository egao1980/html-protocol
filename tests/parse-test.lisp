(in-package #:html-protocol/tests)

(deftest parse-paragraph
  (let* ((doc (parse "<p class=\"x\">hello</p>"))
         (p (first (select doc "p"))))
    (ok (html-document-p doc))
    (ok p)
    (ok (string-equal "p" (html-element-name p)))
    (ok (string= "x" (element-attr p "class")))
    (ok (string= "hello" (element-text p)))))

(deftest parse-unclosed-is-lenient
  (let ((doc (parse "<div><p>hi")))
    (ok (string= "hi" (element-text (first (select doc "p")))))))

(deftest parse-script-opaque
  (let* ((doc (parse "<script>if (a<b) { /* not a tag <p> */ }</script>"))
         (script (first (select doc "script"))))
    (ok script)
    (ok (opaque-text-element-p script))
    (ok (search "a<b" (element-text script)))
    (ok (null (select script "p")))))

(deftest parse-style-opaque
  (let* ((doc (parse "<style>p { color: red; }</style>"))
         (style (first (select doc "style"))))
    (ok style)
    (ok (opaque-text-element-p style))
    (ok (search "color" (element-text style)))))

(deftest parse-doctype
  (let ((doc (parse "<!DOCTYPE html><html><body></body></html>")))
    (ok (find-if #'html-doctype-p (html-document-children doc)))))

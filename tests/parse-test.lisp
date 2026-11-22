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

(defun attribute-names (doc tag)
  (mapcar #'car (html-element-attributes (first (select doc tag)))))

;; Source order must not depend on how plump's attribute hash table iterates
;; (SBCL: insertion order; ECL: not), so these cover every plump tag path.
(deftest parse-attribute-order-every-tag-kind
  ;; plain element (read-standard-tag)
  (ok (equal '("z" "b" "m" "a")
             (attribute-names (parse "<p z=1 b=2 m=3 a=4>x</p>") "p")))
  ;; void element (define-self-closing-element)
  (ok (equal '("src" "alt" "width" "height")
             (attribute-names (parse "<img src=a alt=b width=1 height=2>") "img")))
  ;; fulltext element (define-fulltext-element)
  (ok (equal '("type" "src" "defer")
             (attribute-names (parse "<script type=m src=s defer></script>") "script")))
  ;; nested elements keep their own order
  (let ((doc (parse "<div q=1 c=2><span y=1 x=2 w=3>t</span></div>")))
    (ok (equal '("q" "c") (attribute-names doc "div")))
    (ok (equal '("y" "x" "w") (attribute-names doc "span")))))

(deftest parse-attribute-order-odd-syntax
  ;; mixed quoting, valueless, whitespace around = and newlines
  (ok (equal '("hidden" "data-z" "title" "id")
             (attribute-names
              (parse (format nil "<div hidden data-z = 'z'~%  title=\"t\" id=i></div>"))
              "div")))
  ;; a repeated name keeps its first position; case folds
  (let ((attrs (html-element-attributes
                (first (select (parse "<a ID=1 href=h id=2></a>") "a")))))
    (ok (equal '("id" "href") (mapcar #'car attrs)))
    (ok (= 2 (length attrs))))
  ;; self-closing syntax on a plain element
  (ok (equal '("k" "j")
             (attribute-names (parse "<x-y k=1 j=2/>") "x-y"))))

;; On SBCL hash iteration already matches insertion order, so the tests above
;; would pass without the recorded order.  These check the mechanism itself:
;; the parse records each element's order, and the conversion follows that
;; record rather than the hash table.
(deftest plump-backend-records-attribute-order
  (let* ((html-backend-plump::*attribute-order* (make-hash-table :test 'eq))
         (root (html-backend-plump::%parse-ordered "<div b=1 a=2><i z=1 y=2></i></div>"))
         (div (aref (plump:children root) 0))
         (i (aref (plump:children div) 0)))
    (ok (equal '("b" "a") (gethash div html-backend-plump::*attribute-order*)))
    (ok (equal '("z" "y") (gethash i html-backend-plump::*attribute-order*)))
    ;; reverse the record: the alist must follow it, not the table
    (setf (gethash div html-backend-plump::*attribute-order*) '("a" "b"))
    (ok (equal '(("a" . "2") ("b" . "1"))
               (html-backend-plump::%attr-alist div)))))

(deftest plump-backend-unrecorded-attributes-sorted
  (let* ((html-backend-plump::*attribute-order* nil)
         (el (plump:make-element (plump:make-root) "p")))
    (setf (plump:attribute el "m") "1"
          (plump:attribute el "c") "2"
          (plump:attribute el "x") "3")
    (ok (equal '("c" "m" "x")
               (mapcar #'car (html-backend-plump::%attr-alist el))))))

(in-package #:html-protocol/tests)

(deftest serialize-void
  (let* ((doc (make-html-document
               :children (list (make-html-element "br"))))
         (s (serialize doc)))
    (ok (search "<br>" s))
    (ng (search "</br>" s))))

(deftest serialize-escapes-text
  (let* ((doc (make-html-document
               :children (list (make-html-element "p"
                                                  :children (list (make-instance 'html-text
                                                                                 :data "a<b&c"))))))
         (s (serialize doc)))
    (ok (search "a&lt;b&amp;c" s))))

(deftest serialize-script-raw
  (let* ((doc (make-html-document
               :children (list (make-html-element "script"
                                                  :children (list (make-instance 'html-text
                                                                                 :data "a<b"))))))
         (s (serialize doc)))
    (ok (search "a<b" s))
    (ng (search "&lt;" s))))

(deftest parse-serialize-roundtrip-text
  (let* ((doc (parse "<p id=\"n\">hello</p>"))
         (again (parse (serialize doc))))
    (ok (string= "hello" (element-text (first (select again "p")))))
    (ok (string= "n" (element-attr (first (select again "#n")) "id")))))

(defun %text (data)
  (make-instance 'html-text :data data))

(defparameter *hostile* "<script>alert(\"x\" & 'y')</script>")

(defparameter *hostile-escaped*
  "&lt;script&gt;alert(\"x\" &amp; 'y')&lt;/script&gt;")

(deftest serialize-escapes-top-level-text
  (ok (string= *hostile-escaped*
               (serialize (make-html-document :children (list (%text *hostile*)))))))

(deftest serialize-escapes-fragment-text
  ;; A fragment is a document holding a node list: every text node is escaped.
  (ok (string= (concatenate 'string *hostile-escaped* "<b>" *hostile-escaped* "</b>"
                            *hostile-escaped*)
               (serialize (make-html-document
                           :children (list (%text *hostile*)
                                           (make-html-element "b" :children (list (%text *hostile*)))
                                           (%text *hostile*)))))))

(deftest serialize-escapes-nested-text
  (let* ((i (make-html-element "i" :children (list (%text *hostile*))))
         (p (make-html-element "p" :children (list i)))
         (div (make-html-element "div" :children (list p))))
    (ok (string= (concatenate 'string "<div><p><i>" *hostile-escaped* "</i></p></div>")
                 (serialize (make-html-document :children (list div)))))))

(deftest serialize-escapes-attribute-values
  (ok (string= "<a title=\"&lt;script>alert(&quot;x&quot; &amp; 'y')&lt;/script>\"></a>"
               (serialize (make-html-document
                           :children (list (make-html-element "a" :attributes (list (cons "title" *hostile*)))))))))

(deftest serialize-style-raw
  (ok (string= "<style>a > b { content: \"&\" }</style>"
               (serialize (make-html-document
                           :children (list (make-html-element
                                            "style" :children (list (%text "a > b { content: \"&\" }")))))))))

(deftest serialize-attributes-in-given-order
  (ok (string= "<a href=\"x\" id=\"y\" class=\"z\"></a>"
               (serialize (make-html-document
                           :children (list (make-html-element
                                            "a" :attributes '(("href" . "x") ("id" . "y") ("class" . "z")))))))))

(deftest setf-element-attr-appends
  (let ((a (make-html-element "a" :attributes '(("href" . "x")))))
    (setf (element-attr a "id") "y")
    (setf (element-attr a "class") "z")
    (setf (element-attr a "href") "w")
    (ok (string= "<a href=\"w\" id=\"y\" class=\"z\"></a>"
                 (serialize (make-html-document :children (list a)))))))

(deftest parse-serialize-preserves-attribute-order
  (dolist (src '("<a href=\"x\" id=\"y\" class=\"z\" title=\"t\">k</a>"
                 "<a title=\"t\" class=\"z\" id=\"y\" href=\"x\">k</a>"
                 "<div data-b=\"2\" data-a=\"1\" data-d=\"4\" data-c=\"3\" data-f=\"6\" data-e=\"5\"></div>"))
    (ok (string= src (serialize (parse src))) src)))

(deftest setf-element-attr-leaves-caller-alist-alone
  (let* ((given (list (cons "href" "x")))
         (a (make-html-element "a" :attributes given)))
    (setf (element-attr a "href") "w")
    (ok (string= "x" (cdr (first given))))
    (ok (string= "w" (element-attr a "href")))))

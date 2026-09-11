(in-package #:html-protocol/tests)

(deftest select-tag-class-id
  (let ((doc (parse "<div id=\"root\"><span class=\"a b\">x</span><span class=\"a\">y</span></div>")))
    (ok (= 2 (length (select doc "span"))))
    (ok (= 2 (length (select doc ".a"))))
    (ok (= 1 (length (select doc "span.b"))))
    (ok (string= "div" (html-element-name (first (select doc "#root")))))))

(deftest select-descendant-and-child
  (let ((doc (parse "<div><p><span>1</span></p><span>2</span></div>")))
    (ok (= 2 (length (select doc "div span"))))
    (ok (= 1 (length (select doc "div > span"))))
    (ok (= 1 (length (select doc "p > span"))))))

(deftest select-star
  (let ((doc (parse "<div><p></p><span></span></div>")))
    (ok (>= (length (select doc "*")) 3))))

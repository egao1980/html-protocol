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

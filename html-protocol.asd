(defsystem "html-protocol"
  :version "0.1.0"
  :description "CLOS HTML document + parse/serialize + tiny CSS select (stack-html)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :properties (:cl-repo (:ci (:with ("html-backend-plump"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "nodes")
               (:file "serialize")
               (:file "select")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "html-protocol/tests"))))

(defsystem "html-protocol/tests"
  :depends-on ("html-protocol" "html-backend-plump" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "parse-test")
               (:file "serialize-test")
               (:file "select-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

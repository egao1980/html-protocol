(defsystem "html-backend-plump"
  :version "0.1.0"
  :description "html-protocol backend — Shinmera plump (lenient HTML)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("html-protocol" "plump")
  :serial t
  :pathname "src/backend-plump"
  :components ((:file "package")
               (:file "backend")))

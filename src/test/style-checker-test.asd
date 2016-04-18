(asdf:defsystem style-checker-test
  :name "Style Checker Tests"
  :version "0.1"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :serial t
  :components ((:file "package") (:file "main"))
  :depends-on (:style-checker))

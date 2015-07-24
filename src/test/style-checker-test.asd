; For why this is the way it is, see src/main/style-checker.asd
(asdf:defsystem style-checker-test.internal
  :components ((:file "package")
               (:file "main")))

(asdf:defsystem style-checker-test
  :name "Experiment Tests"
  :version "0.0.1"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :serial t
  :depends-on (:style-checker style-checker-test.internal))

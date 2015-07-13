(asdf:defsystem style-checker
 :name "Style Checker"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package") (:file "syntax-checker"))
 :depends-on (:cl-ppcre))

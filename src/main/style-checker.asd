(asdf:defsystem style-checker
 :name "Style Checker"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :components ((:file "package") (:file "syntax-checker"))
 :serial t
 :depends-on #-travis (:cl-ppcre) #+travis nil) ; We don't load up systems if in travis mode

(asdf:defsystem checker :serial t
 :components ((:file "package") (:file "checker"))
 :depends-on (:cl-ppcre))

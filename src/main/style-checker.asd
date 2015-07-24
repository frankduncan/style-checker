; The systems are split up into two because travis will run more smoothly
; if we use a custom built sbcl that has all the deps pre-loaded since
; we are sure those will work just fine :)
;
; You should link to this file in your systems directory, or however you
; handle your asdf configurations.  Then just (asdf:load-system :clnl)
;
; There's probably a better way, but I don't know it

(asdf:defsystem style-checker.internal
 :serial t
 :components ((:file "package") (:file "syntax-checker")))

(asdf:defsystem style-checker
 :name "Style Checker"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :depends-on (:cl-ppcre :style-checker.internal))

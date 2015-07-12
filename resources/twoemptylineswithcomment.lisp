(in-package #:nothing)

; comment resets line counter

; this one does two

; but we'll get an error soon!


(defun hello-world (a b c)
 (progn
  (with-open-file (str "increasinglylongfilenamesfailme.dat" :direction :input :if-does-not-exist :create)
   (when
    (read-line str)
    (format t "This file had some things in int, yay!~%")))))

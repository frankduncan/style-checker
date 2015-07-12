(in-package #:nothing)

(defun hello-world (a b c)
 (progn
  (with-open-file (str "increasinglylongfilenamesfailme.dat" :direction :input :if-does-not-exist :create)
   (when
    (read-line nothing::str)
    (format t "This file had some things in int, yay!~%")))))

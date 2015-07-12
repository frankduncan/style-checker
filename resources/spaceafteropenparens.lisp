(in-package #:nothing)

(defun small-guy (a b) (+ a b))
(defun small-guy-2 (a b) (+ a b))

(defun hello-world (a b c)
 (progn
  (let
   ((x y)
    (z 9))
   (with-open-file (str "increasinglylongfilenamesfailme.dat" :direction :input :if-does-not-exist :create)
    ( when
     (read-line str)
     (format t "This file had some things in int, yay!~%")))))

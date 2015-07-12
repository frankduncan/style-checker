(in-package #:nothing)

(defun small-guy (a b) (+ a b))
(defun small-guy-2 (a b) (+ a b))

; This comment is awesome
(defun hello-world (a b c)
 (progn
  (let
   ((x y)                            ; Ok, this comment is also great
    (z 9))                           ; so is this one!
   (with-open-file (str "increasinglylongfilenamesfailme.dat" :direction :input :if-does-not-exist :create)
    (when
     (read-line str)
     (format t "This file had some things in int, yay!~%"))))))

(defvar *x* "hello world
 this is a multiline string
 with \" some escaped things
 and some (_) and whatnot
 ")

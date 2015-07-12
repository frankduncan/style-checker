(in-package #:nothing)

(defun small-guy (a b) (+ a b))
  ; great comment here
(defun small-guy-2 (a b) (+ a b))

; this is a function definition
; comment about things
; yo
(defun hello-world (a b c)   ; This comment is a test
 (progn                      ; of things
  (let                       ; ( and how
   ((x y) ; comments can break
    (z 9))  ; the   evaluator and everything is ok
   (with-open-file (str "increasinglylongfilenamesfailme.dat" :direction :input :if-does-not-exist :create)
    (when ; because we don't care
     (read-line str)
     (format t "This file had some things in int, yay!~%"))))))
; what kinds of comments
; do to a file
;

; header comments
(defun multiline-1 (a b c)
 (+ a b))

; header comments
(defun multiline-1 (a b c)
 (+ a b))

; then a comment for
; informational purpose

(x y z)

; Then a final comment in the file

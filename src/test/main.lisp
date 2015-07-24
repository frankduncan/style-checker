(in-package #:style-checker-test)

(defvar *tests* nil)

(defmacro deftest (filename success &optional msg line-no col-no)
 `(push
   (lambda ()
    (let
     ((green (format nil "~c[1;32m" #\Esc))
      (red (format nil "~c[1;31m" #\Esc))
      (result (syntax-checker:check-file ,filename)))
     (cond
      ((not (eql ,success (car result)))
       (format t "~A- ~A failed, expected ~A and got ~A~c[0m~%"
        red ,filename ,success (car result) #\Esc))
      ((and (eql :failure ,success) (string/= ,msg (third result)))
       (format t "~A- ~A failed, expected msg ~A and got msg ~A~c[0m~%"
        red ,filename ,msg (third result) #\Esc))
      ((and (eql :failure ,success) (/= ,line-no (fourth result)))
       (format t "~A- ~A failed, expected line ~A and got line ~A~c[0m~%"
        red ,filename ,line-no (fourth result) #\Esc))
      ((and (eql :failure ,success) (/= ,col-no (fifth result)))
       (format t "~A- ~A failed, expected column ~A and got column ~A~c[0m~%"
        red ,filename ,col-no (fifth result) #\Esc))
      (t (format t "~A- ~A passed ~c[0m~%" green ,filename #\Esc) t))))
   *tests*))

; This really is just here to check against regressions
(defun run-all-tests ()
 (let
  ((results (mapcar #'funcall *tests*)))
  (every #'identity results)))

(deftest #P"resources/commentneedsspace.lisp" :failure "Multiline top level forms must be separated by a space" 6 0)
(deftest #P"resources/emptylineatend.lisp" :failure "Must not end with empty line" 8 0)
(deftest #P"resources/good.lisp" :success)
(deftest #P"resources/goodcomment.lisp" :success)
(deftest #P"resources/hangingcloseparens1.lisp" :failure "No hanging close parens" 11 0)
(deftest #P"resources/hangingcloseparens2.lisp" :failure "No hanging close parens" 12 1)
(deftest #P"resources/internalsymbols.lisp" :failure "No internal symbols from other packages" 6 22)
(deftest #P"resources/invalidindent1.lisp" :failure "All form elements must be indented equally" 9 5)
(deftest #P"resources/invalidindent2.lisp" :failure "All form elements must be indented equally" 11 6)
(deftest #P"resources/longform.lisp" :failure "Forms can't be over 50 lines long" 117 60)
(deftest #P"resources/longline.lisp" :failure "Line longer than 120 characters" 2 0)
(deftest #P"resources/newlineafteropenparens.lisp" :failure "No new line after opening form" 11 5)
(deftest #P"resources/nopackage.lisp" :failure "Must begin with in-package form" 0 0)
(deftest #P"resources/package.lisp" :success)
(deftest #P"resources/spaceafteropenparens.lisp" :failure "No space after opening parens" 11 5)
(deftest #P"resources/tabs.lisp" :failure "Must not use tabs" 4 0)
(deftest #P"resources/toplevelindented.lisp" :failure "Top level forms must begin on first column" 3 1)
(deftest #P"resources/twoemptylines.lisp" :failure "Must not have two empty lines in a row" 2 0)
(deftest #P"resources/twoemptylineswithcomment.lisp" :failure "Must not have two empty lines in a row" 8 0)
(deftest #P"resources/twoinpackage.lisp" :failure "Only one in-package per file" 9 0)
(deftest #P"resources/twospaces.lisp" :failure "Only one space between items of a form" 8 6)
(deftest #P"resources/unspacedforms.lisp" :failure "Multiline top level forms must be separated by a space" 4 0)
(deftest #P"resources/whitespaceendline.lisp" :failure "No whitespace at end of line" 4 106)
(deftest #P"resources/whitespacelines.lisp" :failure "No whitespace only lines" 1 1)

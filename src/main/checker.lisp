(in-package #:style-checker)

; Rules
; - Elements in each form must be indented the same amount
; * No form longer than 50 lines
; - Top level multiline forms must be separated by exactly one space
; * No line longer than 120 characters
; - No use of unexported symbols in other packages
; - No tabs
; - Only one space between elements in a form on a single line
; * in-package must be first line in file unless file is package.lisp
; * No whitespace at end of line
; * No lines that are only whitespace
; - No empty lines at end of file
; * Only one in-package per file
;
; Some thoughts
; - form starting reader macros will have to be hand added to this code
; - exceptions will eventually arise, and the rule file will have to be changed
; - the proper formatting of "loop" is weird

(define-condition check-failure nil ((msg :initarg :msg :reader check-failure-msg)
                                     (line-no :initarg :line-no :reader check-failure-line-no)
                                     (col-no :initarg :col-no :reader check-failure-col-no)))

(defvar *state* nil)
(defvar *line-no* nil)
(defvar *col-no* nil)
(defvar *evaluators* nil)
(defvar *form-stack* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter *possible-states*
  '(:begin ; start of file
    :normal ; normal processing
    :beginning-of-line
    :beginning-of-symbols
   )))


(defun set-state (state)
 (when (not (find state *possible-states*))
  (error "Can't set state to ~A" state))
 (setf *state* state)
 nil)

(defmacro defevaluator (state match func)
 (when (not (find state *possible-states*)) (error "~A is an invalid state" state))
 (let
  ((scanner (gensym)))
 `(let
   ((,scanner (when (stringp ,match) (cl-ppcre:create-scanner ,match))))
   (pushnew
    (list
     (lambda (state text)
      (and
       (eql ,state state)
       (or
        (and (symbolp text) (eql text ,match))
        (and ,scanner
             (stringp text)
             (multiple-value-bind (start end) (cl-ppcre:scan ,scanner text)
              (and start end (= 0 start)))))))
     (lambda (text) (second (multiple-value-list (cl-ppcre:scan ,scanner text))))
     ,func)
    *evaluators*))))

(defun evaluate (text)
 (if (string= "" text)
     (let*
      ((evaluator (find-if (lambda (f) (funcall f *state* :eof)) *evaluators* :from-end t :key #'car))
       (problem (when evaluator (funcall (third evaluator)))))
      (when problem (error (make-condition 'check-failure :msg problem :line-no *line-no* :col-no *col-no*))))
     (let
      ((evaluator (find-if (lambda (f) (funcall f *state* text)) *evaluators* :from-end t :key #'car)))
      (when (not evaluator) (error (make-condition 'check-failure :msg (format nil "Can't check in state ~S: ~S..." *state* (subseq text 0 (min (length text) 10))) :line-no *line-no* :col-no *col-no*)))
      (let
       ((problem (funcall (third evaluator))))
       (when problem (error (make-condition 'check-failure :msg problem :line-no *line-no* :col-no *col-no*)))
       (let
        ((length-of-match (funcall (cadr evaluator) text)))
        (incf *col-no* length-of-match)
        (when (< 120 *col-no*) (error (make-condition 'check-failure :msg "Line longer than 120 characters" :line-no *line-no* :col-no 0)))
        (evaluate (subseq text length-of-match)))))))

(defun slurp-file (filename &key (element-type 'character) (sequence-type 'string))
 (with-open-file (str filename :element-type element-type)
  (let ((seq (make-sequence sequence-type (file-length str)))) (read-sequence seq str) seq)))

(defun check-file (file)
 (set-state :begin)
 (setf *line-no* 0)
 (setf *col-no* 0)
 (format t "~%File: ~A~%" file)
 (handler-case
  (progn (evaluate (slurp-file file)) t)
  (check-failure (cf)
   (format t " - Had an error: ~S at ~A:~A~%" (check-failure-msg cf) (check-failure-line-no cf) (check-failure-col-no cf))
   nil)))

(defun check-directory (dir)
 (every #'identity (mapcar #'check-file (directory (format nil "~A/**/*.lisp" dir)))))

; These are in reverse order
(progn
 (setf *evaluators* nil)
 (defevaluator :begin "\\(in-package[^\\)]*\\)"
  (lambda ()
   (set-state :normal) nil))
 (defevaluator :begin ".*"
  (constantly "Must begin with in-package form"))
 (defevaluator :normal "\\( *in-package "
  (constantly "Only one in-package per file"))
 (defevaluator :normal "\\n"
  (lambda ()
   (set-state :beginning-of-line)
   (incf *line-no*)
   (setf *col-no* -1)
   nil))
 (defevaluator :normal " +\\n"
  (lambda ()
   "No whitespace at end of line"))
 (defevaluator :beginning-of-line " *"
  (lambda ()
   (set-state :beginning-of-symbols)
   nil))
 (defevaluator :beginning-of-symbols "\\n"
  (lambda () (when (< 0 *col-no*) "No whitespace only lines")))
 (defevaluator :beginning-of-symbols ""
  (lambda ()
   (set-state :normal)
   nil))
 (defevaluator :normal "\\("
  (lambda ()
   (push
    (list *line-no* *col-no*)
    *form-stack*)
   nil))
 (defevaluator :normal "\\)"
  (lambda ()
   (let
    ((form (pop *form-stack*)))
    (cond
     ((not form) "Unmatched ending paren")
     ((< 50 (- *line-no* (car form))) "Forms can't be over 50 lines long")))))

(cl-ppcre:scan (cl-ppcre:create-scanner " *") "
asdf asdf")

 (defevaluator :normal "." (constantly nil))
 )

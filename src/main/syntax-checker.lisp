(in-package #:syntax-checker)

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
(defvar *form-ended-on-same-line* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter *possible-states*
  '(:begin ; start of file
    :normal ; normal processing
    :beginning-of-line
    :beginning-of-line-with-separator ; empty space in there
    :beginning-of-symbols
    :beginning-of-symbols-with-separator
    :comment-with-separator ; weird edge case for pre-function comments
    :beginning-of-line-with-comment-and-separator ; weird edge case part 2
    :first-symbol ; first symbol of form/line
    :all ; matches everything
    :in-string)))

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
        (or (eql :all ,state) (eql ,state state))
        (or
         (and (symbolp text) (eql text ,match))
         (and
          ,scanner
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
   (when (not evaluator)
    (error
     (make-condition 'check-failure
      :msg (format nil "Can't check in state ~S: ~S..."
            *state* (subseq text 0 (min (length text) 10))) :line-no *line-no* :col-no *col-no*)))
   (let
    ((problem (funcall (third evaluator))))
    (when problem
     (error (make-condition 'check-failure :msg problem :line-no *line-no* :col-no *col-no*)))
    (let
     ((length-of-match (funcall (cadr evaluator) text)))
     (incf *col-no* length-of-match)
     (when (< 120 *col-no*)
      (error (make-condition 'check-failure :msg "Line longer than 120 characters" :line-no *line-no* :col-no 0)))
     (evaluate (subseq text length-of-match)))))))

(defun slurp-file (filename &key (element-type 'character) (sequence-type 'string))
 (with-open-file (str filename :element-type element-type)
  (let ((seq (make-sequence sequence-type (file-length str)))) (read-sequence seq str) seq)))

(defun check-file (file)
 "CHECK-FILE FILE => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:success FILENAME MSG LINE-NO COL-NO)

ARGUMENTS AND VALUES:

  FILE: a pathname
  FILENAME: the file this check was run on
  MSG: a string containing the failure message
  LINE-NO: an integer, the line number on which the failure appeared
  COL-NO: an integer, the column number on which the failure appeared

DESCRIPTION:

  CHECK-FILE runs all the checks against a file and returns
  as soon as the first style error is found.

EXAMPLES:

  (check-file #P\"path/to/file.lisp\") => (:success \"path/to/file.lisp\")
  (check-file #P\"path/to/error.lisp\") => (:failure \"path/to/error.lisp\" \"File cannot end with empty line\" 20 0)"

 (if (string= "package" (pathname-name file))
  (set-state :normal)
  (set-state :begin))
 (setf *line-no* 0)
 (setf *col-no* 0)
 (setf *form-stack* nil)
 (setf *form-ended-on-same-line* nil)
 (handler-case
  (progn
   (evaluate (slurp-file file))
   (list :success file))
  (check-failure (cf)
   (list :failure file (check-failure-msg cf) (check-failure-line-no cf) (check-failure-col-no cf)))))

(defun check-directory (dir)
 "CHECK-DIRECTORY DIR => RESULTS

  RESULTS: RESULT*

ARGUMENTS AND VALUES:

  DIR: A directory to recurse into and check files
  RESULT: A result as returned by check-file

DESCRIPTION:

  CHECK-DIRECTORY grabs all .lisp files in the tree under DIR, and loads
  checks them all.

  The results are then put together into a list which can be programatically
  evaluated.  As opposed to pretty-print-check-directory, this function doesn't
  clutter up your standard out."
 (mapcar #'check-file (directory (format nil "~A/**/*.lisp" dir))))

(defun any-failures (checks)
 (find :failure checks :key #'car))

(defun print-failure (failure)
 (format nil
  "Style error in ~A at ~A:~A: ~A~%- ~A~%~VT^"
  (second failure)
  (1+ (fourth failure))
  (1+ (fifth failure))
  (third failure)
  (with-open-file (str (second failure)) (loop :repeat (fourth failure) :do (read-line str)) (read-line str))
  (+ (fifth failure) 2)))

(defun pretty-print-check-directory (dir)
 "PRETTY-PRINT-CHECK-DIRECTORY DIR => SUCCESS

ARGUMENTS AND VALUES:

  DIR: A directory to recurse into and check files
  SUCCESS: T if there were no failures

DESCRIPTION:

  PRETTY-PRINT-CHECK-DIRECTORY checks DIR for any errors, dumping them to output
  and returning a single flag.

  Unlike check-directory, PRETTY-PRINT-CHECK-DIRECTORY is built for continuous
  integration, dumping errors to standard out and returning a singular result.

EXAMPLES:

  (pretty-print-check-directory \"src\") => nil"
 (let
  ((checks (check-directory dir)))
  (format t "In ~A: Checked ~A files with ~A failures~%~%"
   dir (length checks) (length (remove :success checks :key #'car)))
  (format t "~{~A~%~}" (mapcar #'print-failure (remove :success checks :key #'car)))
  (not (any-failures checks))))

; These are in reverse order
(defevaluator :beginning-of-symbols " *;[^\\n]*"
 (lambda () (set-state :normal)))

(defevaluator :beginning-of-symbols-with-separator " *;[^\\n]*"
 (lambda () (set-state :comment-with-separator)))

(defevaluator :normal " *;[^\\n]*"
 (lambda () (set-state :normal)))

(defevaluator :normal "\\("
 (lambda ()
  (push (list *line-no* *col-no*) *form-stack*)
  (set-state :first-symbol)))

(defevaluator :first-symbol "\\("
 (lambda ()
  (cond
   ((and (not *form-stack*) (not (zerop *col-no*))) "Top level forms must begin on first column")
   ((and *form-stack* (/= (1+ (cadr (car *form-stack*))) *col-no*))
    "All form elements must be indented equally")
   (t
    (push (list *line-no* *col-no*) *form-stack*)
    (set-state :first-symbol)))))

(defevaluator :all "\\t" (constantly "Must not use tabs"))

(defevaluator :begin "\\(in-package[^\\)]*\\)" (lambda () (set-state :normal)))

(defevaluator :beginning-of-line-with-separator :eof
 (lambda ()
  (incf *line-no* -1)
  "Must not end with empty line"))

(defevaluator :beginning-of-line-with-separator "\\n" (constantly "Must not have two empty lines in a row"))

(defevaluator :begin ".*" (constantly "Must begin with in-package form"))

(defevaluator :all "\\( *in-package " (constantly "Only one in-package per file"))

(defevaluator :normal "\\n"
 (lambda ()
  (incf *line-no*)
  (setf *col-no* -1)
  (set-state :beginning-of-line)))

(defevaluator :comment-with-separator "\\n"
 (lambda ()
  (incf *line-no*)
  (setf *col-no* -1)
  (set-state :beginning-of-line-with-comment-and-separator)
  nil))

(defevaluator :normal " +\\n" (constantly "No whitespace at end of line"))

(defevaluator :beginning-of-line " *" (lambda () (set-state :beginning-of-symbols)))

(defevaluator :beginning-of-line-with-separator " *" (lambda () (set-state :beginning-of-symbols-with-separator)))

(defevaluator :beginning-of-line-with-comment-and-separator "\\n"
 (lambda ()
  (progn
   (incf *line-no*)
   (setf *col-no* -1)
   (set-state :beginning-of-line-with-separator))))

(defevaluator :beginning-of-line-with-comment-and-separator " *"
 (lambda () (set-state :beginning-of-symbols-with-separator)))

(defevaluator :beginning-of-symbols "\\n"
 (lambda ()
  (if
   (< 0 *col-no*)
   "No whitespace only lines"
   (progn
    (incf *line-no*)
    (setf *col-no* -1)
    (set-state :beginning-of-line-with-separator)))))

(defevaluator :beginning-of-symbols "\\)" (constantly "No hanging close parens"))

(defevaluator :beginning-of-symbols-with-separator "\\)" (constantly "No hanging close parens"))

(defevaluator :beginning-of-symbols ""
 (lambda ()
  (if
   (and (not *form-stack*) (not *form-ended-on-same-line*))
   "Multiline top level forms must be separated by a space"
   (set-state :first-symbol))))

(defevaluator :beginning-of-symbols-with-separator ""
 (lambda ()
  (set-state :first-symbol)))

(defevaluator :normal "\\)"
 (lambda ()
  (let
   ((form (pop *form-stack*)))
   (cond
    ((not form) "Unmatched ending paren")
    ((< 50 (- *line-no* (car form))) "Forms can't be over 50 lines long")
    (t (setf *form-ended-on-same-line* (= *line-no* (car form))) nil)))))

(defevaluator :normal "::" (constantly "No internal symbols from other packages"))

(defevaluator :in-string "\\\\\"" (constantly nil))

(defevaluator :normal "\"" (lambda () (set-state :in-string)))

(defevaluator :in-string "\"" (lambda () (set-state :normal)))

(defevaluator :in-string "\\n"
 (lambda ()
  (incf *line-no*)
  (setf *col-no* -1)
  nil))

(defevaluator :in-string "." (constantly nil))

(defevaluator :first-symbol "\\n" (constantly "No new line after opening form"))

(defevaluator :first-symbol " " (constantly "No space after opening parens"))

(defevaluator :first-symbol ""
 (lambda ()
  (cond
   ((and *form-stack* (/= (1+ (cadr (car *form-stack*))) *col-no*))
    "All form elements must be indented equally")
   (t (set-state :normal)))))

(defevaluator :normal "  " (constantly "Only one space between items of a form"))

(defevaluator :normal "." (constantly nil))

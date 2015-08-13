(defpackage #:syntax-checker (:use :cl)
 (:export #:check-file #:check-directory #:pretty-print-check-directory)
 (:documentation
  "Enforces arbitrary set of style guidelines.

This package walks over common lisp code to make sure it adheres to a set
of syntactic guidelines designed to ensure a semblance of uniformity.  No
current one was found that could be configured to work the way this one does,
so instead of writing a configurable tool, another unconfigurable one was born."))

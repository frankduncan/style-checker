(setf *compile-print* nil)
(require 'asdf)
(asdf:initialize-source-registry `(:source-registry (:tree ,(car (directory "src"))) :INHERIT-CONFIGURATION))
(asdf:load-system :style-checker)
(asdf:load-system :style-checker-test)
#-travis (asdf:load-system :docgen)

(format t "~%~c[1;33mRunning Tests~c[0m~%" #\Esc #\Esc)
(when (not (style-checker-test:run-all-tests))
 (format t "~c[1;31mFailed tests!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))

(format t "~%~c[1;33mChecking Style~c[0m~%" #\Esc #\Esc)
(when (not (syntax-checker:pretty-print-check-directory "src"))
 (format t "~c[1;31mFailed style check!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))
(format t "~c[1;32m- Style Passed!~c[0m~%" #\Esc #\Esc)

(format t "~%~c[1;33mChecking Docs~c[0m~%" #\Esc #\Esc)
(when (not (docgen:pretty-print-validate-packages :docgen))
 (format t "~c[1;31mFailed doc check!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))
(format t "~c[1;32m- Doc Check Passed!~c[0m~%" #\Esc #\Esc)

(format t "~c[1;32mSuccess!~c[0m~%" #\Esc #\Esc)
(sb-ext:exit :code 0)

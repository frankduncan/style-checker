# Common Lisp Style Checker
Enforcement of my guidelines for common lisp style.

If you like, you can [download it](https://github.com/frankduncan/style-checker/releases/download/0.1/style-checker_0.1.tar.gz)

## Syntax Checking Rules
* Elements on new line in each form must be indented the same amount
* No space/newline after open parens
* No form longer than 50 lines
* Top level multiline forms must be separated by exactly one space
* No line longer than 120 characters
* No use of unexported symbols in other packages
* No tabs
* Only one space between elements in a form on a single line
* in-package must be first line in file unless file is package.lisp
* No whitespace at end of line
* No lines that are only whitespace
* No empty lines at end of file
* Never have two empty lines in a row
* Only one in-package per file
* No hanging close parens

### Exceptions
* comments
* multiline strings
* exclude in-package check from package.lisp

## Usage

See the [wiki](https://github.com/frankduncan/style-checker/wiki).

Also, see bin/travis.lisp to see how it's used in this package.

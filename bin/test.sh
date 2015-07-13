#!/bin/bash

sbcl --eval "(asdf:load-system :style-checker)" --eval '(syntax-checker:pretty-print-check-directory "src")'
sbcl --eval "(asdf:load-system :style-checker)" --eval '(syntax-checker:pretty-print-check-directory "resources")'

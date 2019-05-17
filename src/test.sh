#!/bin/sh

sbcl --noinform --eval "(proclaim '(optimize (debug 3)))" --eval '(setf sb-ext:*evaluator-mode* :interpret)' --script "$(dirname $(readlink -f "$0"))/test.lisp" "$(dirname $(readlink -f "$0"))/" "$@"

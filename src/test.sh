#!/bin/sh

sbcl --noinform --eval "(proclaim '(optimize (debug 3)))" --eval '(setf sb-ext:*evaluator-mode* :interpret)' --load test.lisp --eval '(ivy-hll:main (cl-user::portable-argv))' --quit --end-toplevel-options "$@"


(defpackage :ix-hll
  (:use :cl :optima)
  (:export :main))

(defpackage :ix-hll-kw
  (:export
   :defun :defstruct
   :int32))

(defpackage :ix-hll-user
  (:use :ix-hll-kw))

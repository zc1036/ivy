
(defpackage :ix-util
  (:use :cl)
  (:export :let+))

(defpackage :ix-il
  (:use :cl)
  (:export :r :i :move))

(defpackage :ix-hll
  (:use :cl :optima :ix-il)
  (:export :main))

(defpackage :ix-hll-kw
  (:export
   :fun :struct
   :defun :defstruct
   :int32
   :+ :-
   :let))

(defpackage :ix-hll-user
  (:use :ix-hll-kw))

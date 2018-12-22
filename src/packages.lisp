
(defpackage :ix-util
  (:use :cl)
  (:export :let+ :with-gensyms))

(defpackage :ix-il
  (:use :cl)
  (:export :r :i :optr :optr.repr
           :move :ext :add :sub
           :reg.name :reg.bytesize :imm.value :imm.bytesize
           :with-reg))

(defpackage :ix-arch
  (:use :cl)
  (:export :arch :arch.bits :make-arch))

(defpackage :ix-hll
  (:use :cl :optima :ix-util :ix-arch)
  (:export :main))

(defpackage :ix-hll-kw
  (:export
   :fun :struct
   :defun :defstruct
   :int32
   :+ :- :=
   :let))

(defpackage :ix-hll-user
  (:use :ix-hll-kw))

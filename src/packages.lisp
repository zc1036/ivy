
(defpackage :ix-util
  (:use :cl)
  (:export :let+ :with-gensyms :list-of :all-of-type))

(defpackage :ix-il
  (:use :cl)
  (:export :r :i :optr :optr.repr
           :move :ext :add :sub
           :reg.name :reg.bytesize :imm.value :imm.bytesize
           :with-reg))

(defpackage :ix-arch
  (:use :cl)
  (:export :arch :arch.bits :make-arch))

(defpackage :ix-state
  (:use :cl :ix-util :ix-arch)
  (:export
   :lexical-scope :make-lexical-scope :lexical-scope.bindings :lexical-scope.next :lexical-scope.lookup
   :state :make-state :state.functions :state.lex-vars :state.glob-vars :state.lookup-lexical-var
   :*state* :*target-arch*))

(defpackage :ix-type
  (:use :cl :ix-util :ix-state :ix-arch :optima)
  (:export
   :hltype :hltype.name
   :hltype-agg-member :hltype-agg-member.name :hltype-agg-member.type
   :hltype-structure :hltype-astructure.members
   :hltype-union :hltype-union.members
   :hltype-builtin :hltype-builtin.signed-p :hltype-builtin.float-p :hltype-builtin.bytesize :hltype-builtin.numeric
   :typespec :typespec.to-string
   :typespec-atom :typespec-atom.ref
   :typespec-const :typespec-const.ref
   :typespec-volatile :typespec-volatile.ref
   :typespec-pointer :typespec-pointer.ref
   :typespec-array :typespec-array.elt-type :typespec-array.size
   :typespec-function :typespec-function.ret-type :typespec-function.arg-types
   :typespec.sizeof :typespec.alignof
   :hltype.sizeof :hltype.alignof
   :remove-cv :typespec-equalp :is-numeric :const-p

   ;; slots
   :name :ret-type :type :members :signed-p :float-p :bytesize :numeric :ref :elt-type :size :arg-types))

(defpackage :ix-ast
  (:use :cl :ix-util :optima :ix-arch :ix-type :ix-state)
  (:export
   :ast :ast.type
   :gast
   :ast-funcall :ast-funcall.target :ast-funcall.args
   :decl :decl.name
   :decl-var-binding :decl-var-binding.name :decl-var-binding.type :decl-var-binding.init
   :decl-function :decl-function.ret-type :decl-function.args :decl-function.body-src :decl-function.body
   :decl-variable :decl-variable.type :decl-variable.storage
   :ast-var-ref :ast-var-ref.var
   :ast-func-ref :ast-func-ref.func
   :ast-binop :ast-binop.left :ast-binop.right :ast-binop.opstr
   :ast-let :ast-let.bindings :ast-let.body
   :ast.type :gast.type
   :binop-+ :ast-binop-+ :binop-- :ast-binop-- :ast-binop-= :binop-=
   :lvalue-p

   ;; slots
   :type :name :init :ret-type :args :body-src :body :type :storage :target
   :var :func :left :right :opstr :bindings))

(defpackage :ix-hll-kw
  (:export
   :fun :struct
   :defun :defstruct
   :int16 :int32
   :+ :- :=
   :let))

(defpackage :ix-hll
  (:use :cl :optima :ix-util :ix-arch :ix-ast :ix-type :ix-state)
  (:export :main))

(defpackage :ix-hll-user
  (:use :ix-hll-kw))

(defpackage :ix-target-il
  (:use :cl :ix-util :ix-ast :ix-state :ix-type)
  (:export :emit-decl))

(defpackage :ix-target-c
  (:use :cl :ix-util :ix-ast :ix-state :ix-type)
  (:export :emit-decl))

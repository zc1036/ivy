
(defpackage :ivy-util
  (:use :cl)
  (:export :let+ :with-gensyms :list-of :all-of-type))

(defpackage :ivy-il
  (:use :cl)
  (:export :r :i :optr :optr.repr
           :move :ext :add :sub :icall :dcall :pset :rset :pget :rget :jump :jumpc :jump-target
           :reg.name :reg.bytesize :imm.value :imm.bytesize
           :with-reg))

(defpackage :ivy-platform
  (:use :cl)
  (:export :platform :platform.bits :platform.char-signed-p :platform.name :make-platform))

(defpackage :ivy-state
  (:use :cl :ivy-util :ivy-platform)
  (:export
   :lexical-scope :make-lexical-scope :lexical-scope.bindings :lexical-scope.next :lexical-scope.lookup
   :state :make-state :state.emittables :state.lex-vars :state.glob-vars :state.lookup-lexical-var
   :*state* :*target-platform*))

(defpackage :ivy-type
  (:use :cl :ivy-util :ivy-state :ivy-platform :optima)
  (:export
   :hltype :hltype.name
   :hltype-agg-member :hltype-agg-member.name :hltype-agg-member.type :agg-lookup-member :make-hltype-agg-member
   :hltype-structure :hltype-structure.members
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
   :remove-cv :propagate-cv :deduplicate-cv :typespec-equalp :is-numeric :const-p

   :hlts-char :hlts-int8 :hlts-int16 :hlts-int32 :hlts-void

   ;; slots
   :name :ret-type :type :members :signed-p :float-p :bytesize :numeric :ref :elt-type :size :arg-types))

(defpackage :ivy-ast
  (:use :cl :ivy-util :optima :ivy-platform :ivy-type :ivy-state)
  (:export
   :ast :ast.type
   :gast
   :ast-funcall :ast-funcall.target :ast-funcall.args
   :decl :decl.name
   ;;:decl-var-binding :decl-var-binding.name :decl-var-binding.type :decl-var-binding.init
   :decl-function :decl-function.ret-type :decl-function.args :decl-function.body-src :decl-function.body
   :decl-variable :decl-variable.type :decl-variable.storage :decl-variable.init
   :ast-var-ref :ast-var-ref.var
   :ast-func-ref :ast-func-ref.func
   :ast-unop :ast-unop.operand :ast-unop.opstr
   :ast-unop-deref :ast-unop-cast
   :ast-binop :ast-binop.left :ast-binop.right :ast-binop.opstr
   :ast-binop-mbracc :ast-binop-mbracc.left :ast-binop-mbracc.right :ast-binop-mbracc.type
   :ast-let :ast-let.bindings :ast-let.body
   :ast.type :gast.type
   :binop-+ :ast-binop-+ :binop-- :ast-binop-- :ast-binop-= :binop-= :binop-aref :ast-binop-aref
   :member-access-syntax
   :ast-do :ast-while
   :lvalue-p

   ;; slots
   :type :name :init :ret-type :args :body-src :body :type :storage :target :visibility
   :var :func :left :right :opstr :bindings :condition))

(defpackage :ivy-hll-kw
  (:export
   :include
   :function :array :const :vltl :&
   :fun :struct
   :defun :defun2 :defstruct :defunion :defvar
   :do :while
   :char :int16 :int32 :int8 :void
   :+ :- := :* :/ :%
   :cast
   :aref
   :$ :$$ :$$$ :$$$$
   :let
   :export))

(defpackage :ivy-hll
  (:use :cl :optima :ivy-util :ivy-platform :ivy-ast :ivy-type :ivy-state)
  (:shadow :compile-file)
  (:export :compile-file))

(defpackage :ivy-core
  (:use :cl))

(defpackage :ivy-hll-user
  (:use :ivy-hll-kw :ivy-core))

(defpackage :ivy-target-il
  (:use :cl :ivy-util :ivy-ast :ivy-state :ivy-type :optima)
  (:export :emit))

(defpackage :ivy-target-c
  (:use :cl :ivy-util :ivy-ast :ivy-state :ivy-type)
  (:export :emit))

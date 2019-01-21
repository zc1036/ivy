
(in-package :ix-ast)

(defclass ast ()
  ((type :type typespec :accessor ast.type)))

;; gast = generalized ast, means an ast type OR a literal type (int, string,
;; etc)
(deftype gast ()
  '(or ast integer))

(defclass decl ()
  ((name :type symbol :initarg :name :accessor decl.name)))

(defstruct (decl-var-binding (:conc-name decl-var-binding.))
  (name nil :type symbol)
  (type nil :type typespec)
  (init nil :type (or null gast)))

(defclass decl-function (decl)
  ((ret-type :type typespec                   :initarg :ret-type :accessor decl-function.ret-type)
   (args     :type (list-of decl-var-binding) :initarg :args     :accessor decl-function.args)
   (body-src :type list                       :initarg :body-src :accessor decl-function.body-src)
   (body     :type (list-of gast)             :initarg :body     :accessor decl-function.body)))

(defclass decl-variable (decl)
  ((type     :type typespec :initarg :type    :accessor decl-variable.type)
   ;; can be :local or :global
   (storage  :type symbol   :initarg :storage :accessor decl-variable.storage)))

(defclass ast-funcall (ast)
  ((target :type gast           :initarg :target :accessor ast-funcall.target)
   (args   :type (list-of gast) :initarg :args   :accessor ast-funcall.args)))

(defclass ast-var-ref (ast)
  ((var :type decl-variable :initarg :var :accessor ast-var-ref.var)))

(defclass ast-func-ref (ast)
  ((func :type decl-function :initarg :func :accessor ast-func-ref.func)
   (type :type typespec      :initarg :type :accessor ast.type)))

(defclass ast-unop (ast)
  ((operand :type gast     :initarg :operand :accessor ast-unop.operand)
   (opstr   :type string   :initarg :opstr   :accessor ast-unop.opstr)
   (type    :type typespec :initarg :type    :accessor ast.type)))

(defclass ast-binop (ast)
  ((left  :type gast     :initarg :left  :accessor ast-binop.left)
   (right :type gast     :initarg :right :accessor ast-binop.right)
   (opstr :type string   :initarg :opstr :accessor ast-binop.opstr)
   (type  :type typespec :initarg :type  :accessor ast.type)))

(defclass ast-binop-mbracc (ast)
  ((left  :type gast     :initarg :left  :accessor ast-binop-mbracc.left)
   (right :type symbol   :initarg :right :accessor ast-binop-mbracc.right)
   (type  :type typespec :initarg :type  :accessor ast-binop-mbracc.type))
  (:documentation
   "An AST node for structure/union member access"))
 
(defclass ast-let (ast)
  ((bindings :type (list-of decl-var-binding) :initarg :bindings :accessor ast-let.bindings)
   (body     :type (list-of gast)             :initarg :body     :accessor ast-let.body)))

(defclass ast-while (ast)
  ((condition :type gast           :initarg :condition :accessor ast-while.condition)
   (body      :type (list-of gast) :initarg :body      :accessor ast-while.body)))

;;; ast.type methods

(defmethod ast.type ((x ast-funcall))
  (let ((target-type (remove-cv (ast.type (ast-funcall.target x)))))
    (ematch target-type
      ((class typespec-function ret-type)
       ret-type)
      (_ (error "Cannot determine type for function call")))))

(defmethod ast.type ((x ast-var-ref))
  (decl-variable.type (ast-var-ref.var x)))

;;; type-related functions

(defgeneric gast.type (gast))

(defmethod gast.type ((a ast))
  (ast.type a))

(defmethod gast.type ((a integer))
  (declare (special ix-hll-kw:int32))
  
  ix-hll-kw:int32)

(defun lvalue-p (a)
  (etypecase a
    (ast-var-ref t)
    (ast-binop-aref t)
    (ast-unop-deref t)
    (t nil)))

;;; hll operators

(defmacro define-nary-syntax-by-binary (nary-fn-name binop-fn-name assoc)
  `(defun ,nary-fn-name (a b &rest args)
     (if args
         ,(ecase assoc
            (:left
             `(apply #',nary-fn-name (cons (,binop-fn-name a b) args)))
            (:right
             `(,binop-fn-name a (apply #',nary-fn-name (cons b args)))))
         (,binop-fn-name a b))))

(defmacro define-binary-operator-with-nary-syntax (opstr assoc binop-fn-name binop-class-name nary-fn-name type-check type-result)
  `(progn
     (defclass ,binop-class-name (ast-binop)
       ((opstr :initform ,opstr)))

     (defun ,binop-fn-name (a b)
       (let* ((a-type (gast.type a))
              (b-type (gast.type b))
              (a-type-nocv (remove-cv a-type))
              (b-type-nocv (remove-cv b-type)))
         (unless (,type-check a-type-nocv b-type-nocv)
           (error "Applying binary operator ~a to expressions of incorrect type" ,opstr))
         (make-instance ',binop-class-name
                        :type (,type-result a-type-nocv b-type-nocv)
                        :left a
                        :right b)))

     (define-nary-syntax-by-binary ,nary-fn-name ,binop-fn-name ,assoc)))

(defun numeric-binop-type-check (atype-nocv btype-nocv)
  (and (is-numeric atype-nocv)
       (is-numeric btype-nocv)
       (typespec-equalp atype-nocv btype-nocv)))

(defun numeric-type-result (atype-nocv btype-nocv)
  (declare (ignore btype-nocv))
  atype-nocv)

(defun aref-type-check (atype-nocv btype-nocv)
  (and (or (typecase atype-nocv
             (typespec-pointer t)
             (typespec-array t)))
       (is-numeric btype-nocv)))

(defun aref-type-result (atype-nocv btype-nocv)
  (declare (ignore btype-nocv))
  (ematch atype-nocv
    ((typespec-pointer ref) ref)
    ((typespec-array elt-type) elt-type)))

(define-binary-operator-with-nary-syntax "+"    :left binop-+    ast-binop-+    ix-hll-kw:+    numeric-binop-type-check numeric-type-result)
(define-binary-operator-with-nary-syntax "-"    :left binop--    ast-binop--    ix-hll-kw:-    numeric-binop-type-check numeric-type-result)
(define-binary-operator-with-nary-syntax "aref" :left binop-aref ast-binop-aref ix-hll-kw:aref aref-type-check          aref-type-result)

(defclass ast-unop-deref (ast-unop)
  ((opstr :initform "$")))

(defun ix-hll-kw:$ (opnd)
  (let ((type (remove-cv (gast.type opnd))))
    (ematch type
      ((class typespec-pointer ref)
       (make-instance 'ast-unop-deref
                      :operand opnd
                      :type ref))
      (_
       (error "Object is not a pointer")))))

(defun ix-hll-kw:$$ (opnd)
  (ix-hll-kw:$ (ix-hll-kw:$ opnd)))

(defun ix-hll-kw:$$$ (opnd)
  (ix-hll-kw:$ (ix-hll-kw:$ (ix-hll-kw:$ opnd))))

(defun ix-hll-kw:$$$$ (opnd)
  (ix-hll-kw:$ (ix-hll-kw:$ (ix-hll-kw:$ (ix-hll-kw:$ opnd)))))

(defclass ast-binop-= (ast-binop)
  ((opstr :initform "=")))

(defun binop-= (a b)
  (let ((a-type (gast.type a))
        (b-type (gast.type b)))
    (unless (typespec-equalp (remove-cv a-type) (remove-cv b-type))
      (error "Assigning an expression of one type to an lvalue of another type"))
    (unless (lvalue-p a)
      (error "Assigning to a non-lvalue"))
    (when (const-p a-type)
      (error "Assigning to a constant expression"))
    (make-instance 'ast-binop-=
                   :type (ast.type a)
                   :left a
                   :right b)))

(define-nary-syntax-by-binary ix-hll-kw:= binop-= :right)

(defun mbr (obj member)
  (check-type member symbol)

  (let+ ((type (gast.type obj))
         (type-nocv (remove-cv type))
         ((members name type-name)
          (ematch type-nocv
            ((class typespec-atom (ref (class hltype-structure name members)))
             (list members name "struct"))
            ((class typespec-atom (ref (class hltype-union name members)))
             (list members name "union"))
            (_
             (error "Object of type ~a is not a struct or union" (typespec.to-string type))))))
    (let ((mbr-info (agg-lookup-member members member)))
      (unless mbr-info
        (error "Member ~a not present in ~a ~a" member type-name name))
      (make-instance 'ast-binop-mbracc
                     :left obj
                     :right member
                     :type (deduplicate-cv (propagate-cv type (hltype-agg-member.type mbr-info)))))))

(defun deref-mbr (obj member)
  (let* ((type (gast.type obj))
         (type-nocv (remove-cv type)))
    (ematch type-nocv
      ((class typespec-pointer ref)
       (deref-mbr (ix-hll-kw:$ obj) member))
      (_
       (mbr obj member)))))

(defun make-member-access-ast (a &optional b &rest rest)
  (ematch b
    ((list 'quote x)
     (if rest
         (apply #'make-member-access-ast (cons `(deref-mbr ,a ,b) rest))
         `(deref-mbr ,a ,b)))
    (var
     (if rest
         (apply #'make-member-access-ast (cons `(ix-hll-kw:aref ,a ,b) rest))
         `(ix-hll-kw:aref ,a ,b)))))

(defun member-access-syntax (stream char)
  "We want things like
    [array 1]
    [array idx]
    [struct'mbr]
    [ptr_to_struct'mbr]
    [(func) 'mbr1'mbr2 idx (+ 1 2)]
   to work."
  (declare (ignore char))

  (apply #'make-member-access-ast (read-delimited-list #\] stream t)))

;;; hll struct/union definition

(defun process-struct-members (mbrs)
  (loop for mbr in mbrs collect
       (ematch mbr
         ((list name type)
          (unless (symbolp name)
            (error "Invalid structure member name ~a" name))
          (unless (typep type 'typespec)
            (error "Invalid structure member type ~a" type))
          (make-hltype-agg-member :name name :type type))
         (_ (error "Invalid structure member specification ~a" mbr)))))

(defun make-struct-member-specs (mbrs)
  (loop for mbr in mbrs collect
       `(list ',(car mbr) ,@(cdr mbr))))

(defun ix-hll-kw:struct (s)
  (unless (typep s 'hltype-structure)
    (error "~a is not a structure" s))

  (make-instance 'typespec-atom :ref s))

(defun define-aggregate-type (name hltype pretty-name gensym-prefix body)
  `(progn
     (defvar ,name nil)
     (when ,name
       (error "Defining ~a ~a: name already defined" ,pretty-name ',name))
     (setf ,name
           (make-instance ',hltype
                          :name (gensym ,gensym-prefix)
                          :members (process-struct-members
                                    (list ,@(make-struct-member-specs body)))))
     (setf (hltype.name ,name) ',name)))

(defmacro ix-hll-kw:defstruct (name &body body)
  (define-aggregate-type name 'hltype-structure "structure" "STRUCT" body))

(defmacro ix-hll-kw:defunion (name &body body)
  (define-aggregate-type name 'hltype-union "union" "UNION" body))

;;; LET form

(defun make-let-bindings (names types% inits%)
  (let ((bindings-and-inits
         (loop for name in names
               for i from 0
               for decl-sym = (gensym "LOCAL") 
            collect
              (list
               `(make-decl-var-binding :name ',name
                                       :type (nth ,i ,types%)
                                       :init (nth ,i ,inits%))
               `(,decl-sym
                 (make-instance 'decl-variable
                                :name ',name
                                :type (nth ,i ,types%)
                                :storage :local))
               `(,name
                 (make-instance 'ast-var-ref
                                :var ,decl-sym))
               `(when (nth ,i ,inits%)
                  (binop-= ,name (nth ,i ,inits%)))))))
    (values (mapcar #'first  bindings-and-inits)
            (mapcar #'second bindings-and-inits)
            (mapcar #'third  bindings-and-inits)
            (mapcar #'fourth bindings-and-inits))))

(defmacro ix-hll-kw:let (args &body body)
  (let ((types% (gensym "TYPES"))
        (inits% (gensym "INITS"))
        (names (mapcar #'car args)))
    `(let ((,types% (list ,@(mapcar #'cadr args)))
           (,inits% (list ,@(mapcar #'caddr args))))
       ,(multiple-value-bind (bindings decl-bindings macro-bindings initializers) (make-let-bindings names types% inits%)
          `(make-instance 'ast-let
                          :bindings (list ,@bindings)
                          :body (let ,decl-bindings
                                  (symbol-macrolet ,macro-bindings
                                    (list
                                     ,@initializers
                                     ,@body))))))))

(defmacro ix-hll-kw:while (condition &body body)
  (let ((cond% (gensym)))
    `(progn
       (let ((,cond% ,condition))
         (unless (is-numeric (gast.type ,cond%))
           (error "Condition of while-loop must be integral"))
         (make-instance 'ast-while
                        :condition ,cond%
                        :body (list ,@body))))))

(defun make-funcall (funcref args)
  (let ((func (ast-func-ref.func funcref)))
    (loop for arg in args for param in (decl-function.args func) do
         (when (not (typespec-equalp (remove-cv (gast.type arg))
                                     (remove-cv (decl-var-binding.type param))))
           (error "Argument incompatible with parameter ~a in call to ~a; expected ~a, got ~a"
                  (decl-var-binding.name param)
                  (decl.name func)
                  (typespec.to-string (decl-var-binding.type param))
                  (typespec.to-string (gast.type arg))))))
  (make-instance 'ast-funcall
                 :target funcref
                 :args args))

;;; hll function definition

(defmacro ix-hll-kw:fun (ret-type args &body body)
  (let ((types% (gensym "TYPES"))
        (names (mapcar #'car args)))
    (when (not (every (lambda (x) (= (length x) 2))
                    args))
      (error "Malformed argument list ~a" args))
    `(let ((,types% (list ,@(mapcar #'cadr args))))
       ,(multiple-value-bind (bindings decl-bindings macro-bindings initializers) (make-let-bindings names types% nil)
          `(make-instance 'decl-function
                          :name (gensym "FN")
                          :ret-type ,ret-type
                          :args (list ,@bindings)
                          :body-src (list ,@(mapcar (lambda (x) `(quote ,x))
                                                    body))
                          :body (let ,decl-bindings
                                  (symbol-macrolet ,macro-bindings
                                    (list
                                     ,@body))))))))

(defmacro ix-hll-kw:defun (name ret-type args &body body)
  (let ((rest% (gensym))
        (fn% (gensym))
        (ret-type% (gensym)))
    `(progn
       (defvar ,name nil)

       (when ,name
         (error "Defining function ~a: name already defined" ',name))

       (setf ,name (make-instance 'ast-func-ref))

       (defun ,name (&rest ,rest%)
         (make-funcall ,name ,rest%))

       (let* ((,ret-type% ,ret-type)
              (,fn% (ix-hll-kw:fun ,ret-type% ,args ,@body)))
         (setf (decl.name ,fn%) ',name)
         (push ,fn% (state.functions *state*))
         ;; we do this here rather than moving the setf of ,name down here
         ;; because in the case of recursive functions, we want them to be able
         ;; to grab a reference to themselves before that reference is filled
         ;; out
        (setf (ast.type ,name)
               (make-instance 'typespec-function
                              :ret-type ,ret-type%
                              :arg-types (mapcar #'decl-var-binding.type
                                                 (decl-function.args ,fn%))))
         (setf (ast-func-ref.func ,name)
               (car (state.functions *state*)))))))

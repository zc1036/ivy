
;;;; ix-hll.lisp

(in-package :ix-hll)

(defun all-of-type (type list)
  (every type list))

(deftype list-of (type)
  (declare (ignore type))
  'list)

;; hltypes have referential equality

(defclass hltype ()
  ((name :type symbol :initarg :name :accessor hltype.name)))

(defstruct (hlt-agg-member (:conc-name hlt-agg-member.))
  (name nil :type symbol)
  (type nil :type typespec))

(defclass hlt-structure (hltype)
  ((members :type (list-of hlt-agg-member)
            :initarg :members
            :accessor hlt-structure.members)))

(defclass hlt-union (hltype)
  ((members :type (list-of hlt-agg-member)
            :initarg :members
            :accessor hlt-union.members)))

(defclass hlt-builtin (hltype)
  ((signed-p :type boolean :initarg :signed-p :accessor hlt-builtin.signed-p)
   (float-p  :type boolean :initarg :float-p  :accessor hlt-builtin.float-p)
   (bytesize :type integer :initarg :bytesize :accessor hlt-builtin.bytesize)
   (numeric  :type boolean :initarg :numeric  :accessor hlt-builtin.numeric)))

;; typespecs have structural equality

(defclass typespec ()
  ())

(defclass typespec-atom (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-atom.ref)))

(defclass typespec-const (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-const.ref)))

(defclass typespec-volatile (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-volatile.ref)))

(defclass typespec-pointer (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-pointer.ref)))

(defclass typespec-function (typespec)
  ((ret-type  :type typespec           :initarg :ret-type  :accessor typespec-function.ret-type)
   (arg-types :type (list-of typespec) :initarg :arg-types :accessor typespec-function.arg-types)))

(defclass decl ()
  ((name :type symbol :initarg :name :accessor decl.name)))

(defstruct (decl-function-arg (:conc-name decl-function-arg.))
  (name nil :type symbol)
  (type nil :type typespec))

(defclass decl-function (decl)
  ((ret-type :type typespec                    :initarg :ret-type :accessor decl-function.ret-type)
   (args     :type (list-of decl-function-arg) :initarg :args     :accessor decl-function.args)
   (body     :type (list-of gast)              :initarg :body     :accessor decl-function.body)))

(defclass ast ()
  ((type :type typespec :accessor ast.type)))

(deftype gast ()
  '(or ast integer))

;; Converts the given gast to IL
(defgeneric gast.emit (gast))

(defclass ast-funcall (ast)
  ((target :type gast           :initarg :expr :accessor ast-funcall.target)
   (args   :type (list-of gast) :initarg :args :accessor ast-funcall.args)))

(defclass decl-variable (decl)
  ((type     :type typespec :initarg :type    :accessor decl-variable.type)
   ;; can be :local or :global
   (storage  :type symbol   :initarg :storage :accessor decl-variable.storage)))

(defclass ast-var-ref (ast)
  ((var :type decl-variable :initarg :var :accessor ast-var-ref.var)))

(defclass ast-func-ref (ast)
  ((func :type decl-function :initarg :func :accessor ast-func-ref.func)
   (type :type typespec      :initarg :type :accessor ast.type)))

(defclass ast-binop (ast)
  ((left  :type gast     :initarg :left  :accessor ast-binop.left)
   (right :type gast     :initarg :right :accessor ast-binop.right)
   (opstr :type string   :initarg :opstr :accessor ast-binop.opstr)
   (type  :type typespec :initarg :type)))
 
(defstruct (ast-let-binding (:conc-name ast-let-binding.))
  (name nil :type symbol)
  (type nil :type typespec)
  (init nil :type (or null gast)))

(defclass ast-let (ast)
  ((bindings :type (list-of ast-let-binding) :initarg :bindings :accessor ast-let.bindings)
   (body     :type (list-of gast)            :initarg :body     :accessor ast-let.body)))

;;; the global hll compiler state

(defstruct (lexical-scope (:conc-name lexical-scope.))
  (names nil :type list)
  (next  nil :type (or lexical-scope null)))

(defstruct (state (:conc-name state.))
  (functions nil :type (list-of decl-function)))

(defparameter *state* (make-state))

;;; ast.type methods

(defmethod ast.type ((x ast-funcall))
  (let ((target-type (remove-cv (ast.type (ast-funcall.target x)))))
    (match target-type
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

(defgeneric remove-cv (typespec))

(defmethod remove-cv ((x typespec-const))
  (remove-cv (typespec-const.ref x)))

(defmethod remove-cv ((x typespec-volatile))
  (remove-cv (typespec-volatile.ref x)))

(defmethod remove-cv ((x typespec))
  x)

(defun is-numeric (x)
  (ematch x
    ((class typespec-atom (ref (class hlt-builtin numeric)))
     numeric)
    ((class typespec)
     nil)))

(defun typespec-equalp (a b)
  (ematch (list a b)
    ((list (class typespec-const (ref aref)) (class typespec-const (ref bref)))
     (typespec-equalp aref bref))
    ((list (class typespec-volatile (ref aref)) (class typespec-volatile (ref bref)))
     (typespec-equalp aref bref))
    ((list (class typespec-pointer (ref aref)) (class typespec-pointer (ref bref)))
     (typespec-equalp aref bref))
    ((list (class typespec-function (ret-type a-ret) (arg-types a-args))
           (class typespec-function (ret-type b-ret) (arg-types b-args)))
     (and (typespec-equalp a-ret b-ret)
          (= (length a-args) (length b-args))
          (every #'typespec-equalp a-args b-args)))
    ((list (class typespec-atom (ref aref)) (class typespec-atom (ref bref)))
     (eq aref bref))
    ((list (class typespec) (class typespec))
     nil)))

;;(defgeneric typespec-equalp (a b))
;;
;;(defmethod typespec-equalp ((a typespec-const) (b typespec-const))
;;  (typespec-equalp (typespec-const.ref a) (typespec-const.ref b)))
;;
;;(defmethod typespec-equalp ((a typespec-volatile) (b typespec-volatile))
;;  (typespec-equalp (typespec-volatile.ref a) (typespec-volatile.ref b)))
;;
;;(defmethod typespec-equalp ((a typespec-pointer) (b typespec-pointer))
;;  (typespec-equalp (typespec-pointer.ref a) (typespec-pointer.ref b)))
;;
;;(defmethod typespec-equalp ((a typespec-function) (b typespec-function))
;;  (with-slots ((a-ret ret-type) (a-args arg-types)) a
;;    (with-slots ((b-ret ret-type) (b-args arg-types)) b
;;      (and (typespec-equalp a-ret b-ret)
;;           (= (length a-args) (length b-args))
;;           (every #'typespec-equalp a-args b-args)))))
;;
;;(defmethod typespec-equalp ((a typespec-atom) (b typespec-atom))
;;  (eq (typespec-atom.ref a) (typespec-atom.ref b)))
;;
;;(defmethod typespec-equalp (a b)
;;  nil)

(defun const-p (a)
  (match a
    ((class typespec-volatile ref) (const-p ref))
    ((class typespec-const)        t)
    (_ nil)))

(defun lvalue-p (a)
  (typecase a
    (ast-binop-= t)
    (ast-var-ref t)))

;;; hll global names

(defvar hlts-int32 (make-instance 'hlt-builtin
                                  :signed-p t
                                  :float-p nil
                                  :bytesize 4
                                  :name :int32
                                  :numeric t))

(defvar ix-hll-kw:int32 (make-instance 'typespec-atom :ref hlts-int32))

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

(defmacro define-numeric-binary-operator-with-nary-syntax (opstr assoc binop-fn-name binop-class-name nary-fn-name)
  `(progn
     (defclass ,binop-class-name (ast-binop)
       ((opstr :initform ,opstr)))

     (defun ,binop-fn-name (a b)
       (let* ((a-type (gast.type a))
              (b-type (gast.type b))
              (a-type-nocv (remove-cv a-type))
              (b-type-nocv (remove-cv b-type)))
         (unless (and (is-numeric a-type-nocv) (is-numeric b-type-nocv))
           (error "Applying binary operator ~a to expressions of non-numeric type" ,opstr))
         (unless (typespec-equalp a-type-nocv b-type-nocv)
           (error "Applying binary operator ~a to expressions of different types" ,opstr))
         (make-instance ',binop-class-name
                        :type a-type-nocv
                        :left a
                        :right b)))

     (define-nary-syntax-by-binary ,nary-fn-name ,binop-fn-name ,assoc)))

(define-numeric-binary-operator-with-nary-syntax "+" :left binop-+ ast-binop-+ ix-hll-kw:+)
(define-numeric-binary-operator-with-nary-syntax "-" :left binop-- ast-binop-- ix-hll-kw:-)

(defclass ast-binop-= (ast-binop)
  ((opstr :initform "=")))

(defun binop-= (a b)
  (unless (typespec-equalp (remove-cv a) (remove-cv b))
    (error "Assigning an expression of one type to an lvalue of another type"))
  (when (const-p a)
    (error "Assigning to a constant expression"))
  (unless (lvalue-p a)
    (error "Assigning to a non-lvalue"))
  (make-instance 'ast-binop-=
                 :type (ast.type a)
                 :left a
                 :right b))

(define-nary-syntax-by-binary ix-hll-kw:= binop-= :right)

;;; gast.emit helpers

(defun reg-extend (a b)
  "Returns a list ((areg ainstrs) (breg binstrs))"
  (let ((regsize (max (ix-il:reg.bytesize a) (ix-il:reg.bytesize b))))
    (list
     (if (= (ix-il:reg.bytesize a) regsize)
         (list a nil)
         (ix-il:with-reg newreg regsize (ix-il:ext newreg a)))
     (if (= (ix-il:reg.bytesize b) regsize)
         (list b nil)
         (ix-il:with-reg newreg regsize (ix-il:ext newreg b))))))

;;; gast.emit methods

(defmethod gast.emit ((a integer))
  (let* ((imm (ix-il:i a))
         (reg (ix-il:r (ix-il:imm.bytesize imm))))
    (list reg (ix-il:move reg imm))))

(defmethod gast.emit ((a ast-binop-+))
  (with-slots (left right) a
    (let+ (((left-res left-il)              (gast.emit left))
           ((right-res right-il)            (gast.emit right))
           (((areg ainstrs) (breg binstrs)) (reg-extend left-res right-res)))
      (ix-il:with-reg resreg areg
        (list left-il right-il ainstrs binstrs (ix-il:add resreg areg breg))))))

(defmethod gast.emit ((a ast-binop--))
  (with-slots (left right) a
    (let+ (((left-res left-il)              (gast.emit left))
           ((right-res right-il)            (gast.emit right))
           (((areg ainstrs) (breg binstrs)) (reg-extend left-res right-res)))
      (ix-il:with-reg resreg areg
        (list left-il right-il ainstrs binstrs (ix-il:sub resreg areg breg))))))

(defmethod gast.emit ((a ast-var-ref))
  (let ((var (ast-var-ref.var a)))
    )
  ;; resume here, mapping decl-variables to IL registers
  )

;;; hll struct definition

(defun process-struct-members (mbrs)
  (loop for mbr in mbrs collect
       (match mbr
         ((list name type)
          (unless (symbolp name)
            (error "Invalid structure member name ~a" name))
          (unless (typep type 'typespec)
            (error "Invalid structure member type ~a" type)))
         (_ (error "Invalid structure member specification ~a" mbr)))))

(defun make-struct-member-specs (mbrs)
  (loop for mbr in mbrs collect
       `(list ',(car mbr) ,@(cdr mbr))))

(defmacro ix-hll-kw:struct (&body body)
  `(make-instance 'hlt-structure
                  :name (gensym "STRUCT")
                  :members (process-struct-members
                            (list ,@(make-struct-member-specs body)))))

(defmacro ix-hll-kw:defstruct (name &body body)
  `(progn
     (defvar ,name nil)
     (when ,name
       (error "Defining structure ~a: name already defined" ',name))
     (setf ,name (ix-hll-kw:struct ,@body))
     (setf (hltype.name ,name) ',name)))

;;; LET form

(defun make-let-bindings (names types% inits%)
  (let ((bindings-and-inits
         (loop for name in names for i from 0 collect
              (list
               `(make-ast-let-binding :name ',name
                                      :type (nth ,i ,types%)
                                      :init (nth ,i ,inits%))
               `(,name
                 (make-instance 'ast-var-ref
                                :var (make-instance 'decl-variable
                                                    :name ',name
                                                    :type (nth ,i ,types%)
                                                    :storage :local)))
               `(when (nth ,i ,inits%)
                  (binop-= ,name (nth ,i ,inits%)))))))
    (values (mapcar #'car  bindings-and-inits)
            (mapcar #'cadr bindings-and-inits)
            (mapcar #'caddr bindings-and-inits))))

(defmacro ix-hll-kw:let (bindings &body body)
  (let ((types% (gensym "TYPES"))
        (inits% (gensym "INITS"))
        (names (mapcar #'car bindings)))
    `(let ((,types% (list ,@(mapcar #'cadr bindings)))
           (,inits% (list ,@(mapcar #'caddr bindings))))
       ,(multiple-value-bind (bindings let-bindings initializers) (make-let-bindings names types% inits%)
          `(make-instance 'ast-let
                          :bindings (list ,@bindings)
                          :body (let ,let-bindings
                                  (list
                                   ,@initializers
                                   ,@body)))))))

;;; hll function definition

(defun make-defun-arg-types (args)
  (loop for arg in args collect
       (match arg
         ((list name type)
          `(make-decl-function-arg :name ',name :type ,type))
         (_ (error "Invalid argument specification in function definition")))))

(defun make-defun-arg-bindings (args)
  (multiple-value-bind (let-bindings initializers) (make-let-bindings args)
    (declare (ignore initializers))
    let-bindings))

(defmacro ix-hll-kw:fun (ret-type args &body body)
  `(make-instance 'decl-function
                  :name (gensym "FN")
                  :ret-type ,ret-type
                  :args (list ,@(make-defun-arg-types args))
                  :body (let ,(make-defun-arg-bindings args)
                          (list ,@body))))

(defmacro ix-hll-kw:defun (name ret-type args &body body)
  (let ((rest% (gensym))
        (fn% (gensym))
        (ret-type% (gensym)))
    `(progn
       (defvar ,name nil)

       (when ,name
         (error "Defining function ~a: name already defined" ',name))

       (let* ((,ret-type% ,ret-type)
              (,fn% (ix-hll-kw:fun ,ret-type% ,args ,@body)))
         (setf (decl.name ,fn%) ',name)
         (push ,fn% (state.functions *state*))
         (setf ,name (make-instance 'ast-func-ref
                                    :type (make-instance 'typespec-function
                                                         :ret-type ,ret-type%
                                                         :arg-types (mapcar #'decl-function-arg.type
                                                                            (decl-function.args ,fn%)))
                                    :func (car (state.functions *state*)))))

       (defun ,name (&rest ,rest%)
         (make-instance 'ast-funcall
                        :target ,name
                        :args ,rest%)))))

;;; main program

(defun print-instrs (instrs)
  (etypecase instrs
    (list (mapc #'print-instrs instrs))
    (ix-il:optr (format t "  ~a~%" (ix-il:optr.repr instrs)))))

(defun main (argv)
  (loop for arg in (cdr argv) do
       (format t "Compiling ~a~%~%" arg)
       (let ((*package* (find-package 'ix-hll-user)))
         (load arg)))

  (loop for func in (state.functions *state*) do
       (format t "Function ~a:~%" (decl.name func))

       (loop for elem in (decl-function.body func) do
            (let+ (((result ops) (gast.emit elem)))
              result
              (print-instrs ops)
              (format t " ;;~%")))))

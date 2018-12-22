
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

(defstruct (hltype-agg-member (:conc-name hltype-agg-member.))
  (name nil :type symbol)
  (type nil :type typespec))

(defclass hltype-structure (hltype)
  ((members :type (list-of hltype-agg-member)
            :initarg :members
            :accessor hltype-structure.members)))

(defclass hltype-union (hltype)
  ((members :type (list-of hltype-agg-member)
            :initarg :members
            :accessor hltype-union.members)))

(defclass hltype-builtin (hltype)
  ((signed-p :type boolean :initarg :signed-p :accessor hltype-builtin.signed-p)
   (float-p  :type boolean :initarg :float-p  :accessor hltype-builtin.float-p)
   (bytesize :type integer :initarg :bytesize :accessor hltype-builtin.bytesize)
   (numeric  :type boolean :initarg :numeric  :accessor hltype-builtin.numeric)))

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

(defclass typespec-array (typespec)
  ((elt-type :type typespec          :initarg :elt-type :accessor typespec-array.elt-type)
   (size     :type (or null integer) :initarg :size     :accessor typespec-array.size)))

(defclass typespec-function (typespec)
  ((ret-type  :type typespec           :initarg :ret-type  :accessor typespec-function.ret-type)
   (arg-types :type (list-of typespec) :initarg :arg-types :accessor typespec-function.arg-types)))

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
 
(defclass ast-let (ast)
  ((bindings :type (list-of decl-var-binding) :initarg :bindings :accessor ast-let.bindings)
   (body     :type (list-of gast)             :initarg :body     :accessor ast-let.body)))

;;; the global hll compiler state

(defstruct (lexical-scope (:conc-name lexical-scope.))
  (bindings nil :type list) ;; alist map of decl-variables to ix-il:regs
  (next     nil :type (or lexical-scope null)))

(defstruct (state (:conc-name state.))
  (functions nil :type (list-of decl-function))
  (lex-vars  nil :type (or null lexical-scope))
  (glob-vars nil :type list)) ;; alist of decl-variables to ix-il:regs

(defparameter *state* (make-state))
(defparameter *target-arch* (make-arch :name "x86-64"
                                       :bits 64))

(defun lexical-scope.lookup (scope var)
  (when scope
    (let ((binding (assoc var (lexical-scope.bindings scope))))
      (if binding
          (cdr binding)
          (lexical-scope.lookup (lexical-scope.next scope) var)))))

(defun state.lookup-var (state var)
  (lexical-scope.lookup (state.lex-vars state) var))

;;; typespec.alignof functions

(defgeneric typespec.alignof (typespec))

(defmethod typespec.alignof ((a typespec-atom))
  (hltype.alignof (typespec-atom.ref a)))

(defmethod typespec.alignof ((a typespec-const))
  (hltype.alignof (typespec-const.ref a)))

(defmethod typespec.alignof ((a typespec-volatile))
  (hltype.alignof (typespec-volatile.ref a)))

(defmethod typespec.alignof ((a typespec-pointer))
  (/ (arch.bits *target-arch*) 8))

(defmethod typespec.alignof ((a typespec-array))
  (typespec.alignof (typespec-array.elt-type a)))

(defmethod typespec.alignof ((a typespec-function))
  (error "Cannot evaluate alignment of function"))

;;; hltype.alignof functions

(defgeneric hltype.alignof (hltype))

(defmethod hltype.alignof ((a hltype-builtin))
  (hltype.sizeof a))

(defmethod hltype.alignof ((a hltype-structure))
  (apply #'max (mapcar #'typespec.alignof (mapcar #'hltype-agg-member.type (hltype-structure.members a)))))

(defmethod hltype.alignof ((a hltype-union))
  (apply #'max (mapcar #'typespec.alignof (mapcar #'hltype-agg-member.type (hltype-union.members a)))))

;;; typespec.sizeof functions

(defgeneric typespec.sizeof (typespec))

(defmethod typespec.sizeof ((a typespec-atom))
  (hltype.sizeof (typespec-atom.ref a)))

(defmethod typespec.sizeof ((a typespec-const))
  (hltype.sizeof (typespec-const.ref a)))

(defmethod typespec.sizeof ((a typespec-volatile))
  (hltype.sizeof (typespec-volatile.ref a)))

(defmethod typespec.sizeof ((a typespec-pointer))
  (/ (arch.bits *target-arch*) 8))

(defmethod typespec.sizeof ((a typespec-array))
  (with-slots (size elt-type) a
    (unless size
      (error "Cannot evaluate size of an unsized array"))
    (* size (typespec.sizeof elt-type))))

(defmethod typespec.sizeof ((a typespec-function))
  (error "Cannot evaluate size of function"))

;;; hltype.sizeof functions

(defun round-up-to-nearest (x round-to)
  (floor (* (/ (+ (1- round-to) x) round-to) round-to)))

(defun agg-member-type-size-offset-align (members)
  (let* ((sum 0)
         (mbr-types (mapcar #'hltype-agg-member.type members))
         (mbr-sizes (mapcar #'typespec.sizeof mbr-types))
         (mbr-aligns (mapcar #'typespec.alignof mbr-types)))
    (loop
       for type in mbr-types
       for size in mbr-sizes
       for align in mbr-aligns
       collect
         (let ((offset (round-up-to-nearest sum align)))
           (setf sum (+ offset size))
           (list type size offset align)))))

(defgeneric hltype.sizeof (hltype))

(defmethod hltype.sizeof ((a hltype-builtin))
  (hltype-builtin.bytesize a))

(defmethod hltype.sizeof ((a hltype-structure))
  (let+ ((member-info (agg-member-type-size-offset-align (hltype-structure.members a)))
         (struct-align (hltype.alignof a))
         ((type size offset align) (last member-info)))
    (progn type align (round-up-to-nearest (+ offset size) struct-align))))

(defmethod hltype.sizeof ((a hltype-union))
  (apply #'max (mapcar #'second (agg-member-type-size-offset-align (hltype-union.members a)))))

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
    ((class typespec-atom (ref (class hltype-builtin numeric)))
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
    ((list (class typespec-array (elt-type a-elt) (size a-size))
           (class typespec-array (elt-type b-elt) (size b-size)))
     (and (typespec-equalp a-elt b-elt)
          (or (and (not a-size) (not b-size))
              (and a-size b-size (= a-size b-size)))))
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

(defvar hlts-int32 (make-instance 'hltype-builtin
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
  (let ((a-type (gast.type a))
        (b-type (gast.type b)))
    (unless (typespec-equalp (remove-cv a-type) (remove-cv b-type))
      (error "Assigning an expression of one type to an lvalue of another type"))
    (when (const-p a-type)
      (error "Assigning to a constant expression"))
    (unless (lvalue-p a)
      (error "Assigning to a non-lvalue"))
    (make-instance 'ast-binop-=
                   :type (ast.type a)
                   :left a
                   :right b)))

(define-nary-syntax-by-binary ix-hll-kw:= binop-= :right)

;;; gast.emit helpers

(defun reg-extend (a b)
  "Emits necessary EXT instructions to extend register A to the size of B or
   vice versa, whichever creates a larger register. 
   Returns a list ((areg ainstrs) (breg binstrs))"
  (let ((regsize (max (ix-il:reg.bytesize a) (ix-il:reg.bytesize b))))
    (list
     (if (= (ix-il:reg.bytesize a) regsize)
         (list a nil)
         (ix-il:with-reg newreg regsize (ix-il:ext newreg a)))
     (if (= (ix-il:reg.bytesize b) regsize)
         (list b nil)
         (ix-il:with-reg newreg regsize (ix-il:ext newreg b))))))

;;; gast.emit methods

(defmethod gast.emit ((a null))
  ;; nothing
  (list () ()))

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

(defmacro with-lexical-scope (bindings &body body)
  ;;; given BINDINGS :: (list-of decl-var-binding), evaluates BODY in a new
  ;;; lexical context wherein each binding in BINDINGS is active.
  (with-gensyms (old-scope% new-scope% binding%)
    `(let* ((,old-scope% (state.lex-vars *state*))
            (,new-scope% (make-lexical-scope :next ,old-scope%)))
       (loop for ,binding% in ,bindings do
            (push (cons (decl-var-binding.name ,binding%)
                        (ix-il:r (typespec.sizeof (decl-var-binding.type ,binding%)) (decl-var-binding.name ,binding%)))
                  (lexical-scope.bindings ,new-scope%)))
       (setf (state.lex-vars *state*) ,new-scope%)
       (unwind-protect
            (progn ,@body)
         (setf (state.lex-vars *state*) ,old-scope%)))))

(defmethod gast.emit ((a ast-let))
  (with-slots (bindings body) a
    (with-lexical-scope bindings
      (let ((body-emissions (mapcar #'gast.emit body)))
        ;; LET forms evaluate to the last expression in their body
        ;; the rest of the result registers are discarded
        (list (caar (last body-emissions))
              (mapcar #'second body-emissions))))))

(defmethod gast.emit ((a ast-var-ref))
  (let ((var (ast-var-ref.var a)))
    (ecase (decl-variable.storage var)
      (:local
       (let ((local-var-pair (state.lookup-var *state* (decl.name var))))
         (unless local-var-pair
           (error "Lexical variable ~a isn't mapped somehow, this is probably a bug ~a"
                  (decl.name var)
                  (lexical-scope.bindings (state.lex-vars *state*))))
         (list local-var-pair ())))
      (:global
       (let ((glob-var-pair (assoc var (state.glob-vars *state*))))
         (unless glob-var-pair
           (error "Global variable ~a isn't mapped somehow, this is probably a bug" (decl.name var)))
         (list (cdr glob-var-pair) ()))))))

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
  `(make-instance 'hltype-structure
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

;;; hll function definition

(defmacro ix-hll-kw:fun (ret-type args &body body)
  (let ((types% (gensym "TYPES"))
        (inits% (gensym "INITS"))
        (names (mapcar #'car args)))
    `(let ((,types% (list ,@(mapcar #'cadr args)))
           (,inits% (list ,@(mapcar #'caddr args))))
       ,(multiple-value-bind (bindings decl-bindings macro-bindings initializers) (make-let-bindings names types% inits%)
          `(make-instance 'decl-function
                          :name (gensym "FN")
                          :ret-type ,ret-type
                          :args (list ,@bindings)
                          :body-src (list ,@(mapcar (lambda (x) `(quote ,x))
                                                    (append initializers body)))
                          :body (let ,decl-bindings
                                  (symbol-macrolet ,macro-bindings
                                    (list
                                     ,@initializers
                                     ,@body))))))))

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
                                                         :arg-types (mapcar #'decl-var-binding.type
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

       (with-slots (ret-type args body-src body) func
         (with-lexical-scope args
           (loop for elem in body for elem-src in body-src do
                (let+ (((result ops) (gast.emit elem)))
                  result
                  (with-input-from-string (in (format nil "~a" elem-src))
                    (loop for line = (read-line in nil)
                          while line
                       do (format t " ;; ~a~%" line)))
                  (print-instrs ops)
                  (format t " ;~%")))))))

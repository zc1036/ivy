
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
   (bytesize :type integer :initarg :bytesize :accessor hlt-builtin.bytesize)))

;; typespecs have structural equality

(defclass typespec ()
  ())

(defclass typespec-atom (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-atom.ref)))

(defclass typespec-const (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-const.ref)))

(defclass typespec-volatile (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-volatile.ref)))

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

(defclass ast-funcall (ast)
  ((target :type gast           :initarg :expr :accessor ast-funcall.target)
   (args   :type (list-of gast) :initarg :args :accessor ast-funcall.args)))

(defclass decl-variable (decl)
  ((type     :type typespec :initarg :type    :accessor decl-variable.type)
   ;; can be :local or :global
   (storage  :type symbol   :initarg :storage :accessor decl-variable.storage)))

(defclass ast-var-ref (ast)
  ((var  :type decl-variable :initarg :var  :accessor ast-var-ref.var)
   (type :type typespec      :initarg :type :accessor ast.type)))

(defclass ast-func-ref (ast)
  ((func :type decl-function :initarg :func :accessor ast-func-ref.func)
   (type :type typespec      :initarg :type :accessor ast.type)))

;;; the global hll compiler state

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

;;; remove-cv methods

(defgeneric remove-cv (typespec))

(defmethod remove-cv ((x typespec-const))
  (remove-cv (typespec-const.ref x)))

(defmethod remove-cv ((x typespec-volatile))
  (remove-cv (typespec-volatile.ref x)))

(defmethod remove-cv ((x typespec))
  x)

;;; hll global names

(defvar hlts-int32 (make-instance 'hlt-builtin
                                  :signed-p t
                                  :float-p nil
                                  :bytesize 4
                                  :name :int32))

(defvar ix-hll-kw:int32 (make-instance 'typespec-atom :ref hlts-int32))

;;; hll operators

(defclass ast-binop (ast)
  ((left  :type gast   :initarg :left  :accessor ast-binop.left)
   (right :type gast   :initarg :right :accessor ast-binop.right)
   (opstr :type string :initarg :opstr :accessor ast-binop.opstr)))

(defmacro define-binary-operator-with-nary-syntax (opstr assoc binop-fn-name binop-class-name nary-fn-name)
  `(progn
     (defclass ,binop-class-name (ast-binop)
       ((opstr :initform ,opstr)))

     (defun ,binop-fn-name (a b)
       (make-instance ',binop-class-name
                      :left a
                      :right b))

     (defun ,nary-fn-name (a b &rest args)
       (if args
           ,(ecase assoc
              (:left
               `(apply #',nary-fn-name (cons (,binop-fn-name a b) args)))
              (:right
               `(,binop-fn-name a (apply #',nary-fn-name (cons b args)))))
           (,binop-fn-name a b)))))

(define-binary-operator-with-nary-syntax "+" :left binop-+ ast-binop-+ ix-hll-kw:+)

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

(defmacro ix-hll-kw:defstruct (name &body body)
  `(progn
     (defvar ,name nil)
     (when ,name
       (error "Defining structure ~a: name already defined" ',name))
     (setf ,name
           (make-instance 'hlt-structure
                          :name ',name
                          :members (process-struct-members
                                    (list ,@(make-struct-member-specs body)))))))

;;; hll function definition

(defun make-defun-arg-types (args)
  (loop for arg in args collect
       (match arg
         ((list name type)
          `(make-decl-function-arg :name ,name :type ,type))
         (_ (error "Invalid argument specification in function definition" )))))

(defmacro ix-hll-kw:fun (ret-type args &body body)
  `(make-instance 'decl-function
                  :name (gensym "FN")
                  :ret-type ,ret-type
                  :args (list ,@(make-defun-arg-types args))
                  :body (list ,@body)))

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

(defun main (argv)
  (loop for arg in (cdr argv) do
       (format t "hi ~a~%" arg)
       (let ((*package* (find-package 'ix-hll-user)))
         (load arg))))

(loop for func in (nreverse (state.functions *state))
     emit functions here)


(in-package :ix-hll)

(defclass hltype ()
  ((name :type symbol :initarg :name :accessor hltype.name)))

(defclass hlt-structure (hltype)
  ((members :type list   :initarg :members :accessor hlt-structure.members)))

(defclass hlt-builtin (hltype)
  ((signed-p :type boolean :initarg :signed-p :accessor hlt-builtin.signed-p)
   (float-p  :type boolean :initarg :float-p  :accessor hlt-builtin.float-p)
   (bytesize :type integer :initarg :bytesize :accessor hlt-builtin.bytesize)))

(defclass hlt-function (hltype)
  ((ret-type  :type typespec :initarg ret-type  :accessor hlt-function.ret-type)
   (arg-types :type list     :initarg arg-types :accessor hlt-function.arg-types)))

(defclass typespec ()
  ())

(defclass typespec-atom (typespec)
  ((ref :type hltype :initarg :ref :accessor typespec-atom.ref)))

(defvar hlts-int32 (make-instance 'hlt-builtin
                                  :signed-p t
                                  :float-p nil
                                  :bytesize 4
                                  :name :int32))

(defvar ix-hll-kw:int32 (make-instance 'typespec-atom :ref hlts-int32))

(defclass ast ()
  ())

(defclass ast-funcall (ast)
  ((target :type ast  :initarg :expr :accessor ast-funcall.target)
   (args   :type list :initarg :args :accessor ast-funcall.args)))

(defstruct function-record
  (type nil :type hlt-function)
  (body nil :type ast))

(defstruct (state (:conc-name state.))
  (functions nil :type list))

(defparameter *state* (make-state))

(defun process-struct-members (mbrs)
  (loop for mbr in mbrs collect
       (match mbr
         ((list name type)
          (unless (symbolp name)
            (error :text "foo"))
          (unless (typep type 'typespec)
            (error :text "blah")))
         (_ (error :text "Invalid structure member specification ~a" mbr)))))

(defun make-struct-member-specs (mbrs)
  (loop for mbr in mbrs collect
       `(list ',(car mbr) ,@(cdr mbr))))

(defmacro ix-hll-kw:defstruct (name &body body)
  `(progn
     (defvar ,name nil)
     (when ,name
       (error :text "hi"))
     (setf ,name
           (make-instance 'hlt-structure
                          :name ',name
                          :members (process-struct-members (list ,@(make-struct-member-specs body)))))))

(defmacro ix-hll-kw:defun (name ret-type args &body body)
  (let ((rest% (gensym)))
    `(progn
       (push
        (make-function-record :type (make-instance 'hlt-function
                                                   :name ',name
                                                   :ret-type ,ret-type
                                                   :arg-types (list ,@(make-defun-arg-types args))))
        (state.functions *state*))
       (defun ,name (&rest ,rest%)
         (make-instance 'ast-funcall
                        :target )))))

(defun main (argv)
  (loop for arg in (cdr argv) do
       (format t "hi ~a~%" arg)
       (let ((*package* (find-package 'ix-hll-user)))
         (load arg))))

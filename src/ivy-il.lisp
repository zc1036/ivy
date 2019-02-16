
;;;; ivy-il.lisp

(in-package :ivy-il)

;;; operand/operator classes

(defstruct (reg (:conc-name reg.))
  (name     nil :type string)
  ;; bytesize must be a power of two
  (bytesize nil :type integer))

(defstruct (imm (:conc-name imm.))
  (value    nil :type integer)
  ;; bytesize must be a power of two
  (bytesize nil :type integer))

(deftype opnd ()
  '(or reg imm))

;; Converts the given opnd to a human-readable string
(defgeneric opnd.repr (opnd))

(defmethod opnd.repr ((o reg))
  (with-slots (name bytesize) o
    (format nil "~a/~a" name bytesize)))

(defmethod opnd.repr ((o imm))
  (with-slots (value bytesize) o
    (format nil "~a/~a" value bytesize)))

(defclass optr ()
  ((name :type string :initform :name :accessor optr.name)))

;; Converts the given optr to a human-readable string
(defgeneric optr.repr (optr))

(defparameter next-reg-num 0)

(defun r (sizespec &optional name)
  (incf next-reg-num)
  (make-reg :name (if name
                      (format nil "~a_~a" name next-reg-num)
                      (format nil "r~a" next-reg-num))
            :bytesize (etypecase sizespec
                        (number sizespec)
                        (reg (reg.bytesize sizespec)))))

(defun i (value &optional size)
  (make-imm :value value
            :bytesize (or size (let ((value (1+ (abs value))))
                                 (ceiling (log value 2) 8)))))

(defmacro with-reg (regname size &body body)
  `(let ((,regname (r ,size)))
     (list ,regname ,@body)))

;;; ternary non-write-back operators

(defclass tern-optr (optr)
  ((a :type opnd :initarg :a :accessor tern-optr.a)
   (b :type opnd :initarg :b :accessor tern-optr.b)
   (c :type opnd :initarg :c :accessor tern-optr.c)))

(defmacro define-tern-optr (fname classname nicename)
  `(progn
     (defclass ,classname (tern-optr)
       ((name :initform ,nicename)))
     (defun ,fname (a b c)
       (make-instance ',classname :a a :b b :c c))))

(define-tern-optr rset ternop-rset "rset")
(define-tern-optr pset ternop-pset "pset")

;;; binary write-back operators

(defclass bin-optr (optr)
  ((a   :type reg  :initarg :a   :accessor bin-optr.a)
   (b   :type opnd :initarg :b   :accessor bin-optr.b)
   (dst :type reg  :initarg :dst :accessor bin-optr.dst)))

(defmacro define-bin-optr (fname classname nicename)
  `(progn
     (defclass ,classname (bin-optr)
       ((name :initform ,nicename)))
     (defun ,fname (dst a b)
       (make-instance ',classname :a a :b b :dst dst))))

(define-bin-optr add binop-add "add")
(define-bin-optr sub binop-sub "sub")

(define-bin-optr rget binop-rget "rget")
(define-bin-optr pget binop-pget "pget")

;;; unary write-back operators

(defclass un-optr (optr)
  ((a   :type opnd :initarg :a   :accessor un-optr.a)
   (dst :type reg  :initarg :dst :accessor un-optr.dst)))

(defmacro define-un-optr (fname classname nicename)
  `(progn
     (defclass ,classname (un-optr)
       ((name :initform ,nicename)))
     (defun ,fname (dst a)
       (make-instance ',classname :a a :dst dst))))

(define-un-optr move unop-move "move")
(define-un-optr ext unop-ext "ext")

;;; nullary and unary non-write-back operators

(defclass jump-target (optr)
  ((id :type integer :initarg :id :accessor jump-target.id)))

(defparameter *next-jump-target* 0)

(defun jump-target ()
  (make-instance 'jump-target :id (incf *next-jump-target*)))

(defclass jump (optr)
  ((target :type jump-target    :initarg :target :accessor jump.target)
   (a      :type (or null opnd) :initarg :a      :accessor jump.a)
   (b      :type (or null opnd) :initarg :b      :accessor jump.b)

   ;; CONDITION is one of: :UNCONDITIONAL :< :<= :> :>= := :/=
   ;;                        
   (condition :type symbol :initarg :condition :accessor jump.condition)))

(defun jumpc (a condition b tgt)
  (make-instance 'jump :a a :b b :condition condition :target tgt))

(defun jump (tgt)
  (make-instance 'jump :condition :unconditional :target tgt))

;;; nary write-back operators

(defclass call (optr)
  ((result :type reg    :initarg :result :accessor call.result)
   (target :type symbol :initarg :target :accessor call.target)
   (args   :type list   :initarg :args   :accessor call.target)))

(defclass dcall (call)
  ((name   :initform "dcall")))

(defclass icall (call)
  ((name   :initform "icall")))

(defun dcall (result target args)
  (make-instance 'dcall :result result :target target :args args))

(defun icall (result target args)
  (make-instance 'icall :result result :target target :args args))

;;; optr.repr definitions

(defmethod optr.repr ((o bin-optr))
  (with-slots (name a b dst) o
    (format nil "~a ~a, ~a, ~a" name (opnd.repr dst) (opnd.repr a) (opnd.repr b))))

(defmethod optr.repr ((o un-optr))
  (with-slots (name a dst) o
    (format nil "~a ~a, ~a" name (opnd.repr dst) (opnd.repr a))))

(defmethod optr.repr ((o call))
  (with-slots (name result target args) o
    (format nil "~a ~{~a~^, ~}" name (list* (opnd.repr result) target (mapcar #'opnd.repr args)))))

(defmethod optr.repr ((o tern-optr))
  (with-slots (name a b c) o
    (format nil "~a ~a, ~a, ~a" name (opnd.repr a) (opnd.repr b) (opnd.repr c))))

(defmethod optr.repr ((o jump))
  (with-slots (a b condition target) o
    (if (eq condition :unconditional)
        (format nil "jump LABEL~a" (jump-target.id target))
        (format nil "jumpc LABEL~a, ~a ~a ~a" (jump-target.id target) (opnd.repr a) condition (opnd.repr b)))))

(defmethod optr.repr ((o jump-target))
  (format nil "LABEL~a:" (jump-target.id o)))

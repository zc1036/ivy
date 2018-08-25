
;;;; ix-il.lisp

(in-package :ix-il)

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
    (format nil "r~a/~a" name bytesize)))

(defmethod opnd.repr ((o imm))
  (with-slots (value bytesize) o
    (format nil "~a/~a" value bytesize)))

(defclass optr ()
  ((name :type string :initform :name :accessor optr.name)))

;; Converts the given optr to a human-readable string
(defgeneric optr.repr (optr))

(defparameter next-reg-num 0)

(defun r (sizespec)
  (incf next-reg-num)
  (make-reg :name (format nil "~a" next-reg-num)
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

;;; binary operators

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

;;; unary operators

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

;;; optr.repr definitions

(defmethod optr.repr ((o bin-optr))
  (with-slots (name a b dst) o
    (format nil "~a ~a, ~a, ~a" name (opnd.repr dst) (opnd.repr a) (opnd.repr b))))

(defmethod optr.repr ((o un-optr))
  (with-slots (name a dst) o
    (format nil "~a ~a, ~a" name (opnd.repr dst) (opnd.repr a))))

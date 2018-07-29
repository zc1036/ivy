
;;;; ix-il.lisp

(in-package :ix-il)

;;; operand/operator classes

(defstruct (reg (:conc-name reg.))
  (name     nil :type string)
  (bytesize nil :type integer))

(defstruct (imm (:conc-name imm.))
  (value    nil :type integer)
  (bytesize nil :type integer))

(deftype opnd ()
  '(or reg imm))

(defgeneric opnd.repr (opnd)
  "Converts the given opnd to a human-readable string")

(defmethod opnd.repr ((o reg))
  (with-slots (name bytesize) o
    (format nil "~a/r~a" name bytesize)))

(defmethod opnd.repr ((o imm))
  (with-slots (value bytesize) o
    (format nil "~a/i~a" value bytesize)))

(defclass optr ()
  ((name :type string :initform :name :accessor optr.name)))

(defgeneric optr.repr (optr)
  "Converts the given optr to a human-readable string")

(defparameter next-reg-num 0)

(defun r (bytesize)
  (make-reg :name (format nil "~a" next-reg-num)
            :bytesize bytesize))

(defun i (value &optional size)
  (make-reg :value value
            :bytesize (or size (ceiling (log value 2)))))

;;; binary operators

(defclass bin-optr (optr)
  ((a   :type reg  :initform :a   :accessor bin-optr.a)
   (b   :type opnd :initform :b   :accessor bin-optr.b)
   (dst :type reg  :initform :dst :accessor bin-optr.dst)))

(defmacro define-bin-optr (fname classname nicename)
  `(progn
     (defclass ,classname (bin-optr)
       ((:name :initform ,nicename)))
     (defun ,fname (dst a b)
       (make-instance ',classname :a a :b b :dst dst))))

(define-bin-optr add "add")
(define-bin-optr sub "sub")

;;; unary operators

(defclass un-optr (optr)
  ((a   :type opnd :initform :a   :accessor un-optr.a)
   (dst :type reg  :initform :dst :accessor un-optr.dst)))

(defmacro define-un-optr (fname classname nicename)
  `(progn
     (defclass ,classname (un-optr)
       ((name :initform ,nicename)))
     (defun ,fname (dst a)
       (make-instance ',classname :a a :dst dst))))

(define-un-optr move "move")

;;; optr.repr definitions

(defmethod optr.repr ((o bin-optr))
  (with-slots (name a b dst) o
    (format nil "~a ~a, ~a, ~a" name (opnd.repr dst) (opnd.repr a) (opnd.repr b))))

(defmethod optr.repr ((o un-optr))
  (with-slots (name a dst) o
    (format nil "~a ~a, ~a" name (opnd.repr dst) (opnd.repr a))))

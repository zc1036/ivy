
(in-package :ix-type)

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
  ((ref :type typespec :initarg :ref :accessor typespec-const.ref)))

(defclass typespec-volatile (typespec)
  ((ref :type typespec :initarg :ref :accessor typespec-volatile.ref)))

(defclass typespec-pointer (typespec)
  ((ref :type typespec :initarg :ref :accessor typespec-pointer.ref)))

(defclass typespec-array (typespec)
  ((elt-type :type typespec          :initarg :elt-type :accessor typespec-array.elt-type)
   (size     :type (or null integer) :initarg :size     :accessor typespec-array.size)))

(defclass typespec-function (typespec)
  ((ret-type  :type typespec           :initarg :ret-type  :accessor typespec-function.ret-type)
   (arg-types :type (list-of typespec) :initarg :arg-types :accessor typespec-function.arg-types)))

;;; typespec.to-string functions

(defgeneric typespec.to-string (typespec))

(defmethod typespec.to-string ((ts typespec-atom))
  (format nil "~a" (hltype.name (typespec-atom.ref ts))))

(defmethod typespec.to-string ((ts typespec-const))
  (format nil "(const ~a)" (typespec.to-string (typespec-const.ref ts))))

(defmethod typespec.to-string ((ts typespec-volatile))
  (format nil "(vltl ~a)" (typespec.to-string (typespec-volatile.ref ts))))

(defmethod typespec.to-string ((ts typespec-pointer))
  (format nil "(& ~a)" (typespec.to-string (typespec-pointer.ref ts))))

(defmethod typespec.to-string ((ts typespec-array))
  (if (typespec-array.size ts)
      (format nil "(array ~a ~a)"
              (typespec.to-string (typespec-array.elt-type ts))
              (typespec-array.size ts))
      (format nil "(array ~a)" (typespec.to-string (typespec-array.elt-type ts)))))

(defmethod typespec.to-string ((ts typespec-function))
  (format nil "(function ~a ~a)"
          (typespec.to-string (typespec-function.ret-type ts))
          (mapcar #'typespec.to-string (typespec-function.arg-types ts))))

;;; typespec.alignof functions

(defgeneric typespec.alignof (typespec))

(defmethod typespec.alignof ((a typespec-atom))
  (hltype.alignof (typespec-atom.ref a)))

(defmethod typespec.alignof ((a typespec-const))
  (typespec.alignof (typespec-const.ref a)))

(defmethod typespec.alignof ((a typespec-volatile))
  (typespec.alignof (typespec-volatile.ref a)))

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
  (typespec.sizeof (typespec-const.ref a)))

(defmethod typespec.sizeof ((a typespec-volatile))
  (typespec.sizeof (typespec-volatile.ref a)))

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

;;; hll global names

(defun ix-hll-kw:function (ret-type arg-types)
  (check-type elt-type typespec)

  (loop for arg-type in arg-types do
       (check-type arg-type typespec))

  (make-instance 'typespec-function :ret-type ret-type :arg-types arg-types))

(defun ix-hll-kw:array (elt-type &optional size)
  (check-type elt-type typespec)
  (check-type size (or null integer))

  (make-instance 'typespec-array :elt-type elt-type :size size))

(defun ix-hll-kw:& (ref)
  (check-type ref typespec)

  (make-instance 'typespec-pointer :ref ref))

(defun ix-hll-kw:const (ref)
  (check-type ref typespec)

  (make-instance 'typespec-const :ref ref))

(defun ix-hll-kw:vltl (ref)
  (check-type ref typespec)

  (make-instance 'typespec-volatile :ref ref))

(defvar hlts-int32 (make-instance 'hltype-builtin
                                  :signed-p t
                                  :float-p nil
                                  :bytesize 4
                                  :name :int32
                                  :numeric t))

(defvar ix-hll-kw:int32 (make-instance 'typespec-atom :ref hlts-int32))

(defvar hlts-int16 (make-instance 'hltype-builtin
                                  :signed-p t
                                  :float-p nil
                                  :bytesize 2
                                  :name :int16
                                  :numeric t))

(defvar ix-hll-kw:int16 (make-instance 'typespec-atom :ref hlts-int16))

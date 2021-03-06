
(in-package :ivy-target-c)

(defparameter *indent* 0)

(defun indent ()
  (make-array *indent* :element-type 'character :initial-element #\space))

(defmacro new-indent (&body body)
  `(let ((*indent* (+ *indent* 2)))
     ,@body))

(defun hltype.to-c-string (hlt)
  (let ((name (hltype.name hlt)))
    (etypecase hlt
      (hltype-structure
       (format nil "struct ~a" name))
      (hltype-union
       (format nil "union ~a" name))
      (hltype-builtin
       name))))

(defgeneric typespec.to-c-string* (td name &optional extra))

(defmethod typespec.to-c-string* ((td typespec-atom) name &optional extra)
  extra
  (format nil "~a~a~a" (hltype.to-c-string (typespec-atom.ref td)) (if (= (length name) 0) "" " ") name))

(defmethod typespec.to-c-string* ((td typespec-pointer) name &optional extra)
  extra
  (typespec.to-c-string* (typespec-pointer.ref td) (format nil "* ~a" name)))

(defmethod typespec.to-c-string* ((td typespec-function) name &optional arg-names)
  (with-slots (ret-type arg-types) td
    (typespec.to-c-string*
     ret-type
     (format nil "(~a)(~{~A~^, ~})"
             name
             (if arg-names
                 (mapcar (lambda (x y) (typespec.to-c-string* x (symbol-name y)))
                         arg-types arg-names)
                 (mapcar #'typespec.to-c-string* arg-types))))))

(defmethod typespec.to-c-string* ((td typespec-volatile) name &optional extra)
  extra
  (typespec.to-c-string* (typespec-volatile.ref td) (format nil "volatile ~a" name)))

(defmethod typespec.to-c-string* ((td typespec-const) name &optional extra)
  extra
  (typespec.to-c-string* (typespec-const.ref td) (format nil "const ~a" name)))

(defmethod typespec.to-c-string* ((td typespec-array) name &optional extra)
  extra
  (with-slots (elt-type size) td
    (typespec.to-c-string*
     elt-type
     (format nil "(~a)[~a]"
             name
             (if size
                 (gast.emit size)
                 "")))))

(defun typespec.to-c-string (td &optional (name ""))
  (typespec.to-c-string* td name))

(defgeneric gast.emit (gast))

(defmethod gast.emit ((a null))
  "")

(defmethod gast.emit ((a integer))
  (format nil "(~a)" a))

(defmacro define-binop-emitter (class oprepr)
  `(defmethod gast.emit ((a ,class))
     (format nil "(~a ~a ~a)"
             (gast.emit (ast-binop.left a))
             ,oprepr
             (gast.emit (ast-binop.right a)))))

(define-binop-emitter ast-binop-+ "+")
(define-binop-emitter ast-binop-- "-")
(define-binop-emitter ast-binop-= "=")

(defmethod gast.emit ((a ast-var-ref))
  (let ((var (ast-var-ref.var a)))
    (ecase (decl-variable.storage var)
      (:local
       (let ((local-var-pair (state.lookup-lexical-var *state* (decl.name var))))
         (unless local-var-pair
           (error "Lexical variable ~a isn't mapped somehow, this is probably a bug ~a"
                  (decl.name var)
                  (lexical-scope.bindings (state.lex-vars *state*))))
         
         (decl.name var)))
      (:global
       (let ((glob-var-pair (assoc (decl.name var) (state.glob-vars *state*))))
         (unless glob-var-pair
           (error "Global variable ~a isn't mapped somehow, this is probably a bug" (decl.name var)))
         (decl.name var))))))

(defmethod gast.emit ((a ast-binop-aref))
  (with-slots (left right) a
    (format nil "(~a[~a])"
            (gast.emit left)
            (gast.emit right))))

(defmethod gast.emit ((a ast-func-ref))
  (format nil "~a" (decl.name (ast-func-ref.func a))))

(defmethod gast.emit ((a ast-funcall))
  (format nil "(~a(~{~a~^, ~}))"
          (gast.emit (ast-funcall.target a))
          (mapcar #'gast.emit (ast-funcall.args a))))

(defmethod gast.emit ((a ast-binop-mbracc))
  (format nil "(~a.~a)"
          (gast.emit (ast-binop-mbracc.left a))
          (ast-binop-mbracc.right a)))

(defmethod gast.emit ((a ast-unop-deref))
  (format nil "(* ~a)"
          (gast.emit (ast-unop.operand a))))

(defmethod gast.emit ((a ast-unop-cast))
  (format nil "((~a)~a)"
          (typespec.to-c-string (ast.type a))
          (gast.emit (ast-unop.operand a))))

(defmacro with-lexical-scope (bindings &body body)
  ;;; given BINDINGS :: (list-of (pair (symbol decl-variable))), evaluates BODY in a new
  ;;; lexical context wherein each binding in BINDINGS is active.
  (with-gensyms (old-scope% new-scope% binding%)
    `(let* ((,old-scope% (state.lex-vars *state*))
            (,new-scope% (make-lexical-scope :next ,old-scope%)))
       (loop for ,binding% in ,bindings do
            (push (cons (car ,binding%)
                        t) ;; while in the IL target we need some instance of a
                           ;; register in the cdr position here, we don't need
                           ;; it for C because we're not doing any register
                           ;; allocation or anything.
                           ;; (ivy-il:r (typespec.sizeof (decl-var-binding.type ,binding%)) (decl-var-binding.name ,binding%))
                  (lexical-scope.bindings ,new-scope%)))
       (setf (state.lex-vars *state*) ,new-scope%)
       (unwind-protect
            (progn ,@body)
         (setf (state.lex-vars *state*) ,old-scope%)))))

(defmethod gast.emit ((a ast-let))
  (with-output-to-string (result)
    (with-slots (bindings body) a
      (with-lexical-scope bindings
        (format result "{~%")
        (new-indent
         (loop for (name . vardecl) in bindings do
              (with-slots (type init) vardecl
                (format result "~a~a~a~a;~%"
                        (indent)
                        (typespec.to-c-string type (symbol-name name))
                        (if init " = " "")
                        (if init (gast.emit init) ""))))
         (loop for emission in (mapcar #'gast.emit body) do
              (format result "~a~a;~%" (indent) emission)))
        (format result "~a}" (indent))))
    result))

(defmethod gast.emit ((a ast-do))
  (with-output-to-string (result)
    (with-slots (condition body) a
      (format result "{~%")
      (new-indent
       (loop for emission in (mapcar #'gast.emit body) do
            (format result "~a~a;~%" (indent) emission)))
      (format result "~a}" (indent)))))

(defmethod gast.emit ((a ast-while))
  (with-output-to-string (result)
    (with-slots (condition body) a
      (format result "while (~a) {~%" (gast.emit condition))
      (new-indent
       (loop for emission in (mapcar #'gast.emit body) do
            (format result "~a~a;~%" (indent) emission)))
      (format result "~a}" (indent)))))

(defun emit-function (decl)
  (with-slots (name visibility ret-type args body) decl
    (with-lexical-scope args
      (format t "~a~a {~%"
              (ecase visibility
                (:internal "static ")
                (:external ""))
              (typespec.to-c-string*
               (make-instance 'typespec-function
                              :ret-type ret-type
                              :arg-types (mapcar (lambda (x) (decl-variable.type (cdr x)))
                                                 args))
               name
               (mapcar #'car args)))
      (new-indent
        (loop for (name . vardecl) in args do
             (when (decl-variable.init vardecl)
               (format t "~a~a;~%" (indent) (gast.emit (decl-variable.init vardecl)))))
        (loop for elem in body do
             (format t "~a~a;~%" (indent) (gast.emit elem))))
      (format t "}~%"))))

(defun emit-variable (decl)
  (with-slots (name type init) decl
    (push (cons name type) (state.glob-vars *state*))
    (format t "static ~a~a~a;~%"
            (typespec.to-c-string* type name)
            (if init
                " = "
                "")
            (if init
                (gast.emit init)
                ""))))

(defun emit-aggregate (aggtype agg members)
  (format t "~a~a ~a {~%" (indent) aggtype (hltype.name agg))
  (new-indent
    (loop for member in members do
         (format t "~a~a;~%" (indent) (typespec.to-c-string (hltype-agg-member.type member)
                                                            (hltype-agg-member.name member)))))
  (format t "~a};~%" (indent)))

(defun emit (emittable)
  (etypecase emittable
    (decl-function
     (emit-function emittable))
    (decl-variable
     (emit-variable emittable))
    (hltype-structure
     (emit-aggregate "struct" emittable (hltype-structure.members emittable)))
    (hltype-union
     (emit-aggregate "union" emittable (hltype-union.members emittable)))))

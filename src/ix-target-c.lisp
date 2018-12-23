
(in-package :ix-target-c)

(defparameter *indent* 0)

(defun indent ()
  (make-array *indent* :element-type 'character :initial-element #\space))

(defmacro new-indent (&body body)
  `(let ((*indent* (+ *indent* 2)))
     ,@body))

(defgeneric typespec.to-string* (td name &optional extra))

(defmethod typespec.to-string* ((td typespec-atom) name &optional extra)
  extra
  (format nil "~a~a~a" (hltype.name (typespec-atom.ref td)) (if (= (length name) 0) "" " ") name))

(defmethod typespec.to-string* ((td typespec-pointer) name &optional extra)
  extra
  (typespec.to-string* (typespec-pointer.ref td) (format nil "* ~a" name)))

(defmethod typespec.to-string* ((td typespec-function) name &optional arg-names)
  (with-slots (ret-type arg-types) td
    (typespec.to-string*
     ret-type
     (format nil "(~a)(~{~A~^, ~})"
             name
             (if arg-names
                 (mapcar (lambda (x y) (typespec.to-string* x (symbol-name y)))
                         arg-types arg-names)
                 (mapcar #'typespec.to-string* arg-types))))))

(defmethod typespec.to-string* ((td typespec-volatile) name &optional extra)
  extra
  (typespec.to-string* (typespec-volatile.ref td) (format nil "volatile ~a" name)))

(defmethod typespec.to-string* ((td typespec-const) name &optional extra)
  extra
  (typespec.to-string* (typespec-const.ref td) (format nil "const ~a" name)))

(defmethod typespec.to-string* ((td typespec-array) name &optional extra)
  extra
  (with-slots (elt-type size) td
    (typespec.to-string*
     elt-type
     (format nil "(~a)[~a]"
             name
             (if size
                 (gast.emit size)
                 "")))))

(defun typespec.to-string (td &optional (name ""))
  (typespec.to-string* td name))

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
       (let ((local-var-pair (state.lookup-var *state* (decl.name var))))
         (unless local-var-pair
           (error "Lexical variable ~a isn't mapped somehow, this is probably a bug ~a"
                  (decl.name var)
                  (lexical-scope.bindings (state.lex-vars *state*))))
         (format nil "~a" (decl.name var))))
      (:global
       (let ((glob-var-pair (assoc var (state.glob-vars *state*))))
         (unless glob-var-pair
           (error "Global variable ~a isn't mapped somehow, this is probably a bug" (decl.name var)))
         (list (cdr glob-var-pair) ()))))))

(defmacro with-lexical-scope (bindings &body body)
  ;;; given BINDINGS :: (list-of decl-var-binding), evaluates BODY in a new
  ;;; lexical context wherein each binding in BINDINGS is active.
  (with-gensyms (old-scope% new-scope% binding%)
    `(let* ((,old-scope% (state.lex-vars *state*))
            (,new-scope% (make-lexical-scope :next ,old-scope%)))
       (loop for ,binding% in ,bindings do
            (push (cons (decl-var-binding.name ,binding%)
                        ,binding% ;; (ix-il:r (typespec.sizeof (decl-var-binding.type ,binding%)) (decl-var-binding.name ,binding%))
                        )
                  (lexical-scope.bindings ,new-scope%)))
       (setf (state.lex-vars *state*) ,new-scope%)
       (unwind-protect
            (progn ,@body)
         (setf (state.lex-vars *state*) ,old-scope%)))))

(defmethod gast.emit ((a ast-let))
  (with-output-to-string (result)
    (with-slots (bindings body) a
      (with-lexical-scope bindings
        (let ((body-emissions (mapcar #'gast.emit body)))
          (format result "{~%")
          (new-indent
           (loop for emission in body-emissions do
                (format result "~a~a;~%" (indent) emission)))
          (format result "~a}" (indent)))))
    result))

(defun emit-decl (decl)
  (with-slots (name ret-type args body-src body) decl
    (with-lexical-scope args
      (format t "~a {~%"
              (typespec.to-string*
               (make-instance 'typespec-function
                              :ret-type ret-type
                              :arg-types (mapcar #'decl-var-binding.type args))
               name
               (mapcar #'decl-var-binding.name args)))
      (new-indent
       (loop for elem in body for elem-src in body-src do
            (format t "  ~a;~%" (gast.emit elem))))
      (format t "}~%"))))


(in-package :ix-target-il)

#|

General workings of the IL target:

The IX-IL package defines classes that describe the IL operators and
operands. The IL is a simple register-based linear language. It is not in SSA
form yet.

After converting an AST into a linear IL representation, we scan the IL and
convert it to a CFG. Then we transform that IL+CFG into another IL+CFG with all
the assignment instructions replaced by PHI instructions to make it SSA.

|#

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

;; Converts the given gast to IL
(defgeneric gast.emit (gast)
  (:documentation
   "Returns a list of two elements, the first being the register in which the
    result is stored, and the second a (possibly nested; the list is flattened
    before processing) list of the instructions required to calculate the value
    of the register."))

(defmethod gast.emit ((a null))
  ;; nothing
  (list () ()))

(defmethod gast.emit ((a integer))
  (let* ((imm (ix-il:i a 4))
         (reg (ix-il:r 4)))
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

(defmethod gast.emit ((a ast-binop-=))
  (with-slots (left right) a
    ;; this is a reiteration of the types supported in ix-ast:lvalue-p
    (ematch left
      ((class ast-var-ref)
       (let+ (((left-res left-il)   (gast.emit left))
              ((right-res right-il) (gast.emit right)))
         (list left-res (list left-il right-il (ix-il:move left-res right-res)))))
      ((class ast-binop-aref (left base) (right offset))
       (let+ (((base-res base-il)     (gast.emit base))
              ((offset-res offset-il) (gast.emit offset))
              ((elt-res elt-il)       (gast.emit right)))
         ;; we have two different types of assignment, one requiring a
         ;; dereference (a pointer) and one not (a structure/union/array)
         (list elt-res (list base-il offset-il elt-il
                             (etypecase (gast.type base)
                               (typespec-pointer
                                (ix-il:pset base-res offset-res elt-res))
                               (typespec-array
                                (ix-il:rset base-res offset-res elt-res))))))))))

(defmethod gast.emit ((a ast-funcall))
  (with-slots (target args) a
    (let* ((arg-gen (mapcar #'gast.emit args))
           (arg-regs (mapcar #'first arg-gen))
           (arg-instrs (mapcar #'second arg-gen))
           (result (ix-il:r (typespec.sizeof (gast.type a)))))
      (list result (list arg-instrs
                         (etypecase target
                           (ast-func-ref
                            (ix-il:dcall result (decl.name (ast-func-ref.func target)) arg-regs))
                           (t
                            (ix-il:icall result (decl.name (ast-func-ref.func target)) arg-regs))))))))

(defmethod gast.emit ((a ast-binop-aref))
  (with-slots (left right) a
    (let+ (((left-res left-il)   (gast.emit left))
           ((right-res right-il) (gast.emit right)))
      (ematch (remove-cv (gast.type left))
        ((class typespec-pointer ref)
         (ix-il:with-reg result (typespec.sizeof ref)
           (list left-il right-il (ix-il:pget result left-res right-res))))
        ((class typespec-array elt-type)
         (ix-il:with-reg result (typespec.sizeof elt-type)
           (list left-il right-il (ix-il:rget result left-res right-res))))))))

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

(defmethod gast.emit ((a ast-while))
  (with-slots (condition body) a
    (let+ (((cond-res cond-il) (gast.emit condition))
           (body-ils (loop for elem in body collect (second (gast.emit elem))))
           (skip-body (ix-il:jump-target))
           (loopback (ix-il:jump-target)))
      (list nil (list
                 loopback
                 cond-il
                 (ix-il:jumpc cond-res := (ix-il:i 0) skip-body)
                 body-ils
                 (ix-il:jump loopback)
                 skip-body)))))

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
       (let ((local-var-pair (state.lookup-lexical-var *state* (decl.name var))))
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

(defun print-instrs (instrs)
  (etypecase instrs
    (list (mapc #'print-instrs instrs))
    (ix-il:optr (format t "  ~a~%" (ix-il:optr.repr instrs)))))

(defun emit-decl (decl)
  (with-slots (ret-type args body-src body) decl
    (with-lexical-scope args
      (loop for elem in body for elem-src in body-src do
           (let+ (((result ops) (gast.emit elem)))
             result
             (with-input-from-string (in (format nil "~a" elem-src))
               (loop for line = (read-line in nil)
                  while line
                  do (format t " ;; ~a~%" line)))
             (print-instrs ops)
             (format t " ;~%"))))))

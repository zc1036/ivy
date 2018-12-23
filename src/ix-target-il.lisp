
(in-package :ix-target-il)

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

;; Converts the given gast to IL
(defgeneric gast.emit (gast))

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

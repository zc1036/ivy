
(in-package :ix-state)

;;; the global hll compiler state

(defstruct (lexical-scope (:conc-name lexical-scope.))
  (bindings nil :type list) ;; alist map of decl-variables to some target-specific thing (ix-il:reg in some cases)
  (next     nil :type (or lexical-scope null)))

(defun lexical-scope.lookup (scope var)
  (when scope
    (let ((binding (assoc var (lexical-scope.bindings scope))))
      (if binding
          (cdr binding)
          (lexical-scope.lookup (lexical-scope.next scope) var)))))

(defstruct (state (:conc-name state.))
  (functions nil :type (list-of decl-function))
  (lex-vars  nil :type (or null lexical-scope))
  (glob-vars nil :type list)) ;; alist of decl-variables to some target-specific thing (ix-il:reg in some cases)

(defun state.lookup-var (state var)
  (lexical-scope.lookup (state.lex-vars state) var))

(defparameter *state* nil)
(defparameter *target-arch* (make-arch :name "x86-64"
                                       :bits 64))

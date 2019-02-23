
(in-package :ivy-state)

;;; the global hll compiler state

(defstruct (lexical-scope (:conc-name lexical-scope.))
  (bindings nil :type list) ;; alist map of decl-variables to some
                            ;; target-specific thing (ivy-il:reg when targetting
                            ;; IL, etc)
  (next     nil :type (or lexical-scope null)))

(defun lexical-scope.lookup (scope var)
  (when scope
    (let ((binding (assoc (string var) (lexical-scope.bindings scope) :test #'string=)))
      (if binding
          (cdr binding)
          (lexical-scope.lookup (lexical-scope.next scope) var)))))

(defstruct (state (:conc-name state.))
  (emittables nil :type (list-of (or decl-function hltype-structure hltype-union)))
  (lex-vars   nil :type (or null lexical-scope))
  (glob-vars  nil :type list)) ;; alist map of decl-variables to some             
                              ;; target-specific thing (ivy-il:reg when targetting
                              ;; IL, etc)                                        

(defun state.lookup-lexical-var (state var)
  (lexical-scope.lookup (state.lex-vars state) var))

(defparameter *state* nil)
(defparameter *target-platform* (make-platform :name "Linux x86-64"
                                               :bits 64
                                               :char-signed-p t))

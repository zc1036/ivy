
(in-package :ix-util)

(defun make-let+-binding (bindings body)
  (if bindings
      (if (consp (caar bindings))
          `(destructuring-bind ,(caar bindings) ,(cadar bindings)
             ,(make-let+-binding (cdr bindings) body))
          `(let (,(car bindings))
             ,(make-let+-binding (cdr bindings) body)))
      (cons 'progn body)))

(defmacro let+ (bindings &body body)
  (make-let+-binding bindings body))

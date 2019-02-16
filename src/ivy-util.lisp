
(in-package :ivy-util)

(defun all-of-type (type list)
  (every type list))

(deftype list-of (type)
  (declare (ignore type))
  'list)

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

;; (defun make-defunt-arg-list (typed-lambda-list)
;;   (when typed-lambda-list
;;     (car
;;      (ecase (car typed-lambda-list)
;;        (&optional (car '&optional (make-defunt-arg-list (cdr typed-lambda-list))))
;;        (&rest     (car '&rest     (make-defunt-arg-list (cdr typed-lambda-list))))
;;        (&key      (car '&key      (make-defunt-arg-list (cdr typed-lambda-list))))
;;        (&allow-other-keys (car '&rest     (make-defunt-arg-list (cdr typed-lambda-list))))
;;        (otherwise
;;         (ematch (car typed-lambda-list)
;;           ()))))))
;; 
;; (defmacro defunt (name typed-lambda-list)
;;   `(defun ,name ))
;; 

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

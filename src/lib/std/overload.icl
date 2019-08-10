
(defpackage :std.overload
  (:use :cl)
  (:import-from :ivy-type :typespec.to-string)
  (:import-from :ivy-ast :gast.type))

(in-package :std.overload)

(defun concat (list)
  (apply #'concatenate (cons 'string list)))

(defun type-list-to-overload-string (types)
  (concat (mapcar #'typespec.to-string types)))

(defparameter *next-overload* 0)

(defmacro ivy-hll-kw:defun2 (name ret-type args &body body)
  (let ((arg-types% (gensym))
        (types-str% (gensym))
        (overload-name (intern
                        (concatenate 'string (symbol-name name)
                                     "__overload__"
                                     (write-to-string (incf *next-overload*)))))
        (overload-table-name (intern
                              (concatenate 'string
                                           (symbol-name name)
                                           "--overload--table")))
        (rest% (gensym)))
    `(let* ((,arg-types% (list ,@(mapcar #'cadr args)))
            (,types-str% (type-list-to-overload-string ,arg-types%)))
       (defvar ,overload-table-name (make-hash-table :test 'equal))

       (when (not (fboundp ',name))
         (defmacro ,name (&rest ,rest%)
           (let ((types%% (gensym))
                 (args%% (gensym))
                 (fn%% (gensym)))
             `(let* ((,args%% (list ,@,rest%))
                     (,types%% (mapcar #'gast.type ,args%%))
                     (,fn%% (gethash (concat (mapcar #'typespec.to-string ,types%%))
                                     ,',overload-table-name)))
                (if ,fn%%
                    (apply ,fn%% ,args%%)
                    (error "Overload of ~a with parameter types (~{~a~^, ~}) not found"
                           ',',name
                           (mapcar #'typespec.to-string ,types%%)))))))

       (ivy-hll-kw:defun ,overload-name ,ret-type ,args
         ,@body)

       (setf (gethash ,types-str% ,overload-table-name)
             #',overload-name))))

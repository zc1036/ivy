
;;;; ivy-core

(in-package :ivy-core)

(defparameter *include-paths* '("lib/std/"))

(defun do-include (filename)
  (loop for path in *include-paths* do
       (let ((file (concatenate 'string path filename ".icl")))
         (if (probe-file file)
             (progn
               (let ((*package* (find-package :cl)))
                 (load file))
               (return t))
             (let ((file (concatenate 'string path filename ".icy")))
               (when (probe-file file)
                 (load file)
                 (return t)))))))

(defun ivy-hll-kw:include (file)
  (when (not (do-include file))
      (error "No file found when trying to include ~s" file)))

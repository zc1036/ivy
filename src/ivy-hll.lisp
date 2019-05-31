
;;;; ivy-hll.lisp

(in-package :ivy-hll)

;;; main program

(defun target-string-to-target (target)
  (let ((target (string-upcase target)))
    (cond
      ((string= target "C")
       (setf *target* #'ivy-target-c:emit))
      ((string= target "IL")
       (setf *target* #'ivy-target-il:emit))
      (t
       (error "No target by the name of ~a exists" target)))))

(defun compile-file (file target)
  (set-macro-character #\[ #'member-access-syntax t)

  (let ((*state* (make-state))
        (emitter (target-string-to-target target)))
    (format t "~%#include \"icy.h\"~%~%")

    (let ((*package* (find-package 'ivy-hll-user)))
      (load file))

    (loop for emittable in (reverse (state.emittables *state*)) do
         (funcall emitter emittable))))

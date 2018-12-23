
;;;; ix-hll.lisp

(in-package :ix-hll)

;;; main program

(defparameter *target* #'ix-target-il:emit-decl)

(defun target-string-to-target (target)
  (let ((target (string-upcase target)))
    (cond
      ((string= target "C")
       (setf *target* #'ix-target-c:emit-decl))
      ((string= target "IL")
       (setf *target* #'ix-target-il:emit-decl))
      (t
       (error "No target by the name of ~a exists" target)))))

(defun main (argv)
  (let ((sources nil))
    (pop argv)
    (loop while argv do
      (let ((arg (car argv)))
        (cond
          ((string= arg "--target")
           (pop argv)
           (setf *target* (target-string-to-target (car argv))))
          (t
           (push arg sources))))
      (pop argv))

    (loop for arg in sources do
         (let ((*state* (make-state)))
           (format t "Compiling ~a~%~%" arg)
           (let ((*package* (find-package 'ix-hll-user)))
             (load arg))

           (loop for func in (state.functions *state*) do
                (format t "Function ~a:~%" (decl.name func))

                (funcall *target* func))))))

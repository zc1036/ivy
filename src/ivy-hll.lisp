
;;;; ivy-hll.lisp

(in-package :ivy-hll)

;;; main program

(defparameter *target* #'ivy-target-il:emit)

(defun target-string-to-target (target)
  (let ((target (string-upcase target)))
    (cond
      ((string= target "C")
       (setf *target* #'ivy-target-c:emit))
      ((string= target "IL")
       (setf *target* #'ivy-target-il:emit))
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

    (set-macro-character #\[ #'member-access-syntax t)

    (loop for arg in sources do
         (let ((*state* (make-state)))
           (format t "Compiling ~a~%~%" arg)

           (let ((*package* (find-package 'ivy-hll-user)))
             (load arg))

           (loop for emittable in (reverse (state.emittables *state*)) do
                (etypecase emittable
                  (decl-function
                   (format t "Function ~a:~%" (decl.name emittable)))
                  (decl-variable
                   (format t "Variable ~a:~%" (decl.name emittable)))
                  ((or hltype-structure hltype-union)
                   (format t "Aggregate ~a:~%" (hltype.name emittable))))

                (funcall *target* emittable))))))

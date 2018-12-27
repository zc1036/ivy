
(defstruct foo
  (hi int32))

(defun blah int32 ((face int32) (face2 int32))
       (let ((x int32 (+ face2 face 45)))
         x)
       (blah 1 2))

(defun berp int32 ()
       (blah 1 2))

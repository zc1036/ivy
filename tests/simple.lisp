
(defstruct foo
  (hi int32))

(defun blah int16 ((face int32) (face2 int32))
       (while face
         (= face (+ face 1)))

       (let ((x int32 (+ face2 face 45)))
         (= face x)))

(defun berp int32 ((face int32) (asdf int16) (arr (& int32)))
       (+ asdf (blah 2 (aref arr asdf))))

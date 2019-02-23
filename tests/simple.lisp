
(defstruct foo
  (hi (& int16)))

(defun blah int16 ((face int32) (face2 int32))
  (while face
    (= face (+ face 1)))

  (let ((x int32 (+ face2 face 45)))
    (= face x)))

(defvar an-variable int32)

(defun |main| int32 ((face (& (& (& (struct foo)))))
                     (asdf (const (& int16)))
                     (arr (& int32)))
  (do
   (+ [ face 'hi 4 ] ($ asdf) (blah 2 (aref arr ($ asdf))) (cast int16 4))))

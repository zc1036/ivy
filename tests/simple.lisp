
(include "overload")

(defstruct foo
  (hi (& int16)))

(defun2 blah int16 ((face int32) (face2 int32))
  (while face
    (= face (+ face 1)))

  (let ((x int32 (+ face2 face 45)))
    (= face x)))

(defun2 blah int16 ((face int16) (face2 int32))
        (blah face face2)
        )

(defun asdfasdf int32 ()
       (asdfasdf))

(defvar an-variable int32)

(defun |main| int32 ((face (& (& (& (struct foo)))))
                     (asdf (const (& int16)))
                     (arr (& int32)))
  (do
   (+ [ face 'hi 4 ] ($ asdf) (blah (cast int16 2) (aref arr ($ asdf))) (cast int16 4))))

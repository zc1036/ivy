
(in-package :ix-arch)

(defstruct (arch (:conc-name arch.))
  (bits nil :type integer)
  (name nil :type string))

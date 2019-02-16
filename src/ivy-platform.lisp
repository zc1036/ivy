
(in-package :ivy-platform)

(defstruct (platform (:conc-name platform.))
  (bits nil          :type integer)
  (name nil          :type string)
  (char-signed-p nil :type boolean))

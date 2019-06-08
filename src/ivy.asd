;;; -*- Mode: Lisp -*-

(defpackage :ivy-system
  (:use :cl :asdf))

(in-package :ivy-system)

(defsystem :ivy
  :name "ivy"
  :author "hi"
  :version "0.1.0"
  :license "GNU General Public License"
  :description "A thing"
  :serial t
  :depends-on (:optima)
  :components ((:file "packages")
               (:file "ivy-util")
               (:file "ivy-il")
               (:file "ivy-platform")
               (:file "ivy-state")
               (:file "ivy-type")
               (:file "ivy-ast")
               (:file "ivy-target-il")
               (:file "ivy-target-c")
               (:file "ivy-hll")
               (:file "ivy-core")))

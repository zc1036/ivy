;;; -*- Mode: Lisp -*-

(defpackage :ix-system
  (:use :cl :asdf))

(in-package :ix-system)

(defsystem :ix
  :name "ix"
  :author "hi"
  :version "0.1.0"
  :license "GNU General Public License"
  :description "A thing"
  :serial t
  :depends-on (:optima)
  :components ((:file "packages")
               (:file "ix-util")
               (:file "ix-il")
               (:file "ix-platform")
               (:file "ix-state")
               (:file "ix-type")
               (:file "ix-ast")
               (:file "ix-target-il")
               (:file "ix-target-c")
               (:file "ix-hll")))

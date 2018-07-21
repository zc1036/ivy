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
               (:file "ix-hll")))

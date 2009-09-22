;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:ltk-asd
  (:use :cl :asdf))

(in-package :ltk-asd)

(defsystem ltk
  :name "LTK"
  :version "0.9.1"
  :author "Peter Herth"
  :licence "LGPL"
  :description "LTK"
  :long-description "Lisp bindings for the Tk toolkit"
  :components ((:module "ltk"
                        :components ((:file "ltk")))))

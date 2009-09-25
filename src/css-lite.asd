;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :css-lite
  :serial t
  :version "0.01"
  :components ((:module "css-lite"
                        :serial t
                        :components ((:file "package")
                                     (:file "css-lite")))))


;;; -*- Lisp -*-
(in-package :cl)

(asdf:defsystem #:fare-csv
  :depends-on ()
  :licence "MIT"
  :components ((:module "fare-csv"
                        :serial t
                        :components ((:file "package") (:file "csv")))))

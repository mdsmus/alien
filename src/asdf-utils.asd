;;; -*- Mode: Lisp

(in-package :asdf)

(defsystem :asdf-utils
    :name "Utilities for asdf"
    :author ""
    :version "0.1"
    :components ((:module "asdf-utils"
                          :serial t
                          :components ((:file "asdf-extras")
                                       (:file "asdf-load")))))

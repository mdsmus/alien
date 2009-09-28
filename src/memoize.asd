;;; -*- Mode: Lisp

(in-package :asdf)

(defsystem :memoize
    :name "Memoize"
    :author "Tim Bradshaw"
    :version "2007-04-19"
    :components ((:module "memoize"
                          :components ((:file "memoize")))))

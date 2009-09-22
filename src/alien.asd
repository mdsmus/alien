(in-package :cl-user)

(defpackage :alien-asd
  (:use :cl :asdf))

(in-package :alien-asd)

(defsystem :alien
  :version "0.1"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (:cl-extensions
               :unittest
               :regexp
               :trivial-gray-streams
               :cxml
               :closure-html
               :ltk
               ))

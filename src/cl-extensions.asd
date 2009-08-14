(in-package :asdf)

(defsystem :cl-extensions
  :version "1.0"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (#+sbcl :sb-posix :cl-fad)
  :components ((:file "package")
               (:file "macros")
               (:file "arrays")
               (:file "conditions")
               (:file "control-structures")
               (:file "packages-and-symbols")
               (:file "environment")
               (:file "evaluation")
               (:file "lists")
               (:file "sequences")
               (:file "strings")
               (:file "function")
               (:file "hash-tables")
               (:file "io")
               (:file "math")
               (:file "oop")
               (:file "types")
               (:file "0-optional-names")
               ))


(defsystem :cl-extensions
  :version "0.1"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (#+sbcl :sb-posix
               #+allegro :osi)
  :components ((:module "extensions"
                        :serial t
                        :components ((:file "package")
                                     (:file "macros")
                                     (:file "arrays")
                                     (:file "conditions")
                                     (:file "control-structures")
                                     (:file "packages-and-symbols")
                                     (:file "evaluation")
                                     (:file "lists")
                                     (:file "sequences")
                                     (:file "strings")
                                     (:file "environment")
                                     (:file "function")
                                     (:file "hash-tables")
                                     #+:cormanlisp (:file "corman")
                                     #+:openmcl (:file "openmcl")
                                     (:file "io")
                                     (:file "math")
                                     (:file "oop")
                                     (:file "types")
                                     (:file "iterate")
                                     (:file "0-optional-names")))))

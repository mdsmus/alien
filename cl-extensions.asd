(in-package :asdf)

(defsystem :alien
  :version "0.1"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (:cl-extensions :unittest))

(defsystem :cl-extensions
  :version "0.1"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (#+sbcl :sb-posix :cl-fad)
  :components ((:module src
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
                                     ))))


(defsystem :unittest
    :author "Edward Marco Baringer <mb@bese.it>"
    :properties ((:test-suite-name . :unittest))
    :serial t
    :components ((:module "lib/unittest"
                          :components ((:static-file "fiveam.asd")
                                       (:file "packages")
                                       (:file "check")
                                       (:file "classes")
                                       (:file "explain")
                                       (:file "fixture")
                                       (:file "random")
                                       (:file "test")
                                       (:file "suite")
                                       (:file "run")))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :unittest))))
  (funcall (intern (string :run!) (string :unittest)) :unittest))

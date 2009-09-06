(defsystem :unittest
    :author "Edward Marco Baringer <mb@bese.it>"
    :properties ((:test-suite-name . :unittest))
    :serial t
    :depends-on (:cl-extensions)
    :components ((:module "unittest"
                          :components ((:file "packages")
                                       (:file "check")
                                       (:file "classes")
                                       (:file "explain")
                                       (:file "fixture")
                                       (:file "random")
                                       (:file "test")
                                       (:file "suite")
                                       (:file "run")))))

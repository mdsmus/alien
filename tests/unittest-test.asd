(defsystem :unittest-test
    :author "Edward Marco Baringer <mb@bese.it>"
    :serial t
    :depends-on (:cl-extensions :unittest)
    :components ((:module "unittest-test"
                          :components ((:file "suite")
                                       (:file "run")))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :unittest))))
  (funcall (intern (string :run!) (string :unittest)) :unittest))

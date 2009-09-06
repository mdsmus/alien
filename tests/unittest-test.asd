(defsystem :unittest-test
    :author "Edward Marco Baringer <mb@bese.it>"
    :serial t
    :depends-on (:alien)
    :components ((:module "unittest-test"
                          :components (
                                       ;(:file "suite")
                                       (:file "tests")))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :unittest))))
  (funcall (intern (string :run!) (string :unittest)) :unittest-test))

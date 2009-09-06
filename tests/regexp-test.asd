
(defsystem :regexp-test
  :depends-on (:regexp :flexi-streams)
  :components ((:module "regexp/test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "tests")
                                     (:file "perl-tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :regexp))))
  (operate 'load-op :regexp-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :regexp-test))))

;; -*- lisp -*-

(defpackage :it.bese.FiveAM.system
  (:use :common-lisp
        :asdf))

(in-package :it.bese.FiveAM.system)

(defsystem :FiveAM
    :author "Edward Marco Baringer <mb@bese.it>"
    :properties ((:test-suite-name . :it.bese.fiveam))
    :components ((:static-file "fiveam.asd")
                  (:file "check" :depends-on ("packages"))
                  (:file "classes" :depends-on ("packages"))
                  (:file "explain" :depends-on ("classes" "packages" "check"))
                  (:file "fixture" :depends-on ("packages"))
                  (:file "packages")
                  (:file "run" :depends-on ("packages" "classes" "test" "suite" "check"))
                  (:file "suite" :depends-on ("packages" "test" "classes"))
                  (:file "random" :depends-on ("packages" "check"))
                  (:file "test" :depends-on ("packages" "classes"))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :FiveAM))))
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :it.bese.FiveAM))

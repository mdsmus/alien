;; -*- lisp -*-

(defpackage :unittest.system
  (:use :common-lisp
        :asdf))

(in-package :unittest.system)

(defsystem :unittest
    :author "Edward Marco Baringer <mb@bese.it>"
    :components ((:module :unittest
                  :components ((:file "extras" :depends-on ("packages"))
                               (:file "check" :depends-on ("packages" "extras"))
			       (:file "classes" :depends-on ("packages"))
			       (:file "explain" :depends-on ("classes" "packages" "check"))
			       (:file "fixture" :depends-on ("packages" "extras"))
			       (:file "packages")
			       (:file "run" :depends-on ("packages" "classes" "test" "suite" "check"))
			       (:file "suite" :depends-on ("packages" "test" "classes"))
                               (:file "random" :depends-on ("packages" "check"))
			       (:file "test" :depends-on ("packages" "classes"))))
		 ;; (:module :t
		 ;;  :components ((:file "suite")
		 ;;               (:file "tests" :depends-on ("suite")))
		 ;;  :depends-on (:fiveam))
                 )
    ;;:depends-on (:cl-extensions)
    )

;; (defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :FiveAM))))
;;   (funcall (intern (string :run!) (string :unittest)) :unittest))

;;;;@include "src/packages.lisp"

;;;;@include "t/example.lisp"

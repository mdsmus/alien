

(in-package #:cl-user)

(defpackage #:py-configparser-system
    (:use #:cl #:asdf))

(in-package #:py-configparser-system)

(defsystem py-configparser
  :name "py-configparser"
  :author "Erik Huelsmann"
  :version "1.0.3"
  :license "MIT"
  :description "Common Lisp implementation of the Python ConfigParser module"
  :depends-on (#:cl-extensions)
  :components ((:module "py-configparser"
                        :components ((:file "package")
                                     (:file "config" :depends-on ("package"))
                                     (:file "parser" :depends-on ("config"))))))


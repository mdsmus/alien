;;; -*- Mode: Lisp

(in-package :asdf)

(defsystem :screamer
    :name "Screamer"
    :author "Jeffrey Siskind and David McAllester"
    :components ((:module "screamer"
                          :components ((:file "screamer")))))

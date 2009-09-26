;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2009 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Francois-Rene Rideau                           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :command-line-arguments
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "small library to deal with command-line arguments"
  :long-description "A library to abstract away the parsing of Unix-style command-line arguments"
  ;;:depends-on (:cl-launch) ; cl-launch affects the build too much for unsuspecting ASDF users.
  :components
  ((:module "command-line-arguments"
            :components ((:file "pkgdcl")
                         (:file "get-command-line-arguments" :depends-on ("pkgdcl"))
                         (:file "parse-command-line-arguments" :depends-on ("get-command-line-arguments"))))))

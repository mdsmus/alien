(in-package :cl-user)

(defpackage :alien-asd
  (:use :cl :asdf))

(in-package :alien-asd)

(defsystem :alien
  :version "0.1"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (:cl-extensions
               :unittest
               :regexp
               :trivial-gray-streams
               :cxml
               :s-xml
               :closure-html
               :ltk
               :usocket
               :cffi
               :cl+ssl
               :rfc2388
               ;;:mel-base
               :cl-json
               :cl-base64
               :cl-ppcre
               :cl-qprint
               :cl-mime
               :s-xml-rpc
               :lisp-cgi-utils
               :cl-smtp
               :cl-who
               :html-template
               :parenscript
               :css-lite
               :uri-template
               :midi
               :cl-difflib
               :cl-unicode
               :cl-interpol
               :closer-mop
               :cl-store
               :salza2
               :archive
               :md5
               :ironclad
               :cl-l10n
               :cl-i18n
               :timer
               :bordeaux-threads
               :cl-muproc
               :chunga
               :date-calc
               :local-time
               :yacc
               :cl-period
               :command-line-arguments
               :log5
               :cl-colors
               :trivial-garbage
               :cl-cairo2
               ))

;; -*- lisp -*-

(in-package :cl-user)

(defpackage :cl-stm.system
  (:use :cl :asdf))

(in-package :cl-stm.system)

(defsystem :cl-stm
  :name "CL-STM"
  :author "Hoan Ton-That <hoan@ton-that.org>"
  :version "0"
  :maintainer "Hoan Ton-That <hoan@ton-that.org>"
  :licence ""
  :description "Software Transactional Memory"
  :long-description ""
  :depends-on (:arnesi
               :bordeaux-threads
               :closer-mop)
  :components ((:static-file "cl-stm.asd")
               (:module "cl-stm"
                :components ((:file "packages")
                             (:file "loggers" :depends-on ("packages"))
                             (:file "protocol" :depends-on ("packages"))
                             (:file "standard-tlog" :depends-on ("protocol" "loggers"))
                             (:file "standard-transaction" :depends-on ("standard-tlog"))
                             (:file "transactional-class" :depends-on ("standard-tlog"))
                             (:file "walker" :depends-on ("protocol"))
                             (:file "interface" :depends-on ("transactional-class" "standard-transaction" "standard-tlog" "walker"))))
               (:module :examples
                :components ((:file "cell" :depends-on ("utils"))
                             (:file "chan")
                             (:file "counter")
                             (:file "queue")
                             (:file "utils"))
                :depends-on (:src))))

(defsystem :cl-stm.test
  :components ((:module :test
                :components ((:file "suite")
                             (:file "standard-tlog" :depends-on ("suite"))
                             (:file "walker" :depends-on ("suite")))))
  :depends-on (:cl-stm :FiveAM)
  :in-order-to ((compile-op (load-op :cl-stm))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-stm))))
  (asdf:oos 'asdf:load-op :cl-stm.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-stm))

;;;;@include "src/packages.lisp"

;;;; * Examples

;;;;@include "examples/counter.lisp"

;;;;@include "examples/mvar.lisp"

;;;;@include "src/interface.lisp"

;;;;@include "src/protocol.lisp"

;;;;@include "src/standard-transaction.lisp"

;;;;@include "src/standard-tlog.lisp"

;;;;@include "src/transactional-class.lisp"

;;;;@include "src/walker.lisp"

;;;;@include "src/loggers.lisp"

;; Copyright (c) 2006 Hoan Ton-That
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Hoan Ton-That, nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

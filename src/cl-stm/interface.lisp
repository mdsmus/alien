;; -*- lisp -*-

(in-package :cl-stm)

(eval-always
  (enable-pf-reader))

;;;; * Interface

;;;; ** Composing

(defmacro try (&body body)
  "Return a transaction that executes each transaction in BODY
atomically from left to right until one succeeds.

The return value of the transaction is the value of the
transaction that succeeds."
  (reduce [list 'orelse] body :from-end t))

;;;; ** Retrying

(defun retry ()
  "Return a transaction that when executed, aborts the current
transaction and re-executes it from scratch.

The transaction will wait on all variables that have been read so
far during the transaction."
  (trans (throw 'retry (current-tlog))))

;;;; ** Lifting

(defmacro trans (body)
  "Create a new anonymous transaction."
  `(new 'standard-transaction
        :thunk
        (lambda ()
          ,body)))

(defun untrans (transaction)
  (funcall (thunk-of transaction)))

;;;; ** Defining

(defmacro deftransaction (name (&rest args) &body body)
  "Define a new transaction called NAME.

DEFTRANSACTION is just like DEFMETHOD but the body is walked into
an STM action."
  (labels ((extract-vars (args)
             (mapcan (lambda (name trans?)
                       (if trans? (list name) nil))
                     (extract-argument-names args :allow-specializers t)
                     (extract-funargs        args)))
           (extract-funargs (args)
             (mapcar [subtypep _ 'transaction] (extract-specializer-names args))))
    `(progn
       (eval-always
         (pushnew '(,name ,@(extract-funargs args)) *trans-funs*))
       (defmethod ,name ,args
         ,(with-trans-vars (extract-vars args)
            (with-trans-funs (list `(,name ,@(extract-funargs args)))
	      (stm `(progn ,@body)))))
       ',name)))

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

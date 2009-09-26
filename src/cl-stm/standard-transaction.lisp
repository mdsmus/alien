;; -*- lisp -*-

(in-package :cl-stm)

;;;; * Transactions

(defclass standard-transaction (transaction)
  ((thunk :accessor thunk-of
          :initarg :thunk)
   (id :accessor id-of
       :initarg :id
       :initform (next-transaction-id))))

;;;; ** ID Counter

(defvar *transaction-counter* 0
  "A counter used for the ID of transactions.")

(defvar *transaction-counter-lock* (make-lock "*TRANSACTION-COUNTER*")
  "A lock around *TRANSACTION-COUNTER*.")

(defun next-transaction-id ()
  "Return the next transaction id."
  (with-lock-held (*transaction-counter-lock*)
    (incf *transaction-counter*)))

;;;; ** Running

(defmethod perform ((tx standard-transaction) &key wait?)
  (let1 thunk
      (lambda ()
        (without-returning
          (block perform
            (let1 id (id-of tx)
              (prog (x-tlog x-values)
               execute
               (multiple-value-bind (retry? tlog values) (execute tx)
                 (if retry?
                     (progn
                       (stm.perform.dribble "Trying to re-execute transaction ~A" id)
                       (wait tlog)
                       (go execute))
                     (progn
                       (setq x-tlog   tlog
                             x-values values)
                       (go commit))))
               commit
               (if (not (check? x-tlog))
                   (progn
                     (stm.perform.dribble "Trying to re-execute transaction ~A" id)
                     (go execute))
                   (if (not (commit x-tlog))
                       (progn
                         (stm.perform.dribble "Trying to re-commit transaction ~A" id)
                         (wait x-tlog)
                         (go commit))
                       (go done)))
               done
               (return-from perform (values-list x-values)))))))
    (if wait?
        (funcall     thunk)
        (make-thread thunk :name (format nil "TRANSACTION-~A" (id-of tx))))))

(defmethod execute ((tx standard-transaction))
  (stm.execute.dribble "Creating fresh transaction log")
  (let1 id (id-of tx)
    (with-new-tlog tlog
      (stm.execute.dribble "Executing transaction ~A" id)
      (let1 x (catch 'retry (multiple-value-list (funcall (thunk-of tx))))
        (etypecase x
          (tlog
           (stm.execute.debug "Transaction ~A retried" id)
           (values t x))
          (list
           (stm.execute.debug "Transaction ~A finished executing with: ~{~A ~}" id x)
           (values nil tlog x)))))))

;;;; ** Composing

(defmethod bind ((tx1 standard-transaction) (tx2 standard-transaction))
  (new 'standard-transaction
       :thunk
       (lambda ()
         (funcall (thunk-of tx1))
         (funcall (thunk-of tx2)))))

(defmethod orelse ((tx1 standard-transaction) (tx2 standard-transaction))
  (new 'standard-transaction
       :thunk
       (lambda ()
         (block orelse
           (let ((id1 (id-of tx1))
                 (id2 (id-of tx2)))
             (prog (tlog1 tlog2 x-values)
              execute-tx1
              (multiple-value-bind (retry? tlog values) (execute tx1)
                (if retry?
                    (progn
                      (stm.orelse.dribble "Transaction ~A retried, trying transaction ~A" id1 id2)
                      (go execute-tx2))
                    (progn
                      (setq tlog1    tlog
                            x-values values)
                      (go commit-tx1))))
              commit-tx1
              (if (not (check? tlog1))
                  (progn
                    (stm.orelse.dribble "Transaction log of ~A invalid, trying transaction ~A" id1 id2)
                    (go execute-tx2))
                  (if (not (commit tlog1))
                      (progn
                        (stm.orelse.dribble "Transaction log of ~A not committed, trying transaction ~A" id1 id2)
                        (go execute-tx2))
                      (go done)))
              execute-tx2
              (multiple-value-bind (retry? tlog values) (execute tx2)
                (if retry?
                    (progn
                      (stm.orelse.dribble "Transaction ~A retried, retrying both ~A and ~A" id2 id1 id2)
                      (throw 'retry (merge-logs tlog1 tlog)))
                    (progn
                      (setq tlog2    tlog
                            x-values values)
                      (go commit-tx2))))
              commit-tx2
              (if (not (check? tlog2))
                  (progn
                    (stm.orelse.dribble "Transaction log of ~A invalid, retrying both ~A and ~A" id2 id1 id2)
                    (throw 'retry (merge-logs tlog1 tlog2)))
                  (if (not (commit tlog2))
                      (progn
                        (stm.orelse.dribble "Transaction log of ~A not committed, retrying both ~A and ~A" id2 id1 id2)
                        (throw 'retry (merge-logs tlog1 tlog2)))
                      (go done)))
              done
              (return-from orelse (values-list x-values))))))))

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

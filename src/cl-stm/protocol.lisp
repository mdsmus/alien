;; -*- lisp -*-

(in-package :cl-stm)

;;;; * Protocol

;;;; ** Transactions

(defclass transaction ()
  ())

(defgeneric perform (transaction &key wait?)
  (:documentation "Run TRANSACTION atomically with respect to
all other threads in a new thread.

If the transaction retries, then the thread loops again
re-executing the transaction and commiting it to memory.

When WAIT? is true, then PERFORM returns the result of executing
TRANSACTION.  Otherwise PERFORM returns a thread created by
BORDEAUX-THREADS as its value."))

(defgeneric execute (transaction)
  (:documentation "Execute TRANSACTION and return 3 values.

1. A boolean specifying whether or not the transaction retried.

2. A transaction log of all the changes that occured.

3. A list of values that TRANSACTION returns."))

(defgeneric orelse (tx1 tx2)
  (:documentation "Compose two transactions alternatively.
ORELSE returns a new transaction.

We perform TX1 and if it succeeds then we're done.  If it fails
then we perform TX2.

If performing TX2 succeeds then we're done.  If it fails we try
to perform TX1 again, and so on.

The transaction's return value (not ORELSE's) is nondeterministic
because we can't tell in advance which transaction succeeds.

ORELSE is just the plumbing behind the TRY macro, which is
just a variable arity version of ORELSE."))

(defgeneric bind (tx1 tx2)
  (:documentation "Compose two transactions sequentially.
BIND returns a new transaction.

If either TX1 or TX2 retries then the whole transaction retries.

The transaction's return value (not BIND's) is the return value
of TX2."))

;;;; ** Transaction logs

(defclass tlog ()
  ()
  (:documentation "A transaction log (TLOG) is a record of what
reads and writes have been done.

Transaction logs are written during the execution of a
transaction (using READ-TVAR and WRITE-TVAR).  Transactions logs
are committed to memory by COMMIT later on."))

(defgeneric commit (log)
  (:documentation "Commit a LOG to memory.

It returns a boolean specifying whether or not the transaction
log was committed.  If the transaction log couldn't be committed
it probably means that another transaction log that writes the
same variables is being committed."))

(defgeneric check? (log)
  (:documentation "Check that LOG is consistent.

It returns a boolean specifying whether or not LOG is valid with
respect to the current Common Lisp world.  CHECK can be used to
see if a transaction isn't consistent before committing it."))

(defgeneric merge-logs (log1 log2)
  (:documentation "Merge LOG1 and LOG2.

Any reads and writes in LOG2 shadow the reads and writes in
LOG1."))

(defgeneric wait (log)
  (:documentation "WAIT causes the current thread to wait for a
change in any of LOG's reads and writes."))

;;;; ** Transaction variables

(defclass tvar ()
  ()
  (:documentation "A transactional variable (TVAR) holds a value.

See READ-TVAR and WRITE-TVAR for reading and writing them."))

(defgeneric read-tvar (var log)
  (:documentation "Record the reading of VAR to LOG.

READ-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

READ-TVAR is just the plumbing behind taking the SLOT-VALUE of a
transactional slot.  Just use readers and accessors as you
normally would on transactional objects."))

(defgeneric write-tvar (var log val)
  (:documentation "Record the writing of VAL to VAR to LOG.

WRITE-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

WRITE-TVAR is just the plumbing behind SETF'ing a transactional
slot.  Just use SETF as you normally would on transactional
objects."))

(defgeneric unwait (var)
  (:documentation "UNWAIT causes all threads waiting for VAR to
change to wake up."))

;;;; ** Recording transactions

(defvar *record-transactions* nil
  "A boolean specifying whether or not transactions are recorded
to log.")

(defun recording? ()
  *record-transactions*)

(defmacro with-recording (&body body)
  "Turn recording of reads and writes on.  Recording is normally
on inside transactions."
  `(let1 *record-transactions* t
     ,@body))

(defmacro without-recording (&body body)
  "Turn recording of reads and writes off.  Recording is normally
off outside transactions (ie at the REPL) and when initializing
transactional objects."
  `(let1 *record-transactions* nil
     ,@body))

;;;; ** Returning contents

(defvar *return-contents* t
  "A boolean specifying whether or not slot accessors should
return the contents of the transactional variable instead of the
variable itself.")

(defun returning? ()
  *return-contents*)

(defmacro with-returning (&body body)
  `(let1 *return-contents* t
     ,@body))

(defmacro without-returning (&body body)
  `(let1 *return-contents* nil
     ,@body))

;;;; ** Current transaction log

(defvar *tlog* nil
  "The current transaction log.")

(defun current-tlog ()
  *tlog*)

(defmacro with-tlog (log &body body)
  "Execute BODY with the default transasction log being LOG."
  `(let1 *tlog* ,log
     (with-recording
       ,@body)))

(defmacro with-new-tlog (var &body body)
  "Execute BODY with the default transaction log being a newly
allocated transaction log bound to VAR."
  `(let1 ,var (new 'standard-tlog)
     (with-tlog ,var
       ,@body)))

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

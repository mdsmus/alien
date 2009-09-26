;; -*- lisp -*-

(in-package :cl-stm)

;;;; * Transaction logs

(defclass standard-tlog (tlog)
  ((reads :accessor reads-of
          :initarg :reads
          :initform (make-hash-table :test #'eq :size 16)
          :type hash-table
          :documentation "Mapping reads done to versions")
   (writes :accessor writes-of
           :initarg :writes
           :initform (make-hash-table :test #'eq :size 16)
           :type hash-table
           :documentation "Mapping variables written to new values")
   (semaphore :accessor semaphore-of
              :initarg :semaphore
              :initform (make-condition-variable))))

;;;; ** Committing

(defmethod commit ((log standard-tlog))
  (let1 acquired '()
    (stm.commit.dribble "Commiting transaction log...")
    (unwind-protect
         (progn
           (maphash (lambda (var val)
                      (declare (ignore val))
                      (let1 lock (lock-of var)
                        (if (acquire-lock lock nil)
                            (progn
                              (push var acquired)
                              (stm.commit.dribble "Acquired lock ~A" lock))
                            (progn
                              (stm.commit.dribble "Couldn't acquire lock ~A" lock)
                              (stm.commit.debug   "Transaction log not committed")
                              (return-from commit nil)))))
                    (writes-of log))
           (unless (check? log)
             (stm.commit.debug "Transaction log not committed")
             (return-from commit nil))
           (maphash (lambda (var val)
                      (setf (value-of var) val)
                      (stm.commit.dribble "Value updated to ~A" val)
                      (incf (version-of var))
                      (stm.commit.dribble "Version updated to ~A" (version-of var)))
                    (writes-of log))
           (stm.commit.debug "Transaction log committed")
           (return-from commit t))
      (mapc (lambda (var)
              (let1 lock (lock-of var)
                (release-lock lock)
                (stm.commit.dribble "Released lock ~A" lock)))
            acquired)
      (mapc (lambda (var)
              (unwait var)
              (stm.commit.dribble "Notified threads waiting on ~A" var))
            acquired))))

(defmethod check? ((log standard-tlog))
  (stm.check.dribble "Checking transaction log...")
  (maphash (lambda (var ver)
             (if (= ver (version-of var))
                 (stm.check.dribble "Version ~A is valid" ver)
                 (progn
                   (stm.check.dribble "Version ~A doesn't match ~A" ver (version-of var))
                   (stm.check.debug   "Transaction log invalid")
                   (return-from check? nil))))
           (reads-of log))
  (stm.check.dribble "Transaction log valid")
  (return-from check? t))

;;;; ** Merging

(defmethod merge-logs ((log1 standard-tlog) (log2 standard-tlog))
  (maphash (lambda (var val)
             (setf (gethash var (writes-of log1))
                   val))
           (writes-of log2))
  (maphash (lambda (var ver)
             (setf (gethash var (reads-of log1))
                   (max ver
                        (aif2 (gethash var (reads-of log1))
                              it
                              0))))
           (reads-of log2))
  log1)

;;;; ** Waiting

(defmethod wait ((log standard-tlog))
  (maphash (lambda (var val)
             (declare (ignore val))
             (with-slots (waiting waiting-lock) var
               (with-lock-held (waiting-lock)
                 (enqueue waiting log))))
           (reads-of log))
  (let1 dummy (make-lock "dummy lock")
    (acquire-lock dummy)
    (condition-wait (semaphore-of log) dummy)))

(defmethod unwait ((log standard-tlog))
  (condition-notify (semaphore-of log)))

;;;; * Transactional variables

(defclass standard-tvar (tvar)
  ((lock :accessor lock-of
         :initarg :lock
         :initform (make-lock "TVAR"))
   (value :accessor value-of
          :initarg :value)
   (version :accessor version-of
            :initarg :version
            :initform 0
            :type integer)
   (waiting :accessor waiting-for
            :initarg :waiting
            :initform (new 'queue :element-type 'standard-tlog)
            :type queue)
   (waiting-lock :accessor waiting-lock-of
                 :initarg :waiting-lock
                 :initform (make-lock "WAITING-LOCK"))))

;;;; ** Printing

(defprint-object (obj standard-tvar)
  (princ (if (slot-boundp obj 'value)
             (value-of obj)
             "<unbound>")))

;;;; ** Reading and Writing

(defmethod read-tvar ((var standard-tvar) (log standard-tlog))
  (aif2 (gethash var (writes-of log))
        it
        (progn (setf (gethash var (reads-of log))
                     (version-of var))
               (value-of var))))

(defmethod write-tvar ((var standard-tvar) (log standard-tlog) val)
  (setf (gethash var (writes-of log)) val))

;;;; ** Waiting

(defmethod unwait ((var standard-tvar))
  (with-slots (waiting waiting-lock) var
    (with-lock-held (waiting-lock)
      (until (queue-empty-p waiting)
        (unwait (dequeue waiting))))))

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

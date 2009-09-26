;; -*- lisp -*-

(in-package :cl-stm)

(eval-always
  (enable-pf-reader))

;;;; * Transactional classes

;;;; ** Metaclasses

(defclass transactional-class (standard-class)
  ()
  (:documentation "The metaclass for transactional classes.

Classes defined with this metaclass have extra slot options,
see the class TRANSACTIONAL-DIRECT-SLOT for details."))

(defclass transactional-direct-slot (standard-direct-slot-definition)
  ((transactional :accessor slot-transactional
                  :initarg :transactional
                  :initform t))
  (:documentation "The class for direct slots of transactional
classes.

Other than the initargs for standard slots the following
options can be passed to component slots:

:transactional [ T | NIL ] - Specify that this slot is a
transactional slot and that all reads and writes should be
committed to log."))

(defclass transactional-effective-slot (standard-effective-slot-definition)
  ((transactional :accessor slot-transactional
                  :initarg :transactional))
  (:documentation "The class for effective slots of transactional
classes.

Exactly like TRANSACTIONAL-EFFECTIVE-SLOT."))

;;;; ** Inheritance

(defmethod validate-superclass ((sub transactional-class)
                                (sup standard-class))
  (declare (ignore sub sup))
  t)

;;;; ** Slot definitions

(defmethod direct-slot-definition-class ((class transactional-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'transactional-direct-slot))

(defmethod effective-slot-definition-class ((class transactional-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'transactional-effective-slot))

(defmethod compute-effective-slot-definition ((class transactional-class)
                                              slot-name direct-slots)
  (declare (ignore slot-name))
  (let ((effective-slot (call-next-method))
        (direct-slots (remove-if-not [typep _ 'transactional-direct-slot] direct-slots)))
    (unless (null (cdr direct-slots))
      (error "More than one :transactional specifier"))
    (let1 direct-slot (car direct-slots)
      (setf (slot-transactional effective-slot)
            (slot-transactional direct-slot)))
    effective-slot))

;;;; ** Slot access

(defmethod slot-value-using-class ((class transactional-class) instance
                                   (slot transactional-effective-slot))
  (declare (ignore instance))
  (cond ((and (slot-transactional slot) (recording?))
         ;; Record the reading of the tvar (which is found with
         ;; `call-next-method') to the current tlog.  We turn off
         ;; recording and returning because we want the actual tvar,
         ;; not the value inside it.
         (without-recording
           (without-returning
             (read-tvar (call-next-method) (current-tlog)))))
        ((and (slot-transactional slot) (returning?))
         ;; Return the value inside the tvar.
         (value-of (call-next-method)))
        (t
         ;; Return the normal value.
         (call-next-method))))

(defmethod (setf slot-value-using-class) (value    (class transactional-class)
                                          instance (slot transactional-effective-slot))
  (cond ((and (slot-transactional slot) (recording?))
         ;; Record the writing of the tvar to the current tlog.  We
         ;; turn off recording and returning because we want the
         ;; actual the tvar, not the value inside the tvar.
         (let1 var (without-recording (without-returning (slot-value-using-class class instance slot)))
           (write-tvar var (current-tlog) value)))
        ((and (slot-transactional slot) (returning?))
         ;; Write the slot normally.  We turn off recording and
         ;; returning because we want the tvar to be written, not the
         ;; value inside it.
         (without-recording
           (without-returning
             (call-next-method))))
        (t
         (call-next-method))))

(defmethod slot-boundp-using-class ((class transactional-class) instance
                                    (slot transactional-effective-slot))
  ;; Check if the slot is bound.  We turn off recording and returning
  ;; because we want the actual tvar, not the value inside it.
  (if (and (slot-transactional slot) (or (recording?) (returning?)))
      (let1 var (without-recording (without-returning (slot-value-using-class class instance slot)))
        (slot-boundp var 'value))
      (without-returning
        (call-next-method))))

(defmethod slot-makunbound-using-class ((class transactional-class) instance
                                        (slot transactional-effective-slot))
  ;; Make the slot unbound.  We turn off recording and returning
  ;; because we want the actual tvar, not the value inside it.
  (if (and (slot-transactional slot) (or (recording?) (returning?)))
      (let1 var (without-recording (without-returning (slot-value-using-class class instance slot)))
        (slot-makunbound var 'value))
      (without-returning
        (call-next-method))))

;;;; ** Transactional objects

(defclass transactional-object ()
  ()
  (:metaclass transactional-class)
  (:documentation "Superclass of all transactional objects."))

;;;; ** Defining

(defmacro deftclass (class (&rest superclasses) (&rest slots) &rest class-options)
  "Define a new transactional class caleed CLASS.

DEFTCLASS is just like DEFCLASS except the default metaclass is
transactional class, slots are transactional, and it inherits
from TRANSACTIONAL-OBJECT by default."
  (let1 superclasses (or superclasses '(transactional-object))
    `(eval-always
       (defclass ,class ,superclasses
         ,slots
         ,@class-options
         (:metaclass transactional-class))
       ',class)))

;;;; ** Initializing

(defmethod shared-initialize ((instance transactional-object)
                              slot-name &rest initargs)
  (declare (ignore initargs slot-name))
  ;; We turn off recording in the initialization so that any slot
  ;; changes are NOT recorded to the log.
  (without-recording
    (without-returning
      (prog1
          (call-next-method)
        ;; For every transactional slot we turn its value into a tvar.
        (dolist (slotd (class-slots (class-of instance)))
          (let1 slot-name (slot-definition-name slotd)
            ;; Only initialize those where `slot-transactional' is true.
            (when (and (typep slotd 'transactional-effective-slot)
                       (slot-transactional slotd))
              (setf (slot-value instance slot-name)
                    ;; Check if the initarg was specified.
                    (if (slot-boundp instance slot-name)
                        (new 'standard-tvar :value (slot-value instance slot-name))
                        (new 'standard-tvar))))))))))

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

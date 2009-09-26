;; -*- lisp -*-

(in-package :cl-stm)

;;;; ** Concurrent queue

(deftclass tqueue ()
  ((head :accessor head-of
         :initarg :head
         :initform 0)
   (tail :accessor tail-of
         :initarg :tail
         :initform 0)
   (buffer :accessor buffer-of)))

(defmethod initialize-instance :after
    ((q tqueue) &key (initial-size 20) (element-type t) &allow-other-keys)
  (declare (ignore slot-name))
  (unless (> initial-size 1)
    (error "Initial size of a queue must be greater that 1."))
  (setf (buffer-of q)
        (new 'standard-tvar
             :value (make-array (1+ initial-size) :element-type element-type))))

(deftransaction deq ((q tqueue) &key remove? block?)
  (with-slots (head tail buffer) q
    (if (empty? q)
        (if block?
            (retry)
            nil)
        (prog1
            (aref buffer tail)
          (when remove?
            (move-tail q))))))

(deftransaction enq ((q tqueue) val)
  (with-slots (head tail size buffer) q
    (setf (aref buffer head) val)
    (move-head q)
    q))

(deftransaction empty? ((q tqueue))
  (= (head-of q) (tail-of q)))

(deftransaction full? ((q tqueue))
  (with-slots (head tail buffer) q
    (= (mod tail      (length buffer))
       (mod (1+ head) (length buffer)))))

(deftransaction qlength ((q tqueue))
  (with-slots (head tail buffer) q
    (cond
      ((= head tail)
       0)
      ((< tail head)
       (- head tail))
      ((> tail head)
       (- (+ (length buffer) head) tail)))))

(defmacro incf-mod (place divisor)
  `(setf ,place (mod (1+ ,place) ,divisor)))

(deftransaction move-tail ((q tqueue))
  (incf-mod (tail-of q) (length (buffer-of q))))

(deftransaction move-head ((q tqueue))
  (incf-mod (head-of q) (length (buffer-of q))))

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

;; -*- lisp -*-

(in-package :cl-stm.test)

(in-suite :cl-stm)

(test untrans-form
  (let ((f1 '(untrans (increment *c*)))
        (f2 '(increment *c*))
        (f3 '(untrans ((lambda (x) (* 2 x)) 3)))
        (f4 '((lambda (x) (* 2 x)) 3)))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test constant-form
  (let ((f1 ''quote)
        (f2 '(trans 'quote))
        (f3 '2)
        (f4 '(trans 2)))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test if-form
  (let ((f1 '(if (zerop (count-of c))
                 (retry)
                 (decrement (count-of x))))
        (f2 '(trans
              (if (zerop (count-of c))
                  (untrans (retry))
                  (untrans (decrement (count-of x))))))
        (f3 '(if (empty? mvar)
                 nil
                 (take mvar)))
        (f4 '(trans
              (if (untrans (empty? mvar))
                  nil
                  (untrans (take mvar))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test progn-form
  (let ((f1 '(progn (if a b (increment c)) (decrement d) e))
        (f2 '(trans
              (progn
                (if a b (untrans (increment c)))
                (untrans (decrement d))
                e)))
        (f3 '(progn (swap *c1* *c2*)))
        (f4 '(swap *c1* *c2*)))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test progv-form
  (let ((f1 '(progv '(*v1* *v2* *v3*) (reverse (list 1 2 3))
              (values 1 2 3)))
        (f2 '(trans
              (progv '(*v1* *v2* *v3*) (reverse (list 1 2 3))
                (values 1 2 3))))
        (f3 '(progv (reverse '(*v1* *v2*)) (list (increment *c1*) (decrement *c2*))
              (increment *c1* *v1*)
              (decrement *c2* *v2*)))
        (f4 '(trans
              (progv (reverse '(*v1* *v2*)) (list (untrans (increment *c1*))
                                                  (untrans (decrement *c2*)))
                (untrans (increment *c1* *v1*))
                (untrans (decrement *c2* *v2*))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test variable-binding-form
  (let ((f1 '(let* ((x 1)
                    (y 10))
              (let* ((x (+ x 10))
                     (y (* x 10)))
                (values x y))))
        (f2 '(trans
              (let* ((x 1)
                     (y 10))
                (let* ((x (+ x 10))
                       (y (* x 10)))
                  (values x y)))))
        (f3 '(let* ((x (increment *c1*))
                    (y (increment *c2*)))
              (+ x y)))
        (f4 '(trans
              (let* ((x (untrans (increment *c1*)))
                     (y (untrans (increment *c2*))))
                (+ x y))))
        (f5 '(let ((x 1)
                   (y 10))
              (let ((x (+ x 10))
                    (y (* x 10)))
                (values x y))))
        (f6 '(trans
              (let ((x 1)
                    (y 10))
                (let ((x (+ x 10))
                      (y (* x 10)))
                  (values x y)))))
        (f7 '(let ((x (increment *c1*))
                   (y (increment *c2*)))
              (+ x y)))
        (f8 '(trans
              (let ((x (untrans (increment *c1*)))
                    (y (untrans (increment *c2*))))
                (+ x y)))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))
    (is-true (equal (stm f5) f6))
    (is-true (equal (stm f7) f8))))

(test flet-form
  (let ((f1 '(flet ((f (x)
                      (* 2 x))
                     (g (x)
                      (f x)))
             (+ (f 3) (f 4))))
        (f2 '(trans
              (flet ((f (x)
                       (trans
                        (* 2 x)))
                     (g (x)
                       (trans
                        (f x))))
                (+ (untrans (f 3)) (untrans (f 4)))))))
    (is-true (equal (stm f1) f2))))

(test labels-form
  (let ((f1 '(labels ((f (x)
                       (* 2 x))
                      (g (x)
                       (f x)))
              (+ (f 3) (f 4))))
        (f2 '(trans
              (labels ((f (x)
                         (trans
                           (* 2 x)))
                       (g (x)
                         (f x)))
                (+ (untrans (f 3)) (untrans (f 4)))))))
    (is-true (equal (stm f1) f2))))

(test lambda-function-form
  (let ((f1 '(lambda (x)
               (* 2 x)))
        (f2 '(trans
              #'(lambda (x)
                  (trans (* 2 x)))))
        (f3 '(lambda (c1 c2)
               (swap c2 c1)))
        (f4 '(trans
              #'(lambda (c1 c2)
                  (swap c2 c1)))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test setq-form
  (let ((f1 '(setq count (+ 1 count)))
        (f2 '(trans
              (setq count (+ 1 count))))
        (f3 '(setq count (increment *c1* (decrement *c2*))))
        (f4 '(trans
              (setq count (untrans (increment *c1* (untrans (decrement *c2*))))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test application-form
  (let ((f1  '(* 2 3))
        (f2  '(trans (* 2 3)))
        (f3  '(empty! *mvar*))
        (f4  '(empty! *mvar*))
        (f5  '(funcall #'+ 1 2))
        (f6  '(trans (funcall #'+ 1 2)))
        (f7  '(funcall '+ 1 2))
        (f8  '(trans (funcall '+ 1 2)))
        (f9  '(funcall #'increment *c*))
        (f10 '(funcall #'increment *c*))
        (f11 '(funcall  'increment *c*))
        (f12 '(funcall  'increment *c*))
        (f13 '(apply #'+ '(1 2)))
        (f14 '(trans (apply #'+ '(1 2))))
        (f15 '(apply '+ '(1 2)))
        (f16 '(trans (apply '+ '(1 2))))
        (f17 '(apply #'increment '(*c* 12)))
        (f18 '(apply #'increment '(*c* 12)))
        (f19 '(apply  'increment '(*c* 12)))
        (f20 '(apply  'increment '(*c* 12))))
    (is-true (equal (stm f1)  f2))
    (is-true (equal (stm f3)  f4))
    (is-true (equal (stm f5)  f6))
    (is-true (equal (stm f7)  f8))
    (is-true (equal (stm f9)  f10))
    (is-true (equal (stm f11) f12))
    (is-true (equal (stm f13) f14))
    (is-true (equal (stm f15) f16))
    (is-true (equal (stm f17) f18))
    (is-true (equal (stm f19) f20))))

(test function-object-form
  (let ((f1 '#'+)
        (f2 '(trans #'+))
        (f3 '#'(setf count-of))
        (f4 '(trans #'(setf count-of))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test lambda-application-form
  (let ((f1 '((lambda (x)
                (* x 2))
              3))
        (f2 '((lambda (x)
                (trans (* x 2)))
              3))
        (f3 '(+ ((lambda (x)
                   (+ 2 x))
                 3)
                ((lambda (x)
                   (* 2 x))
                 3)))
        (f4 '(trans
              (+ (untrans
                  ((lambda (x)
                     (trans (+ 2 x)))
                   3))
                 (untrans
                  ((lambda (x)
                     (trans (* 2 x)))
                   3))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test multiple-value-call-form
  (let ((f1 '(multiple-value-call #'values 1 2 3))
        (f2 '(trans (multiple-value-call #'values 1 2 3)))
        (f3 '(multiple-value-call 'values 1 2 3))
        (f4 '(trans (multiple-value-call 'values 1 2 3)))
        (f5 '(multiple-value-call #'increment *c* 1111))
        (f6 '(multiple-value-call #'increment *c* 1111))
        (f7 '(multiple-value-call  'increment *c* 1111))
        (f8 '(multiple-value-call  'increment *c* 1111)))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))
    (is-true (equal (stm f5) f6))
    (is-true (equal (stm f7) f8))))

(test multiple-value-prog1-form
  (let ((f1 '(multiple-value-prog1 (values 1 2) (reset *c*)))
        (f2 '(trans
              (multiple-value-prog1 (values 1 2) (untrans (reset *c*)))))
        (f3 '(multiple-value-prog1 (values (take *m1*) (take *m2*))
              (put *m1* 1111)
              (put *m2* 1111)
              1111))
        (f4 '(trans
              (multiple-value-prog1 (values (untrans (take *m1*)) (untrans (take *m2*)))
                (untrans (put *m1* 1111))
                (untrans (put *m2* 1111))
                1111))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test block-form
  (let ((f1 '(block emacs
               (take *m*)
               (return-from emacs 1111)
               (take *m*)))
        (f2 '(trans
              (block emacs
                (untrans (take *m*))
                (return-from emacs 1111)
                (untrans (take *m*)))))
        (f3 '(block nil
              (return-from emacs 3))))
    (is-true (equal (stm f1) f2))
    (signals return-from-unknown-block (stm f3))))

(test catch-form
  (let ((f1 '(catch 'done
               (setq *global* :done)
               (throw 'done (empty? *m*))))
        (f2 '(trans
              (catch 'done
                (setq *global* :done)
                (throw 'done (untrans (empty? *m*))))))
        (f3 '(catch 'retry
               (throw (gensym) (setq *global* :done))))
        (f4 '(trans
              (catch 'retry
                (throw (gensym) (setq *global* :done))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test tagbody-form
  (let ((f1 '(tagbody
                 (setq val 1)
                 (go point-a)
                 (setq val (+ val 16))
               point-c
                 (setq val (+ val 4))
                 (go point-b)
                 (setq val (+ val 32))
               point-a
                 (setq val (+ val 2))
                 (go point-c)
                 (setq val (+ val 64))
               point-b
                 (setq val (+ val 8))))
        (f2 '(trans
              (tagbody
                 (setq val 1)
                 (go point-a)
                 (setq val (+ val 16))
               point-c
                 (setq val (+ val 4))
                 (go point-b)
                 (setq val (+ val 32))
               point-a
                 (setq val (+ val 2))
                 (go point-c)
                 (setq val (+ val 64))
               point-b
                 (setq val (+ val 8)))))
        (f3 '(tagbody
                 (reset c)
                 (go point-a)
                 (increment c 16)
               point-c
                 (increment c 4)
                 (go point-b)
                 (increment c 32)
               point-a
                 (increment c 2)
                 (go point-c)
                 (increment c 64)
               point-b
                 (increment c 8)))
        (f4 '(trans
              (tagbody
                 (untrans (reset c))
                 (go point-a)
                 (untrans (increment c 16))
               point-c
                 (untrans (increment c 4))
                 (go point-b)
                 (untrans (increment c 32))
               point-a
                 (untrans (increment c 2))
                 (go point-c)
                 (untrans (increment c 64))
               point-b
                 (untrans (increment c 8))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test unwind-protect-form
  (let ((f1 '(catch 'foo
              (format t "The inner catch returns ~s.~%"
                      (catch 'foo
                        (unwind-protect (throw 'foo :first-throw)
                          (throw 'foo :second-throw))))
              :outer-catch))
        (f2 '(trans
              (catch 'foo
                (format t "The inner catch returns ~s.~%"
                        (catch 'foo
                          (unwind-protect (throw 'foo :first-throw)
                            (throw 'foo :second-throw))))
                :outer-catch)))
        (f3 '(unwind-protect
                   (put *m* x)
               (take *m*)
               'done))
        (f4 '(trans
              (unwind-protect
                   (untrans (put *m* x))
                (untrans (take *m*))
                'done))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test the-form
  (let ((f1 '(the cons (cons 1 2)))
        (f2 '(trans (the cons (cons 1 2))))
        (f3 '(the number (decrement *c*)))
        (f4 '(trans (the number (untrans (decrement *c*))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

(test eval-when-form
  (let ((f1 '(eval-when (:compile-time)
               (* 2 3)))
        (f2 '(trans
              (eval-when (:compile-time)
                (* 2 3))))
        (f3 '(eval-when (:compile-time)
               (take *m*)))
        (f4 '(trans
              (eval-when (:compile-time)
                (untrans (take *m*))))))
    (is-true (equal (stm f1) f2))
    (is-true (equal (stm f3) f4))))

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

(defpackage :cl-ext-test
  (:use :cl :cl-ext))

(in-package :cl-ext-test)

(test copy-array.1
  (let* ((orig (vector 1 2 3))
         (copy (copy-array orig)))
    (is (eq orig copy) nil)
    (is (equalp orig copy) t)))

(test copy-array.2
  (let ((orig (make-array 1024 :fill-pointer 0)))
    (vector-push-extend 1 orig)
    (vector-push-extend 2 orig)
    (vector-push-extend 3 orig)
    (let ((copy (copy-array orig)))
      (is (eq orig copy) nil)
      (is (equalp orig copy) t)
      (is (array-has-fill-pointer-p copy) t)
      (is (eql (fill-pointer orig) (fill-pointer copy)) t))))

(test array-index.1
    (is (typep 0 'array-index)))

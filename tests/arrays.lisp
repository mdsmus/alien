(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

(test copy-array.1
  "test array"
  (let* ((orig (vector 1 2 3))
         (copy (copy-array orig)))
    (is (not (eq orig copy)))
    (is (equalp orig copy))))

(test copy-array.2
  "test array"
  (let ((orig (make-array 1024 :fill-pointer 0)))
    (vector-push-extend 1 orig)
    (vector-push-extend 2 orig)
    (vector-push-extend 3 orig)
    (let ((copy (copy-array orig)))
      (is (eq orig copy))
      (is (equalp orig copy))
      (is (array-has-fill-pointer-p copy))
      (is (eql (fill-pointer orig) (fill-pointer copy))))))

(test array-index.1
    (is (typep 0 'array-index)))


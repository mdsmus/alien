(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; ensure-function
;; make-collector
;; make-pusher
;; with-reducer
;; with-collector
;; with-collectors

(test disjoin.1
  (is (equal (let ((disjunction (disjoin (lambda (x)
                                           (and (consp x) :cons))
                                         (lambda (x)
                                           (and (stringp x) :string)))))
               (list (funcall disjunction 'zot)
                     (funcall disjunction '(foo bar))
                     (funcall disjunction "test")))
             '(nil :cons :string))))

(test conjoin.1
  (is (equal (let ((conjunction (conjoin #'consp
                                         (lambda (x)
                                           (stringp (car x)))
                                         (lambda (x)
                                           (char (car x) 0)))))
               (list (funcall conjunction 'zot)
                     (funcall conjunction '(foo))
                     (funcall conjunction '("foo"))))
             '(nil nil #\f))))

(test compose.1
  (is (eql (let ((composite (compose '1+
                                     (lambda (x)
                                       (* x 2))
                                     #'read-from-string)))
             (funcall composite "1"))
           3)))

(test compose.2
  (is (eql (let ((composite
                  (locally (declare (notinline compose))
                    (compose '1+
                             (lambda (x)
                               (* x 2))
                             #'read-from-string))))
             (funcall composite "2"))
           5)))

(test compose.3
  (is (eql (let ((compose-form (funcall (compiler-macro-function 'compose)
                                        '(compose '1+
                                          (lambda (x)
                                            (* x 2))
                                          #'read-from-string)
                                        nil)))
             (let ((fun (funcall (compile nil `(lambda () ,compose-form)))))
               (funcall fun "3")))
           7)))

(test multiple-value-compose.1
  (is (equal (let ((composite (multiple-value-compose
                               #'truncate
                               (lambda (x y)
                                 (values y x))
                               (lambda (x)
                                 (with-input-from-string (s x)
                                   (values (read s) (read s)))))))
               (multiple-value-list (funcall composite "2 7")))
             '(3 1))))

(test multiple-value-compose.2
  (is (equal (let ((composite (locally (declare (notinline multiple-value-compose))
                                (multiple-value-compose
                                 #'truncate
                                 (lambda (x y)
                                   (values y x))
                                 (lambda (x)
                                   (with-input-from-string (s x)
                                     (values (read s) (read s))))))))
               (multiple-value-list (funcall composite "2 11")))
             '(5 1))))

(test multiple-value-compose.3
  (is (equal (let ((compose-form (funcall (compiler-macro-function 'multiple-value-compose)
                                          '(multiple-value-compose
                                            #'truncate
                                            (lambda (x y)
                                              (values y x))
                                            (lambda (x)
                                              (with-input-from-string (s x)
                                                (values (read s) (read s)))))
                                          nil)))
               (let ((fun (funcall (compile nil `(lambda () ,compose-form)))))
                 (multiple-value-list (funcall fun "2 9"))))
             '(4 1))))

(test curry.1
  (is (eql (let ((curried (curry '+ 3)))
             (funcall curried 1 5))
           9)))

(test curry.2
  (is (eql (let ((curried (locally (declare (notinline curry))
                            (curry '* 2 3))))
             (funcall curried 7))
           42)))

(test curry.3
  (is (eql (let ((curried-form (funcall (compiler-macro-function 'curry)
                                        '(curry '/ 8)
                                        nil)))
             (let ((fun (funcall (compile nil `(lambda () ,curried-form)))))
               (funcall fun 2)))
           4)))

(test rcurry.1
  (is (eql (let ((r (rcurry '/ 2)))
             (funcall r 8))
           4)))

(test named-lambda.1
  (is (eql (let ((fac (named-lambda fac (x)
                        (if (> x 1)
                            (* x (fac (- x 1)))
                            x))))
             (funcall fac 5))
           120)))

(test named-lambda.2
  (is (eql (let ((fac (named-lambda fac (&key x)
                        (if (> x 1)
                            (* x (fac :x (- x 1)))
                            x))))
             (funcall fac :x 5))
           120)))

(test make-reducer
  (let ((r (make-reducer #'+ 0)))
    (funcall r 0)
    (funcall r 1 2)
    (funcall r 1 2 3)
    (is (= 9 (funcall r)))))

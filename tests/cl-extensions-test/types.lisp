(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; coercef

(test of-type.1
  (is (equal (locally
                 (declare (notinline of-type))
               (let ((f (of-type 'string)))
                 (list (funcall f "foo")
                       (funcall f 'bar))))
             '(t nil))))

(test type=.1
  (is (equal (type= 'string 'string)
             '(t t))))

(test type=.2
  (is (equal (type= 'list '(or null cons))
             '(t t))))

(test type=.3
  (is (equal (type= 'null '(and symbol list))
             '(t t))))

(test type=.4
  (is (equal (type= 'string '(satisfies emptyp))
             '(nil nil))))

(test type=.5
  (is (equal (type= 'string 'list)
             '(nil t))))

(macrolet
    ((%test (type numbers)
       `(test ,(format-symbol t "CDR5.~A" type)
          (let ((numbers ,numbers))
            (values (mapcar (of-type ',(format-symbol t "NEGATIVE-~A" type)) numbers)
                    (mapcar (of-type ',(format-symbol t "NON-POSITIVE-~A" type)) numbers)
                    (mapcar (of-type ',(format-symbol t "NON-NEGATIVE-~A" type)) numbers)
                    (mapcar (of-type ',(format-symbol t "POSITIVE-~A" type)) numbers)))
	  (t t t nil nil nil nil)
	  (t t t t nil nil nil)
	  (nil nil nil t t t t)
	  (nil nil nil nil t t t))))
  (%test fixnum       (list most-negative-fixnum       -42      -1     0     1     42      most-positive-fixnum))
  (%test integer      (list (1- most-negative-fixnum)  -42      -1     0     1     42      (1+ most-positive-fixnum)))
  (%test rational     (list (1- most-negative-fixnum)  -42/13   -1     0     1     42/13   (1+ most-positive-fixnum)))
  (%test real         (list most-negative-long-float   -42/13   -1     0     1     42/13   most-positive-long-float))
  (%test float        (list most-negative-short-float  -42.02   -1.0   0.0   1.0   42.02   most-positive-short-float))
  (%test short-float  (list most-negative-short-float  -42.02s0 -1.0s0 0.0s0 1.0s0 42.02s0 most-positive-short-float))
  (%test single-float (list most-negative-single-float -42.02f0 -1.0f0 0.0f0 1.0f0 42.02f0 most-positive-single-float))
  (%test double-float (list most-negative-double-float -42.02d0 -1.0d0 0.0d0 1.0d0 42.02d0 most-positive-double-float))
  (%test long-float   (list most-negative-long-float   -42.02l0 -1.0l0 0.0l0 1.0l0 42.02l0 most-positive-long-float)))

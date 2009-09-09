(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; expt-mod
;; radix-values
;; divf
;; factorial
;; binomial-coefficient
;; subfactorial
;; count-permutations

(test 10^
  (is (= (10^ 2) 100)))

(test maxf.1
  (is (eql (let ((x 1))
             (maxf x 2)
             x)
           2)))

(test maxf.2
  (is (eql (let ((x 1))
             (maxf x 0)
             x)
           1)))

(test maxf.3
  (is (equal (let ((x 1)
                   (c 0))
               (maxf x (incf c))
               (list x c))
             '(1 1))))

(test maxf.4
  (is (equal (let ((xv (vector 0 0 0))
                   (p 0))
               (maxf (svref xv (incf p)) (incf p))
               (list p xv))
             '(2 #(0 2 0)))))

(test minf.1
  (is (eql (let ((y 1))
             (minf y 0)
             y)
           0)))

(test minf.2
  (is (equal (let ((xv (vector 10 10 10))
                   (p 0))
               (minf (svref xv (incf p)) (incf p))
               (list p xv))
             '(2 #(10 2 10)))))

(test mulf
  (let ((a 0))
    (is (= 0 (mulf a 10)))
    (is (= 0 a)))
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (let ((orig-a a))
      (mulf a b)
      (is (= a (* orig-a b)))))

  (let ((a 1))
    (is (= 4 (mulf a 4)))
    (is (= 1 (mulf a (/ 4))))
    (is (= 1 a))))

(test minf
  (let ((a 10))
    (is (= 5 (minf a 5)))
    (is (= 5 a)))

  (let ((a 0))
    (is (= 0 (minf a 10)))
    (is (= 0 a))))

(test parse-float
  (is (= 0 (parse-float "0")))
  (is (= -1 (parse-float "-1")))
  (is (= 1 (parse-float "1")))

  (dolist (type '(short-float single-float double-float long-float))
    (for-all ((float (gen-float :type type :bound 1000)))
      (let* ((*print-base* 10)
             (*print-radix* nil))
        (is (= float (parse-float (princ-to-string float) :type type)))))))

(test clamp.1
  (is (equal (list (clamp 1.5 1 2)
                   (clamp 2.0 1 2)
                   (clamp 1.0 1 2)
                   (clamp 3 1 2)
                   (clamp 0 1 2))
             '(1.5 2.0 1.0 2 1))))

(test lerp.1
  (is (eql (lerp 0.5 1 2)
           1.5)))

(test lerp.2
  (is (eql (lerp 0.1 1 2)
           1.1)))

(test mean.1
  (is (eql (mean '(1 2 3))
           2)))

(test mean.2
  (is (eql (mean '(1 2 3 4))
           5/2)))

(test mean.3
  (is (eql (mean '(1 2 10))
           13/3)))

(test median.1
  (is (eql (median '(100 0 99 1 98 2 97))
           97)))

(test median.2
  (is (eql (median '(100 0 99 1 98 2 97 96))
           195/2)))

(test variance.1
  (is (eql (variance (list 1 2 3))
           2/3)))

(test standard-deviation.1
  (is (eql (< 0 (standard-deviation (list 1 2 3)) 1))))

(test gaussian-random.1
  (is (equal (let ((min -0.2)
                   (max +0.2))
               (multiple-value-bind (g1 g2)
                   (gaussian-random min max)
                 (values (<= min g1 max)
                         (<= min g2 max)
                         (/= g1 g2)       ;uh
                         )))
             '(t t t))))

;;; FIXME: write better test
(test parse-number.1
  (let ((test-values '("1" "-1" "1034" "-364" "80/335" "3.5333" "2.4E4" "6.8d3" "#xFF" "#b-1000" "#o-101/75" "13.09s3" "35.66l5" "21.4f2" "#C(1 2)" "#c ( #xF #o-1 ) " "#c(1d1 2s1)" "#16rFF" "#9r10" "#C(#9r44/61 4f4)")))
    (format t "~&~16@A (~16@A) = ~16A~%~%"
            "String value" "READ value" "Parsed value")
    (dolist (value test-values)
      (format t "~&~16@A (~16@A) = ~16A~%"
              value
              (read-from-string value)
              (parse-number value)))))


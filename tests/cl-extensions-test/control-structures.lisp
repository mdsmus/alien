;; awhile
;; while*
;; until*
;; varsymp
;; binding
;; list-match
;; list-match-case
;; let1

(test switch.1
  (is (eql (switch (13 :test =)
             (12 :oops)
             (13.0 :yay))
           :yay)))

(test switch.2
  (is (eql (switch (13)
             ((+ 12 2) :oops)
             ((- 13 1) :oops2)
             (t :yay))
           :yay)))

(test eswitch.1
  (is (eql (let ((x 13))
             (eswitch (x :test =)
               (12 :oops)
               (13.0 :yay)))
           :yay)))

(test eswitch.2
  (is (eql (let ((x 13))
             (eswitch (x :key 1+)
               (11 :oops)
               (14 :yay)))
           :yay)))

(test cswitch.1
  (is (eql (cswitch (13 :test =)
             (12 :oops)
             (13.0 :yay))
           :yay)))

(test cswitch.2
  (is (eql (cswitch (13 :key 1-)
             (12 :yay)
             (13.0 :oops))
           :yay)))

(test whichever.1
  (is (let ((x (whichever 1 2 3)))
        (and (member x '(1 2 3)) t))))

(test whichever.2
  (is (let* ((a 1)
             (b 2)
             (c 3)
             (x (whichever a b c)))
        (and (member x '(1 2 3)) t))))

(test xor.1
  (is (equal (multiple-value-list (xor nil nil 1 nil))
             '(1 t))))

(test flow-control
  (let ((ht (make-hash-table)))
    (setf (gethash 'a ht) 1)
    (setf (gethash 'b ht) 'a)

    ;; if-bind and aif
    (is (= 3 (if-bind var (gethash 'z ht) (1+ var) 3)))
    (is (= 2 (if-bind var (gethash 'a ht) (1+ var) 3)))
    (is (= 3 (aif (gethash 'z ht) (1+ it) 3)))
    (is (= 2 (aif (gethash 'a ht) (1+ it) 3)))
    ;; when-bind and awhen
    (let ((result nil))
      (when-bind var (gethash 'z ht)
        (setf result (1+ var)))
      (is (null result))
      (when-bind var (gethash 'a ht)
        (setf result (1+ var)))
      (is (= 2 result))
      (setf result nil)
      (awhen (gethash 'z ht)
        (setf result (1+ it)))
      (is (null result))
      (awhen (gethash 'a ht)
        (setf result (1+ it)))
      (is (= 2 result)))
    ;; cond-bind and acond
    (is (= 99 (cond-bind var
                ((gethash 'z ht) (1+ var))
                ((gethash 'y ht) (1+ var))
                (t 99))))
    (is (= 2 (cond-bind var
               ((gethash 'z ht) (1+ var))
               ((gethash 'a ht) (1+ var))
                (t 99))))
    (is (= 1 (cond-bind var
              ((gethash 'z ht))
              ((gethash 'y ht))
              ((gethash 'a ht))
              (t 99))))
    (is (= 99 (acond
               ((gethash 'z ht) (1+ it))
               ((gethash 'y ht) (1+ it))
                (t 99))))
    (is (= 2 (acond
              ((gethash 'z ht) (1+ it))
              ((gethash 'a ht) (1+ it))
              (t 99))))
    (is (= 2 (acond
              ((gethash 'z ht))
              ((gethash 'a ht) (1+ it))
              (t 99))))
    ;; and-bind and aand
    (is-false (and-bind var (gethash 'z ht) (gethash var ht) (1+ var)))
    (is (= 2 (and-bind var (gethash 'b ht) (gethash var ht) (1+ var))))
    (is-false (aand (gethash 'z ht) (gethash it ht) (1+ it)))
    (is (= 2 (aand (gethash 'b ht) (gethash it ht) (1+ it))))))

(declaim (notinline opaque))
(defun opaque (x)
  x)

(test if-let.1
  (is (eql (if-let (x (opaque :ok))
             x
             :bad)
           :ok)))

(test if-let.2
  (is (eql (if-let (x (opaque nil))
             :bad
             (and (not x) :ok))
           :ok)))

(test if-let.3
  (is (eql (let ((x 1))
             (if-let ((x 2)
                      (y x))
               (+ x y)
               :oops))
           3)))

(test if-let.4
  (is (eql (if-let ((x 1)
                    (y nil))
             :oops
             (and (not y) x))
           1)))

(test if-let.5
  (is (eql (if-let (x)
             :oops
             (not x))
           t)))

(test if-let.error.1
  (is (eql (handler-case
               (eval '(if-let x
                       :oops
                       :oops))
             (type-error ()
               :type-error))
           :type-error)))

(test when-let.1
  (is (equal (when-let (x (opaque :ok))
               (setf x (cons x x))
               x)
             '(:ok . :ok))))

(test when-let.2
  (is (eql (when-let ((x 1)
                      (y nil)
                      (z 3))
             :oops)
           nil)))

(test when-let.3
  (is (eql (let ((x 1))
             (when-let ((x 2)
                        (y x))
               (+ x y)))
           3)))

(test when-let.error.1
  (is (eql (handler-case
               (eval '(when-let x :oops))
             (type-error ()
               :type-error))
           :type-error)))

(test when-let*.1
  (is (eql (let ((x 1))
             (when-let* ((x 2)
                         (y x))
               (+ x y)))
           4)))

(test when-let*.2
  (is (eql (let ((y 1))
             (when-let* (x y)
               (1+ x)))
           2)))

(test when-let*.3
  (is (eql (when-let* ((x t)
                       (y (consp x))
                       (z (error "OOPS")))
             t)
           nil)))

(test when-let*.error.1
  (is (eql (handler-case
               (eval '(when-let* x :oops))
             (type-error ()
               :type-error))
           :type-error)))

(test nth-value-or.1
  (is (multiple-value-bind (a b c)
          (nth-value-or 1
            (values 1 nil 1)
            (values 2 2 2))
        (= a b c 2))))

(test define-constant.1
  (is (equal (multiple-value-list (let ((name (gensym)))
                                    (eval `(define-constant ,name "FOO" :test 'equal))
                                    (eval `(define-constant ,name "FOO" :test 'equal))
                                    (values (equal "FOO" (symbol-value name))
                                            (constantp name))))
             '(t t))))

(test define-constant.2
  (is (equal (multiple-value-list (let ((name (gensym)))
                                    (eval `(define-constant ,name 13))
                                    (eval `(define-constant ,name 13))
                                    (values (eql 13 (symbol-value name))
                                            (constantp name))))
             '(t t))))

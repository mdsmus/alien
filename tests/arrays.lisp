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

;;;; Conditions

(test unwind-protect-case.1
  (let (result)
    (unwind-protect-case ()
        (random 10)
      (:normal (push :normal result))
      (:abort  (push :abort result))
      (:always (push :always result)))
    result)
  (:always :normal))

(test unwind-protect-case.2
  (let (result)
    (unwind-protect-case ()
        (random 10)
      (:always (push :always result))
      (:normal (push :normal result))
      (:abort  (push :abort result)))
    result)
  (:normal :always))

(test unwind-protect-case.3
  (let (result1 result2 result3)
    (ignore-errors
      (unwind-protect-case ()
          (error "FOOF!")
        (:normal (push :normal result1))
        (:abort  (push :abort result1))
        (:always (push :always result1))))
    (catch 'foof
      (unwind-protect-case ()
          (throw 'foof 42)
        (:normal (push :normal result2))
        (:abort  (push :abort result2))
        (:always (push :always result2))))
    (block foof
      (unwind-protect-case ()
          (return-from foof 42)
        (:normal (push :normal result3))
        (:abort  (push :abort result3))
        (:always (push :always result3))))
    (values result1 result2 result3))
  (:always :abort)
  (:always :abort)
  (:always :abort))

(test unwind-protect-case.4
  (let (result)
    (unwind-protect-case (aborted-p)
        (random 42)
      (:always (setq result aborted-p)))
    result)
  nil)

(test unwind-protect-case.5
  (let (result)
    (block foof
      (unwind-protect-case (aborted-p)
          (return-from foof)
        (:always (setq result aborted-p))))
    result)
  t)

;;;; Control flow

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

;;;; Definitions

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

;;;; Errors

(test required-argument.1
  (is (multiple-value-bind (res err)
          (ignore-errors (required-argument))
        (typep err 'error))))

;;;; Hash tables

(test ensure-hash-table.1
  (is (eql (let ((table (make-hash-table))
                 (x (list 1)))
             (multiple-value-bind (value already-there)
                 (ensure-gethash x table 42)
               (and (= value 42)
                    (not already-there)
                    (= 42 (gethash x table))
                    (multiple-value-bind (value2 already-there2)
                        (ensure-gethash x table 13)
                      (and (= value2 42)
                           already-there2
                           (= 42 (gethash x table))))))))))

(test copy-hash-table.1
  (is (equal (let ((orig (make-hash-table :test 'eq :size 123))
                   (foo "foo"))
               (setf (gethash orig orig) t
                     (gethash foo orig) t)
               (let ((eq-copy (copy-hash-table orig))
                     (eql-copy (copy-hash-table orig :test 'eql))
                     (equal-copy (copy-hash-table orig :test 'equal))
                     (equalp-copy (copy-hash-table orig :test 'equalp)))
                 (list (hash-table-size eq-copy)
                       (hash-table-count eql-copy)
                       (gethash orig eq-copy)
                       (gethash (copy-seq foo) eql-copy)
                       (gethash foo eql-copy)
                       (gethash (copy-seq foo) equal-copy)
                       (gethash "FOO" equal-copy)
                       (gethash "FOO" equalp-copy))))
             '(123 2 t nil t t nil t))))

(test copy-hash-table.2
  (is (equal (let ((ht (make-hash-table))
                   (list (list :list (vector :A :B :C))))
               (setf (gethash 'list ht) list)
               (let* ((shallow-copy (copy-hash-table ht))
                      (deep1-copy (copy-hash-table ht :key 'copy-list))
                      (list         (gethash 'list ht))
                      (shallow-list (gethash 'list shallow-copy))
                      (deep1-list   (gethash 'list deep1-copy)))
                 (list (eq ht shallow-copy)
                       (eq ht deep1-copy)
                       (eq list shallow-list)
                       (eq list deep1-list) ; outer list was copied.
                       (eq (second list) (second shallow-list))
                       (eq (second list) (second deep1-list)) ; inner vector wasn't copied.
                       )))
             '(nil nil t nil t t))))

(test maphash-keys.1
  (is (eql (let ((keys nil)
                 (table (make-hash-table)))
             (declare (notinline maphash-keys))
             (dotimes (i 10)
               (setf (gethash i table) t))
             (maphash-keys (lambda (k) (push k keys)) table)
             (set-equal keys '(0 1 2 3 4 5 6 7 8 9))))))

(test maphash-values.1
  (is (eql (let ((vals nil)
                 (table (make-hash-table)))
             (declare (notinline maphash-values))
             (dotimes (i 10)
               (setf (gethash i table) (- i)))
             (maphash-values (lambda (v) (push v vals)) table)
             (set-equal vals '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))))))

(test hash-table-keys.1
  (is (eql (let ((table (make-hash-table)))
             (dotimes (i 10)
               (setf (gethash i table) t))
             (set-equal (hash-table-keys table) '(0 1 2 3 4 5 6 7 8 9))))))

(test hash-table-values.1
  (is (eql (let ((table (make-hash-table)))
             (dotimes (i 10)
               (setf (gethash (gensym) table) i))
             (set-equal (hash-table-values table) '(0 1 2 3 4 5 6 7 8 9))))))

(test hash-table-alist.1
  (is (equal (let ((table (make-hash-table)))
               (dotimes (i 10)
                 (setf (gethash i table) (- i)))
               (let ((alist (hash-table-alist table)))
                 (list (length alist)
                       (assoc 0 alist)
                       (assoc 3 alist)
                       (assoc 9 alist)
                       (assoc nil alist))))
             '(10 (0 . 0) (3 . -3) (9 . -9) nil))))

(test hash-table-plist.1
  (is (equal (let ((table (make-hash-table)))
               (dotimes (i 10)
                 (setf (gethash i table) (- i)))
               (let ((plist (hash-table-plist table)))
                 (list (length plist)
                       (getf plist 0)
                       (getf plist 2)
                       (getf plist 7)
                       (getf plist nil))))
             '(20 0 -2 -7 nil))))

(test alist-hash-table.1
  (is (equal (let* ((alist '((0 a) (1 b) (2 c)))
                    (table (alist-hash-table alist)))
               (list (hash-table-count table)
                     (gethash 0 table)
                     (gethash 1 table)
                     (gethash 2 table)
                     (hash-table-test table)))
             '(3 (a) (b) (c) eql))))

(test plist-hash-table.1
  (is (equal (let* ((plist '(:a 1 :b 2 :c 3))
                    (table (plist-hash-table plist :test 'eq)))
               (list (hash-table-count table)
                     (gethash :a table)
                     (gethash :b table)
                     (gethash :c table)
                     (gethash 2 table)
                     (gethash nil table)
                     (hash-table-test table)))
             '(3 1 2 3 nil nil eq))))

;;;; Functions

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

;;;; Lists

(test alist-plist.1
  (is (equal (alist-plist '((a . 1) (b . 2) (c . 3)))
             '(a 1 b 2 c 3))))

(test plist-alist.1
  (is (equal (plist-alist '(a 1 b 2 c 3))
             '((a . 1) (b . 2) (c . 3)))))

(test unionf.1
  (is (equal (multiple-value-list (let* ((list (list 1 2 3))
                                         (orig list))
                                    (unionf list (list 1 2 4))
                                    (values (equal orig (list 1 2 3))
                                            (eql (length list) 4)
                                            (set-difference list (list 1 2 3 4))
                                            (set-difference (list 1 2 3 4) list))))
             '(t t nil nil))))

(test nunionf.1
  (is (equal (multiple-value-list (let ((list (list 1 2 3)))
                                    (nunionf list (list 1 2 4))
                                    (values (eql (length list) 4)
                                            (set-difference (list 1 2 3 4) list)
                                            (set-difference list (list 1 2 3 4)))))
             '(t nil nil))))

(test appendf.1
  (is (equal (let* ((list (list 1 2 3))
                    (orig list))
               (appendf list '(4 5 6) '(7 8))
               (list list (eq list orig)))
             '((1 2 3 4 5 6 7 8) nil))))

(test nconcf.1
  (is (equal (let ((list1 (list 1 2 3))
                   (list2 (list 4 5 6)))
               (nconcf list1 list2 (list 7 8 9))
               list1)
             '(1 2 3 4 5 6 7 8 9))))

(test circular-list.1
  (is (equal (let ((circle (circular-list 1 2 3)))
               (list (first circle)
                     (second circle)
                     (third circle)
                     (fourth circle)
                     (eq circle (nthcdr 3 circle))))
             '(1 2 3 1 t))))

(test circular-list-p.1
  (is (equal (let* ((circle (circular-list 1 2 3 4))
                    (tree (list circle circle))
                    (dotted (cons circle t))
                    (proper (list 1 2 3 circle))
                    (tailcirc (list* 1 2 3 circle)))
               (list (circular-list-p circle)
                     (circular-list-p tree)
                     (circular-list-p dotted)
                     (circular-list-p proper)
                     (circular-list-p tailcirc)))
             '(t nil nil nil t))))

(test circular-list-p.2
  (is (eql (circular-list-p 'foo)
           nil)))

(test circular-tree-p.1
  (is (equal (let* ((circle (circular-list 1 2 3 4))
                    (tree1 (list circle circle))
                    (tree2 (let* ((level2 (list 1 nil 2))
                                  (level1 (list level2)))
                             (setf (second level2) level1)
                             level1))
                    (dotted (cons circle t))
                    (proper (list 1 2 3 circle))
                    (tailcirc (list* 1 2 3 circle))
                    (quite-proper (list 1 2 3))
                    (quite-dotted (list 1 (cons 2 3))))
               (list (circular-tree-p circle)
                     (circular-tree-p tree1)
                     (circular-tree-p tree2)
                     (circular-tree-p dotted)
                     (circular-tree-p proper)
                     (circular-tree-p tailcirc)
                     (circular-tree-p quite-proper)
                     (circular-tree-p quite-dotted)))
             '(t t t t t t nil nil))))

(test proper-list-p.1
  (is (equal (let ((l1 (list 1))
                   (l2 (list 1 2))
                   (l3 (cons 1 2))
                   (l4 (list (cons 1 2) 3))
                   (l5 (circular-list 1 2)))
               (list (proper-list-p l1)
                     (proper-list-p l2)
                     (proper-list-p l3)
                     (proper-list-p l4)
                     (proper-list-p l5)))
             '(t t nil t nil))))

(test proper-list-p.2
  (is (eql (proper-list-p '(1 2 . 3))
           nil)))

(test proper-list.type.1
  (is (equal (let ((l1 (list 1))
                   (l2 (list 1 2))
                   (l3 (cons 1 2))
                   (l4 (list (cons 1 2) 3))
                   (l5 (circular-list 1 2)))
               (list (typep l1 'proper-list)
                     (typep l2 'proper-list)
                     (typep l3 'proper-list)
                     (typep l4 'proper-list)
                     (typep l5 'proper-list)))
             '(t t nil t nil))))

(test lastcar.1
  (is (equal (let ((l1 (list 1))
                   (l2 (list 1 2)))
               (list (lastcar l1)
                     (lastcar l2)))
             '(1 2))))

(test lastcar.error.2
  (is (eql (handler-case
               (progn
                 (lastcar (circular-list 1 2 3))
                 nil)
             (error ()
               t)))))

(test setf-lastcar.1
  (is (equal (let ((l (list 1 2 3 4)))
               (list (lastcar l)
                     (progn
                       (setf (lastcar l) 42)
                       (lastcar l))))
             '(4 42))))

(test setf-lastcar.2
  (is (eql (let ((l (circular-list 1 2 3)))
             (multiple-value-bind (res err)
                 (ignore-errors (setf (lastcar l) 4))
               (typep err 'type-error))))))

(test make-circular-list.1
  (is (equal (let ((l (make-circular-list 3 :initial-element :x)))
               (setf (car l) :y)
               (list (eq l (nthcdr 3 l))
                     (first l)
                     (second l)
                     (third l)
                     (fourth l)))
             '(t :y :x :x :y))))

(test circular-list.type.1
  (is (equal (let* ((l1 (list 1 2 3))
                    (l2 (circular-list 1 2 3))
                    (l3 (list* 1 2 3 l2)))
               (list (typep l1 'circular-list)
                     (typep l2 'circular-list)
                     (typep l3 'circular-list)))
             '(nil t t))))

(test ensure-list.1
  (is (equal (let ((x (list 1))
                   (y 2))
               (list (ensure-list x)
                     (ensure-list y)))
             '((1) (2)))))

(test ensure-cons.1
  (is (equal (let ((x (cons 1 2))
                   (y nil)
                   (z "foo"))
               (list (ensure-cons x)
                     (ensure-cons y)
                     (ensure-cons z)))
             '((1 . 2) (nil) ("foo")))))

(test setp.1
  (is (eql (setp '(1)))))

(test setp.2
  (is (eql (setp nil))))

(test setp.3
  (is (eql (setp "foo")
           nil)))

(test setp.4
  (is (eql (setp '(1 2 3 1))
           nil)))

(test setp.5
  (is (eql (setp '(1 2 3)))))

(test setp.6
  (is (eql (setp '(a :a)))))

(test setp.7
  (is (eql (setp '(a :a) :key 'character)
           nil)))

(test setp.8
  (is (eql (setp '(a :a) :key 'character :test (constantly nil)))))

(test set-equal.1
  (is (eql (set-equal '(1 2 3) '(3 1 2)))))

(test set-equal.2
  (is (eql (set-equal '("Xa") '("Xb")
                      :test (lambda (a b) (eql (char a 0) (char b 0)))))))

(test set-equal.3
  (is (eql (set-equal '(1 2) '(4 2))
           nil)))

(test set-equal.4
  (is (eql (set-equal '(a b c) '(:a :b :c) :key 'string :test 'equal))))

(test set-equal.5
  (is (eql (set-equal '(a d c) '(:a :b :c) :key 'string :test 'equal)
           nil)))

(test set-equal.6
  (is (eql (set-equal '(a b c) '(a b c d))
           nil)))

(test map-product.1
  (is (equal (map-product 'cons '(2 3) '(1 4))
             '((2 . 1) (2 . 4) (3 . 1) (3 . 4)))))

(test map-product.2
  (is (equal (map-product #'cons '(2 3) '(1 4))
             '((2 . 1) (2 . 4) (3 . 1) (3 . 4)))))

(test flatten.1
  (is (equal (flatten '((1) 2 (((3 4))) ((((5)) 6)) 7))
             '(1 2 3 4 5 6 7))))

(test remove-from-plist.1
  (is (equal (let ((orig '(a 1 b 2 c 3 d 4)))
               (list (remove-from-plist orig 'a 'c)
                     (remove-from-plist orig 'b 'd)
                     (remove-from-plist orig 'b)
                     (remove-from-plist orig 'a)
                     (remove-from-plist orig 'd 42 "zot")
                     (remove-from-plist orig 'a 'b 'c 'd)
                     (remove-from-plist orig 'a 'b 'c 'd 'x)
                     (equal orig '(a 1 b 2 c 3 d 4))))
             '((b 2 d 4)
               (a 1 c 3)
               (a 1 c 3 d 4)
               (b 2 c 3 d 4)
               (a 1 b 2 c 3)
               nil
               nil
               t))))

(test mappend.1
  (is (equal (mappend (compose 'list '*) '(1 2 3) '(1 2 3))
             '(1 4 9))))

;;;; Numbers

(test clamp.1
  (is (equal (list (clamp 1.5 1 2)
                   (clamp 2.0 1 2)
                   (clamp 1.0 1 2)
                   (clamp 3 1 2)
                   (clamp 0 1 2))
             '(1.5 2.0 1.0 2 1))))

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

(test iota.1
  (is (equal (iota 3)
             '(0 1 2))))

(test iota.2
  (is (equal (iota 3 :start 0.0d0)
             '(0.0d0 1.0d0 2.0d0))))

(test iota.3
  (is (equal (iota 3 :start 2 :step 3.0)
             '(2.0 5.0 8.0))))

(test map-iota.1
  (is (equal (let (all)
               (declare (notinline map-iota))
               (list (map-iota (lambda (x) (push x all))
                               3
                               :start 2
                               :step 1.1d0)
                     all))
             '(3 (4.2d0 3.1d0 2.0d0)))))

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

;;;; Arrays

#+nil
(test array-index.type)

#+nil
(test copy-array)

;;;; Sequences

(test rotate.1
  (is (equal (list (rotate (list 1 2 3) 0)
                   (rotate (list 1 2 3) 1)
                   (rotate (list 1 2 3) 2)
                   (rotate (list 1 2 3) 3)
                   (rotate (list 1 2 3) 4))
             '((1 2 3)
               (3 1 2)
               (2 3 1)
               (1 2 3)
               (3 1 2)))))


(test rotate.2
  (is (eql #(1 2 3 4) (rotate (vector 1 2 3 4) 0))
      (eql #(4 1 2 3) (rotate (vector 1 2 3 4)))
      (eql #(3 4 1 2) (rotate (vector 1 2 3 4) 2))
      (eql #(2 3 4 1) (rotate (vector 1 2 3 4) 3))
      (eql #(1 2 3 4) (rotate (vector 1 2 3 4) 4))
      (eql #(4 1 2 3) (rotate (vector 1 2 3 4) 5))))

(test rotate.3
  (is (equal (list (rotate (list 1 2 3) 0)
                   (rotate (list 1 2 3) -1)
                   (rotate (list 1 2 3) -2)
                   (rotate (list 1 2 3) -3)
                   (rotate (list 1 2 3) -4))
             '((1 2 3)
               (2 3 1)
               (3 1 2)
               (1 2 3)
               (2 3 1)))))

(test rotate.4
  (is (eql #(1 2 3 4) (rotate (vector 1 2 3 4) 0))
      (eql #(2 3 4 1) (rotate (vector 1 2 3 4) -1))
      (eql #(3 4 1 2) (rotate (vector 1 2 3 4) -2))
      (eql #(4 1 2 3) (rotate (vector 1 2 3 4) -3))
      (eql #(1 2 3 4) (rotate (vector 1 2 3 4) -4))
      (eql #(2 3 4 1) (rotate (vector 1 2 3 4) -5))))

(test rotate.5
  (is (equal (list (rotate (list 1) 17)
                   (rotate (list 1) -5))
             '((1) (1)))))

(test shuffle.1
  (is (equal (let ((s (shuffle (iota 100))))
               (list (equal s (iota 100))
                     (every (lambda (x)
                              (member x s))
                            (iota 100))
                     (every (lambda (x)
                              (typep x '(integer 0 99)))
                            s)))
             '(nil t t))))

(test shuffle.2
  (is (equal (let ((s (shuffle (coerce (iota 100) 'vector))))
               (list (equal s (coerce (iota 100) 'vector))
                     (every (lambda (x)
                              (find x s))
                            (iota 100))
                     (every (lambda (x)
                              (typep x '(integer 0 99)))
                            s)))
             '(nil t t))))

(test random-elt.1
  (is (equal (let ((s1 #(1 2 3 4))
                   (s2 '(1 2 3 4)))
               (list (dotimes (i 1000 nil)
                       (unless (member (random-elt s1) s2)
                         (return nil))
                       (when (/= (random-elt s1) (random-elt s1))
                         (return t)))
                     (dotimes (i 1000 nil)
                       (unless (member (random-elt s2) s2)
                         (return nil))
                       (when (/= (random-elt s2) (random-elt s2))
                         (return t)))))
             '(t t))))

(test removef.1
  (is (equalp (let* ((x '(1 2 3))
                    (x* x)
                    (y #(1 2 3))
                    (y* y))
               (removef x 1)
               (removef y 3)
               (list x x* y y*))
              '((2 3) (1 2 3) #(1 2) #(1 2 3)))))

(test deletef.1
  (is (equalp (let* ((x (list 1 2 3))
                    (x* x)
                    (y (vector 1 2 3)))
               (deletef x 2)
               (deletef y 1)
               (list x x* y))
             '((1 3) (1 3) #(2 3)))))

(test map-permutations.1
  (is (equal (let ((seq (list 1 2 3))
                   (seen nil)
                   (ok t))
               (map-permutations (lambda (s)
                                   (unless (set-equal s seq)
                                     (setf ok nil))
                                   (when (member s seen :test 'equal)
                                     (setf ok nil))
                                   (push s seen))
                                 seq
                                 :copy t)
               (list ok (length seen)))
             '(t 6))))

(test proper-sequence.type.1
  (is (equal (mapcar (lambda (x)
                       (typep x 'proper-sequence))
                     (list (list 1 2 3)
                           (vector 1 2 3)
                           #2a((1 2) (3 4))
                           (circular-list 1 2 3 4)))
             '(t t nil nil))))

(test emptyp.1
  (is (equal (mapcar #'emptyp
                     (list (list 1)
                           (circular-list 1)
                           nil
                           (vector)
                           (vector 1)))
             '(nil nil t t nil))))

(test sequence-of-length-p.1
  (is (equal (mapcar #'sequence-of-length-p
                     (list nil
                           #()
                           (list 1)
                           (vector 1)
                           (list 1 2)
                           (vector 1 2)
                           (list 1 2)
                           (vector 1 2)
                           (list 1 2)
                           (vector 1 2))
                     (list 0
                           0
                           1
                           1
                           2
                           2
                           1
                           1
                           4
                           4))
             '(t t t t t t nil nil nil nil))))

(test length=.1
  (is (equal (mapcar #'length=
                     (list nil
                           #()
                           (list 1)
                           (vector 1)
                           (list 1 2)
                           (vector 1 2)
                           (list 1 2)
                           (vector 1 2)
                           (list 1 2)
                           (vector 1 2))
                     (list 0
                           0
                           1
                           1
                           2
                           2
                           1
                           1
                           4
                           4))
             '(t t t t t t nil nil nil nil))))

(test length=.2
  ;; test the compiler macro
  (is (equal (macrolet ((x (&rest args)
                          (funcall
                           (compile nil
                                    `(lambda ()
                                       (length= ,@args))))))
               (list (x 2 '(1 2))
                     (x '(1 2) '(3 4))
                     (x '(1 2) 2)
                     (x '(1 2) 2 '(3 4))
                     (x 1 2 3)))
             '(t t t t nil))))

(test copy-sequence.1
  (is (equal (let ((l (list 1 2 3))
                   (v (vector #\a #\b #\c)))
               (declare (notinline copy-sequence))
               (let ((l.list (copy-sequence 'list l))
                     (l.vector (copy-sequence 'vector l))
                     (l.spec-v (copy-sequence '(vector fixnum) l))
                     (v.vector (copy-sequence 'vector v))
                     (v.list (copy-sequence 'list v))
                     (v.string (copy-sequence 'string v)))
                 (list (member l (list l.list l.vector l.spec-v))
                       (member v (list v.vector v.list v.string))
                       (equal l.list l)
                       (equalp l.vector #(1 2 3))
                       (eq 'fixnum (array-element-type l.spec-v))
                       (equalp v.vector v)
                       (equal v.list '(#\a #\b #\c))
                       (equal "abc" v.string))))
             '(nil nil t t t t t t))))

(test first-elt.1
  (is (equal (mapcar #'first-elt
                     (list (list 1 2 3)
                           "abc"
                           (vector :a :b :c)))
             '(1 #\a :a))))

(test first-elt.error.1
  (is (equal (mapcar (lambda (x)
                       (handler-case
                           (first-elt x)
                         (type-error ()
                           :type-error)))
                     (list nil
                           #()
                           12
                           :zot))
             '(:type-error
               :type-error
               :type-error
               :type-error))))

(test setf-first-elt.1
  (is (equalp (let ((l (list 1 2 3))
                   (s (copy-seq "foobar"))
                   (v (vector :a :b :c)))
               (setf (first-elt l) -1
                     (first-elt s) #\x
                     (first-elt v) 'zot)
               (list l s v))
             '((-1 2 3) "xoobar" #(zot :b :c)))))

(test setf-first-elt.error.1
  (is (eql (let ((l 'foo))
             (multiple-value-bind (res err)
                 (ignore-errors (setf (first-elt l) 4))
               (typep err 'type-error))))))

(test last-elt.1
  (is (equal (mapcar #'last-elt
                     (list (list 1 2 3)
                           (vector :a :b :c)
                           "FOOBAR"
                           #*001
                           #*010))
             '(3 :c #\R 1 0))))

(test last-elt.error.1
  (is (equal (mapcar (lambda (x)
                       (handler-case
                           (last-elt x)
                         (type-error ()
                           :type-error)))
                     (list nil
                           #()
                           12
                           :zot
                           (circular-list 1 2 3)
                           (list* 1 2 3 (circular-list 4 5))))
             '(:type-error
               :type-error
               :type-error
               :type-error
               :type-error
               :type-error))))

(test setf-last-elt.1
  (is (equal (let ((l (list 1 2 3))
                   (s (copy-seq "foobar"))
                   (b (copy-seq #*010101001)))
               (setf (last-elt l) '???
                     (last-elt s) #\?
                     (last-elt b) 0)
               (list l s b))
             '((1 2 ???) "fooba?" #*010101000))))

(test setf-last-elt.error.1
  (is (eql (handler-case
               (setf (last-elt 'foo) 13)
             (type-error ()
               :type-error))
           :type-error)))

(test starts-with.1
  (is (equal (list (starts-with 1 '(1 2 3))
                   (starts-with 1 #(1 2 3))
                   (starts-with #\x "xyz")
                   (starts-with 2 '(1 2 3))
                   (starts-with 3 #(1 2 3))
                   (starts-with 1 1)
                   (starts-with nil nil))
             '(t t t nil nil nil nil))))

(test starts-with.2
  (is (equal (list (starts-with 1 '(-1 2 3) :key '-)
                   (starts-with "foo" '("foo" "bar") :test 'equal)
                   (starts-with "f" '(#\f) :key 'string :test 'equal)
                   (starts-with -1 '(0 1 2) :key #'1+)
                   (starts-with "zot" '("ZOT") :test 'equal))
             '(t t t nil nil))))

(test ends-with.1
  (is (equal (list (ends-with 3 '(1 2 3))
                   (ends-with 3 #(1 2 3))
                   (ends-with #\z "xyz")
                   (ends-with 2 '(1 2 3))
                   (ends-with 1 #(1 2 3))
                   (ends-with 1 1)
                   (ends-with nil nil))
             '(t t t nil nil nil nil))))

(test ends-with.2
  (is (equal (list (ends-with 2 '(0 13 1) :key '1+)
                   (ends-with "foo" (vector "bar" "foo") :test 'equal)
                   (ends-with "X" (vector 1 2 #\X) :key 'string :test 'equal)
                   (ends-with "foo" "foo" :test 'equal))
             '(t t t nil))))

(test ends-with.error.1
  (is (eql (handler-case
               (ends-with 3 (circular-list 3 3 3 1 3 3))
             (type-error ()
               :type-error))
           :type-error)))

(test with-unique-names.1
  (is (equal (let ((*gensym-counter* 0))
               (let ((syms (with-unique-names (foo bar quux)
                             (list foo bar quux))))
                 (list (find-if #'symbol-package syms)
                       (equal '("FOO0" "BAR1" "QUUX2")
                              (mapcar #'symbol-name syms)))))
             '(nil t))))

(test with-unique-names.2
  (is (equal (let ((*gensym-counter* 0))
               (let ((syms (with-unique-names ((foo "_foo_") (bar -bar-) (quux #\q))
                             (list foo bar quux))))
                 (list (find-if #'symbol-package syms)
                       (equal '("_foo_0" "-BAR-1" "q2")
                              (mapcar #'symbol-name syms)))))
             '(nil t))))

(test with-unique-names.3
  (is (let ((*gensym-counter* 0))
        (multiple-value-bind (res err)
            (ignore-errors
              (eval
               '(let ((syms
                       (with-unique-names ((foo "_foo_") (bar -bar-) (quux 42))
                         (list foo bar quux))))
                 (list (find-if #'symbol-package syms)
                  (equal '("_foo_0" "-BAR-1" "q2")
                   (mapcar #'symbol-name syms))))))
          (typep err 'error)))))

(test once-only.1
  (is (equal (macrolet ((cons1.good (x)
                          (once-only (x)
                            `(cons ,x ,x)))
                        (cons1.bad (x)
                          `(cons ,x ,x)))
               (let ((y 0))
                 (list (cons1.good (incf y))
                       y
                       (cons1.bad (incf y))
                       y)))
             '((1 . 1) 1 (2 . 3) 3))))

(test once-only.2
  (is (equal (macrolet ((cons1 (x)
                          (once-only ((y x))
                            `(cons ,y ,y))))
               (let ((z 0))
                 (list (cons1 (incf z))
                       z
                       (cons1 (incf z)))))
             '((1 . 1) 1 (2 . 2)))))

(test parse-body.1
  (is (equalp (parse-body '("doc" "body") :documentation t)
              '("body" nil "doc"))))

(test parse-body.2
  (is (equalp (parse-body '("body") :documentation t)
              '(("body") nil nil))))

(test parse-body.3
  (is (equalp (parse-body '("doc" "body"))
              '(("doc" "body") nil nil))))

(test parse-body.4
  (is (equalp (parse-body '((declare (foo)) "doc" (declare (bar)) body) :documentation t)
              (list (body) ((declare (foo)) (declare (bar))) "doc"))))

(test parse-body.5
  (is (equalp (parse-body '((declare (foo)) "doc" (declare (bar)) body))
              (list ("doc" (declare (bar)) body) ((declare (foo))) nil))))

(test parse-body.6
  (is (eql (multiple-value-bind (res err)
               (ignore-errors
                 (parse-body '("foo" "bar" "quux")
                             :documentation t))
             (typep err 'error)))))

;;;; Symbols

(test ensure-symbol.1
  (is (equal (ensure-symbol :cons :cl)
             '(cons :external))))

(test ensure-symbol.2
  (is (equal (ensure-symbol "CONS" :alexandria)
             '(cons :inherited))))

(test ensure-symbol.3
  (is (equal (ensure-symbol 'foo :keyword)
             '(:foo :external))))

(test ensure-symbol.4
  (is (equal (ensure-symbol #\* :alexandria)
             '(* :inherited))))

(test format-symbol.1
  (is (equal (let ((s (format-symbol nil "X-~D" 13)))
               (list (symbol-package s)
                     (symbol-name s)))
             '(nil "X-13"))))

(test format-symbol.2
  (is (eql (format-symbol :keyword "SYM-~A" :bolic)
           :sym-bolic)))

(test format-symbol.3
  (is (eql (let ((*package* (find-package :cl)))
             (format-symbol t "FIND-~A" 'package))
           find-package)))

(test make-keyword.1
  (is (equal (list (make-keyword 'zot)
                   (make-keyword "FOO")
                   (make-keyword #\Q))
             '(:zot :foo :q))))

(test make-gensym-list.1
  (is (equal (let ((*gensym-counter* 0))
               (let ((syms (make-gensym-list 3 "FOO")))
                 (list (find-if 'symbol-package syms)
                       (equal '("FOO0" "FOO1" "FOO2")
                              (mapcar 'symbol-name syms)))))
             '(nil t))))

(test make-gensym-list.2
  (is (equal (let ((*gensym-counter* 0))
               (let ((syms (make-gensym-list 3)))
                 (list (find-if 'symbol-package syms)
                       (equal '("G0" "G1" "G2")
                              (mapcar 'symbol-name syms)))))
             '(nil t))))

;;;; Type-system

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

;; (macrolet
;;     ((test (type numbers)
;;        `(test ,(format-symbol t "CDR5.~A" type)
;;           (let ((numbers ,numbers))
;;             (values (mapcar (of-type ',(format-symbol t "NEGATIVE-~A" type)) numbers)
;;                     (mapcar (of-type ',(format-symbol t "NON-POSITIVE-~A" type)) numbers)
;;                     (mapcar (of-type ',(format-symbol t "NON-NEGATIVE-~A" type)) numbers)
;;                     (mapcar (of-type ',(format-symbol t "POSITIVE-~A" type)) numbers)))
;; 	  (t t t nil nil nil nil)
;; 	  (t t t t nil nil nil)
;; 	  (nil nil nil t t t t)
;; 	  (nil nil nil nil t t t))))
;;   (test fixnum       (list most-negative-fixnum       -42      -1     0     1     42      most-positive-fixnum))
;;   (test integer      (list (1- most-negative-fixnum)  -42      -1     0     1     42      (1+ most-positive-fixnum)))
;;   (test rational     (list (1- most-negative-fixnum)  -42/13   -1     0     1     42/13   (1+ most-positive-fixnum)))
;;   (test real         (list most-negative-long-float   -42/13   -1     0     1     42/13   most-positive-long-float))
;;   (test float        (list most-negative-short-float  -42.02   -1.0   0.0   1.0   42.02   most-positive-short-float))
;;   (test short-float  (list most-negative-short-float  -42.02s0 -1.0s0 0.0s0 1.0s0 42.02s0 most-positive-short-float))
;;   (test single-float (list most-negative-single-float -42.02f0 -1.0f0 0.0f0 1.0f0 42.02f0 most-positive-single-float))
;;   (test double-float (list most-negative-double-float -42.02d0 -1.0d0 0.0d0 1.0d0 42.02d0 most-positive-double-float))
;;   (test long-float   (list most-negative-long-float   -42.02l0 -1.0l0 0.0l0 1.0l0 42.02l0 most-positive-long-float)))

;;;; Bindings

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

(test doplist.1
  (is (equal (let (keys values)
               (doplist (k v '(a 1 b 2 c 3) (values t (reverse keys) (reverse values) k v))
                 (push k keys)
                 (push v values)))
             '(t (a b c) (1 2 3) nil nil))))

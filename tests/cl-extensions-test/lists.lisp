;; do-range
;; delete-from-plist
;; unflatten
;; dolist*
;; partition
;; dotree
;; push*
;; append1
;; length=1
;; make-pairs
;; list-to-alist
;; transpose-list

(test range.1
  (is (equal (range 1 5) '(1 2 3 4 5))
      (equal (range 5) '(0 1 2 3 4 5))
      (equal (range 1 10 2 '(2 4 6 8 10)))))

(test map-range.1
  (is (equal (map-range #'1+ 1 5) '(2 3 4 5 6))))

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

(test alist-to-plist.1
  (is (equal (alist-to-plist '((a . 1) (b . 2) (c . 3)))
             '(a 1 b 2 c 3))))

(test plist-to-alist.1
  (is (equal (plist-to-alist '(a 1 b 2 c 3))
             '((a . 1) (b . 2) (c . 3)))))

(test doplist.1
  (is (equal (let (keys values)
               (doplist (k v '(a 1 b 2 c 3) (values t (reverse keys) (reverse values) k v))
                 (push k keys)
                 (push v values)))
             '(t (a b c) (1 2 3) nil nil))))

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

(test ensure-car.1
  (is (eql (ensure-car (cons a b)) 'a))
  (is (eql (ensure-car 'a) 'a)))

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

(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; starts-with-subseq
;; map-combinations
;; map-derangements
;; remove-keywords
;; remf-keywords
;; tail
;; but-tail
;; head
;; but-head
;; read-sequence*
;; levenshtein-distance
;; split-sequence
;; split-sequence-if
;; split-sequence-if-not

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
                     (list 0 0 1 1 2 2 1 1 4 4))
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
                     (list 0 0 1 1 2 2 1 1 4 4))
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

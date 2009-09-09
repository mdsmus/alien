(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; parse-ordinary-lambda-list
;; rebinding
;; rebind

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

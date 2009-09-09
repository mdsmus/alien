(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; featurep
;; maybe-intern
;; symbolicate
;; intern-concat

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

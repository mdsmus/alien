(in-package :cl-extensions)

(defmacro defalias (function redefinition)
  `(eval-always
    (progn
      (setf (fdefinition ',redefinition) (function ,function))
      ',redefinition)))

(defmacro defvaralias (variable redefinition)
  `(eval-always
    (defvar ,redefinition ,variable)))

(defmacro defmacalias (macro redefinition)
  #-allegro
  (with-unique-names (args)
    `(eval-always
      (defmacro ,redefinition (&rest ,args)
        `(,',macro ,@,args))))
  #+allegro ;; with-unique-names is undefined in allegro, why? This is a quick fix.
  (let ((args (gensym)))
    `(eval-always
      (defmacro ,redefinition (&rest ,args)
        `(,',macro ,@,args)))))


(defmacalias lambda fun)

(defalias make-instance new)

;; conflicts with unittest
;;(setf (symbol-function '!) #'factorial)

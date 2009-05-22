(defmacro cons1 (x)
  (once-only (x) `(cons ,x ,x)))

(let ((y 0)) (cons1 (incf y))) => (1 . 1)

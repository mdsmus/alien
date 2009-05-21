(setf r (make-reducer #'+ 5))
(funcall r 0) => 5
(funcall r 1 2) => 8
(funcall r) => 8

(defun map-range (lambda min max &optional (step 1))
  (loop for i from min upto max by step
        collect (funcall lambda i)))

(defmacro do-range ((index &optional min max step return-value)
                    &body body)
  (assert (or min max)
          (min max)
          "Must specify at least MIN or MAX")
  `(loop for ,index ,@(when min `(from ,min))
         ,@(when max `(upto ,max))
         ,@(when step `(by ,step))
         do (progn ,@body)
         finally (return ,return-value)))

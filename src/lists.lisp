(defun %range (start end step)
  "Builds a range of numbers from a to b."
  (assert (and (<= start end) (plusp step)))
  (loop for x from start to end by step collect x))

(defun range (a1 &optional a2 (step 1))
  "Return a list of n numbers, starting from START (with numeric
contagion from STEP applied), each consequtive number being the sum of
the previous one and STEP. START defaults to 0 and STEP to 1."
  (if a2
      (%range a1 a2 step)
      (%range 0 a1 step)))

(defun map-range (lambda start end &optional (step 1))
  (loop for i from start upto end by step
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

(defun iota (n &key (start 0) (step 1))
  "Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1."
  (declare (type (integer 0) n) (number start step))
  (loop repeat n
     ;; KLUDGE: get numeric contagion right for the first element too
     for i = (+ start (- step step)) then (+ i step)
     collect i))

(defun map-iota (function n &key (start 0) (step 1))
  "Calls FUNCTION with N numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1. Returns N."
  (declare (type (integer 0) n) (number start step))
  (loop repeat n
        ;; KLUDGE: get numeric contagion right for the first element too
        for i = (+ start (- step step)) then (+ i step)
        do (funcall function i))
  n)

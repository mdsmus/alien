(unwind-protect-case ()
    (protected-form)
  (:normal (format t "This is only evaluated if PROTECTED-FORM executed normally.~%"))
  (:abort  (format t "This is only evaluated if PROTECTED-FORM aborted preemptively.~%"))
  (:always (format t "This is evaluated in either case.~%")))

(unwind-protect-case (aborted-p)
    (protected-form)
  (:always (perform-cleanup-if aborted-p)))

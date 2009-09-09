(test required-argument.1
  (is (multiple-value-bind (res err)
          (ignore-errors (required-argument))
        (typep err 'error))))

(test unwind-protect-case.1
  (is (equal (let (result)
             (unwind-protect-case ()
                 (random 10)
               (:normal (push :normal result))
               (:abort  (push :abort result))
               (:always (push :always result)))
             result)
           '(:always :normal))))

(test unwind-protect-case.2
  (is (equal (let (result)
             (unwind-protect-case ()
                 (random 10)
               (:always (push :always result))
               (:normal (push :normal result))
               (:abort  (push :abort result)))
             result)
             '(:normal :always))))

(test unwind-protect-case.3
  (is (equal '(:always :abort)
             (let (result1)
               (ignore-errors
                 (unwind-protect-case ()
                    (error "FOOF!")
                  (:normal (push :normal result1))
                  (:abort  (push :abort result1))
                  (:always (push :always result1)))))))
  (is (equal '(:always :abort)
             (let (result2)
               (catch 'foof
                (unwind-protect-case ()
                    (throw 'foof 42)
                  (:normal (push :normal result2))
                  (:abort  (push :abort result2))
                  (:always (push :always result2)))))))
  (is (equal '(:always :abort)
             (let (result3)
               (block foof
                (unwind-protect-case ()
                    (return-from foof 42)
                  (:normal (push :normal result3))
                  (:abort  (push :abort result3))
                  (:always (push :always result3))))))))

(test unwind-protect-case.4
  (is (eql (let (result)
             (unwind-protect-case (aborted-p)
                 (random 42)
               (:always (setq result aborted-p)))
             result)
           nil)))

(test unwind-protect-case.5
  (is (eql (let (result)
             (block foof
               (unwind-protect-case (aborted-p)
                   (return-from foof)
                 (:always (setq result aborted-p))))
             result))))

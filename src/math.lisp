(defun 10^ (n)
  (expt 10 n))

(defun parse-float (float-string
                    &key (start 0) (end nil) (radix 10)
                         (junk-allowed t)
                         (type 'single-float)
                         (decimal-character #\.))
  (let ((radix-array (radix-values radix))
        (integer-part 0)
        (mantissa 0)
        (mantissa-size 1)
        (sign 1))
    (with-input-from-string (float-stream (string-upcase (string-trim '(#\Space #\Tab) float-string)) :start start :end end)
      (labels ((peek () (peek-char nil float-stream nil nil nil))
               (next () (read-char float-stream nil nil nil))
               (sign () ;; reads the (optional) sign of the number
                 (cond
                   ((char= (peek) #\+) (next) (setf sign 1))
                   ((char= (peek) #\-) (next) (setf sign -1)))
                 (integer-part))
               (integer-part ()
                 (cond
                   ((position (peek) radix-array)
                    ;; the next char is a valid char
                    (setf integer-part (+ (* integer-part radix)
                                          (position (next) radix-array)))
                    ;; again
                    (return-from integer-part (integer-part)))
                   ((null (peek))
                    ;; end of string
                    (done))
                   ((char= decimal-character (peek))
                    ;; the decimal seperator
                    (next)
                    (return-from integer-part (mantissa)))                   
                   ;; junk
                   (junk-allowed (done))
                   (t (bad-string))))
               (mantissa ()                 
                 (cond
                   ((position (peek) radix-array)
                    (setf mantissa (+ (* mantissa radix)
                                      (position (next) radix-array))
                          mantissa-size (* mantissa-size radix))
                    (return-from mantissa
                      (mantissa)))
                   ((or (null (peek)) junk-allowed)
                    ;; end of string
                    (done))
                   (t (bad-string))))
               (bad-string ()
                 (error "Unable to parse ~S." float-string))
               (done ()
                 (return-from parse-float
                   (coerce (* sign (+ integer-part (/ mantissa mantissa-size))) type))))
        (sign)))))

(define-modify-macro mulf (B)
  *
  "SETF NUM to the result of (* NUM B).")

(define-modify-macro divf (B)
  /
  "SETF NUM to the result of (/ NUM B).")

(defun do-minf (current other)
  (if (< other current)
      other
      current))

(define-modify-macro minf (other)
  do-minf
  "Sets the place to new-value if new-value is #'< the current value")

(defun do-maxf (current other)
  (if (> other current)
      other
      current))

(define-modify-macro maxf (other)
  do-maxf
  "Sets the place to new-value if new-value is #'> the current value")

(defun clamp (number min max)
  "Clamps the NUMBER into [MIN, MAX] range. Returns MIN if NUMBER lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (cond ((< number min) min)
        ((> number max) max)
        (t number)))

(defun lerp (v a b)
  "Returns the result of linear interpolation between A and B, using the
interpolation coefficient V."
   (+ a (* v (- b a))))

(defun mean (sample)
  "Returns the mean of SAMPLE. SAMPLE must be a sequence of numbers."
  (/ (reduce #'+ sample) (length sample)))

(defun median (sample)
  "Returns median of SAMPLE. SAMPLE must be a sequence of real numbers."
  (let* ((vector (sort (copy-sequence 'vector sample) #'<))
         (length (length vector))
         (middle (truncate length 2)))
    (if (oddp length)
        (aref vector middle)
        (/ (+ (aref vector middle) (aref vector (1+ middle))) 2))))

(defun variance (sample &key (biased t))
  "Variance of SAMPLE. Returns the biased variance if BIASED is true (the default),
and the unbiased estimator of variance if BIASED is false. SAMPLE must be a
sequence of numbers."
  (let ((mean (mean sample)))
    (/ (reduce (lambda (a b)
                 (+ a (expt (- b mean) 2)))
               sample
               :initial-value 0)
       (- (length sample) (if biased 0 1)))))

(defun standard-deviation (sample &key (biased t))
  "Standard deviation of SAMPLE. Returns the biased standard deviation if
BIASED is true (the default), and the square root of the unbiased estimator
for variance if BIASED is false (which is not the same as the unbiased
estimator for standard deviation). SAMPLE must be a sequence of numbers."
  (sqrt (variance sample :biased biased)))

;;; KLUDGE: This is really platform dependant: ideally we would use
;;; (load-time-value (find-good-direct-multiplication-limit)) instead.
(defconstant +factorial-direct-multiplication-limit+ 13)

(defun %multiply-range (i j)
  ;; We use a a bit of cleverness here:
  ;;
  ;; 1. For large factorials we bisect in order to avoid expensive bignum
  ;;    multiplications: 1 x 2 x 3 x ... runs into bignums pretty soon,
  ;;    and once it does that all further multiplications will be with bignums.
  ;;
  ;;    By instead doing the multiplication in a tree like
  ;;       ((1 x 2) x (3 x 4)) x ((5 x 6) x (7 x 8))
  ;;    we manage to get less bignums.
  ;;
  ;; 2. Division isn't exactly free either, however, so we don't bisect
  ;;    all the way down, but multiply ranges of integers close to each
  ;;    other directly.
  ;;
  ;; For even better results it should be possible to use prime
  ;; factorization magic, but Nikodemus ran out of steam.
  ;;
  ;; KLUDGE: We support factorials of bignums, but it seems quite
  ;; unlikely anyone would ever be able to use them on a modern lisp,
  ;; since the resulting numbers are unlikely to fit in memory... but
  ;; it would be extremely unelegant to define FACTORIAL only on
  ;; fixnums, _and_ on lisps with 16 bit fixnums this can actually be
  ;; needed.
  (labels ((bisect (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (if (< (- k j) +factorial-bisection-range-limit+)
                 (multiply-range j k)
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (bisect j middle)
                      (bisect (+ middle 1) k)))))
           (bisect-big (j k)
             (declare (type (integer 1) j k))
             (if (= j k)
                 j
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (if (<= middle most-positive-fixnum)
                          (bisect j middle)
                          (bisect-big j middle))
                      (bisect-big (+ middle 1) k)))))
           (multiply-range (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (do ((f k (* f m))
                  (m (1- k) (1- m)))
                 ((< m j) f)
               (declare (type (integer 0 (#.most-positive-fixnum)) m)
                        (type unsigned-byte f)))))
    (bisect i j)))

(declaim (inline factorial))
(defun %factorial (n)
  (if (< n 2)
      1
      (%multiply-range 1 n)))

(defun factorial (n)
  "Factorial of non-negative integer N."
  (check-type n (integer 0))
  (%factorial n))

(setf (symbol-function '!) #'factorial)

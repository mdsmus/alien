(in-package :cl-extensions)

(defun 10^ (n)
  (expt 10 n))

(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))  
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))

(defun radix-values (radix)
  (assert (<= 2 radix 35)
          (radix)
          "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
  (make-array radix
              :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :displaced-index-offset 0
              :element-type 
              #+lispworks 'base-char
              #-lispworks 'character))

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

(define-modify-macro maxf (&rest numbers) max
  "Modify-macro for MAX. Sets place designated by the first argument to the
maximum of its original value and NUMBERS.")

(define-modify-macro minf (&rest numbers) min
  "Modify-macro for MIN. Sets place designated by the first argument to the
minimum of its original value and NUMBERS.")

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

;;; KLUDGE: This is really dependant on the numbers in question: for
;;; small numbers this is larger, and vice versa. Ideally instead of a
;;; constant we would have RANGE-FAST-TO-MULTIPLY-DIRECTLY-P.
(defconstant +factorial-bisection-range-limit+ 8)

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

(defun gaussian-random (&optional min max)
  "Returns two gaussian random double floats as the primary and secondary value,
optionally constrained by MIN and MAX. Gaussian random numbers form a standard
normal distribution around 0.0d0."
  (labels ((gauss ()
             (loop
                for x1 = (- (random 2.0d0) 1.0d0)
                for x2 = (- (random 2.0d0) 1.0d0)
                for w = (+ (expt x1 2) (expt x2 2))
                when (< w 1.0d0)
                do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                     (return (values (* x1 v) (* x2 v))))))
           (guard (x min max)
             (unless (<= min x max)
               (tagbody
                :retry
                  (multiple-value-bind (x1 x2) (gauss)
                    (when (<= min x1 max)
                      (setf x x1)
                      (go :done))
                    (when (<= min x2 max)
                      (setf x x2)
                      (go :done))
                    (go :retry))
                :done))
             x))
    (multiple-value-bind (g1 g2) (gauss)
      (values (guard g1 (or min g1) (or max g1))
              (guard g2 (or min g2) (or max g2))))))

(defun binomial-coefficient (n k)
  "Binomial coefficient of N and K, also expressed as N choose K. This is the
number of K element combinations given N choises. N must be equal to or
greater then K."
  (check-type n (integer 0))
  (check-type k (integer 0))
  (assert (>= n k))
  (if (or (zerop k) (= n k))
      1
      (let ((n-k (- n k)))
        (if (= 1 n-k)
            n
            ;; General case, avoid computing the 1x...xK twice:
            ;;
            ;;    N!           1x...xN          (K+1)x...xN
            ;; --------  =  ---------------- =  ------------, N>1
            ;; K!(N-K)!     1x...xK x (N-K)!       (N-K)!
            (/ (%multiply-range (+ k 1) n)
               (%factorial n-k))))))

(defun subfactorial (n)
  "Subfactorial of the non-negative integer N."
  (check-type n (integer 0))
  (case n
    (0 1)
    (1 0)
    (otherwise
     (floor (/ (+ 1 (factorial n)) (exp 1))))))

(defun count-permutations (n &optional (k n))
  "Number of K element permutations for a sequence of N objects.
R defaults to N"
  ;; FIXME: Use %multiply-range and take care of 1 and 2, plus
  ;; check types.
  (/ (factorial n)
     (factorial (- n k))))

(define-condition invalid-number ()
  ((value :reader value
	  :initarg :value
	  :initform nil)
   (reason :reader reason
	   :initarg :reason
	   :initform "Not specified"))
  (:report (lambda (c s)
	     (format s "Invalid number: ~S [Reason: ~A]"
		     (value c) (reason c)))))

(declaim (inline parse-integer-and-places))
(defun parse-integer-and-places (string start end &key (radix 10))
  #+optimizations
  (declare (optimize (speed 3))
	   (type simple-base-string string)
	   (type fixnum start end radix))
  (multiple-value-bind (integer end-pos)
      (if (= start end)
	  (values 0 0)
	  (parse-integer string
			 :start start
			 :end end
			 :radix radix))
    (cons integer (- end-pos start))))
		 
(defun parse-integers (string start end splitting-points &key (radix 10))
  #+optimizations
  (declare (optimize (speed 3))
	   (type simple-base-string string)
	   (type fixnum start end radix))
  (values-list (loop for left = start then (1+ right)
		     for point in splitting-points
		     for right = point
		     collect (parse-integer-and-places string
						       left
						       right
						       :radix radix)
		     into integers
		     finally (return
			       (nconc integers
				      (list
				       (parse-integer-and-places string
								 left
								 end
								 :radix radix
								 )))))))

(declaim (inline number-value places))
(defun number-value (x) (car x))
(defun places (x) (cdr x))

(declaim (type cons *white-space-characters*))
(defparameter *white-space-characters*
  '(#\Space #\Tab #\Return #\Linefeed))

(declaim (inline white-space-p))
(defun white-space-p (x)
  #+optimizations
  (declare (optimize (speed 3) (safety 0))
	   (type character x))
  (and (find x *white-space-characters*) t))

;; Numbers which could've been parsed, but intentionally crippled not to:
;; #xFF.AA
;; #o12e3

;; Numbers which CL doesn't parse, but this does:
;; #10r3.2
;; #2r  11

(defun parse-number (string &key (start 0) (end nil) (radix 10))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec."
  (flet ((invalid-number (reason)
	   (error 'invalid-number
		  :value (subseq string start end)
		  :reason reason)))
    (let ((end (or end (length string))))
      (if (and (eql (char string start) #\#)
	       (member (char string (1+ start)) '(#\C #\c)))
	  (let ((\(-pos (position #\( string :start start :end end))
		(\)-pos (position #\) string :start start :end end)))
	    (when (or (not \(-pos)
		      (not \)-pos)
		      (position #\( string :start (1+ \(-pos) :end end)
		      (position #\) string :start (1+ \)-pos) :end end))
	      (invalid-number "Mismatched/missing parenthesis"))
	    (let ((real-pos (position-if-not #'white-space-p string
					     :start (1+ \(-pos) :end \)-pos)))
	      (unless real-pos
		(invalid-number "Missing real part"))
	      (let ((delimiting-space (position-if #'white-space-p string
						   :start (1+ real-pos)
						   :end \)-pos)))
		(unless delimiting-space
		  (invalid-number "Missing imaginary part"))
		(let ((img-pos (position-if-not #'white-space-p string
						:start (1+ delimiting-space)
						:end \)-pos)))
		  (unless img-pos
		    (invalid-number "Missing imaginary part"))
		  (let ((img-end-pos (position-if #'white-space-p string
						  :start (1+ img-pos)
						  :end \)-pos)))
		    (complex (parse-real-number string
						:start real-pos
						:end delimiting-space
						:radix radix)
			     (parse-real-number string
						:start img-pos
						:end (or img-end-pos
							 \)-pos)
						:radix radix)))))))
	  (parse-real-number string :start start :end end :radix radix)))))

(defun parse-real-number (string &key (start 0) (end nil) (radix 10))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers."
  (let ((end (or end (length string))))
    (case (char string start)
      ((#\-)
       (* -1 (parse-positive-real-number string
					 :start (1+ start)
					 :end end
					 :radix radix)))
      ((#\#)
       (case (char string (1+ start))
	 ((#\x #\X)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 16))
	 ((#\b #\B)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 2))
	 ((#\o #\O)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 8))
	 (t (if (digit-char-p (char string (1+ start)))
		(let ((r-pos (position #\r string
				       :start (1+ start)
				       :end end
				       :key #'char-downcase)))
		  (unless r-pos
		    (error 'invalid-number
			   :value (subseq string start end)
			   :reason "Missing R in #radixR"))
		  (parse-real-number string
				     :start (1+ r-pos)
				     :end end
				     :radix (parse-integer string
							   :start (1+ start)
							   :end r-pos)))))))
      (t (parse-positive-real-number string
				     :start start
				     :end end
				     :radix radix)))))

(defun parse-positive-real-number (string &key (start 0) (end nil) (radix 10))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers and negative numbers."
  (let ((end (or end (length string)))
	(first-char (char string start)))
    (flet ((invalid-number (reason)
	     (error 'invalid-number
		    :value (subseq string start end)
		    :reason reason))
	   (base-for-exponent-marker (char)
	     (case char
	       ((#\d #\D)
		10.0d0)
	       ((#\e #\E)
		10)
	       ((#\s #\S)
		10.0s0)
	       ((#\l #\L)
		10.0l0)
	       ((#\f #\F)
		10.0f0))))
      (case first-char
	((#\-)
	 (invalid-number "Invalid usage of -"))
	((#\/)
	 (invalid-number "/ at beginning of number"))
	((#\d #\D #\e #\E #\l #\L #\f #\F #\s #\S)
	 (when (= radix 10)
	   (invalid-number "Exponent-marker at beginning of number"))))
      (let (/-pos .-pos exp-pos exp-marker)
	(loop for index from start below end
	      for char = (char string index)
	      do (case char
		   ((#\/)
		    (if /-pos
			(invalid-number "Multiple /'s in number")
			(setf /-pos index)))
		   ((#\.)
		    (if .-pos
			(invalid-number "Multiple .'s in number")
			(setf .-pos index)))
		   ((#\e #\E #\f #\F #\s #\S #\l #\L #\d #\D)
		    (when (= radix 10)
		      (when exp-pos
			(invalid-number
			 "Multiple exponent-markers in number"))
		      (setf exp-pos index)
		      (setf exp-marker (char-downcase char)))))
	      when (eql index (1- end))
	      do (case char
		   ((#\/)
		    (invalid-number "/ at end of number"))
		   ((#\d #\D #\e #\E #\s #\S #\l #\L #\f #\F)
		    (when (= radix 10)
		      (invalid-number "Exponent-marker at end of number")))))
	(cond ((and /-pos .-pos)
	       (invalid-number "Both . and / cannot be present simultaneously"))
	      ((and /-pos exp-pos)
	       (invalid-number "Both an exponent-marker and / cannot be present simultaneously"))
	      ((and .-pos exp-pos)
	       (if (< exp-pos .-pos)
		   (invalid-number "Exponent-markers must occur after . in number")
		   (if (/= radix 10)
		       (invalid-number "Only decimal numbers can contain exponent-markers or decimal points")
		       (multiple-value-bind (whole-place frac-place exp-place)
			   (parse-integers string start end
					   (list .-pos exp-pos)
					   :radix radix)
			 (* (+ (number-value whole-place)
			       (/ (number-value frac-place)
				  (expt radix
					(places frac-place))))
			    (expt (base-for-exponent-marker exp-marker)
				  (number-value exp-place)))))))
	      (exp-pos
	       (if (/= radix 10)
		   (invalid-number "Only decimals can contain exponent-markers")
		   (multiple-value-bind (whole-place exp-place)
		       (parse-integers string start end
				       (list exp-pos)
				       :radix radix)
		     (* (number-value whole-place)
			(expt (base-for-exponent-marker exp-marker)
			      (number-value exp-place))))))
	      (/-pos
	       (multiple-value-bind (numerator denominator)
		   (parse-integers string start end
				   (list /-pos)
				   :radix radix)
		 (if (>= (number-value denominator) 0)
		     (/ (number-value numerator)
			(number-value denominator))
		     (invalid-number "Misplaced - sign"))))
	      (.-pos
	       (if (/= radix 10)
		   (invalid-number "Only decimal numbers can contain decimal points")
		   (multiple-value-bind (whole-part frac-part)
		       (parse-integers string start end
				       (list .-pos)
				       :radix radix)
		     (if (>= (number-value frac-part) 0)
			 (+ (number-value whole-part)
			    (/ (number-value frac-part)
			       (expt 10.0 (places frac-part))))
			 (invalid-number "Misplaced - sign")))))
	      (t
	       (values (parse-integer string
				      :start start
				      :end end
				      :radix radix))))))))

(defun square (x) (* x x))

(defun sum (numbers &optional (key #'identity))
  "Add up all the numbers; if KEY is given, apply it to each number first."
  (if (null numbers)
      0
      (+ (funcall key (first numbers)) (sum (rest numbers) key))))

(defun between (x y z)
  "Predicate; return t iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(defun dot-product (l1 l2 &aux (sum 0)) ;;; dot product of two lists
  (mapc #'(lambda (x1 x2) (incf sum (* x1 x2))) l1 l2)
  sum)

(defun random-integer (from to)
  "Return an integer chosen at random from the given interval."
  (+ from (random (+ 1 (- to from)))))


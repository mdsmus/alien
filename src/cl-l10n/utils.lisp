;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

;;  Macros
;;;;;;;;;;;

;; dont worry it's nothing like if*
(defmacro or* (&rest vals)
  "(or* (string= foo a b) (char= foo b)) == 
  (or (string= foo a) (string= foo b) (char= foo b))"
  `(or ,@(mappend #'(lambda (x)
                      (destructuring-bind (test val &rest args) x
                        (if (singlep args)
                            `((,test ,val ,@args))
                            (mapcar #'(lambda (y) 
                                        `(,test ,val ,y))
                                    args))))
                  vals)))


;; Functions
;;;;;;;;;;;;;;

(defun read-key->value-text-file-into-hashtable (file)
  (with-open-file (file-stream file :element-type '(unsigned-byte 8))
    (let ((stream (make-flexi-stream file-stream :external-format :utf-8)))
      (iter (with result = (make-hash-table :test #'equal))
            (for line in-stream stream :using (lambda (stream eof-error-p eof-value)
                                                (let ((result (read-line stream eof-error-p eof-value)))
                                                  (if (eq result eof-value)
                                                      eof-value
                                                      (trim result)))))
            (for line-number from 0)
            (when (or (zerop (length line))
                      (eql (aref line 0) #\;))
              (next-iteration))
            (for pieces = (split (load-time-value
                                  (create-scanner
                                   (concatenate 'string "[ |" (list #\Tab) "]+")))
                                 line))
            (for split-count = (length pieces))
            (when (> split-count 2)
              (warn "Syntax error at line ~A, too many pieces after split: ~A" line-number pieces))
            (for singular = (elt pieces 0))
            (for plural = (if (= split-count 1)
                              singular
                              (elt pieces 1)))
            (setf (gethash singular result) plural)
            (finally (return result))))))

(defun capitalize-first-letter (str)
  (if (and (> (length str) 0)
           (not (upper-case-p (elt str 0))))
      (capitalize-first-letter! (copy-seq str))
      str))

(defun capitalize-first-letter! (str)
  (setf (aref str 0) (char-upcase (aref str 0)))
  str)

(defun mappend (fn &rest lists)
  (apply #'append (apply #'mapcar fn lists)))

(defun required-arg (name)
  (error "~A is a required argument" name))

(defvar *whitespace* (list #\Space #\Tab))

(defun strcat-separated-by (separator &rest args)
  (iter (for el in args)
        (unless el
          (next-iteration))
        (unless (first-time-p)
          (collect separator into components))
        (collect el into components)
        (finally (return (apply #'strcat components)))))

(defun trim (string &optional (bag *whitespace*))
  (string-trim bag string))

(defun group (list n)
  (assert (> n 0))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if list (rec list nil) nil)))

(defun winner (test get seq)
  (if (null seq)
      nil
      (let* ((val (elt seq 0))
             (res (funcall get val)))
        (dolist (x (subseq seq 1) (values val res))
          (let ((call (funcall get x)))
            (when (funcall test call res)
              (setf res call
                    val x)))))))

(defun float-part (float)
  (if (zerop float)
      ""
      (multiple-value-call 'extract-float-part (flonum-to-digits float))))

(defun extract-float-part (dp-pos aft)
  (let ((length (length aft)))
    (if (> dp-pos length)
        ""
        (with-output-to-string (x)
          (cond ((minusp dp-pos)
                 (dotimes (z (abs dp-pos))
                   (princ 0 x))
                 (princ aft x))
                (t (princ (subseq aft dp-pos)
                          x)))))))

;; From sbcl sources (src/code/print.lisp)
(defconstant single-float-min-e
  (nth-value 1 (decode-float least-positive-single-float)))
(defconstant double-float-min-e
  (nth-value 1 (decode-float least-positive-double-float)))

(defun flonum-to-digits (v)
  (let ((print-base 10)                 ; B
        (float-radix 2)                 ; b
        (float-digits (float-digits v)) ; p
        (digit-characters "0123456789")
        (min-e
         (etypecase v
           (single-float single-float-min-e)
           (double-float double-float-min-e))))
    (multiple-value-bind (f e)
        (integer-decode-float v)
      (let ((high-ok (evenp f))
            (low-ok (evenp f))
            (result (make-array 50 :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
        (labels ((scale (r s m+ m-)
                   (do ((k 0 (1+ k))
                        (s s (* s print-base)))
                       ((not (or (> (+ r m+) s)
                                 (and high-ok (= (+ r m+) s))))
                        (do ((k k (1- k))
                             (r r (* r print-base))
                             (m+ m+ (* m+ print-base))
                             (m- m- (* m- print-base)))
                            ((not (or (< (* (+ r m+) print-base) s)
                                      (and (not high-ok) (= (* (+ r m+) print-base) s))))
                             (values k (generate r s m+ m-)))))))
                 (generate (r s m+ m-)
                   (let (d tc1 tc2)
                     (tagbody
                      loop
                       (setf (values d r) (truncate (* r print-base) s))
                       (setf m+ (* m+ print-base))
                       (setf m- (* m- print-base))
                       (setf tc1 (or (< r m-) (and low-ok (= r m-))))
                       (setf tc2 (or (> (+ r m+) s)
                                     (and high-ok (= (+ r m+) s))))
                       (when (or tc1 tc2)
                         (go end))
                       (vector-push-extend (char digit-characters d) result)
                       (go loop)
                      end
                       (let ((d (cond
                                  ((and (not tc1) tc2) (1+ d))
                                  ((and tc1 (not tc2)) d)
                                  (t    ; (and tc1 tc2)
                                   (if (< (* r 2) s) d (1+ d))))))
                         (vector-push-extend (char digit-characters d) result)
                         (return-from generate result))))))
          (if (>= e 0)
              (if (/= f (expt float-radix (1- float-digits)))
                  (let ((be (expt float-radix e)))
                    (scale (* f be 2) 2 be be))
                  (let* ((be (expt float-radix e))
                         (be1 (* be float-radix)))
                    (scale (* f be1 2) (* float-radix 2) be1 be)))
              (if (or (= e min-e) (/= f (expt float-radix (1- float-digits))))
                  (scale (* f 2) (* (expt float-radix (- e)) 2) 1 1)
                  (scale (* f float-radix 2)
                         (* (expt float-radix (- 1 e)) 2) float-radix 1))))))))

#+(or) 
(defun flonum-to-digits (v &optional position relativep)
  (let ((print-base 10) ; B
        (float-radix 2) ; b
        (float-digits (float-digits v)) ; p
        (digit-characters "0123456789")
        (min-e
         (etypecase v
           (single-float single-float-min-e)
           (double-float double-float-min-e))))
    (multiple-value-bind (f e)
        (integer-decode-float v)
      (let (;; FIXME: these even tests assume normal IEEE rounding
            ;; mode.  I wonder if we should cater for non-normal?
            (high-ok (evenp f))
            (low-ok (evenp f))
            (result (make-array 50 :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
        (labels ((scale (r s m+ m-)
                   (do ((k 0 (1+ k))
                        (s s (* s print-base)))
                       ((not (or (> (+ r m+) s)
                                 (and high-ok (= (+ r m+) s))))
                        (do ((k k (1- k))
                             (r r (* r print-base))
                             (m+ m+ (* m+ print-base))
                             (m- m- (* m- print-base)))
                            ((not (or (< (* (+ r m+) print-base) s)
                                      (and (not high-ok)
                                           (= (* (+ r m+) print-base) s))))
                             (values k (generate r s m+ m-)))))))
                 (generate (r s m+ m-)
                   (let (d tc1 tc2)
                     (tagbody
                      loop
                        (setf (values d r) (truncate (* r print-base) s))
                        (setf m+ (* m+ print-base))
                        (setf m- (* m- print-base))
                        (setf tc1 (or (< r m-) (and low-ok (= r m-))))
                        (setf tc2 (or (> (+ r m+) s)
                                      (and high-ok (= (+ r m+) s))))
                        (when (or tc1 tc2)
                          (go end))
                        (vector-push-extend (char digit-characters d) result)
                        (go loop)
                      end
                        (let ((d (cond
                                   ((and (not tc1) tc2) (1+ d))
                                   ((and tc1 (not tc2)) d)
                                   (t ; (and tc1 tc2)
                                    (if (< (* r 2) s) d (1+ d))))))
                          (vector-push-extend (char digit-characters d) result)
                          (return-from generate result)))))
                 (initialize ()
                   (let (r s m+ m-)
                     (if (>= e 0)
                         (let* ((be (expt float-radix e))
                                (be1 (* be float-radix)))
                           (if (/= f (expt float-radix (1- float-digits)))
                               (setf r (* f be 2)
                                     s 2
                                     m+ be
                                     m- be)
                               (setf r (* f be1 2)
                                     s (* float-radix 2)
                                     m+ be1
                                     m- be)))
                         (if (or (= e min-e)
                                 (/= f (expt float-radix (1- float-digits))))
                             (setf r (* f 2)
                                   s (* (expt float-radix (- e)) 2)
                                   m+ 1
                                   m- 1)
                             (setf r (* f float-radix 2)
                                   s (* (expt float-radix (- 1 e)) 2)
                                   m+ float-radix
                                   m- 1)))
                     (when position
                       (when relativep
                         (assert (> position 0))
                         (do ((k 0 (1+ k))
                              ;; running out of letters here
                              (l 1 (* l print-base)))
                             ((>= (* s l) (+ r m+))
                              ;; k is now \hat{k}
                              (if (< (+ r (* s (/ (expt print-base (- k position)) 2)))
                                     (* s (expt print-base k)))
                                  (setf position (- k position))
                                  (setf position (- k position 1))))))
                       (let ((low (max m- (/ (* s (expt print-base position)) 2)))
                             (high (max m+ (/ (* s (expt print-base position)) 2))))
                         (when (<= m- low)
                           (setf m- low)
                           (setf low-ok t))
                         (when (<= m+ high)
                           (setf m+ high)
                           (setf high-ok t))))
                     (values r s m+ m-))))
          (multiple-value-bind (r s m+ m-) (initialize)
            (scale r s m+ m-)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some duplicates copied from various other libs to lower the number of dependencies

(defmacro with-unique-names ((&rest bindings) &body body)
  "Evaluate BODY with BINDINGS bound to fresh unique symbols.

Syntax: WITH-UNIQUE-NAMES ( [ var | (var x) ]* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar (lambda (binding)
                   (check-type binding (or cons symbol))
                   (destructuring-bind (var &optional (prefix (symbol-name var)))
                       (if (consp binding) binding (list binding))
                     (check-type var symbol)
                     `(,var (gensym ,(concatenate 'string prefix "-")))))
                 bindings)
    ,@body))

(defmacro rebinding (bindings &body body)
  "Bind each var in BINDINGS to a gensym, bind the gensym to
var's value via a let, return BODY's value wrapped in this let.

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical
environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (car (if (consp binding) binding (list binding)))
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let* ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                               ,,@body))))))

(defmacro dotree ((name tree &optional ret-val) &body body)
  "Evaluate BODY with NAME bound to every element in TREE. Return RET-VAL."
  (with-unique-names (traverser list list-element)
    `(progn
      (labels ((,traverser (,list)
                 (dolist (,list-element ,list)
                   (if (consp ,list-element)
                       (,traverser ,list-element)
                       (let ((,name ,list-element))
                         ,@body)))))
        (,traverser ,tree)
        ,ret-val))))

(defun strcat (&rest items)
  "Returns a fresh string consisting of ITEMS concat'd together."
  (strcat* items))
  
(defun strcat* (string-designators)
  "Concatenate all the strings in STRING-DESIGNATORS."
  (with-output-to-string (strcat)
    (dotree (s string-designators)
      (when s (princ s strcat)))))

(defun singlep (list)
  (and (consp list) (not (cdr list))))

(defun concatenate-symbol (&rest args)
  "DWIM symbol concatenate, see CONCATENATE-SYMBOL* for details."
  (concatenate-symbol* args))

(defun concatenate-symbol* (symbol-parts &key when-exists)
  "DWIM symbol concatenate: SYMBOL-PARTS will be converted to string and be concatenated
to form the resulting symbol with one exception: when a package is encountered then
it is stored as the target package to use at intern. If there was no package
among the args then the symbol-package of the first symbol encountered will be
used. If there are neither packages nor symbols among the args then the result will
be interned into the current package at the time of calling."
  (let* ((package nil) ; by intention
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (part symbol-parts)
                           (typecase part
                             (string (write-string part str))
                             (package (setf package part))
                             (symbol (unless package
                                       (setf package (symbol-package part)))
                                     (write-string (symbol-name part) str))
                             (integer (write-string (princ-to-string part) str))
                             (character (write-char part) str)
                             (t (error "Cannot convert argument ~S to symbol" part))))))))
    (setf package (or package *package*))
    (when (find-symbol symbol-name package)
      (when when-exists
        (case when-exists
          ((:error :warn) (funcall (if (eq when-exists :error) 'error 'warn)
                                   "Symbol ~S already exists in package ~A" symbol-name package))
          (t (return-from concatenate-symbol* (funcall when-exists symbol-name package))))))
    (intern symbol-name package)))

(defmacro if-bind (var test &body then/else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  "Just like IF-BIND but the var is always IT."
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  "Just like when except VAR will be bound to the
  result of TEST in BODY."
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  "Just like when expect the symbol IT will be
  bound to the result of TEST in BODY."
  `(when-bind it ,test ,@body))

(defmacro cond-bind (var &body clauses)
  "Just like COND but VAR will be bound to the result of the
  condition in the clause when executing the body of the clause."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
          (progn ,@(if body body (list var)))
          (cond-bind ,var ,@others)))
      nil))

(defmacro acond (&rest clauses)
  "Just like cond-bind except the var is automatically IT."
  `(cond-bind it ,@clauses))

(defun getenv (var)
  #+allegro (sys:getenv var)
  #+clisp (ext:getenv var)
  #+cmu
  (cdr (assoc var ext:*environment-list* :test #'string=))
  #+lispworks (lw:environment-variable var)
  #+openmcl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)

  #-(or allegro clisp cmu lispworks openmcl openmcl sbcl)
  (error "Could not define `getenv'."))

;; EOF

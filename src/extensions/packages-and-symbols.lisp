(in-package :cl-extensions)

(defun featurep (feature-expression)
  "Returns T if the argument matches the state of the *FEATURES*
list and NIL if it does not. FEATURE-EXPRESSION can be any atom
or list acceptable to the reader macros #+ and #-."
  (etypecase feature-expression
    (symbol (not (null (member feature-expression *features*))))
    (cons (check-type (first feature-expression) symbol)
          (eswitch ((first feature-expression) :test 'string=)
            (:and (every #'featurep (rest feature-expression)))
            (:or  (some #'featurep (rest feature-expression)))
            (:not (assert (= 2 (length feature-expression)))
                  (not (featurep (second feature-expression))))))))

(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example: (ENSURE-SYMBOL :CONS :CL) => CL:CONS, :EXTERNAL"
  (intern (string name) package))

(defun maybe-intern (name package)
  (values
   (if package
       (intern name (if (eq t package) *package* package))
       (make-symbol name))))

(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to CONTROL as if by FORMAT, and
then creates a symbol named by that string. If PACKAGE is NIL, returns an
uninterned symbol, if package is T, returns a symbol interned in the current
package, and otherwise returns a symbol interned in the package designated by
PACKAGE."
  (maybe-intern (apply #'format nil control arguments) package))

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

(defun make-gensym (name)
  "If NAME is a non-negative integer, calls GENSYM using it. Otherwise NAME
must be a string designator, in which case calls GENSYM using the designated
string as the argument."
  (gensym (if (typep name '(integer 0))
              name
              (string name))))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))

(defun intern-concat (string-designators &optional (package *package*))
  (intern (with-output-to-string (symbol-name)
            (dolist (designator string-designators)
              (write-string (etypecase designator
                              (symbol (symbol-name designator))
                              (string designator))
                            symbol-name)))
          package))

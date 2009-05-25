(defmacro if-let (bindings &body (then-form &optional else-form))
    "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro when-let* (bindings &body forms)
  "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the WHEN-LET*.

Execution of WHEN-LET* stops immediately if any initial-form evaluates to NIL.
If all initial-forms evaluate to true, then FORMS are executed as an implicit
PROGN."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings forms)
               (if bindings
                   `((let (,(car bindings))
                       (when ,(caar bindings)
                         ,@(bind (cdr bindings) forms))))
                   forms)))
      `(let (,(car binding-list))
         (when ,(caar binding-list)
           ,@(bind (cdr binding-list) forms))))))

(defun extract-function-name (spec)
  "Useful for macros that want to mimic the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (setf key (extract-function-name key))
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                      ,value ',test)))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use of a default clause in ~S."
                                       whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms) clause
                              `((,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  "Evaluates first matching clause, returning its values, or evaluates and
returns the values of DEFAULT if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))

(defmacro whichever (&rest possibilities &environment env)
  "Evaluates exactly one of POSSIBILITIES, chosen at random."
  (setf possibilities (mapcar (lambda (p) (macroexpand p env)) possibilities))
  (if (every (lambda (p) (constantp p)) possibilities)
      `(svref (load-time-value (vector ,@possibilities)) (random ,(length possibilities)))
      (with-gensyms (function)
        `(let ((,function (lambda () ,(pop possibilities))))
           (declare (function ,function))
           ,@(let ((p 1))
               (mapcar (lambda (possibility)
                         `(when (zerop (random ,(incf p)))
                            (setf ,function (lambda () ,possibility))))
                       possibilities))
           (funcall ,function)))))

(defmacro xor (&rest datums)
  "Evaluates its argument one at a time, from left to right. If more then one
argument evaluates to a true value no further DATUMS are evaluated, and NIL is
returned as both primary and secondary value. If exactly one argument
evaluates to true, its value is returned as the primary value after all the
arguments have been evaluated, and T is returned as the secondary value. If no
arguments evaluate to true NIL is retuned as primary, and T as secondary
value."
  (with-gensyms (xor tmp true)
    `(let (,tmp ,true)
       (block ,xor
         ,@(mapcar (lambda (datum)
                     `(if (setf ,tmp ,datum)
                          (if ,true
                              (return-from ,xor (values nil nil))
                              (setf ,true ,tmp))))
                   datums)
         (return-from ,xor (values ,true t))))))

(defmacro nth-value-or (nth-value &body forms)
  "Evaluates FORM arguments one at a time, until the NTH-VALUE returned by one
of the forms is non-NIL. It then returns all the values returned by evaluating
that form. If none of the forms return a non-nil nth value, this form returns
NIL."
  (once-only (nth-value)
    (with-gensyms (values)
      `(let ((,values (multiple-value-list ,(first forms))))
         (if (nth ,nth-value ,values)
             (values-list ,values)
             ,(if (rest forms)
                  `(nth-value-or ,nth-value ,@(rest forms))
                  nil))))))

(defun %reevaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Try to redefine the variable as a constant."
                      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (restart-case
                    (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                  (ignore ()
                    :report "Retain the current value."
                    old)
                  (continue ()
                    :report "Try to redefine the constant."
                    new)))))))

(defmacro define-constant (name initial-value &key (test ''eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST is a
/function designator/ that defaults to EQL. If DOCUMENTATION is given, it
becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  `(defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
     ,@(when documentation `(,documentation))))

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

(defmacro aand (&rest forms)
  `(and-bind it ,@forms))

(defmacro and-bind (var &rest forms)
  (cond
    ((cdr forms)
     `(when-bind ,var ,(first forms)
        (and-bind ,var ,@(cdr forms))))
    (forms (first forms))
    (t 't)))

(defmacro if2-bind (var test &body then/else)
  "Anaphoric IF control structure for multiple values.

VAR (a symbol) will be bound to the primary value of TEST.  If
TEST's second value is true then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    (with-unique-names (bool)
      `(multiple-value-bind (,var ,bool) ,test
	 (if ,bool ,then ,else)))))

(defmacro aif2 (test then &optional else)
  "Just like IF-BIND but the var is always IT.

Very useful with functions like GETHASH."
  `(if2-bind it ,test ,then ,else))

(defmacro while* (test &body body)
  "Repeat BODY while TEST is true.

You may exit the loop with (RETURN-FROM WHILE)."
  `(block while
     (loop
	(if ,test
	    (progn ,@body)
	    (return-from while)))))

(defmacro awhile (test &body body)
  "Just like WHILE, but the result of TEST is bound to IT.

You may exit the loop with (RETURN-FROM AWHILE)."
  `(block awhile
     (loop
	(aif ,test
	     (progn ,@body)
	     (return-from awhile)))))

(defmacro until* (test &body body)
  "Repeat BODY until TEST is false.

You may exit the loop with (RETURN-FROM UNTIL)."
  `(block until
     (loop
	(if (not ,test)
	    (progn ,@body)
	    (return-from until)))))


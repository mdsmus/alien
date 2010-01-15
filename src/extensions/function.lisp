(in-package :cl-extensions)

(defun mostn (fn list)
  "Takes a function and a list and returns a list of all the elements
for which the function yields the highest score (along with the score
itself)."
  (when list
    (let ((max-value (funcall fn (first list)))
          (list-max nil))
      (dolist (el list)
        (cond ((> (funcall fn el) max-value)
               (setf max-value (funcall fn el))
               (setf list-max (list el)))
              ((= (funcall fn el) max-value)
               (push el list-max))))
      list-max)))

(declaim (inline ensure-function))	; to propagate return type.
(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(defun disjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning the primary value of the first
predicate that returns true, without calling the remaining predicates.
If none of the predicates returns true, NIL is returned."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate))
	(more-predicates (mapcar #'ensure-function more-predicates)))
    (lambda (&rest arguments)
      (or (apply predicate arguments)
	  (some (lambda (p)
		  (declare (type function p))
		  (apply p arguments))
		more-predicates)))))

(defun conjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning NIL if any of the predicates
returns false, without calling the remaining predicates. If none of the
predicates returns false, returns the primary value of the last predicate."
  (lambda (&rest arguments)
    (and (apply predicate arguments)
	 ;; Cannot simply use CL:EVERY because we want to return the
	 ;; non-NIL value of the last predicate if all succeed.
         (do ((tail (cdr more-predicates) (cdr tail))
              (head (car more-predicates) (car tail)))
             ((not tail)
              (apply head arguments))
           (unless (apply head arguments)
             (return nil))))))

(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(funcall f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(loop for f in funs for arg in args
		   collect `(,f (ensure-function ,arg)))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun multiple-value-compose (function &rest more-functions)
    "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
	    (let ((f (ensure-function f))
		  (g (ensure-function g)))
	      (lambda (&rest arguments)
		(declare (dynamic-extent arguments))
		(multiple-value-call f (apply g arguments)))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun curry (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      ;; Using M-V-C we don't need to append the arguments.
      (multiple-value-call fn (values-list arguments) (values-list more)))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY")))
    `(let ,(mapcar #'list curries arguments)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (lambda (&rest more)
         (apply ,function ,@curries more)))))

(defun rcurry (function &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn (values-list more) (values-list arguments)))))

(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

(defun make-reducer (function &optional (initial-value nil initial-value-p))
  "Create a function which, starting with INITIAL-VALUE, reduces
any other values into a single final value.

FUNCTION will be called with two values: the current value and
the new value, in that order. FUNCTION should return exactly one
value.

The reducing function can be called with n arguments which will
be applied to FUNCTION one after the other (left to right) and
will return the new value.

If the reducing function is called with no arguments it will
return the current value.

Example:

 (setf r (make-reducer #'+ 5))
 (funcall r 0) => 5
 (funcall r 1 2) => 8
 (funcall r) => 8"
  (let ((value initial-value))
    (lambda (&rest next)
      (when next
        ;; supplied a value, reduce
        (if initial-value-p
            ;; have a value to test against
            (dolist (n next)
              (setf value (funcall function value n)))
            ;; nothing to test againts yet
            (setf initial-value-p t
                  value next)))
      ;; didn't supply a value, return the current value
      value)))

(defmacro with-reducer ((name function &optional (initial-value nil))
                        &body body)
  "Locally bind NAME to a reducing function. The arguments
FUNCTION and INITIAL-VALUE are passed directly to MAKE-REDUCER."
  (with-unique-names (reducer)
    `(let ((,reducer (make-reducer ,function ,@(list initial-value))))
       (flet ((,name (&rest items)
                (if items
                    (dolist (i items)
                      (funcall ,reducer i))
                    (funcall ,reducer))))
         ,@body))))

(defun make-collector (&optional initial-value)
  "Create a collector function.

A Collector function will collect, into a list, all the values
passed to it in the order in which they were passed. If the
callector function is called without arguments it returns the
current list of values."
  (let ((value initial-value)
        (cdr (last initial-value)))
    (lambda (&rest items)
      (if items
          (progn
            (if value
                (if cdr
                    (setf (cdr cdr) items
                          cdr (last items))
                    (setf cdr (last items)))
                (setf value items
                      cdr (last items)))
            items)
          value))))

(defun make-pusher (&optional initial-value)
  "Create a function which collects values as by PUSH."
  (let ((value initial-value))
    (lambda (&rest items)
      (if items
          (progn
            (dolist (i items)
              (push i value))
            items)
          value))))

(defmacro with-collector ((name &optional initial-value from-end) &body body)
  "Bind NAME to a collector function and execute BODY. If
  FROM-END is true the collector will actually be a pusher, (see
  MAKE-PUSHER), otherwise NAME will be bound to a collector,
  (see MAKE-COLLECTOR)."
  (with-unique-names (collector)
    `(let ((,collector ,(if from-end
                            `(make-pusher ,initial-value)
                            `(make-collector ,initial-value))))
       (flet ((,name (&rest items)
                (if items
                    (dolist (i items)
                      (funcall ,collector i))
                    (funcall ,collector))))
         ,@body))))

(defmacro with-collectors (names &body body)
  "Bind multiple collectors. Each element of NAMES should be a
  list as per WITH-COLLECTOR's first orgument."
  (if names
      `(with-collector ,(ensure-list (car names))
         (with-collectors ,(cdr names) ,@body))
      `(progn ,@body)))

(defmacro funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

;;; implementation of op and op* by simon.belak

(defun %starts-with (list head)
  "Does LIST start with HEAD?"
  (and (consp list) (eql (first list) head))) 

(defun %walk (function form &key fold (cont #'values))
  "Walk FORM applying FUNCTION to each node. Uses CPS."
  (if form
      (funcall function (first form) fold
               (lambda (node &optional fold)
                 (%walk function (rest form) :fold fold 
                       :cont (lambda (result &optional fold) 
                               (funcall cont (cons node result) fold)))))
      (funcall cont form fold)))
      
(defmacro defwalk (name arguments &body body)
  (let* ((node (gensym)) (fold (gensym)) (cont (gensym))
         (documentation (when (and (rest body) (stringp (first body)))
                              (list (first body))))
         (body (if documentation (rest body) body)))
    `(defun ,name (,node &optional ,fold (,cont #'values))
       ,@documentation
       (%walk (lambda ,arguments ,@body) ,node :fold ,fold :cont ,cont))))
      
(defun rnotany (predicate tree &key (recur-if #'consp))
  "Recursive NOTANY."
  (if (funcall recur-if tree) 
      (every (lambda (tree) (rnotany predicate tree :recur-if recur-if)) tree)
      (not (funcall predicate tree))))
            
(defun recurp (form)
  "Is FORM non-terminal?"
  (not (or (atom form) 
           (member (first form) '(quote op op*))
           (and (%starts-with form 'function) (symbolp (second form))))))
            
(defun simple-slot-p (object)
  "Is OBJECT a simple slot designator?"
  (eq object (intern "_")))

(defun rest-slot-p (object)
  "Is OBJECT a rest slot designator?"
  (eq object (intern "__")))
  
(defun slotp (object)
  "Is OBJECT a slot designator?"
  (or (simple-slot-p object) (rest-slot-p object)))
    
(defwalk slots-to-arguments (subform arguments cont)
  "Assign names to slots."
  (cond ((slotp subform) 
         (let ((argname (gensym "OP-")))
           (funcall cont argname (if (rest-slot-p subform)
                                     (list* argname '&rest arguments)
                                     (cons argname arguments)))))
        ((recurp subform) (slots-to-arguments subform arguments cont))
        (t (funcall cont subform arguments))))        

(defun liftablep (form)
  "Can FORM be evaluated early?"
  (and (recurp form) (rnotany #'slotp form :recur-if #'recurp)))

(defun special-form-p (form)
  "Is FORM a special form?"
  (and (consp form) (special-operator-p (first form))))
 
(defun recur-lift-p (form)
  "Does FORM contain subforms that could be evaluated early?"
  (or (and (recurp form) (not (special-form-p form)))
      (%starts-with form 'progn)
      (%starts-with form 'block)
      (and (%starts-with form 'setq) (= (length form) 3))))
 
(defwalk lift-invariants (subform bindings cont)
  "Bind subforms suitable for early evaluation."
  (declare (special *environment*))
  (let ((expansion (macroexpand subform *environment*)))
    (cond ((liftablep expansion) 
           (let ((bind (or (find subform bindings :test #'equal :key #'second)
                           (list (gensym) subform))))
             (funcall cont (first bind) (adjoin bind bindings :test #'equal))))
          ((recur-lift-p expansion) (lift-invariants expansion bindings cont))
          ((%starts-with expansion 'the) 
           (lift-invariants (third expansion) bindings 
               (lambda (form bindings)
                 (funcall cont `(,@(subseq expansion 0 2) ,form) bindings))))
          (t (funcall cont subform bindings)))))

(defmacro op* (&rest args)
  "Make an anonymous function with implicit arguments. Defer evaluation."
  (multiple-value-bind (form slots) (slots-to-arguments args)
    `(lambda ,(reverse slots) ,form)))
            
(defmacro op (&rest args &environment *environment*)
  "Make an anonymous function with implicit arguments."
  (declare (special *environment*))
  (multiple-value-bind (args invariants) (lift-invariants args)
    (if invariants 
        `(let ,invariants (op* ,@args)) 
        `(op* ,@args))))

(in-package :unittest)

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

(declaim (inline strcat))
(defun strcat (&rest items)
  "Returns a fresh string consisting of ITEMS concat'd together."
  (declare (optimize speed))
  (strcat* items))

(defun strcat* (string-designators)
  "Concatenate all the strings in STRING-DESIGNATORS."
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dotree (str string-designators)
        (when str
          (princ str stream))))))

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

(defmacro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

(defun make-lookup-name (name &rest parts)
  (funcall #'intern-concat parts (symbol-package name)))

(defun build-hash-table (hash-spec inital-contents)
  "Create a hash table containing ``INITAL-CONTENTS``."
  (let ((ht (apply #'make-hash-table hash-spec)))
    (dolist* ((key value) inital-contents)
      (setf (gethash key ht) value))
    ht))

(defmacro deflookup-table
    (name &key (var    (make-lookup-name name "*" name "*"))
               (reader (make-lookup-name name "GET-" name))
               (writer (make-lookup-name name "GET-" name))
               (rem-er (make-lookup-name name "REM-" name))
               (at-redefinition :warn)
               (documentation
                (format nil "Global var for the ~S lookup table" name))
               (test 'eql)
               (initial-contents nil))
  "Creates a hash table and the associated accessors."
  ;; if they explicitly pass in NIL we make the name a gensym
  (unless var
    (setf var    (gensym (strcat "var for " name " lookup table "))))
  (unless reader
    (setf reader (gensym (strcat "reader for " name " lookup table "))))
  (unless writer
    (setf writer (gensym (strcat "writer for " name " lookup table "))))
  (assert (symbolp name) (name)
          "The name of the lookup table must be a symbol.")
  (assert (symbolp var) (var)
          "The name of the underlying var must be a symbol.")
  (assert (symbolp reader) (reader)
          "The name of the reader for a lookup table must be a symbol.")
  (assert (symbolp writer) (writer)
          "The name of the writer for a lookup table must be a symbol.")
  `(progn
     (defvar ,var
       (build-hash-table '(:test ,test) ,initial-contents)
       ,documentation)
     (defun ,reader (key &optional default)
       (gethash key ,var default))
     (defun (setf ,writer) (value key)
       ,(when at-redefinition
          `(when (gethash key ,var)
             ,(case at-redefinition
                (:warn `(warn "Redefining ~A in deflookup-table named ~S"
                         (let ((*package* (find-package "KEYWORD")))
                           (format nil "~S" key))
                         ',name))
                (t at-redefinition))))
       (setf (gethash key ,var) value))
     (defun ,rem-er (key)
       (remhash key ,var))
     (list ',name ',var ',reader '(setf ,writer) ',rem-er)))

(defun intern-concat (string-designators &optional (package *package*))
  (intern (with-output-to-string (symbol-name)
            (dolist (designator string-designators)
              (write-string (etypecase designator
                              (symbol (symbol-name designator))
                              (string designator))
                            symbol-name)))
          package))

(defun check-required (name vars required)
  (dolist (var required)
    (assert (member var vars)
            (var)
            "Unrecognized symbol ~S in ~S." var name)))

(defmacro def-special-environment (name (&key accessor binder binder*)
                                  &rest vars)
  "Define two macros for dealing with groups or related special variables.

ACCESSOR is defined as a macro: (defmacro ACCESSOR (VARS &rest
BODY)).  Each element of VARS will be bound to the
current (dynamic) value of the special variable.

BINDER is defined as a macro for introducing (and binding new)
special variables. It is basically a readable LET form with the
prorpe declarations appended to the body. The first argument to
BINDER must be a form suitable as the first argument to LET.

ACCESSOR defaults to a new symbol in the same package as NAME
which is the concatenation of \"WITH-\" NAME. BINDER is built as
\"BIND-\" and BINDER* is BINDER \"*\"."
  (unless accessor
    (setf accessor (intern-concat (list '#:with- name) (symbol-package name))))
  (unless binder
    (setf binder   (intern-concat (list '#:bind- name) (symbol-package name))))
  (unless binder*
    (setf binder*  (intern-concat (list binder '#:*) (symbol-package binder))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (flet ()
       (defmacro ,binder (requested-vars &body body)
         (check-required ',name ',vars (mapcar #'car requested-vars))
         `(let ,requested-vars
            (declare (special ,@(mapcar #'car requested-vars)))
            ,@body))
       (defmacro ,binder* (requested-vars &body body)
         (check-required ',name ',vars (mapcar #'car requested-vars))
         `(let* ,requested-vars
            (declare (special ,@(mapcar #'car requested-vars)))
            ,@body))
       (defmacro ,accessor (requested-vars &body body)
         (check-required ',name ',vars requested-vars)
         `(locally (declare (special ,@requested-vars))
            ,@body))
       ',name)))

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

(defun varsymp (x)
  (and (symbolp x) (eq (aref (symbol-name x) 0) #\?)))

(defun ensure-list (thing)
  "Returns THING as a list.

If THING is already a list (as per listp) it is returned,
otherwise a one element list containing THING is returned."
  (if (listp thing)
      thing
      (list thing)))

(defun vars (match-spec)
  (let ((vars nil))
    (labels ((find-vars (spec)
               (cond
                 ((null spec) nil)
                 ((varsymp spec) (push spec vars))
                 ((consp spec)
                  (find-vars (car spec))
                  (find-vars (cdr spec))))))
      (find-vars match-spec))
    (delete-duplicates vars)))

(defmacro list-match-case (target &body clauses)
  (if clauses
      (destructuring-bind ((test &rest progn) &rest others)
          clauses
        (with-unique-names (tgt binds success)
          `(let ((,tgt ,target))
             (multiple-value-bind (,binds ,success)
                 (list-match ,tgt ',test)
               (declare (ignorable ,binds))
               (if ,success
                   (let ,(mapcar (lambda (var)
                                   `(,var (cdr (assoc ',var ,binds))))
                                 (vars test))
                     (declare (ignorable ,@(vars test)))
                     ,@progn)
                   (list-match-case ,tgt ,@others))))))
      nil))

(defun collect-timing (lambda)
  "Executes LAMBDA and returns a timing-info object specifying
  how long execution took and how much memory was used.

NB: Not all implementations provide all information. See the
various %collect-timing definitions for details."
  (%collect-timing lambda))

#+sbcl
(defun %collect-timing (fun)
  (declare (type function fun))
  "Implementation of collect-timing for SBCL.

This code is a cut 'n paste from sbcl/src/code/time.lisp. It uses
internal functions, all bets off."
  (let (old-run-utime
        new-run-utime
        old-run-stime
        new-run-stime
        old-real-time
        new-real-time
        old-page-faults
        new-page-faults
        real-time-overhead
        run-utime-overhead
        run-stime-overhead
        page-faults-overhead
        old-bytes-consed
        new-bytes-consed
        cons-overhead)
    ;; Calculate the overhead...
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    ;; Do it a second time to make sure everything is faulted in.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    (multiple-value-setq
        (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (sb-impl::time-get-sys-info))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time (get-internal-real-time))
    (setq old-real-time (get-internal-real-time))
    (setq new-real-time (get-internal-real-time))
    (setq real-time-overhead (- new-real-time old-real-time))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (sb-impl::time-get-sys-info))
    (setq old-real-time (get-internal-real-time))
    (let ((start-gc-run-time sb-impl::*gc-run-time*))
      (progn
        ;; Execute the form and return its values.
        (funcall fun)
        (multiple-value-setq
            (new-run-utime new-run-stime new-page-faults new-bytes-consed)
          (sb-impl::time-get-sys-info))
        (setq new-real-time (- (get-internal-real-time) real-time-overhead))
        (let ((gc-run-time (max (- sb-impl::*gc-run-time* start-gc-run-time) 0)))
          (make-instance 'timing-info
                         :real-time (max (- new-real-time old-real-time) 0.0)
                         :user-time (max (/ (- new-run-utime old-run-utime) 1000.0) 0.0)
                         :system-time (max (/ (- new-run-stime old-run-stime) 1000.0) 0.0)
                         :gc-time (float gc-run-time)
                         :page-faults (max (- new-page-faults old-page-faults) 0)
                         :bytes-consed (max (- new-bytes-consed old-bytes-consed) 0)))))))

#+openmcl
(defun %collect-timing (lambda)
  "Implementation of collect-timing for OpenMCL.

We only report the MAJOR-PAGE-FAULTS, the number of
MINOR-PAGE-FAULTS is ignored."
  (let ((ccl:*report-time-function* #'list))
    (destructuring-bind (&key elapsed-time user-time system-time
                              gc-time bytes-allocated major-page-faults
                              &allow-other-keys)
        (time (funcall lambda))
      (make-instance 'timing-info
                     :real-time elapsed-time
                     :user-time user-time
                     :system-time system-time
                     :gc-time gc-time
                     :bytes-consed bytes-allocated
                     :page-faults major-page-faults))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-unique-names (val foundp)
        (destructuring-bind ((test &rest progn) &rest others)
            clauses
          `(multiple-value-bind (,val ,foundp)
               ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (let ((it (assoc x binds)))
              (if it
                  (or (recbind (cdr it) binds)
                      it)))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

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

(defun list-match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_))
     (values binds t))
    ((binding x binds) (list-match it y binds))
    ((binding y binds) (list-match x it binds))
    ((varsymp x) (values (cons (cons x y) binds) t))
    ((varsymp y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (list-match (car x) (car y) binds))
     (list-match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defmacro when-bind (var test &body body)
  "Just like when except VAR will be bound to the
  result of TEST in BODY."
  `(if-bind ,var ,test (progn ,@body)))

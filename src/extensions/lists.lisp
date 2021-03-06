(in-package :cl-extensions)

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

(defun alist-to-plist (alist)
  "Returns a property list containing the same keys and values as the
association list ALIST in the same order."
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

(defun plist-to-alist (plist)
  "Returns an association list containing the same keys and values as the
property list PLIST in the same order."
  (let (alist)
    (do ((tail plist (cddr tail)))
        ((endp tail) (nreverse alist))
      (push (cons (car tail) (cadr tail)) alist))))

(defun malformed-plist (plist)
  (error "Malformed plist: ~S" plist))

(defmacro doplist ((key val plist &optional values) &body body)
  "Iterates over elements of PLIST. BODY can be preceded by
declarations, and is like a TAGBODY. RETURN may be used to terminate
the iteration early. If RETURN is not used, returns VALUES."
  (multiple-value-bind (forms declarations) (parse-body body)
    (with-gensyms (tail loop results)
      `(block nil
         (flet ((,results ()
                  (let (,key ,val)
                    (declare (ignorable ,key ,val))
                    (return ,values))))
           (let* ((,tail ,plist)
                  (,key (if ,tail
                            (pop ,tail)
                            (,results)))
                 (,val (if ,tail
                           (pop ,tail)
                           (malformed-plist ',plist))))
            (declare (ignorable ,key ,val))
            ,@declarations
            (tagbody
               ,loop
               ,@forms
               (setf ,key (if ,tail
                              (pop ,tail)
                              (,results))
                     ,val (if ,tail
                              (pop ,tail)
                              (malformed-plist ',plist)))
               (go ,loop))))))))

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(define-modify-macro nconcf (&rest lists) nconc
  "Modify-macro for NCONC. Concatenates LISTS to place designated by the first
argument.")

(define-modify-macro unionf (list) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place.")

(define-modify-macro nunionf (list) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place. May modify
either argument.")

(defun circular-list (&rest elements)
  "Creates a circular list of ELEMENTS."
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun circular-tree-p (object)
  "Returns true if OBJECT is a circular tree, NIL otherwise."
  (labels ((circularp (object seen)
             (and (consp object)
                  (do ((fast (cons (car object) (cdr object)) (cddr fast))
                       (slow object (cdr slow)))
                      ((or (not (consp fast)) (not (consp (cdr slow))))
                       (do ((tail object (cdr tail)))
                           ((not (consp tail))
                            nil)
                         (let ((elt (car tail)))
                           (circularp elt (cons object seen)))))
                    (when (or (eq fast slow) (member slow seen))
                      (return-from circular-tree-p t))))))
    (circularp object nil)))

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(deftype proper-list ()
  "Type designator for proper lists. Implemented as a SATISFIES type, hence
not recommended for performance intensive use. Main usefullness as a type
designator of the expected type in a TYPE-ERROR."
  `(and list (satisfies proper-list-p)))

(defun lastcar (list)
  "Returns the last element of LIST. Signals a type-error if LIST is not a
proper list."
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (cadr last)))
    (when (endp (cdr fast))
      (return (car fast)))
    (when (eq fast slow)
      (error 'type-error
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun (setf lastcar) (object list)
  "Sets the last element of LIST. Signals a type-error if LIST is not a proper
list."
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (setf (cadr last) object)))
    (when (endp (cdr fast))
      (return (setf (car fast) object)))
    (when (eq fast slow)
      (error 'type-error
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun make-circular-list (length &key initial-element)
  "Creates a circular list of LENGTH with the given INITIAL-ELEMENT."
  (let ((cycle (make-list length :initial-element initial-element)))
    (nconc cycle cycle)))

(deftype circular-list ()
  "Type designator for circular lists. Implemented as a SATISFIES type, so not
recommended for performance intensive use. Main usefullness as the
expected-type designator of a TYPE-ERROR."
  `(satisfies circular-list-p))

(defun ensure-car (thing)
  "If THING is a CONS, its CAR is returned. Otherwise THING is returned."
  (if (consp thing)
      (car thing)
      thing))

(defun ensure-cons (cons)
  "If CONS is a cons, it is returned. Otherwise returns a fresh cons with CONS
  in the car, and NIL in the cdr."
  (if (consp cons)
      cons
      (cons cons nil)))

(defun ensure-list (thing)
  "Returns THING as a list.

If THING is already a list (as per listp) it is returned,
otherwise a one element list containing THING is returned."
  (if (listp thing)
      thing
      (list thing)))

(defun remove-from-plist (plist &rest keys)
  "Returns a propery-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  ;; FIXME: possible optimization: (remove-from-plist '(:x 0 :a 1 :b 2) :a)
  ;; could return the tail without consing up a new list.
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
        collect key and collect (first rest)))

(defun delete-from-plist (plist &rest keys)
  "Just like REMOVE-FROM-PLIST, but this version may destructively modify the
provided plist."
  ;; FIXME: should not cons
  (apply 'remove-from-plist plist keys))

(define-modify-macro remove-from-plistf (&rest keys) remove-from-plist)
(define-modify-macro delete-from-plistf (&rest keys) delete-from-plist)

(defun mappend (function &rest lists)
  "Applies FUNCTION to respective element(s) of each LIST, appending all the
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'mapcar function lists)
        append results))

(defun setp (object &key (test #'eql) (key #'identity))
  "Returns true if OBJECT is a list that denotes a set, NIL otherwise. A list
denotes a set if each element of the list is unique under KEY and TEST."
  (and (listp object)
       (let (seen)
         (dolist (elt object t)
           (let ((key (funcall key elt)))
             (if (member key seen :test test)
                 (return nil)
                 (push key seen)))))))

(defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
  "Returns true if every element of LIST1 matches some element of LIST2 and
every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (or (member elt keylist2 :test test)
               (return nil)))
         (dolist (elt keylist2 t)
           (or (member elt keylist1 :test test)
               (return nil))))))

(defun map-product (function list &rest more-lists)
  "Returns a list containing the results of calling FUNCTION with one argument
from LIST, and one from each of MORE-LISTS for each combination of arguments.
In other words, returns the product of LIST and MORE-LISTS using FUNCTION.

Example:

 (map-product 'list '(1 2) '(3 4) '(5 6)) => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
                                              (2 3 5) (2 3 6) (2 4 5) (2 4 6))
"
  (labels ((%map-product (f lists)
             (let ((more (cdr lists))
                   (one (car lists)))
               (if (not more)
                   (mapcar f one)
                   (mappend (lambda (x)
                              (%map-product (curry f x) more))
                            one)))))
    (%map-product (if (functionp function)
                      function
                      (fdefinition function))
                  (cons list more-lists))))

(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun unflatten (list n)
  (loop for x from 0 to (1- (/ (length list) n)) collect
        (subseq list (* n x) (* n (1+ x)))))

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

(defun partition (list &rest lambdas)
  "Split LIST into sub lists according to LAMBDAS.

Each element of LIST will be passed to each element of LAMBDAS,
the first function in LAMBDAS which returns T will cause that
element to be collected into the corresponding list.

Examples:

 (partition '(1 2 3) #'oddp #'evenp) => ((1 3) (2))

 (partition '(1 2 3) #'oddp t) => ((1 3) (1 2 3))

 (partition '(1 2 3) #'oddp #'stringp) => ((1 3) nil)"
  (let ((collectors (mapcar (lambda (predicate)
                              (cons (case predicate
                                      ((t :otherwise) 
                                       (constantly t))
                                      ((nil)
                                       (constantly nil))
                                      (t predicate))
                                    (make-collector)))
                            lambdas)))
    (dolist (item list)
      (dolist* ((test-func . collector-func) collectors)
        (when (funcall test-func item)
          (funcall collector-func item))))
    (mapcar #'funcall (mapcar #'cdr collectors))))

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

(defun do-push* (list &rest items)
  (dolist (i items)
    (setf list (cons i list)))
  list)

(define-modify-macro push* (&rest items)
  do-push*
  "Pushes every element of ITEMS onto LIST. Equivalent to calling PUSH
  with each element of ITEMS.")

(defun append1 (&rest itens)
  (loop for x in itens append (ensure-list x)))

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;; FIXME: make it more efficient
(defun make-pairs (list)
  (loop for x from 0 to (1- (length list)) by 2
        collect (subseq list x (+ x 2))))

;; FIXME: make it more efficient
(defun list-to-alist (list)
  (loop for x from 0 to (1- (length list)) by 2
        for key = (nth x list)
        for data = (nth (1+ x) list)
        nconc (acons key data nil)))

(defun transpose-list (m)
  (apply #'mapcar #'list m))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?"
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


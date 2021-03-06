(in-package :closer-mop)

;; Some internal utility functions.

#-(or clozure-common-lisp openmcl)
(define-modify-macro nconcf (&rest lists) nconc)

;; Some utility functions.

(defun required-args (lambda-list &optional (collector #'identity))
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        collect (funcall collector arg)))

(defun ensure-finalized (class &optional (errorp t))
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
  (flet ((get-class (class) (etypecase class
                              (class class)
                              (symbol (find-class class)))))
    
      (loop with class = (get-class class)
            with superclass = (get-class superclass)
            
            for superclasses = (list class)
            then (set-difference 
                  (union (class-direct-superclasses current-class) superclasses)
                  seen)

            for current-class = (first superclasses)

            while current-class
            
            if (eq current-class superclass) return t
            else collect current-class into seen
            
            finally (return nil))))

#-(or clozure-common-lisp openmcl)
(progn
  ;; We need a new standard-class for various things.

  (cl:defclass standard-class (cl:standard-class)
    ())

  ;; validate-superclass for metaclass classes is a little bit
  ;; more tricky than for class metaobject classes because
  ;; we don't want to make all standard-classes compatible to
  ;; each other.

  ;; Our validate-superclass may get passed a class-prototype
  ;; as its second argument, so don't expect its readers to
  ;; yield useful information. (In ANSI parlance, "the
  ;; consequences are undefined...")

  (cl:defmethod validate-superclass
             ((class standard-class)
              (superclass cl:standard-class))
    (or (when (eq (class-of class) (find-class 'standard-class))
          (or (eq (class-of superclass) (find-class 'cl:standard-class))
              (eq (class-of superclass) (find-class 'standard-class))))
        (call-next-method)
        (when (eq (class-of superclass) (find-class 'cl:standard-class))
          (validate-superclass class (class-prototype (find-class 'standard-class))))))

  ;; The following macro ensures that the new standard-class is used
  ;; by default. It would have been useful to fix other deficiencies
  ;; in a complete redefinition of defclass, but there is no portable
  ;; way to ensure the necessary compile-time effects as specified
  ;; by ANSI Common Lisp. Therefore, we just expand to the original
  ;; cl:defclass.
    
  (defmacro defclass (name (&rest supers) &body options)
    (if (member :metaclass options :key #'car)
        `(cl:defclass ,name ,supers ,@options)
      `(cl:defclass ,name ,supers ,@options
         (:metaclass standard-class))))

  ;; In MCL, the list of direct superclasses passed by the
  ;; defclass macro is not empty, as required by AMOP, but
  ;; instead passes the class metaobject for standard-object
  ;; or funcallable-standard-object respectively. This makes
  ;; replacing the default superclass for a new metaclass a bit
  ;; more complicated. In order to avoid the tricky bits in user
  ;; code, the new standard-class adjusts possible incorrect
  ;; direct superclasses by adding or removing the metaobject
  ;; for standard-object as needed before passing them to
  ;; the original standard-class. In user code, just use the
  ;; idiom suggested by AMOP to APPEND your new default superclass
  ;; to the list of direct superclasses.

  (defun modify-superclasses (direct-superclasses)
    (if (null direct-superclasses)
        (list (find-class 'standard-object))
      (let ((standard-object (find-class 'standard-object)))
        (if (eq (car (last direct-superclasses)) standard-object)
            direct-superclasses
          (remove standard-object direct-superclasses)))))

  (cl:defmethod initialize-instance :around
    ((class standard-class) &rest initargs
     &key (name (gensym)) (direct-superclasses ()))
    (declare (dynamic-extent initargs))
    (apply #'call-next-method class
           :name name
           :direct-superclasses (modify-superclasses direct-superclasses)
           initargs))

  (cl:defmethod reinitialize-instance :around
    ((class standard-class) &rest initargs
     &key (direct-superclasses () direct-superclasses-p))
    (declare (dynamic-extent initargs))
    (if direct-superclasses-p
        (apply #'call-next-method class
               :direct-superclasses (modify-superclasses direct-superclasses)
               initargs)
      (call-next-method)))

  (defgeneric typep (object type)
    (:method (object type)
     (cl:typep object type))
    (:method (object (type class))
     (member (class-of object)
             (class-precedence-list type))))
  
  (defgeneric subtypep (type1 type2)
    (:method (type1 type2)
     (cl:subtypep type1 type2))
    (:method ((type1 class) (type2 symbol))
     (let ((class2 (find-class type2 nil)))
       (if class2
           (member class2 (class-precedence-list type1))
         (cl:subtypep type1 type2))))
    (:method ((type1 symbol) (type2 class))
     (let ((class1 (find-class type1 nil)))
       (if class1
           (member type2 (class-precedence-list class1))
         (cl:subtypep type1 type2))))
    (:method ((type1 class) (type2 class))
     (member type2 (class-precedence-list type1)))))

#+(or clozure-common-lisp openmcl)
(progn
  (cl:defclass standard-generic-function (cl:standard-generic-function) ()
    (:metaclass funcallable-standard-class))

  (defun ensure-generic-function
         (name &rest args
               &key (generic-function-class 'standard-generic-function)
               &allow-other-keys)
    (declare (dynamic-extent args))
    (when (fboundp name)
      (let ((function (fdefinition name)))
        (unless (typep function 'generic-function)
          (cerror "Discard existing definition and create generic function."
                  "~S is already fbound, but not as a generic function." name)
          (fmakunbound name))))
    (if (fboundp name)
      (let ((function (fdefinition name)))
        (apply #'ensure-generic-function-using-class
               function name args))
      (apply #'ensure-generic-function-using-class nil name
             :generic-function-class generic-function-class
             args)))

  (defmacro defgeneric (&whole form name (&rest args) &body options)
    (unless (every #'consp options)
      (error "Illegal generic functions options in defgeneric form ~S." form))
    `(cl:defgeneric ,name ,args ,@options
       ,@(unless (member :generic-function-class options :key #'car :test #'eq)
	   '((:generic-function-class standard-generic-function)))))

  (cl:defgeneric compute-discriminating-function (gf)
    (:method ((gf generic-function))
     (let ((non-dt-dcode (ccl::non-dt-dcode-function gf)))
       (if non-dt-dcode
         non-dt-dcode
         (let* ((std-dfun (ccl::%gf-dcode gf))
                (dt (ccl::%gf-dispatch-table gf))
                (proto (cdr (assoc std-dfun ccl::dcode-proto-alist))))
           (if (or (eq proto #'ccl::gag-one-arg)
                   (eq proto #'ccl::gag-two-arg))
             (lambda (&rest args)
               (declare (dynamic-extent args))
               (apply std-dfun dt args))
             (lambda (&rest args)
               (declare (dynamic-extent args))
               (funcall std-dfun dt args))))))))
  
  (defmethod add-method :after ((gf standard-generic-function) method)
    (declare (ignore method))
    (set-funcallable-instance-function
     gf (compute-discriminating-function gf)))

  (defmethod remove-method :after ((gf standard-generic-function) method)
    (declare (ignore method))
    (set-funcallable-instance-function
     gf (compute-discriminating-function gf)))

  (defmethod initialize-instance :after ((gf standard-generic-function) &key)
    (set-funcallable-instance-function
     gf (compute-discriminating-function gf)))

  (defmethod reinitialize-instance :after ((gf standard-generic-function) &key)
    (set-funcallable-instance-function
     gf (compute-discriminating-function gf))))

(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (eval `(defmethod ,(generic-function-name gf) ,@qualifiers
           ,(loop for specializer in specializers
                  for (arg . rest) on lambda-list
                  collect `(,arg ,specializer) into args
                  finally (return (nconc args rest)))
           ,@(cddr lambda-expression))))

;; The following ensures that slot definitions have a documentation in Clozure CL and OpenMCL.

#+(or clozure-common-lisp openmcl)
(defmethod initialize-instance :after ((slot slot-definition) &key documentation)
  (setf (documentation slot 't) documentation))

;; The following can be used in direct-slot-definition-class to get the correct initargs
;; for a slot. Use it like this:
;;
;; (defmethod direct-slot-definition-class
;;            ((class my-standard-class) &rest initargs)
;;   (declare (dynamic-extent initargs))
;;   (destructuring-bind
;;       (&key key-of-interest &allow-other-keys)
;;       (fix-slot-initargs initargs)
;;     ...))

(defvar *standard-slot-keys*
  '(:name :documentation
    :initargs :initform :initfunction
    :readers :writers))

(defun fix-slot-initargs (initargs)
  #+(or clozure-common-lisp openmcl) initargs
  #-(or clozure-common-lisp openmcl)
  (let* ((counts (loop with counts
                       for (key nil) on initargs by #'cddr
                       do (incf (getf counts key 0))
                       finally (return counts)))
         (keys-to-fix (loop for (key value) on counts by #'cddr
                            if (> value 1) collect key)))
    (if keys-to-fix
        (let ((multiple-standard-keys
               (intersection keys-to-fix *standard-slot-keys*)))
          (if multiple-standard-keys
              (error "Too many occurences of ~S in slot initargs ~S."
                     multiple-standard-keys initargs)
            (loop with fixed-keys
                  for (key value) on initargs by #'cddr
                  if (member key keys-to-fix)
                  do (nconcf (getf fixed-keys key) (list value))
                  else nconc (list key value) into fixed-initargs
                  finally (return (nconc fixed-initargs fixed-keys)))))
      initargs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))

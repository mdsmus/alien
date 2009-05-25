(in-package :cl-extensions)

(defmacro defclass-struct (name-and-options supers &rest slots)
  "DEFCLASS with a DEFSTRUCT api.

NAME-AND-OPTIONS:

  name-symbol |
  ( name-symbol [ (:conc-name conc-name ) ]
                [ (:predicate predicate-name ) ]
                class-option* )

SUPERS - a list of super classes passed directly to DEFCLASS.

SLOTS - a list of slot forms:

  name |
  ( name [ init-arg ] [ slot-options* ] )"
  (generate-defclass (first (ensure-list name-and-options))
                     (cdr (ensure-list name-and-options))
                     supers slots))

(defun generate-defclass (class-name options supers slots)
  (let ((conc-name nil)
        (predicate nil)
        (predicate-forms nil)
        (class-options '()))
    (loop
       for (option-name . args) in options
       do (case option-name
            (:conc-name
             (when conc-name
               (error "Can't specify the :CONC-NAME argument more than once."))
             (setf conc-name (first args)))
            (:predicate
             (when predicate
               (error "Can't specify the :PREDICATE argument more than once."))
             (setf predicate (if (eql t (first args))
                                 (intern (strcat class-name :-p) *package*)
                                 (first args))))
            (t
             (push (cons option-name args) class-options))))
    (setf slots
          (mapcar
           (lambda (slot-spec)
             (destructuring-bind (name
                                  &optional initform
                                  &rest options)
                 (ensure-list slot-spec)
               `(,name
                 :initform ,initform
                 ,@(when conc-name
                     `(:accessor ,(intern (strcat conc-name name)
                                          (symbol-package conc-name))))
                 :initarg ,(intern (symbol-name name) :keyword)
                 ,@options)))
           slots)
          predicate-forms
          (if predicate
              (with-unique-names (obj)
                `((defmethod ,predicate ((,obj ,class-name)) t)
                  (defmethod ,predicate ((,obj t)) nil)))
              nil))
    `(prog1
         (defclass ,class-name ,supers ,slots ,@(nreverse class-options))
       ,@predicate-forms)))

(defmacro defprint-object ((self class-name &key (identity t) (type t) with-package
                                 (muffle-errors t))
                           &body body)
  "Define a print-object method using print-unreadable-object.
  An example:
  (defprint-object (self parenscript-dispatcher)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream)
    `(defmethod print-object ((,self ,class-name) ,stream)
      (print-unreadable-object (,self ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream))
          (block printing
            (,@(if muffle-errors
                   `(handler-bind ((error (lambda (error)
                                            (declare (ignore error))
                                            (write-string "<<error printing object>>")
                                            (return-from printing)))))
                   `(progn))
               (let (,@(when with-package `((*package* ,(find-package with-package)))))
                 ,@body))))))))

(defmacro with-accessors* (accessor-names object &body body)
  "Just like WITH-ACCESSORS, but if the slot-entry is a symbol
  assume the variable and accessor name are the same."
  `(with-accessors ,(mapcar (lambda (name)
			      (if (consp name) 
				  name 
				  `(,name ,name)))
			    accessor-names)
       ,object
     ,@body))

(defun class-name-of (obj)
  (class-name (class-of obj)))


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

;;; copy objects

(defun delete-all (set1 set2)
  (set-difference set2 set1))

(defmethod instance-slot-names ((object standard-object))
  (let* ((slots (sb-pcl:class-slots (class-of object)))
	 (instance-slots (remove :class slots
				 :key #'sb-pcl:slot-definition-allocation
				 :test #'eq)))
    (mapcar #'sb-pcl:slot-definition-name instance-slots)))

(defmethod make-uninitialized-instance ((class symbol) &rest initargs)
  (apply #'make-uninitialized-instance (find-class class) initargs))

(defmethod make-uninitialized-instance ((class standard-class)
					&rest initargs)
  (declare (ignore initargs))
  (allocate-instance class))

(defmethod shallow-copy (object &key &allow-other-keys)
  ;; primitive objects are not copied
  object)

(defmethod deep-copy (object &key &allow-other-keys)
  ;; primitive objects are not copied
  object)

(defmethod copy (object &key &allow-other-keys)
  (deep-copy object))

#-lucid
;; class metobject is not supplied by lucid clos
(defmethod uninitialized-copy ((object sb-mop:metaobject) &key &allow-other-keys)
  ;; metaobjects are not copied
  object)

(defmethod shallow-copy ((object sequence) &key &allow-other-keys)
  (copy-seq object))

(defmethod deep-copy ((object list) &key &allow-other-keys)
  (map 'list #'copy object))

(defmethod copy ((object sequence) &key &allow-other-keys)
  ;; sequences are not deep copied by default
  (shallow-copy object))

(defmethod slots-for-identity ((object standard-object))
  ())

(defmethod slots-for-shallow-copy ((object standard-object))
  ())

(defmethod slots-for-deep-copy ((object standard-object))
  ())

(defmethod slots-for-copy ((object standard-object))
  (instance-slot-names object))

(defmethod copy-slot ((from-object standard-object)
		      (to-object standard-object)
		      slot &optional (copy-function #'copy))
  (when (slot-boundp from-object slot)
    (setf (slot-value to-object slot)
	(funcall copy-function (slot-value from-object slot)))))

(defmethod copy-slots ((from-object standard-object)
		       (to-object standard-object)
		       &key discard-slots)
  (dolist (slot (delete-all discard-slots
			    (slots-for-identity from-object)))
      (copy-slot from-object to-object slot #'identity))
  (dolist (slot (delete-all discard-slots
			    (slots-for-copy from-object)))
      (copy-slot from-object to-object slot #'copy))
  (dolist (slot (delete-all discard-slots
			    (slots-for-shallow-copy from-object)))
      (copy-slot from-object to-object slot #'shallow-copy))
  (dolist (slot (delete-all discard-slots
			    (slots-for-deep-copy from-object)))
      (copy-slot from-object to-object slot #'deep-copy)))

(defmethod object-creation-function ((object standard-object))
  #'make-uninitialized-instance)

(defmethod after-initialization ((old-object standard-object)
				 (new-object standard-object))
  nil)

(defmethod before-initialization ((old-object standard-object)
				  (new-object standard-object))
  nil)

(defmethod copy :around ((object standard-object)
			 &key discard-slots &allow-other-keys)
  (declare (special *copy-reference-list*))
  (if (boundp '*copy-reference-list*)
      (or (cdr (assoc object *copy-reference-list* :test #'eq))
	  (let ((copy (uninitialized-copy object)))
	    (unless (eq object copy)
	      (setq *copy-reference-list*
		  (acons object copy *copy-reference-list*))
	      (call-next-method object :use-object copy
				       :discard-slots discard-slots))
	    copy))
    (let ((*copy-reference-list* nil))
      (declare (special *copy-reference-list*))
      (let ((copy (uninitialized-copy object)))
	(unless (eq object copy)
	  (setq *copy-reference-list*
	      (acons object copy *copy-reference-list*))
	  (call-next-method object :use-object copy
				   :discard-slots discard-slots))
	copy))))

(defmethod uninitialized-copy ((object standard-object) &key &allow-other-keys)
  (let ((class-name (class-name (class-of object)))
	(creation-function (object-creation-function object)))
    (funcall creation-function class-name)))
	   
(defmethod deep-copy ((object standard-object) &key discard-slots use-object)
  (let ((new-object (or use-object (uninitialized-copy object))))
    (unless (eq new-object object)
      (before-initialization object new-object)
      (copy-slots object new-object :discard-slots discard-slots)
      (after-initialization object new-object))
    new-object))
	   
(defmethod shallow-copy ((object standard-object)
			 &key discard-slots use-object)
  (let ((new-object (or use-object (uninitialized-copy object))))
    (unless (eq new-object object)
      (dolist (slot (delete-all discard-slots (instance-slot-names object)))
	(copy-slot object new-object slot #'identity)))
    new-object))
	   
(defmethod copy ((object standard-object) &key discard-slots use-object)
  (deep-copy object :discard-slots discard-slots
	            :use-object use-object))

(defmethod get-copy (object)
  (declare (special *copy-reference-list*))
  (and (boundp '*copy-reference-list*)
       (cdr (assoc object *copy-reference-list* :test #'eq))))

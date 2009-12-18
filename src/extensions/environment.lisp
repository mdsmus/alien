(in-package :cl-extensions)

(defun function-lambda-list (fn)
  "Return the signature of the function."
  #+allegro (excl:arglist fn)
  #+clisp (sys::arglist fn)
  #+(or cmu scl)
  (let ((f (coerce fn 'function)))
    (typecase f
      (STANDARD-GENERIC-FUNCTION (pcl:generic-function-lambda-list f))
      (EVAL:INTERPRETED-FUNCTION (eval:interpreted-function-arglist f))
      (FUNCTION (values (read-from-string (kernel:%function-arglist f))))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase fn (symbol (fdefinition fn)) (t fn)))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (si:compiled-function-name fn)))))
          (get fn 'si:debug))
  #+lispworks (lw:function-lambda-list fn)
  #+lucid (lcl:arglist fn)
  #+sbcl (sb-introspect:function-lambda-list fn)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list 'arglist fn)))

(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-keywords opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :wait wait opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

(defun pipe-output (prog &rest args)
  "Return an output stream which will go to the command."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :input :stream :wait nil)
  #+clisp (#+lisp=cl ext:make-pipe-output-stream
           #-lisp=cl lisp:make-pipe-output-stream
                     (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-input (ext:run-program prog args :input :stream
                                            :output t :wait nil))
  #+gcl (si::fp-input-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :output)
  #+lucid (lcl:run-program prog :arguments args :wait nil :output :stream)
  #+sbcl (sb-ext:process-input (sb-ext:run-program prog args :input :stream
                                                   :output t :wait nil))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'pipe-output prog args)))

(defun pipe-input (prog &rest args)
  "Return an input stream from which the command output will be read."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :output :stream :wait nil)
  #+clisp (#+lisp=cl ext:make-pipe-input-stream
           #-lisp=cl lisp:make-pipe-input-stream
                     (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-output (ext:run-program prog args :output :stream
                                             :error t :input t :wait nil))
  #+gcl (si::fp-output-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :input)
  #+lucid (lcl:run-program prog :arguments args :wait nil :input :stream)
  #+sbcl (sb-ext:process-output (sb-ext:run-program prog args :output :stream
                                                    :error t :input t :wait nil))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

;;; Allegro CL: a simple `close' does NOT get rid of the process.
;;; The right way, of course, is to define a Gray stream `pipe-stream',
;;; define the `close' method and use `with-open-stream'.
;;; Unfortunately, not every implementation supports Gray streams, so we
;;; have to stick with this to further the portability.
;;; [2005] actually, all implementations support Gray streams (see gray.lisp)
;;; but Gray streams may be implemented inefficiently

(defun close-pipe (stream)
  "Close the pipe stream."
  (declare (stream stream))
  (close stream)
  ;; CLOSE does not close constituent streams
  ;; CLOSE-CONSTRUCTED-STREAM:ARGUMENT-STREAM-ONLY
  ;; http://www.lisp.org/HyperSpec/Issues/iss052.html
  (typecase stream
    (two-way-stream
     (close (two-way-stream-input-stream stream))
     (close (two-way-stream-output-stream stream))))
  #+allegro (sys:reap-os-subprocess))

(defmacro with-open-pipe ((pipe open) &body body)
  "Open the pipe, do something, then close it."
  `(let ((,pipe ,open))
    (declare (stream ,pipe))
    (unwind-protect (progn ,@body)
      (close-pipe ,pipe))))

(defun gc ()
  "Invoke the garbage collector."
  #+abcl (ext:gc)
  #+allegro (excl:gc)
  #+clisp (#+lisp=cl ext:gc #-lisp=cl lisp:gc)
  #+cmu (ext:gc)
  #+cormanlisp (cl::gc)
  #+gcl (si::gbc)
  #+lispworks (hcl:normal-gc)
  #+lucid (lcl:gc)
  #+sbcl (sb-ext:gc)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'gc)))

(defun quit (&optional (exit-code 0))
  #+abcl (ext:quit exit-code)
  #+openmcl (ccl:quit exit-code)
  #+sbcl (sb-ext:quit :unix-status exit-code)
  #+clisp (ext:quit exit-code)
  #+(or cmu allegro) (declare (ignore exit-code))
  #+cmu (ext:quit exit-code)
  #+cormanlisp (win32:exitprocess exit-code)
  #+gcl (lisp:bye exit-code)
  #+lucid (lcl:quit exit-code)
  #+lispworks (lispworks:quit :status exit-code)
  #+allegro (excl:exit exit-code)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit exit-code)))

(defun getenv (var)
  #+allegro (sys:getenv var)
  #+clisp (ext:getenv var)
  #+cmu (cdr (assoc var ext:*environment-list* :test #'string=))
  #+lispworks (lw:environment-variable var)
  #+gcl (si:getenv var)
  #+lucid (lcl:environment-variable var)
  #+openmcl (ccl::getenv var)
  #+mcl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu lispworks openmcl openmcl sbcl)
  (error "Could not define `getenv'."))

(defun (setf getenv) (val var)
  "Set an environment variable."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl (sb-posix:putenv (format nil "~a=~a" var val))
  #-(or allegro clisp cmu gcl lispworks lucid scl sbcl)
  (error 'not-implemented :proc (list '(setf getenv) var)))

(defun argv ()
  "Return a list with each command line option as a string. The first
argument is the first argument to the program, similarly to $1 in
bash."
  (or #+sbcl (rest sb-ext:*posix-argv*)
      #+LISPWORKS (rest system:*line-arguments-list*)
      ;;; FIXME: cmucl returns the options to the lisp image, they
      ;;; need to be removed. The following code will only work if the
      ;;; lisp code was loaded with -load and not -eval
      #+cmu (rest (extensions:get-command-line-switch "load"))
      #+clisp ext:*args*
      nil))

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

(defclass timing-info ()
  ((real-time :accessor real-time :initarg :real-time
              :initform :not-available
              :documentation "Real time (also known as wall time)
              consumed. Expressed in milliseconds.")
   (user-time :accessor user-time :initarg :user-time
              :initform :not-available
              :documentation "User time. Expressed in milliseconds.")
   (system-time :accessor system-time :initarg :system-time
                :initform :not-available
                :documentation "System time. Expressed in milliseconds.")
   (gc-time :accessor gc-time :initarg :gc-time
            :initform :not-available
            :documentation "GC time. Expressed in milliseconds.")
   (page-faults :accessor page-faults :initarg :page-faults
                :initform :not-available
                :documentation "Number of page faults.")
   (bytes-consed :accessor bytes-consed :initarg :bytes-consed
                 :initform :not-available
                 :documentation "Number of bytes allocated."))
  (:documentation "Specificer for collect-timing info.

Every slot is either a number (with the exact meanining depending
on the slot) or the keyword :not-available in the case the lisp
doesn't provide this information."))

(defun pprint-milliseconds (milliseconds &optional stream)
  (cond
    ((< milliseconds 1000)
     (format stream "~D ms" milliseconds))
    ((= milliseconds 1000)
     (format stream "1.00 second"))
    ((< milliseconds (* 60 1000))
     (format stream "~,2F seconds" (/ milliseconds 1000)))
    ((= milliseconds (* 60 1000))
     (format stream "1.00 minute"))
    (t
     (format stream "~,2F minutes" (/ milliseconds (* 60 1000))))))

(defun pprint-bytes (num-bytes &optional stream)
  "Writes NUM-BYTES to stream, rounds num-bytes and appends a
suffix depending on the size of num-bytes."
  (cond
    ((< num-bytes (expt 2 10))
     (format stream "~D B" num-bytes))
    ((< num-bytes (expt 2 20))
     (format stream "~,2F KiB" (/ num-bytes (expt 2 10))))
    ((< num-bytes (expt 2 30))
     (format stream "~,2F MiB" (/ num-bytes (expt 2 20))))
    ((< num-bytes (expt 2 40))
     (format stream "~,2F GiB" (/ num-bytes (expt 2 30))))
    (t
     (format stream "~,2F TiB" (/ num-bytes (expt 2 40))))))

(defmethod print-object ((info timing-info) stream)
  (print-unreadable-object (info stream :type t :identity t)
    (format stream "~A/~A"
            (pprint-milliseconds (real-time info))
            (pprint-bytes (bytes-consed info)))))

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

#+(and clisp (not mop))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "SLOT-DEFINITION-NAME" "CLOS")
    (pushnew :MOP *features*)))

;; implementations with MOP-ish CLOS

(defun class-slots* (class)
  #+allegro (clos:class-slots class)
  #+clisp (clos::class-slots class)
  #+cmu (pcl::class-slots class)
  #+cormanlisp (cl:class-slots class)
  #+lispworks (hcl::class-slots class)
  #+lucid (clos:class-slots class)
  #+sbcl (sb-pcl::class-slots class)
  #+scl (clos:class-slots class))

(defun class-slots1 (obj)
  (class-slots* (typecase obj
                  (class obj)
                  (symbol (find-class obj))
                  (t (class-of obj)))))

(defun slot-name (slot)
  #+(and allegro (not (version>= 6))) (clos::slotd-name slot)
  #+(and allegro (version>= 6)) (clos:slot-definition-name slot)
  #+(and clisp (not mop)) (clos::slotdef-name slot)
  #+(and clisp mop) (clos::slot-definition-name slot)
  #+cmu (slot-value slot 'pcl::name)
  #+cormanlisp (getf slot :name)
  #+lispworks (hcl::slot-definition-name slot)
  #+lucid (clos:slot-definition-name slot)
  #+sbcl (slot-value slot 'sb-pcl::name)
  #+scl (clos:slot-definition-name slot))

(defun slot-initargs (slot)
  #+(and allegro (not (version>= 6))) (clos::slotd-initargs slot)
  #+(and allegro (version>= 6))
  (clos:slot-definition-initargs slot)
  #+(and clisp (not mop)) (clos::slotdef-initargs slot)
  #+(and clisp mop) (clos::slot-definition-initargs slot)
  #+cmu (slot-value slot 'pcl::initargs)
  #+cormanlisp (getf slot :initargs)
  #+lispworks (hcl::slot-definition-initargs slot)
  #+lucid (clos:slot-definition-initargs slot)
  #+sbcl (slot-value slot 'sb-pcl::initargs)
  #+scl (clos:slot-definition-initargs slot))

(defun slot-one-initarg (slot)
  (car (slot-initargs slot)))

(defun slot-alloc (slot)
  #+(and allegro (not (version>= 6)))
  (clos::slotd-allocation slot)
  #+(and allegro (version>= 6))
  (clos:slot-definition-allocation slot)
  #+(and clisp (not mop)) (clos::slotdef-allocation slot)
  #+(and clisp mop) (clos::slot-definition-allocation slot)
  #+cmu (pcl::slot-definition-allocation slot)
  #+cormanlisp (getf slot :allocation)
  #+lispworks (hcl::slot-definition-allocation slot)
  #+lucid (clos:slot-definition-allocation slot)
  #+sbcl (sb-pcl::slot-definition-allocation slot)
  #+scl (clos:slot-definition-allocation slot))

(defun class-slot-list (class &optional (all t))
  "Return the list of slots of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
  (mapcan (if all (compose #'list #'slot-name)
              (lambda (slot)
                (when (eq (slot-alloc slot) :instance)
                  (list (slot-name slot)))))
          (class-slots1 class)))

(defun class-slot-initargs (class &optional (all t))
  "Return the list of initargs of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
initargs for all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
  (mapcan (if all (compose #'list #'slot-one-initarg)
              (lambda (slot)
                (when (eq (slot-alloc slot) :instance)
                  (list (slot-one-initarg slot)))))
          (class-slots1 class)))

(defun structure-slots (struct)
  "Return the list of structure slot names."
  #+clisp (mapcar #'clos:slot-definition-name (ext:structure-slots struct))
  #-clisp (class-slot-list (find-class struct)))

(defun structure-keyword-constructor (struct)
  "Return the structure keyword constructor name."
  #+clisp (ext:structure-keyword-constructor struct)
  #-clisp                               ; LAME!!!
  (let ((s (concatenate 'string "MAKE-" (symbol-name struct)))
        (p (symbol-package struct)))
    (or (find-symbol s p)
        (error "~S(~S): no symbol ~S in ~S"
	       'structure-keyword-constructor struct s p))))

(defun structure-boa-constructors (struct)
  "Return the list of structure BOA constructor names."
  (declare (ignorable struct))
  #+clisp (ext:structure-boa-constructors struct)
  #-clisp nil)                          ; what else?


(defun structure-copier (struct)
  "Return the structure copier name."
  #+clisp (ext:structure-copier struct)
  #-clisp                               ; LAME!!!
  (let ((s (concatenate 'string "COPY-" (symbol-name struct)))
        (p (symbol-package struct)))
    (or (find-symbol s p)
        (error "~S(~S): no symbol ~S in ~S"
               'structure-copier struct s p))))

(defun structure-predicate (struct)
  "Return the structure predicate name."
  #+clisp (ext:structure-predicate struct)
  #-clisp                               ; LAME!!!
  (let ((s (concatenate 'string (symbol-name struct) "-P"))
        (p (symbol-package struct)))
    (or (find-symbol s p)
        (error "~S(~S): no symbol ~S in ~S"
               'structure-predicate struct s p))))
 ; macrolet

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow "DEFSTRUCT") (export (intern "DEFSTRUCT")))
#+cmu
(defmacro defstruct (name &rest slots)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defstruct ,name ,@slots))
     ,(unless (and (consp name) (assoc :type (cdr name)))
       `(defmethod make-load-form ((self ,(if (consp name) (first name) name))
                                   &optional environment)
          (make-load-form-saving-slots self :environment environment)))))

(defun sysinfo (&optional (out *standard-output*))
  "Print the current environment to a stream."
  (declare (stream out))
  (format out "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~%~
Implementation:~20t~a~%~7tversion:~20t~a~%Machine:  type:~20t~a
~7tversion:~20t~a~%~6tinstance:~20t~a~%Opeating System:~19t"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) (machine-version) (machine-instance))
  #+(or (and clisp win32) (and allegro mswindows))
  (let* ((root-nt "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion")
         (root-9x "SOFTWARE\\Microsoft\\Windows\\CurrentVersion")
         (key-nt #+(and clisp win32) root-nt
                 #+(and allegro mswindows)
                 (ole:open-registry-key ole:rkey-local-machine root-nt))
         (key-9x #+(and clisp win32) root-9x
                 #+(and allegro mswindows)
                 (ole:open-registry-key ole:rkey-local-machine root-9x)))
    (labels ((9x-p ()
               #+(and clisp win32) (sys::registry key-9x "ProductName")
               #+(and allegro mswindows)
               (ole:registry-value key-9x "ProductName"))
             (env (str)
               #+(and clisp win32)
               (sys::registry (if (9x-p) key-9x key-nt) str)
               #+(and allegro mswindows)
               (ole:registry-value (if (9x-p) key-9x key-nt) str)))
      (if (9x-p)
          (format out " ~a (~a - ~a; boot: ~a)~%~5tRegistered to: ~a, ~a [~a]"
                  (env "ProductName") (env "Version") (env "VersionNumber")
                  (env "BootCount") (env "RegisteredOwner")
                  (env "RegisteredOrganization") (env "ProductId"))
          (format
           out " WinNT ~a (build ~a: ~a) ~a~%~5tRegistered to: ~a, ~a [~a]"
           (env "CurrentVersion") (env "CurrentBuildNUmber") (env "CSDVersion")
           (env "CurrentType") (env "RegisteredOwner")
           (env "RegisteredOrganization") (env "ProductId")))))
  #+os/2 (princ " OS/2")
  #+unix (princ " Unix")
  #+dos (princ " DOS")
  #+pc386 (princ " PC386")
  #+amiga (princ " Exec/AmigaDOS")
  (format out "~%Software: type:~20t~a~%~7tversion:~20t~a~%Site:~20t~a (~a)
User home:~20t~a~%Current directory:~20t~a~%Default pathname:~20t~a
Features:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.
Modules:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~%Current package:~30t~a~%"
          (software-type) (software-version) (long-site-name)
          (short-site-name) (user-homedir-pathname) (current-directory)
          *default-pathname-defaults* *features* *modules* *package*)
  #+clisp (format out "[CLISP] Current language:~30t~a~%"
                  (sys::current-language))
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format out "Fixnum length:~25t~3d bits
Short Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Single Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Double Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Long Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~%"
            (integer-length most-positive-fixnum)
            (exdi most-positive-short-float)
            (float-digits most-positive-short-float)
            (exdi most-positive-single-float)
            (float-digits most-positive-single-float)
            (exdi most-positive-double-float)
            (float-digits most-positive-double-float)
            (exdi most-positive-long-float)
            (float-digits most-positive-long-float)))
  #+clisp (format out "[CLISP] long-float-digits:~44t~3d~%"
                  #+lisp=cl (ext:long-float-digits)
                  #-lisp=cl (lisp:long-float-digits))
  (dolist (sy '(array-total-size-limit array-rank-limit array-dimension-limit
                lambda-parameters-limit call-arguments-limit
                multiple-values-limit char-code-limit))
    (format out " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format out "lambda-list-keywords:~30t~{~<~%~30t~1,74:; ~a~>~}~%"
          lambda-list-keywords)
  (format out "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d
Current time:~25t" (/ internal-time-units-per-second) *gensym-counter*)
  ;; FIXME
  ;;(current-time out)
  (format out "~%~75~~%")
  (room)
  (values))


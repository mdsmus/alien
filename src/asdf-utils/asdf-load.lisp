;; from xach blog
;; http://xach.livejournal.com/191187.html

(require 'asdf)

(in-package #:asdf)

(export '(load* reload register register-permanently))

(defvar *registry-file*
  (merge-pathnames (make-pathname :directory '(:relative "asdf")
                                  :name "registry"
                                  :type "sexp")
                   (user-homedir-pathname)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((registry (probe-file *registry-file*)))
    (when registry
      (with-open-file (stream registry)
        (loop for form = (read stream nil)
              while form do (push form *central-registry*))
        (setf *central-registry*
              (remove-duplicates *central-registry* :test #'equalp))))))

(defun load* (system &key verbose)
  (oos 'load-op system :verbose verbose)
  t)

(defun reload (system)
  (let ((name (coerce-name system)))
    (remhash name *defined-systems*)
    (load* name)))

(defun register (form)
  (pushnew form *central-registry* :test #'equalp))

(defun register-permanently (form)
  (register form)
  (ensure-directories-exist *registry-file*)
  (with-open-file (stream *registry-file*
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (prin1 form stream)
    (terpri stream)))

;;; Automatically recompile stale FASLs

(handler-bind ((style-warning #'muffle-warning))
  (defmethod perform :around ((o load-op) (c cl-source-file))
    (handler-case (call-next-method o c)
      (#+sbcl sb-ext:invalid-fasl
       #-sbcl error ()
       (perform (make-instance 'compile-op) c)
       (call-next-method)))))

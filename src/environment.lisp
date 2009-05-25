(in-package :cl-extensions)

(defun quit (&optional (exit-code 0))
  #+openmcl (ccl:quit exit-code)
  #+sbcl (sb-ext:quit :unix-status exit-code)
  #+clisp (ext:quit exit-code)
  #+(or cmu allegro) (declare (ignore exit-code))
  #+cmu (ext:quit)
  #+lispworks (lispworks:quit :status exit-code)
  #+allegro (excl:exit))

(defun getenv (var)
  #+allegro (sys:getenv var)
  #+clisp (ext:getenv var)
  #+cmu
  (cdr (assoc var ext:*environment-list* :test #'string=))
  #+lispworks (lw:environment-variable var)
  #+openmcl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)

  #-(or allegro clisp cmu lispworks openmcl openmcl sbcl)
  (error "Could not define `getenv'."))

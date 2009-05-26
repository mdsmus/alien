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

  #+cmu (subseq cmu-args (1+ (position "cmurameau" extensions:*command-line-strings* :test #'string=)))

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



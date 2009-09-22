(in-package :cl-ext)


;; it's on by default
;;(pushnew :cl-fad *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

;; TODO: deal with hyperdoc
#-:abcl
(defvar *hyperdoc-base-uri* "http://weitz.de/cl-fad/")

#-:abcl
(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-fad
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))

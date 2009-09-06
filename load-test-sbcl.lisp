(load "load-sbcl")

(push (merge-pathnames "tests/" *DEFAULT-PATHNAME-DEFAULTS*) asdf:*central-registry*)

;;; these 2 are temporary
(push "/home/kroger/lisp/lisp-libs/flexi-streams-1.0.7/" asdf:*central-registry*)
(push "/home/kroger/lisp/lisp-libs/trivial-gray-streams-2008-11-02/" asdf:*central-registry*)

(require :alien)
(require :regexp-test)
(require :unittest-test)

(cl-ppcre-test:run-all-tests)
(unittest:run! :unittest-test)

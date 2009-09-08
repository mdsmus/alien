(require :asdf)

(push (concatenate 'string (namestring (ccl::current-directory-name)) "/src/")
      asdf:*central-registry*)

(asdf:oos 'asdf:load-op :alien)

(push (concatenate 'string (namestring (ccl::current-directory-name)) "/tests/")
      asdf:*central-registry*)

;;; these 2 are temporary, required by cl-ppcre tests
(push "/home/kroger/lisp/lisp-libs/flexi-streams-1.0.7/" asdf:*central-registry*)
(push "/home/kroger/lisp/lisp-libs/trivial-gray-streams-2008-11-02/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op :alien)
(asdf:oos 'asdf:load-op :regexp-test)
(asdf:oos 'asdf:load-op :unittest-test)
(asdf:oos 'asdf:load-op :cl-extensions-test)

;;(cl-ppcre-test:run-all-tests)
;;(unittest:run! :unittest-test)
(unittest:run! :cl-ext-tests)
(quit)

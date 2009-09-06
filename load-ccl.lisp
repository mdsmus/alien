(require :asdf)

(push (concatenate 'string (namestring (ccl::current-directory-name)) "/src/")
      asdf:*central-registry*)

(asdf:oos 'asdf:load-op :alien)
(quit)

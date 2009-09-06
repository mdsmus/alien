(require :asdf)

(push (merge-pathnames "src/" *DEFAULT-PATHNAME-DEFAULTS*) asdf:*central-registry*)

(require :alien)

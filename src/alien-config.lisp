(asdf:load-system :asdf-binary-locations)

(setf asdf:*centralize-lisp-binaries*   t
      asdf:*default-toplevel-directory* (merge-pathnames ".lisp/" (user-homedir-pathname))
      asdf:*include-per-user-information* nil)


;;;; -*- lisp -*-

(in-package :unittest)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :it.bese)
    (def-suite :it.bese)))

(def-suite :unittest :in :it.bese)

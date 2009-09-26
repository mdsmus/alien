;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2009 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau                            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+xcvb (module (:depends-on ("pkgdcl")))
;; let's not DEPEND on (:build "/cl-launch") since
;; that opens a can of worm for people using ASDF.

(in-package :command-line-arguments)

#+cl-launch
(defun get-command-line-arguments ()
  cl-launch:*arguments*)

#-cl-launch
(defun get-command-line-arguments ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure (cdr (ccl::command-line-arguments))
  #+gcl (cdr si:*command-args*)
  #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
  #+cmu (cdr extensions:*command-line-strings*)
  #+allegro (cdr (sys:command-line-arguments))
  #+lispworks (cdr sys:*line-arguments-list*)
  #+clisp ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  (error "get-command-line-arguments not supported for your implementation"))

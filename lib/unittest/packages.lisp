;; -*- lisp -*-

(defpackage :unittest
  (:use :common-lisp :cl-ext)
  (:export ;; creating tests and test-suites
           #:make-suite
	   #:def-suite
	   #:in-suite
	   #:in-suite*
	   #:make-test
	   #:test
	   #:get-test
	   #:rem-test
           #:test-names
	   ;; fixtures
	   #:make-fixture
	   #:def-fixture
	   #:with-fixture
	   #:get-fixture
	   #:rem-fixture
	   ;; running checks
           #:is
           #:is-every
           #:is-true
           #:is-false
           #:signals
           #:finishes
           #:skip
	   #:pass
	   #:fail
	   #:*test-dribble*
           #:for-all
           #:gen-integer
           #:gen-float
           #:gen-character
           #:gen-string
           #:gen-list
           #:gen-tree
           #:gen-buffer
           #:gen-one-element
	   ;; running tests
           #:run
           #:run-all-tests
           #:explain
           #:explain!
           #:run!
           #:debug!
           #:!
           #:!!
           #:!!!
           #:*run-test-when-defined*
	   #:*debug-on-error*
           #:*debug-on-failure*
           #:*verbose-failures*
           #:results-status))

;;;; You can use #+5am to put your test-defining code inline with your
;;;; other code - and not require people to have fiveam to run your
;;;; package.

(pushnew :unittest *features*)

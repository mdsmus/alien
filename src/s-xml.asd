;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: s-xml.asd,v 1.3 2008-02-15 13:54:57 scaekenberghe Exp $
;;;;
;;;; The S-XML ASDF system definition
;;;;
;;;; Copyright (C) 2002, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-xml
  :name "S-XML"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "3"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>, Brian Mastenbrook <>, Rudi Schlatte <>"
  :licence "Lisp Lesser General Public License (LLGPL)"
  :description "Simple Common Lisp XML Parser"
  :long-description "S-XML is a Common Lisp implementation of a simple XML parser, with a SAX-like and DOM interface"

  :components
  ((:module "s-xml"
    :components ((:file "package")
                 (:file "xml" :depends-on ("package"))
                 (:file "dom" :depends-on ("package" "xml"))
                 (:file "lxml-dom" :depends-on ("dom"))
                 (:file "sxml-dom" :depends-on ("dom"))
                 (:file "xml-struct-dom" :depends-on ("dom"))))))

(defsystem :s-xml.test
  :depends-on (:s-xml)
  :components ((:module :test
		:components ((:file "test-xml")
 			     (:file "test-xml-struct-dom")
			     (:file "test-lxml-dom")
 			     (:file "test-sxml-dom")))))

(defsystem :s-xml.examples
  :depends-on (:s-xml)
  :components ((:module :examples
		:components ((:file "counter")
			     (:file "echo")
			     (:file "remove-markup")
			     (:file "tracer")))))
;;;; eof


;;;;
;;;; file: examples/simple.lisp
;;;; 
;;;; Example CGI script for lisp-cgi-utils, simple test
;;;;
;;;; Copyright (C) 2003,2004 Alexander Schreiber
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation;
;;;; version 2 of the License.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
;;;; author : Alexander Schreiber <als@thangorodrim.de>
;;;; version: $Id: simple.lisp 1517 2006-10-26 19:44:51Z als $
;;;;


(load "../http.lisp")
(load "../html.lisp")

(http:http-init)
(http:http-send-headers)

(princ (html:html-header "html.lisp example page"))

(princ (html:body
        (html:h1 "test page for html.lisp package")
        (html:h2 "lists")
        (html:ul 
         (html:li "first entry")
         (html:li "second entry"))
        (html:p "a simple paragraph" " and more text")
        (html:p '((class . "tester")) "another one")))
(princ (html:html-footer))

(quit)

;;;;
;;;; file: examples/cookies.cgi
;;;; 
;;;; Example CGI script for lisp-cgi-utils, shows use of cookies
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
;;;; version: $Id: cookies.lisp 1517 2006-10-26 19:44:51Z als $
;;;;


(load "../http.lisp")
(load "../html.lisp")


(defun header (title)
  "Set up the page head"
  (http:http-send-headers)
  (princ (html:html-header title)))

(defun iso8601-timestamp (&optional universal-time)
  "Returns an ISO8601 timestamp, using either current time or the
supplied universal time."
    (multiple-value-bind (ss mm hh dd mo yy dow dst tz)
      (decode-universal-time 
       (if (integerp universal-time)
           universal-time
           (get-universal-time)))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            yy mo dd hh mm ss)))


(defun make-standard-cookies ()
  "Create a set of standard cookies"
  (http:http-add-cookie "timestamp" 
                        (iso8601-timestamp)
                        :Path "/lisp")
  (http:http-add-cookie "application" "lisp-cgi-utils cookietest" 
                        :Path "/lisp")
  (http:http-add-cookie "magic-word" "gogglefrog" 
                        :Path "/lisp"
                        :Max-Age 128)
  (http:http-add-cookie "nasty-one" 
                        "this ; is ; a ; list = true (\\\"false\\\")"))


(defun simple-form-query (text)
  "Prints a simple form, using selfreferential POST"
  (html:table
   (html:tr
    (html:td "your input: ")
    (html:td
     (if (null (http:http-query-parameter "some-string"))
         "no input given"
         (html:kbd
          (http:http-query-parameter "some-string")))))
   (html:tr
    (html:td "some-string: ")
    (html:td
     (html:form-self-post
      (html:p ; needed to satisfy HTML 4.0 strict
       (html:input (pairlis 
                    '(type size name value)
                    (append
                     (list "text" "64" "some-string")
                     (list 
                      (if (null (http:http-query-parameter "some-string"))
                          text
                          (http:http-query-parameter "some-string"))))))
      (html:form-reset-submit)))))))

(defun show-cookies ()
  "show all received cookies"
  (concatenate 'string
               (html:h2 "cookies sent by user agent")
               (html:table '((border . "1"))
                (html:tr
                 (html:th "name")
                 (html:th "value"))
                (apply #'concatenate 'string
                       (mapcar #'(lambda (name)
                                   (html:tr
                                    (html:td
                                     (html:code name))
                                    (html:td
                                     (html:code (http:http-cookie name)))))
                               (http:http-cookie-list))))))
               
;;;; ------------ main -------------------

(http:http-init)
(make-standard-cookies)
(header "lisp-cgi-utils example - cookies")

(princ (html:body
        (html:h1 "lisp-cgi-utils example - cookies")))

(princ (simple-form-query "enter something"))
(princ (show-cookies))
(princ (html:html-footer))

;--------------------


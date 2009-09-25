;;;;
;;;; file: examples/forms.lisp
;;;; 
;;;;
;;;; Forms using example for lisp-cgi-utils
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
;;;; version: $Id: forms.lisp 1517 2006-10-26 19:44:51Z als $
;;;;


(load "../http.lisp")
(load "../html.lisp")



(defun header (title)
  "Set up the page head"
  (http:http-send-headers)
  (princ (html:html-header title)))



(defun simple-form-query (text)
  "Prints a simple form, using selfreferential POST"
  (html:table
   (html:tr
    (html:td "your input: ")
    (html:td
     (if (null (http:http-query-parameter "some-string"))
         "no input given"
         (html:kbd
          (http::url-encode-string
           (http:http-query-parameter "some-string"))))))
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

(defun form-multi-select-query ()
  "Prints a multi selector and the selected results."
  (html:table
   (html:tr
    (html:td "you selected: ")
    (html:td 
     (if (null (http:http-query-parameter "multi-select"))
         "no input given"
         (html:kbd
                (if (null (listp (http:http-query-parameter "multi-select")))
                        (format nil "~a"
                                (http:http-query-parameter "multi-select"))
                        (format nil "~{~a ~}"
                        (http:http-query-parameter "multi-select")))))))

   
   (html:tr
    (html:td "select one or more: ")
    (html:td
     (html:form-self-post
      (html:p
       (html:form-select-multiple "multi-select"
                                  (list 
                                   "one" "two" "three" "four" "five" "six")))
      (html:form-reset-submit))))))



(defun hash-to-proplist (in-hash)
  (let ((prop-list '()))
    (maphash (lambda (key value)
               (setf prop-list 
                     (append prop-list 
                             (list (cons key value)))))
             in-hash)
    prop-list))

(defun list-env-vars ()
  "List all availabe CGI environment variables."
  (concatenate 'string
               (html:h2 "CGI environment variables")
               (html:table
                (html:tr
                 (html:th '(("align" . "left")) "variable")
                 (html:th '(("align" . "left")) "value"))
                (apply #'concatenate 'string
                       (mapcar (lambda (item)
                                 (html:tr
                                  (html:td '(("align" . "left")) 
                                           (car item))
                                  (html:td '(("align" . "left")) 
                                           (cdr item))))
                               (hash-to-proplist 
                                (http:http-get-env-vars)))))))
                       
               
;;;; ------------ main -------------------

(http:http-init)

(header "lisp-cgi-utils example - forms")

(princ (html:body
        (html:h1 "lisp-cgi-utils example - forms")
        (simple-form-query "enter some text ...")
        (form-multi-select-query)
        (list-env-vars)))

(princ (html:html-footer))

(quit)
;--------------------


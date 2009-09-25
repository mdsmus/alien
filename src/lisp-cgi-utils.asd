;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lisp-cgi-utils.asd
;;;; Purpose:       ASDF definition file for lisp-cgi-utils
;;;; Author:        Alexander Schreiber
;;;; Date Started:  2004-07-30
;;;;
;;;; $Id: lisp-cgi-utils.asd 1409 2005-08-07 13:52:05Z als $
;;;;


(defpackage #:lisp-cgi-utils-system (:use #:asdf #:cl))

(in-package :lisp-cgi-utils-system)

(defsystem lisp-cgi-utils
  :name "cl-lisp-cgi-utils"
  :author "Alexander Schreiber"
  :version "0.8"
  :maintainer "Alexander Schreiber <als@thangorodrim.de>"
  :licence "LGPL"
  :description "Lisp CGI utilitiles"
  :long-description "THis package contains code to support writing CGI applications in Common lisp."
    
  :components ((:module "lisp-cgi-utils"
                        :serial t
                        :components ((:file "http")
                                     (:file "html")))))



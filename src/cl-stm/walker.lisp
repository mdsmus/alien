;; -*- lisp -*-

(in-package :cl-stm)

(eval-always
  (enable-pf-reader))

;;;; * Walking

;;;; ** Interface

(defgeneric stm (form)
  (:documentation "Translate FORM into an STM action.

Define methods on this generic function with DEFSTM-WALKER.")
  (:method (form)
    "If FORM isn't a FORM object, we'll convert it to one, apply
the transformation and convert it back."
    (unwalk-form (stm (walk-form form)))))

(defgeneric unstm (form)
  (:documentation "Translate FORM from a STM action to a Lisp
form.

Define methods on this generic function with DEFUNSTM-WALKER."))

(defmacro with-stm (form)
  "Execute FORM as if it were run in the STM."
  (stm form))

;;;; ** Current form

(defvar *form* nil
  "This contains the current form that is being walked.")

;;;; ** Defining

(defmacro defstm-walker (class (&rest slots) &body body)
  "Define a STM walker.

It takes the class of a CL form object, and its slots as
arguments.  It binds the form object to the variable *FORM*."
  (with-unique-names (form)
    `(defmethod stm ((,form ,class))
       (let1 *form* ,form
         (with-slots ,slots *form*
           ,@body)))))

(defmacro defunstm-walker (class (&rest slots) &body body)
  "Define a UNSTM walker.

It takes the class of a CL form object, and its slots as
arguments.  It binds the form object to the variable *FORM*."
  (with-unique-names (form)
    `(defmethod unstm ((,form ,class))
       (let1 *form* ,form
         (with-slots ,slots *form*
           ,@body)))))

;;;; ** Special Variables

(defvar *trans-funs* '((orelse t t)
                       (bind   t t)
                       (retry))
  "When walking code, this variable contains a list of
functions (represented by symbols) which return transactions when
applied.")

(defvar *trans-vars* '()
  "When walking code, this variable contains a list of
variables (represented by symbols) which are transactions.")

(defmacro with-trans-funs (funs &body body)
  `(let1 *trans-funs* (append *trans-funs* ,funs)
     ,@body))

(defmacro with-trans-vars (vars &body body)
  `(let1 *trans-vars* (append *trans-vars* ,vars)
     ,@body))

;;;; ** New forms

(defclass trans-form (form)
  ((form :accessor form
         :initarg :form)))

(defclass untrans-form (form)
  ((form :accessor form
         :initarg :form)))

(defwalker-handler trans (form parent env)
  (unless (= 2 (length form))
    (error "Invalid TRANS form ~A" form))
  (new 'trans-form
       :form   (walk-form (cadr form) parent env)
       :source form
       :parent parent))

(defwalker-handler untrans (form parent env)
  (unless (= 2 (length form))
    (error "Invalid UNTRANS form ~A" form))
  (new 'untrans-form
       :form   (walk-form (cadr form) parent env)
       :source form
       :parent parent))

(defunwalker-handler trans-form (form)
  `(trans ,(unwalk-form form)))

(defunwalker-handler untrans-form (form)
  `(untrans ,(unwalk-form form)))

;;;; ** Utilities

(defun stms (forms)
  (mapcar #'stm forms))

(defun unstms (forms)
  (mapcar #'unstm forms))

(defun stmb (binds)
  (mapcar (lambda (bind)
            (cons (car bind)
                  (stm (cdr bind))))
          binds))

(defun unstmb (binds)
  (mapcar (lambda (bind)
            (cons (car bind)
                  (unstm (cdr bind))))
          binds))

(defgeneric trans-form (form)
  (:method ((form form))
    (new 'trans-form :form form))
  (:method ((form untrans-form))
    (form form)))

(defgeneric untrans-form (form)
  (:method ((form form))
    (new 'untrans-form :form form))
  (:method ((form trans-form))
    (form form)))

(defgeneric transaction-function? (form)
  (:method ((form constant-form))
    (unless (symbolp (value form))
      (error "Invalid function name ~A" (value form)))
    (transaction-function? (value form)))
  (:method ((form function-object-form))
    (etypecase (name form)
      (symbol (transaction-function? (name form)))
      (cons   (and (eq (car (name form)) 'cl:setf)
                   (transaction-function? (cadr (name form)))))))
  (:method ((form symbol))
    (assoc form *trans-funs*))
  (:method ((form form))
    nil))

(defgeneric transaction-arguments (form)
  (:method ((form constant-form))
    (unless (symbolp (value form))
      (error "Invalid function name ~A" (value form)))
    (transaction-arguments (value form)))
  (:method ((form function-object-form))
    (transaction-arguments (name form)))
  (:method ((form symbol))
    (append (cdr (assoc form *trans-funs*))
            (circularize nil)))
  (:method ((form form))
    nil))

;;;; ** Declarations

(defun extract-type-declarations (decls)
  (mapcar #'name
          (remove-if-not (lambda (decl)
                           (and (typep decl 'arnesi::type-declaration-form)
                                (subtypep (type-form decl) 'cl-stm:transaction)))
                         decls)))

(defun extract-ftype-declarations (decls)
  (mapcar #'name
          (remove-if-not (lambda (decl)
                           (and (typep decl 'arnesi::ftype-declaration-form)
                                (subtypep (last1 (type-form decl)) 'cl-stm:transaction)))
                         decls)))

(defmacro with-declarations (decls &body body)
  `(let ((*trans-funs* (append (extract-ftype-declarations ,decls) *trans-funs*))
         (*trans-vars* (append (extract-type-declarations  ,decls) *trans-vars*)))
     ,@body))

;;;; ** STM Walker

(defstm-walker untrans-form (form)
  form)

(defstm-walker constant-form ()
  (trans-form *form*))

(defstm-walker variable-reference (name)
  (if (member name *trans-vars*)
      *form*
      (trans-form *form*)))

(defstm-walker if-form (consequent then else)
  (trans-form
   (new 'if-form
        :consequent (unstm (stm consequent))
        :then       (unstm (stm then))
        :else       (unstm (stm else)))))

(defstm-walker progn-form (body)
  (if (null (cdr body))
      (trans-form (unstm (stm (car body))))
      (trans-form
       (new 'progn-form
            :body (unstms (stms body))))))

(defstm-walker progv-form (vars-form values-form body)
  (trans-form
   (new 'progv-form
        :vars-form   (unstm  (stm  vars-form))
        :values-form (unstm  (stm  values-form))
        :body        (unstms (stms body)))))

(defstm-walker variable-binding-form (binds body declares)
  (trans-form
   (new (class-name-of *form*)
        :binds    (unstmb (stmb binds))
        :declares declares
        :body
        (with-declarations declares
          (unstms (stms body))))))

(defstm-walker labels-form (binds body declares)
  (with-trans-funs (mapcar (compose #'list #'car) binds)
    (trans-form
     (new 'labels-form
          :binds    (unstmb (stmb binds))
          :declares declares
          :body
          (with-declarations declares
            (unstms (stms body)))))))

(defstm-walker flet-form (binds body declares)
  (trans-form
   (new 'flet-form
        :binds    (unstmb (stmb binds))
        :declares declares
        :body
        (with-declarations declares
          (with-trans-funs (mapcar (compose #'list #'car) binds)
            (unstms (stms body)))))))

(defstm-walker lambda-function-form (arguments body declares)
  (trans-form
   (new 'lambda-function-form
        :arguments arguments
        :declares  declares
        :body      (list (stm (new 'progn-form :body body))))))

(defstm-walker setq-form (var value)
  (trans-form
   (new 'setq-form
        :var   var
        :value (unstm (stm value)))))

(defstm-walker symbol-macrolet-form (body binds declares)
  ;; We ignore the binds, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore binds))
  (trans-form
   (new 'locally-form
        :declares declares
        :body     (unstms (stms body)))))

(defstm-walker macrolet-form (body binds declares)
  ;; We ignore the binds, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore binds))
  (trans-form
   (new 'locally-form
        :declares declares
        :body     (unstms (stms body)))))

(defstm-walker locally-form (body declares)
  (trans-form
   (new 'locally-form
        :declares declares
        :body     (unstms (stms body)))))

(defstm-walker application-form (operator arguments)
  (cond ((transaction-function? operator)
         (new 'free-application-form
              :operator  operator
              :arguments (mapcar (lambda (arg trans?)
                                   (if trans?
                                       (stm arg)
                                       (unstm (stm arg))))
                                 arguments
                                 (transaction-arguments operator))))
        ((member operator '(funcall apply))
         (if (transaction-function? (car arguments))
             (new 'free-application-form
                  :operator  operator
                  :arguments (cons (unstm (stm (car arguments)))
                                   (mapcar (lambda (arg trans?)
                                             (if trans?
                                                 (stm arg)
                                                 (unstm (stm arg))))
                                           (cdr arguments)
                                           (transaction-arguments (car arguments)))))
             (trans-form
              (new 'free-application-form
                   :operator  operator
                   :arguments (cons (unstm  (stm  (car arguments)))
                                    (unstms (stms (cdr arguments))))))))
        (t (trans-form
            (new 'free-application-form
                 :operator  operator
                 :arguments (unstms (stms arguments)))))))

(defstm-walker function-object-form ()
  (trans-form *form*))

(defstm-walker lambda-application-form (operator arguments)
  (new 'lambda-application-form
       :operator  (unstm  (stm  operator))
       :arguments (unstms (stms arguments))))

(defstm-walker multiple-value-call-form (func arguments)
  (if (transaction-function? func)
      (new 'multiple-value-call-form
           :func      (unstm (stm func))
           :arguments (mapcar (lambda (arg trans?)
                                (if trans?
                                    (stm arg)
                                    (unstm (stm arg))))
                              arguments
                              (transaction-arguments func)))
      (trans-form
       (new 'multiple-value-call-form
            :func      (unstm  (stm  func))
            :arguments (unstms (stms arguments))))))

(defstm-walker multiple-value-prog1-form (first-form other-forms)
  (trans-form
   (new 'multiple-value-prog1-form
        :first-form  (unstm  (stm  first-form))
        :other-forms (unstms (stms other-forms)))))

(defstm-walker block-form (name body)
  (trans-form
   (new 'block-form
        :name name
        :body (unstms (stms body)))))

(defstm-walker return-from-form (target-block result)
  (trans-form
   (new 'return-from-form
        :target-block target-block
        :result       (unstm (stm result)))))

(defstm-walker catch-form (tag body)
  (trans-form
   (new 'catch-form
        :tag  (unstm  (stm  tag))
        :body (unstms (stms body)))))

(defstm-walker throw-form (tag value)
  (trans-form
   (new 'throw-form
        :tag   (unstm (stm tag))
        :value (unstm (stm value)))))

(defstm-walker tagbody-form (body)
  (trans-form
   (new 'tagbody-form
        :body (unstms (stms body)))))

(defstm-walker go-tag-form ()
  (trans-form *form*))

(defstm-walker go-form ()
  (trans-form *form*))

(defstm-walker unwind-protect-form (protected-form cleanup-form)
  (trans-form
   (new 'unwind-protect-form
        :protected-form (unstm  (stm  protected-form))
        :cleanup-form   (unstms (stms cleanup-form)))))

(defstm-walker the-form (type-form value)
  (trans-form
   (new 'the-form
        :type-form type-form
        :value     (unstm (stm value)))))

(defstm-walker eval-when-form (eval-when-times body)
  (trans-form
   (new 'eval-when-form
        :eval-when-times eval-when-times
        :body            (unstms (stms body)))))

;;;; ** UNSTM Walker

(defunstm-walker form ()
  *form*)

(defunstm-walker trans-form (form)
  form)

(defunstm-walker variable-reference (name)
  (if (member name *trans-vars*)
      (untrans-form *form*)
      *form*))

(defunstm-walker application-form (operator arguments)
  (if (or (transaction-function? operator)
          (and (member operator '(funcall apply))
               (transaction-function? (car arguments))))
      (untrans-form *form*)
      *form*))

(defunstm-walker lambda-application-form ()
  (untrans-form *form*))

;; Copyright (c) 2006 Hoan Ton-That
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Hoan Ton-That, nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

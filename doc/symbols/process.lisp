(defun replace-all (part string replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part is
replaced with replacement. It was taken from the cl-cookbook, it's not
as optimized as cl-ppcre, but if you just want to replace strings it
may be faster in some cases."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

;; type-specifier: mod, satisfies, member, not, and,or, values, eql
;; local-function: next-method-p, call-next-method
;; local-macro: loop-finish, call-method, make-method, pprint-exit-if-list-exhausted pprint-pop
;; special-form: setq
;; symbol: lambda declare

;;; need different way of typesetting
;; "condition-type"
;; "declaration"
;; "restart"
;; "type"

(defparameter *symbols*
  (sort (with-open-file (s "/home/kroger/src/common-lisp-distribution/doc/symbols/all-symbols.lisp")
          (read s))
        #'string< :key #'third))

(defparameter *table-convert* '("accessor" "system-class" "class" "constant" "function"
                                "generic" "macro" "special-operator" "variable"))

(defparameter *cl-symbols*
  (loop for s being the external-symbols of (find-package "CL")
        collect s))

(defparameter *categories*
  (remove-duplicates (mapcar #'third *symbols*) :test #'equal))

(defun generate-latex (stream type symbol args doc)
  (let ((type (cond ((equal type "system-class") "class")
                    ((equal type "special-operator") "specialop")
                    (t type)))
        (doc (or doc "")))
    (format stream "~%\\begin{~a}{~a}{~a}{}{}~%  ~a~%\\end{~a}~%" type symbol args doc type)))

(defun process-args (list)
  (loop for x in list collect
        (if (listp x)
            (first x)
            x)))

(defun parse-args (list)
  (if list
      (let* ((s (process-args list))
             (s (format nil "~a" s))
             (s (subseq (string-downcase s) 1 (1- (length s))))
             (s (replace-all "&rest" s "\\rest"))
             (s (replace-all "&optional" s "\\op"))
             (s (replace-all "&environment" s "\\env"))
             (s (replace-all "&whole" s "\\whole"))
             (s (replace-all "&allow-other-keys" s "\\akeys"))
             (s (replace-all "&key" s "\\key"))
             (s (replace-all "&aux" s "\\aux"))
             (s (replace-all "&body" s "\\body"))
             (s (replace-all "#" s "\\#")))
        s)
      ""))

(defun parse-doc (string)
  (let ((size (length string)))
    (if (plusp size)
        (let* ((s (replace-all "~" string "\\~{}%"))
               (s (replace-all "&" s "\\&"))
               (s (replace-all "%" s "\\%"))
               (s (replace-all "#" s "\\#")))
          s))))


(defun get-symbols (category)
  (sort (remove-if-not (lambda (item) (equal (third item) category)) *symbols*)
        #'string< :key #'second))

(defparameter *dir* "/home/kroger/src/common-lisp-distribution/doc/auto-tex/")

(defun export-to-latex ()
  (loop for category in *categories* do
        (with-open-file (s (make-pathname :directory *dir* :name category :type "tex")
                           :direction :output :if-exists :supersede)
          (format s "~%\\section{~a}~%" category)
          (loop for (symbol type category) in (get-symbols category)
                do
                (let* ((isymbol (intern (string-upcase symbol)))
                       (args (when (fboundp isymbol)
                               (sb-introspect:function-lambda-list isymbol)))
                       (doc (parse-doc (documentation isymbol 'function))))
                  (if (member type *table-convert* :test #'equal)
                      (generate-latex s type symbol (parse-args args) doc)
                      (print (list symbol type category))
                      ;;(format s "~%~a ~a[~a]~%~a~%~%" symbol args type doc)
                      ))))))

(defun fbound-to-function-p (symbol)
     (and (fboundp symbol)
          (not (macro-function symbol))
          (not (special-operator-p symbol))))

(defun get-type-of (symbol)
  (cond ((fbound-to-function-p symbol) "function")
        ((macro-function symbol) "macro")
        ((boundp symbol)
         (cond ((find #\* (symbol-name symbol)) "variable")
               ((find #\+ (symbol-name symbol)) "constant")
               (t "")))
        (t "")))

(defun package-symbols (package)
  (mapcar (lambda (x) (intern x package))
          (sort (loop for symbol being the external-symbols of (find-package package)
               collect (symbol-name symbol))
         #'string<)))

(defun document-symbols-in-package (package)
  (with-open-file (s (make-pathname :directory *dir* :name package :type "tex")
                     :direction :output :if-exists :supersede)
    (loop for symbol in (package-symbols package)
          do
          (let* ((args (when (fboundp symbol)
                         (sb-introspect:function-lambda-list symbol)))
                 (doc (parse-doc (or (documentation symbol 'function)
                                     (documentation symbol 'variable)))))
            (generate-latex s
                            (get-type-of symbol)
                            (string-downcase (symbol-name symbol))
                            (parse-args args) doc)))))

;; (document-symbols-in-package "ALEXANDRIA")
;; (document-symbols-in-package "ARNESI")
(in-package :cl-extensions)

(defun copy-hash-table (table &key key test size
                                   rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))

(declaim (inline maphash-keys))

(defun maphash-keys (function hash-table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table
TABLE."
  (loop for k being the hash-keys of hash-table do (funcall function k)))

(declaim (inline maphash-values))
(defun maphash-values (function hash-table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table
TABLE."
  (loop for v being the hash-keys of hash-table do (funcall function v)))

(defun hash-table-keys (hash-table)
  "Returns a list containing the keys of hash table TABLE."
  (loop for k being the hash-keys of hash-table collect k))

(defun hash-table-values (hash-table)
  (loop for v being the hash-values of hash-table collect v))

(defun hash-to-alist (hash-table)
  "Returns an association list containing the keys and values of hash
table TABLE."
  (loop for k being the hash-keys of hash-table
        collect (cons k (gethash k hash-table))))

(defun hash-to-plist (hash-table)
  "Returns a plist containing the keys and values of hash
table TABLE."
  (loop for k being the hash-keys of hash-table
        append (list k (gethash k hash-table))))

(defun alist-to-hash (alist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (loop for (key . value) in alist
        with table = (apply #'make-hash-table hash-table-initargs)
        do (setf (gethash key table) value)
        finally (return table)))

(defun plist-to-hash (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (setf (gethash (car tail) table) (cadr tail)))
    table))

(defun ensure-gethash (key hash-table &optional default)
  "Like GETHASH, but if KEY is not found in the HASH-TABLE saves the DEFAULT
under key before returning it. Secondary return value is true if key was
already in the table."
  (multiple-value-bind (value ok) (gethash key hash-table)
    (if ok
        (values value ok)
        (values (setf (gethash key hash-table) default) nil))))

(defun build-hash-table (hash-spec inital-contents)
  "Create a hash table containing ``INITAL-CONTENTS``."
  (let ((ht (apply #'make-hash-table hash-spec)))
    (dolist* ((key value) inital-contents)
      (setf (gethash key ht) value))
    ht))

(defun make-lookup-name (name &rest parts)
  (funcall #'intern-concat parts (symbol-package name)))

(defmacro deflookup-table
    (name &key (var    (make-lookup-name name "*" name "*"))
               (reader (make-lookup-name name "GET-" name))
               (writer (make-lookup-name name "GET-" name))
               (rem-er (make-lookup-name name "REM-" name))
               (at-redefinition :warn)
               (documentation
                (format nil "Global var for the ~S lookup table" name))
               (test 'eql)
               (initial-contents nil))
  "Creates a hash table and the associated accessors."
  ;; if they explicitly pass in NIL we make the name a gensym
  (unless var
    (setf var    (gensym (strcat "var for " name " lookup table "))))
  (unless reader
    (setf reader (gensym (strcat "reader for " name " lookup table "))))
  (unless writer
    (setf writer (gensym (strcat "writer for " name " lookup table "))))
  (assert (symbolp name) (name)
          "The name of the lookup table must be a symbol.")
  (assert (symbolp var) (var)
          "The name of the underlying var must be a symbol.")
  (assert (symbolp reader) (reader)
          "The name of the reader for a lookup table must be a symbol.")
  (assert (symbolp writer) (writer)
          "The name of the writer for a lookup table must be a symbol.")
  `(progn
     (defvar ,var
       (build-hash-table '(:test ,test) ,initial-contents)
       ,documentation)
     (defun ,reader (key &optional default)
       (gethash key ,var default))
     (defun (setf ,writer) (value key)
       ,(when at-redefinition
          `(when (gethash key ,var)
             ,(case at-redefinition
                (:warn `(warn "Redefining ~A in deflookup-table named ~S"
                         (let ((*package* (find-package "KEYWORD")))
                           (format nil "~S" key))
                         ',name))
                (t at-redefinition))))
       (setf (gethash key ,var) value))
     (defun ,rem-er (key)
       (remhash key ,var))
     (list ',name ',var ',reader '(setf ,writer) ',rem-er)))


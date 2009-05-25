(in-package :cl-extensions)

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(defvar +lower-case-ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyz"
  "All the lower case letters in 7 bit ASCII.")
(defvar +upper-case-ascii-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "All the upper case letters in 7 bit ASCII.")
(defvar +ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "All letters in 7 bit ASCII.")
(defvar +alphanumeric-ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "All the letters and numbers in 7 bit ASCII.")
(defvar +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "All the characters allowed in base64 encoding.")

(defun random-string (&optional (length 32) (alphabet +ascii-alphabet+))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (loop with id = (make-string length)
        with alphabet-length = (length alphabet)
        for i below length
        do (setf (cl:aref id i)
                 (cl:aref alphabet (random alphabet-length)))
        finally (return id)))

(declaim (inline strcat))
(defun strcat (&rest items)
  "Returns a fresh string consisting of ITEMS concat'd together."
  (declare (optimize speed))
  (strcat* items))

(defun strcat* (string-designators)
  "Concatenate all the strings in STRING-DESIGNATORS."
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dotree (str string-designators)
        (when str
          (princ str stream))))))

;;; A "faster" version for string concatenating.
;;; Could use just (apply #'concatenate 'string list), but that's quite slow
(defun join-strings (list)
  (let* ((length (reduce #'+ list :key #'length))
         (result (make-string length)))
    (loop
       for string in list
       for start = 0 then end
       for end = (+ start (length string))
       while string
       do (replace result string :start1 start :end1 end)
       finally (return result))))

(defun fold-strings (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((strings '())
        (result '()))
    (dolist (object list)
      (typecase object
        (string (push object strings))
        (t (when strings
             (push (join-strings (nreverse strings)) result)
             (setf strings '()))
           (push object result))))
    (when strings
      (push (join-strings (nreverse strings)) result))
    (nreverse result)))

;;;; ** Converting strings to/from foreign encodings

;;;; *** CLISP

#+(and clisp unicode)
(progn
  (defun %encoding-keyword-to-native (encoding)
    (ext:make-encoding
     :charset (case encoding
                (:utf-8    charset:utf-8)
                (:utf-16   charset:utf-16)
                (:us-ascii charset:ascii)
                (t (multiple-value-bind (symbol status)
                       (find-symbol (string encoding) (find-package :charset))
                     (if (eq status :external)
                         (symbol-value symbol)
                         ;; otherwise, if SYSTEM::*HTTP-ENCODING*
                         ;; is available, then use it
                         #+#.(cl:if (cl:and (cl:find-package "SYSTEM")
                                            (cl:find-symbol "*HTTP-ENCODING*" 
                                                            (cl:find-package "SYSTEM")))
                                    '(and) '(or))
                         SYSTEM::*HTTP-ENCODING*
                         ;; otherwise, use EXT:*MISC-ENCODING*
                         #+#.(cl:if (cl:and (cl:find-package "SYSTEM")
                                            (cl:find-symbol "*HTTP-ENCODING*" 
                                                            (cl:find-package "SYSTEM")))
                                    '(or) '(and))
                         EXT:*MISC-ENCODING*))))
     ;; These native encodings will be used for the HTTP protocol, 
     ;; therefore we set the line-terminator to MS-DOS.
     ;; Of course, it would be better if this was explicitely requested...
     :line-terminator :dos
     :input-error-action (code-char #xFFFD)
     :output-error-action #+debug :error #-debug :ignore))
  (defun %string-to-octets (string encoding)
    (ext:convert-string-to-bytes string (encoding-keyword-to-native encoding)))
  (defun %octets-to-string (octets encoding)
    (ext:convert-string-from-bytes octets (encoding-keyword-to-native encoding))))

;;;; *** SBCL

#+(and sbcl sb-unicode)
(progn
  (defun %encoding-keyword-to-native (encoding)
    (case encoding
      (:utf-8 :utf8)
      (:utf-16 :utf16)
      (:us-ascii :us-ascii)
      (t encoding)))
  (defun %string-to-octets (string encoding)
    (sb-ext:string-to-octets string :external-format (encoding-keyword-to-native encoding)))
  (defun %octets-to-string (octets encoding)
    (sb-ext:octets-to-string octets :external-format (encoding-keyword-to-native encoding))))

;;;; *** Allegro

#+allegro
(progn
  (defun %encoding-keyword-to-native (encoding)
    (case encoding
      (:utf-8 :utf8)
      (:iso-8859-1 :iso8859-1)
      (:utf-16 :unicode)
      (:us-ascii :ascii)
      (t encoding)))

  (defun %string-to-octets (string encoding)
     (excl:string-to-octets string :external-format (encoding-keyword-to-native encoding) :null-terminate nil))

  (defun %octets-to-string (octets encoding)
    (multiple-value-bind (displaced-array index) (array-displacement octets)
      (if displaced-array
          (excl:octets-to-string displaced-array :start index :end (+ index (length octets)) :external-format (encoding-keyword-to-native encoding))
          (excl:octets-to-string octets :external-format (encoding-keyword-to-native encoding))))))


;;;; *** LispWorks

;; TODO this is partial. someone with a lispworks at hand should finish it.
;; see this as an example:
;;     (defun encode-lisp-string (string)
;;       (translate-string-via-fli string :utf-8 :latin-1))
;; 
;;     (defun decode-external-string (string)
;;       (translate-string-via-fli string :latin-1 :utf-8))
;; 
;;     ;; Note that a :utf-8 encoding of a null in a latin-1 string is
;;     ;; also null, and vice versa.  So don't have to worry about
;;     ;; null-termination or length. (If we were translating to/from
;;     ;; :unicode, this would become an issue.)
;; 
;;     (defun translate-string-via-fli (string from to)
;;       (fli:with-foreign-string (ptr elements bytes :external-format from)
;; 	  string
;; 	(declare (ignore elements bytes))
;; 	(fli:convert-from-foreign-string ptr :external-format to)))

#+lispworks
(progn
  (defun %encoding-keyword-to-native (encoding)
    (case encoding
      (:utf-8 :utf-8)
      (:iso-8859-1 :latin-1)
      (:utf-16 :unicode)
      (:us-ascii :us-ascii)
      (t encoding)))
  
  (defun %string-to-octets (string encoding)
    (declare (ignore encoding))
    ;; TODO
    (map-into (make-array (length string) :element-type 'unsigned-byte)
              #'char-code string))
  
  (defun %octets-to-string (octets encoding)
    (declare (ignore encoding))
    ;; TODO
    (map-into (make-array (length octets) :element-type 'character)
              #'code-char octets)))


;;;; *** Default Implementation

#-(or (and sbcl sb-unicode) (and clisp unicode) allegro lispworks)
(progn
  (defun %encoding-keyword-to-native (encoding)
    encoding)
  
  (defun %string-to-octets (string encoding)
    (declare (ignore encoding))
    (map-into (make-array (length string) :element-type 'unsigned-byte)
              #'char-code string))
  
  (defun %octets-to-string (octets encoding)
    (declare (ignore encoding))
    (map-into (make-array (length octets) :element-type 'character)
              #'code-char octets)))

(declaim (inline string-to-octets %string-to-octets))
(defun string-to-octets (string encoding)
  "Convert STRING, a list string, a vector of bytes according to ENCODING.

ENCODING is a keyword representing the desired character
encoding. We gurantee that :UTF-8, :UTF-16 and :ISO-8859-1 will
work as expected. Any other values are simply passed to the
underlying lisp's function and the results are implementation
dependant.

On CLISP we intern the ENCODING symbol in the CHARSET package and
pass that. On SBCL we simply pass the keyword."
  (%string-to-octets string encoding))

(declaim (inline octets-to-string %octets-to-string))
(defun octets-to-string (octets encoding)
  (%octets-to-string octets encoding))

(declaim (inline encoding-keyword-to-native))
(defun encoding-keyword-to-native (encoding)
  "Convert ENCODING, a keyword, to an object the native list
accepts as an encoding.

ENCODING can be: :UTF-8, :UTF-16, or :US-ASCII and specify the
corresponding encodings. Any other keyword is passed, as is, to
the underlying lisp."
  (%encoding-keyword-to-native encoding))

(defun string-from-array (array &key (start 0) (end (1- (length array))))
  "Assuming ARRAY is an array of ASCII chars encoded as bytes return
the corresponding string. Respect the C convention of null terminating
strings. START and END specify the zero indexed offsets of a sub range
of ARRAY."
  ;; This is almost always the case
  (assert (<= 0 start (1- (length array)))
          (start)
          "START must be a valid offset of ARRAY.")
  (assert (<= 0 end (1- (length array)))
          (end)
          "END must be a valid offset of ARRAY.")
  (assert (<= start end)
          (start end)
          "START must be less than or equal to END.")
  (assert (every (lambda (element) (<= 0 element 255)) array)
	  (array)
	  "Some element of ~S was not > 0 and < 255" array)
  (let* ((working-array (make-array (1+ (- end start))
                                    :element-type (array-element-type array)
                                    :displaced-to array
                                    :displaced-index-offset start))
	  (length (if-bind pos (position 0 working-array)
		      pos
		      (length working-array))))
    (map-into (make-array length :element-type 'character)
	      #'code-char
	      working-array)))

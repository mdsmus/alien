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

(defun stringify (exp)
  "Coerce argument to a string."
  (cond ((stringp exp) exp)
	((symbolp exp) (symbol-name exp))
	(t (format nil "~A" exp))))

;;; A "faster" version for string concatenating.
;;; Could use just (apply #'concatenate 'string list), but that's quite slow
(defun join-strings (&rest strings)
  (let* ((length (reduce #'+ strings :key #'length))
         (result (make-string length)))
    (loop
       for string in strings
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

(defun empty-string-p (string)
  "Indicates, if a given string is empty (or being nil)."
  (or (null string)
      (zerop (length string))))

(defun split-tab (string)
  (split-sequence #\Tab string :remove-empty-subseqs t))

(defun split-space (string)
  (split-sequence #\Space string :remove-empty-subseqs t))

(defun split-newline (string)
  (split-sequence #\Newline string :remove-empty-subseqs t))

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

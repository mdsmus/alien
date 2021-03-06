(in-package "URI-TEMPLATE")

(defvar *encode-uri-string* t)

(defun count-string-char (s c)
  "Return a count of the number of times a character appears in a string"
  (declare (simple-string s)
           (character c)
           (optimize (speed 3) (safety 0)))
  (do ((len (length s))
       (i 0 (1+ i))
       (count 0))
      ((= i len) count)
    (declare (fixnum i len count))
    (when (char= (schar s i) c)
      (incf count))))

(defun hexchar (n)
  (declare (type (integer 0 15) n))
  (schar "0123456789ABCDEF" n))

(defun charhex (ch)
  "convert hex character to decimal"
  (let ((code (char-code (char-upcase ch))))
    (declare (fixnum ch))
    (if (>= code (char-code #\a))
        (+ 10 (- code (char-code #\a)))
        (- code (char-code #\0)))))

(defun count-string-char-if (pred s)
  "Return a count of the number of times a predicate is true
for characters in a string"
  (declare (simple-string s)
           (type (or function symbol) pred)
           (optimize (speed 3) (safety 0) (space 0)))
  (do ((len (length s))
       (i 0 (1+ i))
       (count 0))
      ((= i len) count)
    (declare (fixnum i len count))
    (when (funcall pred (schar s i))
      (incf count))))

(defun non-alphanumericp (ch)
  (not (alphanumericp ch)))

(defun encode-uri-string (query)
  "Escape non-alphanumeric characters for URI fields"
  (declare (simple-string query)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((count (count-string-char-if #'non-alphanumericp query))
        (len (length query))
        (new-len (+ len (* 2 count)))
        (str (make-string new-len))
        (spos 0 (1+ spos))
        (dpos 0 (1+ dpos)))
      ((= spos len) str)
    (declare (fixnum count len new-len spos dpos)
             (simple-string str))
    (let ((ch (schar query spos)))
      (if (non-alphanumericp ch)
          (let ((c (char-code ch)))
            (setf (schar str dpos) #\%)
            (incf dpos)
            (setf (schar str dpos) (hexchar (logand (ash c -4) 15)))
            (incf dpos)
            (setf (schar str dpos) (hexchar (logand c 15))))
        (setf (schar str dpos) ch)))))

(defun decode-uri-string (query)
  "Unescape non-alphanumeric characters for URI fields"
  (declare (simple-string query)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((count (count-string-char query #\%))
        (len (length query))
        (new-len (- len (* 2 count)))
        (str (make-string new-len))
        (spos 0 (1+ spos))
        (dpos 0 (1+ dpos)))
      ((= spos len) str)
    (declare (fixnum count len new-len spos dpos)
             (simple-string str))
    (let ((ch (schar query spos)))
      (if (char= #\% ch)
          (let ((c1 (charhex (schar query (1+ spos))))
                (c2 (charhex (schar query (+ spos 2)))))
            (declare (fixnum c1 c2))
            (setf (schar str dpos)
                  (code-char (logior c2 (ash c1 4))))
            (incf spos 2))
        (setf (schar str dpos) ch)))))

(defun read-uri-template (stream &optional recursive-p)
  (let ((*readtable* (copy-readtable))
        (token-accumulator ())
        (string-accumulator ()))
    (flet ((collect-string ()
             (when string-accumulator
               (push (coerce (reverse string-accumulator) 'string) token-accumulator)
               (setf string-accumulator ()))))
      (set-syntax-from-char #\} #\Space)
      (let (next-char)
        (loop until (member (setf next-char (read-char stream nil #\Space recursive-p)) '(#\Space #\Newline #\Tab #\))) do
              (case next-char
                (#\{ (collect-string)
                     (let ((sexp (read stream t nil recursive-p)))
                       (push `(maybe-uri-encode ,sexp) token-accumulator)))
                (#\})
                (t (push next-char string-accumulator)))
              finally (unread-char next-char stream) (collect-string)))
      (reverse token-accumulator))))

(defun maybe-uri-encode (x)
  (if *encode-uri-string* (encode-uri-string (princ-to-string x)) x))

#+parenscript (parenscript:defpsmacro maybe-uri-encode (x)
                (if *encode-uri-string* `(encode-u-r-i-component ,x) x))

(defun uri-template (&rest template-args)
  (format nil "~{~A~}" template-args))

#+parenscript (parenscript:defpsmacro uri-template (&rest template-args)
                `(+ ,@template-args))

(defun enable-uri-template-syntax ()
  (set-dispatch-macro-character #\# #\U
    (lambda (stream subchar arg)
      (declare (ignore subchar arg))
      `(uri-template ,@(read-uri-template stream t))))
  (values))

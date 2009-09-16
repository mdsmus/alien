(in-package :cl-extensions)

(defmacro with-input-from-file ((stream-name file-name &rest args &key
					     (direction nil direction-provided-p)
					     &allow-other-keys)
                                &body body)
  "Evaluate BODY with STREAM-NAME bound to an input-stream from file
FILE-NAME. ARGS is passed directly to open."
  (declare (ignore direction))
  (when direction-provided-p
    (error "Can't specifiy :DIRECTION in WITH-INPUT-FROM-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-output-to-file ((stream-name file-name &rest args &key
                                            (direction nil direction-provided-p)
                                            &allow-other-keys)
			       &body body)
  "Evaluate BODY with STREAM-NAME to an output stream on the file
FILE-NAME. ARGS is sent as is to the call te open."
  (declare (ignore direction))
  (when direction-provided-p
    (error "Can't specifiy :DIRECTION in WITH-OUTPUT-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :output :if-exists :supersede ,@args)
     ,@body))

(defun read-file-into-string (pathname &key (buffer-size 4096) (external-format :default))
  "Return the contents of PATHNAME as a fresh string.

The file specified by PATHNAME will be read one ELEMENT-TYPE
element at a time, the EXTERNAL-FORMAT and ELEMENT-TYPEs must be
compatible.

The EXTERNAL-FORMAT parameter will be passed to
ENCODING-KEYWORD-TO-NATIVE, see ENCODING-KEYWORD-TO-NATIVE to
possible values."
  (with-input-from-file
      (file-stream pathname :external-format external-format)
    (let ((*print-pretty* nil))
      (with-output-to-string (datum)
        (let ((buffer (make-array buffer-size :element-type 'character)))
	  (loop
	     :for bytes-read = (read-sequence buffer file-stream)
	     :do (write-sequence buffer datum :start 0 :end bytes-read)
	     :while (= bytes-read buffer-size)))))))

(defun write-string-into-file (string pathname &key (if-exists :error)
						    (if-does-not-exist :error)
						    (external-format :default))
  "Write STRING to PATHNAME.

The EXTERNAL-FORMAT parameter will be passed to
ENCODING-KEYWORD-TO-NATIVE, see ENCODING-KEYWORD-TO-NATIVE to
possible values."
  (with-output-to-file (file-stream pathname :if-exists if-exists
				    :if-does-not-exist if-does-not-exist
				    :external-format external-format)
    (write-sequence string file-stream)))

(defun copy-stream (input output &optional (element-type (stream-element-type input)))
  "Reads data from FROM and writes it to TO. Both FROM and TO must be streams,
they will be passed to read-sequence/write-sequence and must have compatable
element-types."
  (loop with buffer-size = 4096
        with buffer = (make-array buffer-size :element-type element-type)
        for bytes-read =
        #-(or :clisp :cmu) (read-sequence buffer input)
        #+:clisp (ext:read-byte-sequence buf from :no-hang nil)
        #+:cmu (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)
        while (= bytes-read buffer-size)
        do (write-sequence buffer output)
        finally (write-sequence buffer output :end bytes-read)))

(defun mkdir (dir)
  #+allegro (excl:make-directory dir)
  #+clisp (#+lisp=cl ext:make-dir #-lisp=cl lisp:make-dir dir)
  #+cmu (unix:unix-mkdir (directory-namestring dir) #o777)
  #+lispworks (system:make-directory dir)
  #+sbcl (sb-unix:unix-mkdir (directory-namestring dir) #o777)
  #-(or allegro clisp cmu lispworks sbcl)
  (error 'not-implemented :proc (list 'mkdir dir)))

(defun rmdir (dir)
  #+allegro (excl:delete-directory dir)
  #+clisp (#+lisp=cl ext:delete-dir #-lisp=cl lisp:delete-dir dir)
  #+cmu (unix:unix-rmdir dir)
  #+lispworks
  ;; `lw:delete-directory' is present in LWW 4.1.20 but not on LWL 4.1.0
  (if (fboundp 'lw::delete-directory)
      (lw::delete-directory dir)
      (delete-file dir))
  #-(or allegro clisp cmu lispworks) (delete-file dir))

(defun default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
  #+cmu (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #-(or allegro clisp cmu cormanlisp lispworks lucid) (truename "."))

(defun chdir (&optional dir)
  "Change directory and set default pathname"
  (cond
    ((not (null dir))
     (when (and (typep dir 'logical-pathname)
                (translate-logical-pathname dir))
       (setq dir (translate-logical-pathname dir)))
     (when (stringp dir)
       (setq dir (parse-namestring dir)))
     #+allegro (excl:chdir dir)
     #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
     #+sbcl(sb-posix:chdir (pathname dir))
     #+(or cmu scl) (setf (ext:default-directory) dir)
     #+cormanlisp (ccl:set-current-directory dir)
     #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
     #+openmcl (ccl:cwd dir)
     #+gcl (si:chdir dir)
     #+lispworks (hcl:change-directory dir)
     (setq cl:*default-pathname-defaults* dir))
    (t
     (let ((dir
            #+allegro (excl:current-directory)
            #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
            #+(or cmu scl) (ext:default-directory)
            #+sbcl (sb-unix:posix-getcwd/)
            #+cormanlisp (ccl:get-current-directory)
            #+lispworks (hcl:get-working-directory)
            #+mcl (ccl:mac-default-directory)
            #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks) (truename ".")))
       (when (stringp dir)
         (setq dir (parse-namestring dir)))
       dir))))

(defmacro with-dir (directory &body body)
  (with-gensyms (original-dir result)
    `(let ((,original-dir (chdir)))
       (progn
         (chdir ,directory)
         (let ((,result ,@body))
           (chdir ,original-dir)
           ,result)))))

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Creates a wild pathname specifically for CLISP such that
sub-directories are returned by DIRECTORY."
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(defun list-directory (dirname)
  "Returns a fresh list of pathnames corresponding to the truenames of
all files within the directory named by the non-wild pathname
designator DIRNAME.  The pathnames of sub-directories are returned in
directory form - see PATHNAME-AS-DIRECTORY."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+:ecl 
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-:ecl 
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+(or :sbcl :cmu :scl :lispworks) (directory wildcard)
    #+(or :openmcl :digitool) (directory wildcard :directories t)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "LIST-DIRECTORY not implemented"))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  #+(or :sbcl :lispworks :openmcl :ecl :digitool) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :scl :abcl) (or (probe-file (pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "FILE-EXISTS-P not implemented"))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #-(or :allegro :lispworks)
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t)))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  FN will only be applied to files for which the
function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
TEST are applied to directories as well.  If DIRECTORIES is :DEPTH-FIRST,
FN will be applied to the directory's contents first.  If
DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the
directory's content will be skipped. IF-DOES-NOT-EXIST must be
one of :ERROR or :IGNORE where :ERROR means that an error will be
signaled if the directory DIRNAME does not exist."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (dolist (file (list-directory name))
                         (walk file)))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (dolist (file (list-directory name))
                           (walk file))))
                      ;; :DEPTH-FIRST is implicit
                      (t (dolist (file (list-directory name))
                           (walk file))
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (file-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (file-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))

(defun copy-file (from to &key (overwrite t))
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  #+:allegro (excl.osi:copy-file from to :overwrite overwrite)
  #-:allegro
  (let ((element-type #-:cormanlisp '(unsigned-byte 8)
                      #+:cormanlisp 'unsigned-byte))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
                                           :supersede
                                           #-:cormanlisp :error
                                           #+:cormanlisp nil))
        #+:cormanlisp
        (unless out
          (error (make-condition 'file-error
                                 :pathname to
                                 :format-control "File already exists.")))
        (copy-stream in out))))
  (values))

(defun delete-directory-and-files (dirname &key (if-does-not-exist :error))
  "Recursively deletes all files and directories within the directory
designated by the non-wild pathname designator DIRNAME including
DIRNAME itself.  IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE
where :ERROR means that an error will be signaled if the directory
DIRNAME does not exist."
  #+:allegro (excl.osi:delete-directory-and-files dirname
                                                  :if-does-not-exist if-does-not-exist)
  #-:allegro (walk-directory dirname
                             (lambda (file)
                               (cond ((directory-pathname-p file)
                                      #+:lispworks (lw:delete-directory file)
                                      #+:cmu (multiple-value-bind (ok err-number)
                                                 (unix:unix-rmdir (namestring (truename file)))
                                               (unless ok
                                                 (error "Error number ~A when trying to delete ~A"
                                                        err-number file)))
                                      #+:scl (multiple-value-bind (ok errno)
                                                 (unix:unix-rmdir (ext:unix-namestring (truename file)))
                                               (unless ok
                                                 (error "~@<Error deleting ~S: ~A~@:>"
                                                        file (unix:get-unix-error-msg errno))))
                                      #+:sbcl (sb-posix:rmdir file)
                                      #+:clisp (ext:delete-dir file)
                                      #+:openmcl (ccl:delete-directory file)
                                      #+:cormanlisp (win32:delete-directory file)
                                      #+:ecl (si:rmdir file)
                                      #+(or :abcl :digitool) (delete-file file))
                                     (t (delete-file file))))
                             :directories t
                             :if-does-not-exist if-does-not-exist)
  (values))

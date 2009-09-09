(in-package :cl-ext-test)

(in-suite :cl-ext-tests)

;; ensure-gethash
;; build-hash-table
;; deflookup-table

(test ensure-hash-table.1
  (is (eql (let ((table (make-hash-table))
                 (x (list 1)))
             (multiple-value-bind (value already-there)
                 (ensure-gethash x table 42)
               (and (= value 42)
                    (not already-there)
                    (= 42 (gethash x table))
                    (multiple-value-bind (value2 already-there2)
                        (ensure-gethash x table 13)
                      (and (= value2 42)
                           already-there2
                           (= 42 (gethash x table))))))))))

(test copy-hash-table.1
  (is (equal (let ((orig (make-hash-table :test 'eq :size 123))
                   (foo "foo"))
               (setf (gethash orig orig) t
                     (gethash foo orig) t)
               (let ((eq-copy (copy-hash-table orig))
                     (eql-copy (copy-hash-table orig :test 'eql))
                     (equal-copy (copy-hash-table orig :test 'equal))
                     (equalp-copy (copy-hash-table orig :test 'equalp)))
                 (list (hash-table-size eq-copy)
                       (hash-table-count eql-copy)
                       (gethash orig eq-copy)
                       (gethash (copy-seq foo) eql-copy)
                       (gethash foo eql-copy)
                       (gethash (copy-seq foo) equal-copy)
                       (gethash "FOO" equal-copy)
                       (gethash "FOO" equalp-copy))))
             '(123 2 t nil t t nil t))))

(test copy-hash-table.2
  (is (equal (let ((ht (make-hash-table))
                   (list (list :list (vector :A :B :C))))
               (setf (gethash 'list ht) list)
               (let* ((shallow-copy (copy-hash-table ht))
                      (deep1-copy (copy-hash-table ht :key 'copy-list))
                      (list         (gethash 'list ht))
                      (shallow-list (gethash 'list shallow-copy))
                      (deep1-list   (gethash 'list deep1-copy)))
                 (list (eq ht shallow-copy)
                       (eq ht deep1-copy)
                       (eq list shallow-list)
                       (eq list deep1-list) ; outer list was copied.
                       (eq (second list) (second shallow-list))
                       (eq (second list) (second deep1-list)) ; inner vector wasn't copied.
                       )))
             '(nil nil t nil t t))))

(test maphash-keys.1
  (is (eql (let ((keys nil)
                 (table (make-hash-table)))
             (declare (notinline maphash-keys))
             (dotimes (i 10)
               (setf (gethash i table) t))
             (maphash-keys (lambda (k) (push k keys)) table)
             (set-equal keys '(0 1 2 3 4 5 6 7 8 9))))))

(test maphash-values.1
  (is (eql (let ((vals nil)
                 (table (make-hash-table)))
             (declare (notinline maphash-values))
             (dotimes (i 10)
               (setf (gethash i table) (- i)))
             (maphash-values (lambda (v) (push v vals)) table)
             (set-equal vals '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))))))

(test hash-table-keys.1
  (is (eql (let ((table (make-hash-table)))
             (dotimes (i 10)
               (setf (gethash i table) t))
             (set-equal (hash-table-keys table) '(0 1 2 3 4 5 6 7 8 9))))))

(test hash-table-values.1
  (is (eql (let ((table (make-hash-table)))
             (dotimes (i 10)
               (setf (gethash (gensym) table) i))
             (set-equal (hash-table-values table) '(0 1 2 3 4 5 6 7 8 9))))))

(test hash-to-alist.1
  (is (equal (let ((table (make-hash-table)))
               (dotimes (i 10)
                 (setf (gethash i table) (- i)))
               (let ((alist (hash-to-alist table)))
                 (list (length alist)
                       (assoc 0 alist)
                       (assoc 3 alist)
                       (assoc 9 alist)
                       (assoc nil alist))))
             '(10 (0 . 0) (3 . -3) (9 . -9) nil))))

(test hash-to-plist.1
  (is (equal (let ((table (make-hash-table)))
               (dotimes (i 10)
                 (setf (gethash i table) (- i)))
               (let ((plist (hash-to-plist table)))
                 (list (length plist)
                       (getf plist 0)
                       (getf plist 2)
                       (getf plist 7)
                       (getf plist nil))))
             '(20 0 -2 -7 nil))))

(test alist-to-hash.1
  (is (equal (let* ((alist '((0 a) (1 b) (2 c)))
                    (table (alist-to-hash alist)))
               (list (hash-table-count table)
                     (gethash 0 table)
                     (gethash 1 table)
                     (gethash 2 table)
                     (hash-table-test table)))
             '(3 (a) (b) (c) eql))))

(test plist-to-hash.1
  (is (equal (let* ((plist '(:a 1 :b 2 :c 3))
                    (table (plist-to-hash plist :test 'eq)))
               (list (hash-table-count table)
                     (gethash :a table)
                     (gethash :b table)
                     (gethash :c table)
                     (gethash 2 table)
                     (gethash nil table)
                     (hash-table-test table)))
             '(3 1 2 3 nil nil eq))))

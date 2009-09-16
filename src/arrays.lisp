(in-package :cl-extensions)

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer
and adjustability (if any) as the original, unless overridden by
the keyword arguments."
  (let ((dims (array-dimensions array)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy.
    (adjust-array (make-array dims
                              :element-type element-type :fill-pointer fill-pointer
                              :adjustable adjustable :displaced-to array)
                  dims)))

(defun make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))

(defun print-grid (array &key (stream t) (key #'identity) (width 3))
  "Print the contents of a 2-D array, numbering the edges."
  (let ((max-x (- (array-dimension array 0) 1))
	(max-y (- (array-dimension array 1) 1)))
    ;; Print the header
    (format stream "~&") (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream "|") (print-dashes width stream))
    (format stream "|~%")
    ;; Print each row
    (for y1 = 0 to max-y do
	 (let ((y (- max-y y1)))
	   (print-centered y width stream)
	   ;; Print each location
	   (for x = 0 to max-x do
		(format stream "|")
		(print-centered (funcall key (aref array x y)) width stream))
	   (format stream "|~%") 
	   ;; Print a dashed line
	   (print-repeated " " width stream)
	   (for x = 0 to max-x do
		(format stream "|") (print-dashes width stream)))
	 (format stream "|~%"))
    ;; Print the X-coordinates along the bottom
    (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream " ") (print-centered x width stream))
    array))

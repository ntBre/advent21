(defun split-char (str)
  "split a string into a list of one-character strings"
  (loop for c across str
	collecting (string c)))

(defun parse (filename)
  (let ((lst '())
	(cols 0)
	(rows 0))
    (with-open-file (in filename)
      (loop for line = (read-line in nil nil)
	    while line
	    do (push (mapcar #'read-from-string (split-char line)) lst)
	       (setf cols (length (split-char line)))
	    (incf rows)))
    (make-array (list rows cols) :initial-contents (nreverse lst))))


(defun part1 (&optional (filename "ex.txt"))
  (let ((input (parse filename))
	(score 0))
    (destructuring-bind (rows cols)
	(array-dimensions input)
      (dotimes (r rows)
	(dotimes (c cols)
	  (when (and (or (= r (1- rows))
			 (< (aref input r c) (aref input (1+ r) c)))
		     (or (= c (1- cols))
			 (< (aref input r c) (aref input r (1+ c))))
		     (or (= r 0)
			 (< (aref input r c) (aref input (1- r) c)))
		     (or (= c 0)
			 (< (aref input r c) (aref input r (1- c)))))
	    (incf score (1+ (aref input r c)))))))
    score))

(defun part2 ()
  )

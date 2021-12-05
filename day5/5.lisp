(defun extract (line)
  "take lines of the form \"0,9 -> 5,9\" and return a list of the four
  numbers"
  (normalize
   (mapcar #'read-from-string
	   (cdr
	    (cl-ppcre:split
	     "(\\d+),(\\d+) -> (\\d+),(\\d+)" line
	     :with-registers-p t)))))

(defun normalize (coords)
  "normalize the order of x1,y1 -> x2,y2 such that x1 <= x2 and y1 <= y2"
  (destructuring-bind (x1 y1 x2 y2) coords
    (if (or (> x1 x2) (> y1 y2))
	(list x2 y2 x1 y1)
	coords)))

(defun diag (x1 y1 x2 y2)
  (= 1 (abs (slope x1 y1 x2 y2))))

(defun slope (x1 y1 x2 y2)
  (/ (- y2 y1)
     (- x2 x1)))

(defun parse (filename &optional part2)
  (let ((grid (make-array '(1 1) :initial-element 0 :adjustable t)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil nil)
	    while line
	    do (destructuring-bind (x1 y1 x2 y2) (extract line)
		 (destructuring-bind (r c) (array-dimensions grid)
		   (let ((mx (max x1 x2))
			 (my (max y1 y2)))
		     (when (>= mx c) (setf c (1+ mx)))
		     (when (>= my r) (setf r (1+ my)))
		     (adjust-array grid (list r c))))
		 (cond
		   ((or (= x1 x2) (= y1 y2))
		    (loop for x from x1 upto x2
			  do (loop for y from y1 upto y2
				   do (incf (aref grid y x)))))
		   ((and part2 (diag x1 y1 x2 y2))
		    (if (> x1 x2)
			(if (> y1 y2)
			    (loop for x from x1 downto x2
				  and y from y1 downto y2
				  do (incf (aref grid y x)))
			    (loop for x from x1 downto x2
				  and y from y1 upto y2
				  do (incf (aref grid y x))))
			(if (> y1 y2)
			    (loop for x from x1 upto x2
				  and y from y1 downto y2
				  do (incf (aref grid y x)))
			    (loop for x from x1 upto x2
				  and y from y1 upto y2
				  do (incf (aref grid y x))))))))))
    grid))


(defun print-point (x1 y1 x2 y2)
  (format t "~d,~d -> ~d,~d~%" x1 y1 x2 y2))

(defun part1 (filename)
  (let ((grid (parse filename))
	(count 0))
    (destructuring-bind (r c) (array-dimensions grid)
      (loop for i from 0 below (* r c)
	    when (> (row-major-aref grid i) 1)
	      do (incf count)))
    count))

(defun part2 (filename)
  (let ((grid (parse filename t))
	(count 0))
    (destructuring-bind (r c) (array-dimensions grid)
      (loop for i from 0 below (* r c)
	    when (> (row-major-aref grid i) 1)
	      do (incf count)))
    count))

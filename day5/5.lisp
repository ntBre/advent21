(defun normalize (coords)
  "normalize the order of x1,y1 -> x2,y2 such that x1 <= x2 and y1 <= y2"
  (destructuring-bind (x1 y1 x2 y2) coords
    (if (or (> x1 x2) (> y1 y2))
	(list x2 y2 x1 y1)
	coords)))

(defun extract (line)
  "take lines of the form \"0,9 -> 5,9\" and return a list of the four
  numbers"
  (normalize
   (mapcar #'read-from-string
	   (cdr
	    (cl-ppcre:split
	     "(\\d+),(\\d+) -> (\\d+),(\\d+)" line
	     :with-registers-p t)))))

(defun diag (x1 y1 x2 y2)
  (= 1 (abs (slope x1 y1 x2 y2))))

(defun slope (x1 y1 x2 y2)
  (/ (- y2 y1)
     (- x2 x1)))

(defun parse (filename)
  "parse FILENAME into a list of 4-element lists of the form (x1 y1 x2
y2)"
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (extract line))))

(defun max-x (points)
   (apply #'max
	  (append
	   (mapcar #'car points)
	   (mapcar #'caddr points))))

(defun max-y (points)
   (apply #'max
	  (append
	   (mapcar #'cadr points)
	   (mapcar #'cadddr points))))

(defun work (points grid &optional part2)
  (loop for (x1 y1 x2 y2) in points
	do (cond
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
			    do (incf (aref grid y x))))))))
  grid)


(defun print-point (x1 y1 x2 y2)
  (format t "~d,~d -> ~d,~d~%" x1 y1 x2 y2))

(defun part1 (filename)
  (let* ((points (parse filename))
	 (max-x (1+ (max-x points)))
	 (max-y (1+ (max-y points)))
	 (grid (work points (make-array
			     (list max-y max-x)
			     :initial-element 0)))
	 (count 0))
    (loop for i from 0 below (* max-x max-y)
	  when (> (row-major-aref grid i) 1)
	    do (incf count))
    count))

(defun part2 (filename)
  (let* ((points (parse filename))
	 (max-x (1+ (max-x points)))
	 (max-y (1+ (max-y points)))
	 (grid (work points (make-array
			     (list max-y max-x)
			     :initial-element 0)
		     t))
	 (count 0))
    (loop for i from 0 below (* max-x max-y)
	  when (> (row-major-aref grid i) 1)
	    do (incf count))
    count))

(sb-sprof:with-profiling (:report :flat)
  (part1 "input.txt"))

(sb-sprof:with-profiling (:report :flat)
  (part2 "input.txt"))

(use-package :fiveam)
(test full
  (is (= (part1 "ex.txt") 5))
  (is (= (part1 "input.txt") 4826))
  (is (= (part2 "ex.txt") 12))
  (is (= (part2 "input.txt") 16793)))

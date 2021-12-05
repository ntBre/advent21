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
    (let ((x1r x1) (y1r y1)
	  (x2r x2) (y2r y2))
      (when (> x1 x2)
	(setf x1r x2
	      x2r x1))
      (when (> y1 y2)
	(setf y1r y2
	      y2r y1))
	  (list x1r y1r x2r y2r))))

(defun parse (filename)
  (let ((grid (make-array '(1 1) :initial-element 0 :adjustable t)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil nil)
	    while line
	    do (destructuring-bind (x1 y1 x2 y2) (extract line)
		 (destructuring-bind (r c) (array-dimensions grid)
		   (when (>= x2 c) (setf c (1+ x2)))
		   (when (>= y2 r) (setf r (1+ y2)))
		   (adjust-array grid (list r c)))
		 (when (or (= x1 x2)
			   (= y1 y2))
		   (loop for x from x1 upto x2
			 do (loop for y from y1 upto y2
				  do (incf (aref grid y x))))))))
    grid))


(format t "~d,~d -> ~d,~d~%" x1 y1 x2 y2)

(defun part1 (filename)
  (let ((grid (parse filename))
	(count 0))
    (destructuring-bind (r c) (array-dimensions grid)
      (loop for i from 0 below (* r c)
	    when (> (row-major-aref grid i) 1)
	      do (incf count)))
    count))

(defun part2 ()
  )

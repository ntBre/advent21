(defun clean-split (str &key (separator '(#\  #\Tab)))
  (remove-if #'(lambda (s)
		 (string= "" s))
	     (uiop:split-string str :separator separator)))

(defun parse (filename)
  (let (bingo (boards '()) (buf '()))
    (with-open-file (in filename)
      (loop for line = (read-line in nil nil)
	    and i = 0 then (1+ i)
	    while line
	    do (cond
		 ((= i 0)
		  (setf bingo (mapcar #'read-from-string
				      (clean-split line :separator '(#\,)))))
		 ((= i 1) nil)
		 ((= (length line) 0)
		  (push (make-array '(5 5) :initial-contents (nreverse buf)) boards)
		  (setf buf '()))
		 (t
		  (push
		   (mapcar #'read-from-string (clean-split line)) buf)))))
    (push (make-array '(5 5) :initial-contents (nreverse buf)) boards)
    (values bingo (nreverse boards))))

(defun add-ball (boards ball)
  (loop for board in boards
	do (dotimes (r 5)
	     (dotimes (c 5)
	       (let ((v (aref board r c)))
		 (when (= ball v)
		   (setf (aref board r c)
			 (- v)))))))
  boards)

(defun row (arr row)
  "return column row of ARR"
  (let ((ret '()))
    (dotimes (col (ncols arr))
      (push (aref arr row col) ret))
    (nreverse ret)))

(defun col (arr col)
  "return column COL of ARR"
  (let ((ret '()))
    (dotimes (row (nrows arr))
      (push (aref arr row col) ret))
    (nreverse ret)))

(defun check-rows (boards)
  (let ((winners '()))
    (loop for board in boards
	  do (dotimes (r 5)
	       (if (every #'(lambda (x) (<= x 0)) (row board r))
		   (push board winners))))
  (nreverse winners)))

(defun check-cols (boards)
  (let ((winners '()))
    (loop for board in boards
	  do (dotimes (c 5)
	       (if (every #'(lambda (x) (<= x 0)) (col board c))
		   (push board winners))))
    (nreverse winners)))

(defun board-sum (board)
  (let ((sum 0))
    (loop for i from 0 below 25
	  do (when (> (row-major-aref board i) 0)
	       (incf sum (row-major-aref board i))))
    sum))

(defun part1 ()
  (multiple-value-bind (bingo boards) (parse "input.txt")
    (loop for b in bingo
	  do (setf boards (add-ball boards b))
	     (let ((winner (check-rows boards)))
	       (unless winner
		 (setf winner (check-cols boards)))
	       (when winner
		 (return-from part1
		   (* b (board-sum (car winner)))))))
  boards))

(defun part2 ()
  (multiple-value-bind (bingo boards) (parse "input.txt")
    (print (length boards))
    (loop for b in bingo
	  do (setf boards (add-ball boards b))
	     (let ((winners (check-rows boards)))
	       (unless winners
		 (setf winners (check-cols boards)))
	       (cond
		 ((and winners (> (length boards) 1))
		  (loop for winner in winners
			do (setf boards (delete winner boards))))
		 (winners
		  (return-from part2
		    (* b (board-sum (car winners))))))))
  boards))

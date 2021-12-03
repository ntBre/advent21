(defun parse (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))

(defun part1 ()
  (let* ((input (parse "inp.txt"))
	 (len (length (car input)))
	 (counts (make-array len)))
    (dolist (num input)
      (loop for c across num
	    and i = 0 then (1+ i)
	    do (ecase c
		 (#\0
		  (decf (aref counts i)))
		 (#\1
		  (incf (aref counts i))))))
    ;; count > 0 => 1; < 0 => 0
    (let* ((gam (to-bin (mask counts)))
	   ;; now it works for both example and input
	   (eps (logxor gam (ash #b111111111111
				 (- (length counts) 12)))))
      (* gam eps))))

(defun mask (counts)
  (map 'list #'(lambda (x)
	      (if (< x 0)
		  0
		  1))
	  counts))

(defun to-bin (lst)
  (let ((ret 0)
	(len (length lst)))
    (loop for i = 0 then (1+ i)
	  and l in lst
	  do (setf ret
		   (logior ret
			   (ash l (- len i 1)))))
    ret))

(defun nrows (arr)
  (car (array-dimensions arr)))

(defun ncols (arr)
  (cadr (array-dimensions arr)))

(defun most-common (lst)
  (let ((ret 0))
    (mapcan #'(lambda (x) (if (= 1 x)
			      (incf ret)
			      (decf ret)))
	    lst)
    (if (>= ret 0) 1 0)))

(defun least-common (lst)
  (let ((ret 0))
    (mapcan #'(lambda (x) (if (= 1 x)
			      (incf ret)
			      (decf ret)))
	    lst)
    (if (< ret 0) 1 0)))

(defun del-rows (pred arr)
  "delete the rows for which PRED evaluates to t in ARR"
  (let ((ret
	  (loop for i from 0 below (nrows arr)
		for row = (row arr i)
		unless (funcall pred row)
		  collect row)))
    (make-array (list (length ret)
		      (length (car ret)))
		:element-type 'bit
		:initial-contents ret)))

(defun o2-rating (filename)
  (let ((inp (load-array (parse filename))))
    (loop while (> (nrows inp) 1)
	  for i from 0 below (ncols inp)
	  for col = (column inp i)
	  for mc = (most-common col)
	  do (setf inp (del-rows
			#'(lambda (row) (/= (nth i row) mc))
			inp)))
	     (to-bin (row inp 0))))

(defun co2-rating (filename)
  (let ((inp (load-array (parse filename))))
    (loop while (> (nrows inp) 1)
	  for i from 0 below (ncols inp)
	  for col = (column inp i)
	  for mc = (least-common col)
	  do (setf inp (del-rows
			#'(lambda (row) (/= (nth i row) mc))
			inp)))
	     (to-bin (row inp 0))))

(defun part2 (filename)
  (* (o2-rating filename) (co2-rating filename)))

;; find the most common digit in the first column (from the right)

(defun row (arr row)
  "return column row of ARR"
  (let ((ret '()))
    (dotimes (col (ncols arr))
      (push (bit arr row col) ret))
    (nreverse ret)))

(defun column (arr col)
  "return column COL of ARR"
  (let ((ret '()))
    (dotimes (row (nrows arr))
      (push (bit arr row col) ret))
    (nreverse ret)))

;; delete the rows that don't have that digit in that column

;; repeat for the other columns or until there is only one number left

(defun load-array (lst)
  (let* ((rows (length lst))
	 (cols (length (car lst)))
	 (ret (make-array (list rows cols)
			  :element-type 'bit)))
    (loop for r = 0 then (1+ r)
	  and row in lst
	  do (loop for c = 0 then (1+ c)
		   and char across row
		   do (setf (bit ret r c) (digit-char-p char))))
    ret))

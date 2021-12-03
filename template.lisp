(defun parse (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  do
	     )))

(defparameter *input* (parse "ex.txt"))

(defun part1 ()
  )

(defun part2 ()
  )

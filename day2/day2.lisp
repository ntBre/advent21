(let ((h 0)
      (d 0)
      (a 0))
  (with-open-file (in "input.txt" :direction :input)
    (loop for line = (read-line in nil nil)
	  while line
	  do (destructuring-bind (dir dist) (mapcar #'read-from-string (uiop:split-string line))
	       (ecase dir
		 (forward
		  (incf h dist)
		  (incf d (* a dist)))
		 (down
		  (incf a dist))
		 (up
		  (decf a dist)))))
    (print (* h d))))

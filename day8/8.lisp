(defparameter *nums*
  #((a b c e f g)
    (c f)
    (a c d e g)
    (a c d f g)
    (b c d f)
    (a b d f g)
    (a b d e f g)
    (a c f)
    (a b c d e f g)
    (a b c d f g)))

(defparameter *num-lengths*
  (map 'list #'length *nums*))

(defun parse (filename)
  "only parse the second part of the input into a list of their
lengths"
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  collecting (mapcar #'length
			     (cdr (uiop:split-string
				   (cadr
				    (uiop:split-string line :separator "|"))))))))

(defparameter *input* (parse "ex.txt"))

(defun part1 ()
  (reduce #'+ (mapcar #'(lambda (x)
	      (if (member x '(1 2 3 4 7 8))
		  1
		  0))
	  (apply #'append (parse "input.txt")))))

(defun part2 ()
  )

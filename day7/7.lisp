(require :uiop)

(defun parse (filename)
  (with-open-file (in filename)
    (mapcar #'read-from-string
	    (uiop:split-string (read-line in nil nil) :separator ","))))

(defun fuel-cost (crabs i)
  "return the fuel cost of all the crabs moving to point i"
  (reduce #'+ (mapcar #'(lambda (x)
			  (abs (- x i)))
		      crabs)))

(defun part1 ()
  (let ((crabs (parse "input.txt")))
    (loop for i from (apply #'min crabs) upto (apply #'max crabs)
	  minimizing (fuel-cost crabs i))))

(defun fuel-costp (crabs i)
  "return the fuel cost of all the crabs moving to point i"
  (reduce #'+ (mapcar #'(lambda (x)
			  (loop for i from 1 upto (abs (- x i))
				summing i))
		      crabs)))

(defun part2 ()
  (let ((crabs (parse "input.txt")))
    (loop for i from (apply #'min crabs) upto (apply #'max crabs)
	  minimizing (fuel-costp crabs i))))

(use-package :cl-ppcre)

(defun parse (filename)
  "all on one line"
  (with-open-file (in filename)
    (let ((line (read-line in nil nil)))
	  (mapcar #'read-from-string (split "," line)))))

(defun event-loop (fish)
  (let ((new-fish '()))
    (append (mapcar #'(lambda (f)
		(if (= 0 f)
		    (progn
		      (push 8 new-fish)
		      6)
		    (decf f)))
		    fish)
	    new-fish)))

(defun part1 ()
  (loop for i from 0 upto 80
	and fish = (parse "input.txt") then (event-loop fish)
	do (format t "day ~a: ~a fish~%" i (length fish))))

(defun part2 ()
  ;; list not sufficient for this, need to use a vector like I thought
  ;; initially
  (loop for i from 0 upto 256
	and fish = (parse "ex.txt") then (event-loop fish)
	do (format t "day ~a: ~a fish~%" i (length fish))))

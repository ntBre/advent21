(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

(defun parse (filename)
  "all on one line"
  (with-open-file (in filename)
    (let ((line (read-line in nil nil)))
	  (mapcar #'read-from-string (split "," line)))))

(defun event-loop (fish)
  (loop for i from 0 below (length fish)
	do (if (= 0 (aref fish i))
	       (progn
		 (vector-push-extend 8 fish)
		 (setf (aref fish i) 6))
	       (decf (aref fish i))))
  fish)

(defun part1 ()
  (let* ((input (parse "ex.txt"))
	 (l (length input))
	 (fish (make-array l
			   :element-type 'integer
			   :initial-contents input
			   :adjustable t
			   :fill-pointer l)))
    (loop for i from 0 upto 80
	  and f = fish then (event-loop f)
	  do (format t "day ~a: ~a fish~%" i (length f)))))

;; instead of naively putting these in a list, I think I should
;; maintain a list of the count of fish at each age

;; each time through, rotate the list

;; 0 goes to 8 and 6 the rest just shift over

(defun parse2 (filename)
  (let ((ret (make-array 9
			 :element-type 'integer
			 :initial-element 0))
	(fish
	  (with-open-file (in filename)
	    (mapcar #'read-from-string (split "," (read-line in nil nil))))))
    (loop for f in fish
	  do (incf (aref ret f)))
    ret))

(defun part2 ()
  ;; use subseq with setf to "rotate" part of the list
  (let ((fish (parse2 "input.txt")))
    (loop for i from 1 upto 256
	  and zero = (aref fish 0) then (aref fish 0)
	  do (setf (subseq fish 0 8) (subseq fish 1))
	     (setf (aref fish 8) zero)
	     (incf (aref fish 6) zero)
	     (format t "day ~a: ~a fish~%" i
		     (reduce #'+ fish)))))

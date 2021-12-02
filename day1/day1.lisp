(defparameter *example* (list 199 200 208
			      210 200 207
			      240 269 260
			      263))

(defun count-increases (lst)
  (let ((last (car lst))
	(count 0))
    (loop for l in lst
	  do (when (> l last)
	       (incf count))
	     (setf last l))
    count))

(defun load-input (filename)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
	  while line
	    collect (read-from-string line))))

;; compute 3buf sums, then loop over those using count-increases
(defun three-buf (lst)
  (loop for i = 0 then (1+ i)
	while (< i (- (length lst) 2))
	collect (+ (elt lst i)
		   (elt lst (+ 1 i))
		   (elt lst (+ 2 i)))))

(defun part-1 ()
  (count-increases (load-input "input.txt")))

(defun part-2 ()
  (count-increases (three-buf (load-input "input.txt"))))

(defun test ()
  (assert (= (part-1) 1832))
  (assert (= (part-2) 1858))
  'pass)

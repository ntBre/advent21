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

;; first part
(count-increases (load-input "input.txt"))

;; compute 3buf sums, then loop over those using count-increases

(defparameter *input* (load-input "input.txt"))

(defun three-buf (lst)
  (loop for i = 0 then (1+ i)
	while (< i (- (length lst) 2))
	collect (+ (elt lst i)
		   (elt lst (+ 1 i))
		   (elt lst (+ 2 i)))))

(count-increases (three-buf *input*))

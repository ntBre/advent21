(defparameter *example* (list 199 200 208
			      210 200 207
			      240 269 260
			      263))

(defun load-input (filename)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
	  while line
	    collect (read-from-string line))))

(defun count-increases (lst)
  (length
   (remove-if-not
    #'(lambda (x) (> (cadr x) (car x)))
    (mapcar #'list lst (cdr lst)))))

(defun part-1 ()
  (let ((input (load-input "input.txt")))
    (count-increases input)))

(defun part-2 ()
  (let ((input (load-input "input.txt")))
    (count-increases
     (mapcar #'+ input
	     (cdr input)
	     (cddr input)))))

(defun test ()
  (assert (= (part-1) 1832))
  (assert (= (part-2) 1858))
  'pass)

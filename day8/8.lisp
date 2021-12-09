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

(defun parts->num (parts)
    (position parts *nums* :test #'equal))

;; length to number
;; 2 => 1
;; 3 => 7
;; 4 => 4
;; 7 => 8

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

(defun part1 ()
  (reduce #'+ (mapcar #'(lambda (x)
			  (if (member x '(2 3 4 7))
			      1
			      0))
		      (apply #'append (parse "input.txt")))))

;; have to map from one set of characters to another

(defun str->syms (str)
  "convert a string of characters to a sorted list of the
corresponding symbols"
  (mapcar #'read-from-string
	  (sort (loop for c across str
		      collecting (string c))
		#'string<)))

(defun convert (str)
  "convert a string of characters to a binary number"
  (let ((ret 0)
	(letters (str->syms str)))
    (loop for i from 7 downto 0
	  do (setf ret
		   (logior ret
			   (if (find (nth i (reverse '(a b c d e f g))) letters)
			       (ash 1 i)
			       0))))
    ret))

(defun parse2 (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  collecting
	  (mapcar #'(lambda (x)
		      (remove-if #'(lambda (str)
				     (string= "" str))
				 (uiop:split-string x)))
		  (uiop:split-string line :separator "|")))))

(defun fiver (str)
  (= 5 (length str)))

(defun sixer (str)
  (= 6 (length str)))

(defun print-bin (num)
  (format t  "~7,'0b~%" num))

(defun get-fivers (elt)
  (remove-if-not #'fiver elt))
(defun get-sixers (elt)
  (remove-if-not #'sixer elt))

(defun find-n (len input)
  (convert (find-if #'(lambda (x)
			(= len (length x)))
		    input)))

(defun extract-key (mask)
  (let ((keys
	  '((#b1000000 . a)
	    (#b0100000 . b)
	    (#b0010000 . c)
	    (#b0001000 . d)
	    (#b0000100 . e)
	    (#b0000010 . f)
	    (#b0000001 . g))))
  (cdr (assoc mask keys))))

(defun make-translator (input)
  (let* ((one (find-n 2 input))
	 (four (find-n 4 input))
	 (seven (find-n 3 input))
	 (eight (find-n 7 input))
	 (fivers (mapcar #'convert (get-fivers input)))
	 (sixers (mapcar #'convert (get-sixers input)))
	 (a (logxor one seven))
	 (f (apply #'logand one sixers))
	 (c (logxor f one))
	 (d (apply #'logand four fivers))
	 (b (logxor d four one))
	 (g (apply #'logxor a b f sixers))
	 (e (logxor eight a b c d f g)))
    (lambda (x)
      (let ((keys
	      `((,(extract-key a) . a)
		(,(extract-key b) . b)
		(,(extract-key c) . c)
		(,(extract-key d) . d)
		(,(extract-key e) . e)
		(,(extract-key f) . f)
		(,(extract-key g) . g))))
	(cdr (assoc x keys))))))

(defun work (training code)
  (labels ((translator (x)
	     (funcall (make-translator training) x)))
    (mapcar #'parts->num
	    (mapcar #'(lambda (x)
			(sort (mapcar
			       #'translator (str->syms x))
			      #'(lambda (x y)
				  (string< (symbol-name x)
					   (symbol-name y)))))
		    code))))

(defun list->num (lst)
  (read-from-string
   (apply #'uiop:strcat (mapcar #'write-to-string lst))))

(defun part2 (filename)
  (reduce #'+
	  (loop for (training code) in (parse2 filename)
		collecting (list->num (work training code)))))

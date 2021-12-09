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

(defun convert (str)
  "convert a string of characters to a binary number"
  (let ((ret 0)
	(letters
	  (mapcar #'read-from-string
		  (sort (loop for c across str
			      collecting (string c))
			#'string<))))
    (loop for i from 7 downto 0
	  do (setf ret
		   (logior ret
			   (if (find (nth i (reverse '(a b c d e f g))) letters)
			       (ash 1 i)
			       0))))
    ret))

(defun parse2 (filename)
  (mapcar #'butlast
	  (mapcar #'uiop:split-string
		  (with-open-file (in filename)
		    (loop for line = (read-line in nil nil)
			  while line
			  collecting (car (uiop:split-string line :separator "|")))))))

(defun part2 ()
  (let ((*print-base* 2))
    (loop for l in
		(mapcar #'convert (car (parse2 "one.txt")))
	  do (print-bin l))))

(defun fiver (str)
  (= 5 (length str)))

(defun sixer (str)
  (= 6 (length str)))

(defun print-bin (num)
  (format t  "~7,'0b~%" num))

(let ((zero  #b1110111)
      (one   #b0010010)
      (two   #b1011101)
      (three #b1011011)
      (four  #b0111010)
      (five  #b1101011)
      (six   #b1101111)
      (seven #b1010010)
      (eight #b1111111)
      (nine  #b1111011))
  (format t "abcdefg~%")
  (print-bin (logxor one seven)) ;; -> a
  (print-bin (logand zero six nine one)) ;; f
  (print-bin (logxor one (logand zero six nine one))) ;; c
  (print-bin (logand two three five four)) ;; d
  (print-bin (logxor four one (logand two three five four))) ;; b
  (print-bin (logand zero six nine))
  )
  ;; xor 1-7 gives a
  ;; (print-bin (logand two three five))
  ;; (print-bin (logxor two five))
  ;; (print-bin (logxor two three five))
  ;; (print-bin (logxor zero six nine))
  ;; (print-bin (logxor two three five
  ;; 		     zero six nine)))

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

(let* ((input (car (parse2 "one.txt")))
       (one (find-n 2 input))
       (four (find-n 4 input))
       (seven (find-n 3 input))
       (eight (find-n 7 input))
       (fivers (mapcar #'convert (get-fivers input)))
       (sixers (mapcar #'convert (get-sixers input)))
       (a (logxor one seven)) ; actually d
       (f (apply #'logand one sixers)) ; actually e
       (c (logxor f one)) ; actually a
       (d (apply #'logand four fivers)) ; f
       (b (logxor d four one)) ; g
       (g (apply #'logxor a b f sixers))
       (e (logxor #b1111111 a b c d f g)))
  (print-bin (logior d g)) ; actually 1
  (print-bin a)
  (print-bin b)
  (print-bin c)
  (print-bin d)
  (print-bin e)
  (print-bin f)
  (print-bin g))

;; compute masks from these and see what I can extract from that

;; get letters corresponding to the input masks, then use those
;; letters and the true masks to decode the message

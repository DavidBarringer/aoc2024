;; Put the given solutions for the examples here
(setf test-sol-a "No test solution")
(setf test-sol-b "No test solution")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let* ((init (split (format nil "~%~%") (get-file-string input-file)))
	 (left (mapcar (lambda (s) (set (intern s) t)) (split (format nil "~%") (CAR init))))
	 (right (mapcar (lambda (s) (split "," s)) (split (format nil "~%") (CADR init)))))
	 right))
	 
(defun median (l)
  (nth (floor (/ (length l) 2)) l))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (apply '+ (mapcar 'parse-integer
		    (mapcar 'median
			    (loop for pages in parsed-input
				  if (loop for x in pages
					   for rest on (CDR pages)
					   always
					   (loop for r in rest always (boundp (intern (format nil "~A|~A" x r)))))
				    collect pages)))))

(defun reorder (x xs)
  (cond ((null xs) (list x))
	((boundp (intern (format nil "~A|~A" (CAR xs) x))) (cons (CAR xs) (reorder x (CDR xs))))
	(t  (cons x (reorder (CAR xs) (CDR xs))))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (part-a (loop for pages in parsed-input
	if (loop for x in pages
		 for rest on (CDR pages)
		   thereis
		   (loop for r in rest thereis (not (boundp (intern (format nil "~A|~A" x r))))))
	  collect (loop
		    repeat (length pages)
		    do (setf pages (reorder (CAR pages) (CDR pages)))
		    finally (return pages)))))

;; Put the given solutions for the examples here
(setf test-sol-a "16")
(setf test-sol-b "9")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (concatenate 'list s)) (get-file-lines input-file))))
    (make-array (list (length input) (length (CAR input))) :initial-contents input)))

(defun scanLines (arr x y)
  (loop for x-step from -1 to 1
	sum (loop for y-step from -1 to 1
		  count (loop for steps from 1 to 3
			      for check in '(#\M #\A #\S)
			      for new-x = (+ x (* x-step steps))
			      for new-y = (+ y (* y-step steps))
			      always (and (<= 0 new-x) (<= 0 new-y)
					  (> (CAR (array-dimensions arr)) new-x)
					  (> (CADR (array-dimensions arr)) new-y)
					  (eql check (aref arr new-x new-y)))))))

(defun scan-x (arr x y)
  (loop for (x1 y1 x2 y2) in '((-1 -1 1 1) (-1 1 1 -1))
	for new-x1 = (+ x x1)
	for new-x2 = (+ x x2)
	for new-y1 = (+ y y1)
	for new-y2 = (+ y y2)
	always (and (<= 0 new-x1) (<= 0 new-x2) (<= 0 new-y1) (<= 0 new-y2)
		   (> (CAR (array-dimensions arr)) new-x1)
		   (> (CAR (array-dimensions arr)) new-x2)
		   (> (CADR (array-dimensions arr)) new-y1)
		   (> (CADR (array-dimensions arr)) new-y2)
		   (or (and (eql #\M (aref arr new-x1 new-y1)) (eql #\S (aref arr new-x2 new-y2)))
		       (and (eql #\S (aref arr new-x1 new-y1)) (eql #\M (aref arr new-x2 new-y2)))))))
			      

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for x from 0 below (CAR (array-dimensions parsed-input))
	sum (loop for y from 0 below (CADR (array-dimensions parsed-input))
		  if (eql (aref parsed-input x y) #\X) sum (scanLines parsed-input x y))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for x from 0 below (CAR (array-dimensions parsed-input))
	sum (loop for y from 0 below (CADR (array-dimensions parsed-input))
		  if (eql #\A (aref parsed-input x y))  count (scan-x parsed-input x y))))

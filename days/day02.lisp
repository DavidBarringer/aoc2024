;; Put the given solutions for the examples here
(setf test-sol-a "2")
(setf test-sol-b "No test solution")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (mapcar 'parse-integer (split #\  s))) (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for report in parsed-input
	count (safe-report report)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for report in parsed-input
	count  (loop for x from 0 to (length report)
		     for tolerance = (append (subseq report 0 x)
					     (if (/= (length report) x) (subseq report (+ 1 x))))
		       thereis (safe-report tolerance))))

(defun safe-report (report)
    (and
     (or (apply '< report) (apply '> report))
     (loop for (x y) on report until (null y)
	   always (<= (abs (- x y)) 3))))

;; Put the given solutions for the examples here
(setf test-sol-a "11")
(setf test-sol-b "31")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (apply 'mapcar 'list
	 (mapcar (lambda (l) (mapcar 'parse-integer (split "\\s+" l))) (get-file-lines input-file))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let
      ((sort-l1 (sort (CAR parsed-input) '<))
       (sort-l2 (sort (CADR parsed-input) '<)))
    (loop for x in sort-l1
	  for y in sort-l2
	  sum(abs (- x y)))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for x in (CAR parsed-input)
	sum (* x (loop for y in (CADR parsed-input) count (= y x)))))

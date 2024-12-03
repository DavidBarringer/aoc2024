;; Put the given solutions for the examples here
(setf test-sol-a "No test solution")
(setf test-sol-b "No test solution")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (get-file-string input-file))
  

;; Returns the solution for part a
(defun part-a (parsed-input)
  (sum-mults parsed-input))
  

;; Returns the solution for part b
(defun part-b (parsed-input)
  (sum-mults (regex-replace "don't\\(\\).*$" (regex-replace-all "don't\\(\\).*?do\\(\\)" (remove #\Newline parsed-input) "") "")))

(defun sum-mults (input)
  (apply '+ (mapcar 'eval
		    (mapcar 'read-from-string
			    (mapcar (lambda (s)
				      (regex-replace "mul\\(([0-9]+),([0-9]+)\\)" s "(* \\1 \\2)"))
				    (all-matches-as-strings "mul\\(([0-9]+),([0-9]+)\\)" input))))))

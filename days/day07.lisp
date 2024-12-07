;; Put the given solutions for the examples here
(setf test-sol-a "3749")
(setf test-sol-b "11387")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (mapcar (lambda (i) (parse-integer i :junk-allowed t)) (split " " s))) (get-file-lines input-file)))

(defun exprs (expr)
  (if (< 1 (length expr))
      (nconc (exprs (cons (+ (CAR expr) (CADR expr)) (CDDR expr)))
	     (exprs (cons (* (CAR expr) (CADR expr)) (CDDR expr))))
      expr))

(defun b-exprs (expr)
  (if (< 1 (length expr))
      (nconc (b-exprs (cons (+ (CAR expr) (CADR expr)) (CDDR expr)))
	     (b-exprs (cons (* (CAR expr) (CADR expr)) (CDDR expr)))
	     (b-exprs (cons (parse-integer (format nil "~A~A" (CAR expr) (CADR expr))) (CDDR expr))))
      expr))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for target in (mapcar 'CAR parsed-input)
	for expr in (mapcar 'CDR parsed-input)
	if (some (lambda (f) (= target f)) (exprs expr))
	  sum target))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for target in (mapcar 'CAR parsed-input)
	for expr in (mapcar 'CDR parsed-input)
	if (some (lambda (f) (= target f)) (b-exprs expr))
	  sum target))

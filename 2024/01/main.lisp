;; - Input: 2 columns of numbers
;; - Need to sort the lists and find the difference between each index pair of numbers
;; - e.g. difference between the smallest number (0th) and so on
;; - Find the sum of all the pair differnces to get the total distance between the 2 lists of nums

;; Steps
;; - Parse input into 2 lists
;; - Sort both lists
;; - Initialize res var
;; - Iterate over the length each list and copmute the diff at that index
;; - return res var

(defun split-into-nums (str)
  (let* ((space-pos (position #\space str))
	 (s1 (subseq str 0 space-pos))
	 (s2 (subseq str (+ 1 space-pos))))
    (list (parse-integer s1)
	  (parse-integer (string-trim '(#\Space) s2)))))

(defun parse-input (input-file)
  ;; Return 2 lists of numbers
  (let ((a '())
	(b '()))
    (with-open-file (stream input-file :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((nums (split-into-nums line)))
		 (push (first nums) a)
		 (push (second nums) b))))

    ;; The values function allows us to explicitly return multiple values from the function
    (values (nreverse a) (nreverse b))))

(defun main ()
  (multiple-value-bind (a b) (parse-input "/home/ajpkim/projects/aoc/2024/01/input.txt")
    ;; Use copies of the lists for sorting since sorting can be destructive...
    (setq a (sort (copy-list a) #'<))
    (setq b (sort (copy-list b) #'<))
    ;; map reduce for the sum of abs diff between index pairs
    (reduce #'+ (mapcar (lambda (x y) (abs (- x y))) a b))))

(print (main))

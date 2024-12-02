;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Input: 2 columns of numbers

;;;;;;;;;; Part 1 ;;;;;;;;;;
;; - Need to sort the lists and find the difference between each index pair of numbers
;; - e.g. difference between the smallest number (0th) and so on
;; - Find the sum of all the pair differnces to get the total distance between the 2 lists of nums

;; Steps
;; - Parse input into 2 lists
;; - Sort both lists
;; - Initialize res var
;; - Iterate over the length each list and copmute the diff at that index
;; - return res var

;;;;;;;;;; Part 2 ;;;;;;;;;;
;; Calculate a total similarity score by adding up each number in the left list after multiplying it by the number of times that number appears in the right list.

;; Steps
;; - Transform 2nd list into hash table that counts the number of times each num appears
;; - Process each number in the first list and multiply its value by hash-table[n] value


(defvar *input-file* "/home/ajpkim/projects/aoc/2024/01/input.txt")

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

(defun part-1 ()
  (multiple-value-bind (a b) (parse-input *input-file*)
    ;; Use copies of the lists for sorting since sorting can be destructive...
    (let ((sorted-a (sort (copy-list a) #'<))
          (sorted-b (sort (copy-list b) #'<)))
      ;; map reduce for the sum of abs diff between index pairs
      (reduce #'+ (mapcar #'(lambda (x y) (abs (- x y))) sorted-a sorted-b)))))

(defun num-counts (nums)
  ;; Return a hash table with keys of nums in original list and vals set to their count
  (let ((counts (make-hash-table)))
    (dolist (n nums counts)
      (incf (gethash n counts 0)))))

(defun part-2 ()
  (multiple-value-bind (a b) (parse-input *input-file*)
    (let ((counts (num-counts b))
	  (res 0))
      (dolist (n a res)
	(incf res (* n (gethash n counts 0)))))))


(format t "Part 1: ~a~%" (part-1))
(format t "Part 2: ~a~%" (part-2))

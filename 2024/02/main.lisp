;; The unusual data (your puzzle input) consists of many reports, one
;; report per line. Each report is a list of numbers called levels
;; that are separated by spaces. For example:

;; 7 6 4 2 1
;; 1 2 7 8 9
;; 9 7 6 2 1
;; 1 3 2 4 5
;; 8 6 4 4 1
;; 1 3 6 7 9

;; Rules for "safe" reports, BOTH must be true:
;; - The levels are either all increasing or all decreasing.
;; - Any two adjacent levels differ by at least one and at most three.

(ql:quickload "split-sequence")

(defvar *input-file* "/home/ajpkim/projects/aoc/2024/02/input.txt")

(defun parse-input (filepath)
  (with-open-file (stream filepath :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (mapcar #'parse-integer (split-sequence:split-sequence #\Space line)))))

;; ;; Iterative implementation
;; (defun is-report-safe-p (report)
;;   (let ((increasing (> (second report) (first report))))
;;     (loop for prev in report
;;           for curr in (rest report)
;;           for step = (- curr prev)
;;           do (when (or (> (abs step) 3)
;;                        (= step 0)
;;                        (and increasing (< step 0))
;;                        (and (not increasing) (> step 0)))
;;                (return-from is-report-safe-p nil))
;;           finally (return t))))

;; Recursive implementation
(defun is-report-safe-p (report)
  ;; Use 'labels' for inner func definitions
  (labels ((inner-is-report-safe-p (report prev_num increasing)
    ;; Recursive base condition
    (if (null report)
	t
	(let ((step (- (first report) prev_num)))
	  ;; Check our step magnitide and monotonicity conditions
	  (if (or (> (abs step) 3)
		  (= 0 step)
		  (and increasing (< step 0))
		  (and (not increasing) (> step 0)))
	      nil
	      ;; Recursive call with updated state vars
	      (inner-is-report-safe-p (cdr report) (first report) increasing)))))))
  (inner-is-report-safe-p (cdr report) (first report) (> (second report) (first report))))


(defun part-1 ()
  (let ((reports (parse-input *input-file*))
	(res 0))
    (dolist (report reports res)
      (when (is-report-safe-p report)
	(incf res)))))


(format t "Part 1: ~a~%" (part-1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other part 1 implementations... Reduce and Count implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun part-1 ()
;;   (let ((reports (parse-input *input-file*)))
;;     (reduce (lambda (acc report)
;; 	      (if (is-safe-p report)
;; 		  (1+ acc)
;; 		  acc))
;; 	    reports
;; 	    :initial-value 0)))

;; (defun part-1 ()
;;   (let ((reports (parse-input *input-file*)))
;;     (count t reports :test #'is-safe-p)))


;;;;;;; Recursive implementation using 'labels'
;; (defun is-report-safe-p (report)
;;   (labels ((is-report-safe-rec (report prev-num increasing)
;;              (if (null report)
;;                  t
;;                  (let ((step (- (first report) prev-num)))
;;                    (if (or (> (abs step) 3)
;;                            (= step 0)
;;                            (and increasing (< step 0))
;;                            (and (not increasing) (> step 0)))
;;                        nil
;;                        (is-report-safe-rec (cdr report) (first report) increasing))))))
;;     (is-report-safe-rec (cdr report) (first report)
;;                         (> (second report) (first report)))))

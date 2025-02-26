(ql:quickload "split-sequence")

(defvar *input-file* "/home/ajpkim/projects/aoc/2024/03/input.txt")

(defun parse-input (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

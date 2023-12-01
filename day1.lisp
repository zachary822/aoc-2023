; part 1
(ql:quickload :cl-ppcre)

(defun get-row-num (line)
  (let ((row (remove-if-not #'digit-char-p line)))
        (setf row (parse-integer
                   (concatenate 'string
                                (string (char row 0))
                                (string (char row (1- (length row)))))))))

(let ((in (open "day1.txt")) (res (list)))
  (loop
    for line = (read-line in nil) while line do
        (push (get-row-num line) res))
  (format t "~a~%" (reduce #'+ res))
  (close in))

(defparameter *numbers* (list "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(defparameter *integers* (let ((res nil)) (dotimes (i 9) (push (write-to-string (1+ i)) res)) (nreverse res)))

(let ((in (open "day1.txt")) (res nil))
  (loop for line = (read-line in nil)
        while line do (let ((l line))
                        (loop for i in *numbers*
                              for j in *integers*
                              collect (setf l (cl-ppcre:regex-replace-all i l (concatenate 'string i j i))))
                        (push (get-row-num l) res)))
  (format t "~a~%" (reduce #'+ res)))

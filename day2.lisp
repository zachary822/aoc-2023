(ql:quickload :cl-ppcre)

(defparameter *bag* (list 'red 12 'green 13 'blue 14))

(defun split-color (str)
  (let ((s (cl-ppcre:split " " str :limit 2)))
    (setf (car s) (parse-integer (car s)))
    (setf (cdr s) (read-from-string (car (last s))))
    s))

(defun check-game (draws)
  (every 'identity (mapcar #'(lambda (d) (>= (getf *bag* (cdr d)) (car d))) draws)))

(defun parse-game (str)
  (mapcar
   'split-color
   (cl-ppcre:all-matches-as-strings "\\d+ (red|green|blue)" str)))

(defun plist-values (pl)
  (loop for (key value) on pl by #'cddr collect value))

(defun collapse-game (draws)
  (let ((m (list 'red 0 'green 0 'blue 0)))
    (loop for d in draws
          do (when (> (car d) (getf m (cdr d)))
                    (setf (getf m (cdr d)) (car d))))
    (reduce '* (plist-values m))))

; part 1
(let ((in (open "day2.txt")) (res nil))
  (loop for line = (read-line in nil)
        while line do
          (let ((game (cl-ppcre:split ":\\s*" line)))
            (if (check-game
                 (parse-game
                  (car (last game))))
                (push (car game) res))))
  (format t "~a~%" (reduce '+ (mapcar #'(lambda (g) (parse-integer (car (cl-ppcre:all-matches-as-strings "\\d+" g)))) res))))

; part 2
(let ((in (open "day2.txt")) (res nil))
  (loop for line = (read-line in nil)
        while line do
          (let ((game (cl-ppcre:split ":\\s*" line)))
            (let ((parsed (parse-game (car (last game)))))
              (push (collapse-game parsed) res))))
  (format t "~a~%" (reduce '+ res)))

(in-package :aoc.2023)
(defparameter *table-size* 213)

(defun day4-part-one ()
  (loop for line in (file-lines #p"input/day4.input")
        sum (process-line line)))

(defun day4-part-two ()
  (let ((instances (make-array *table-size* :initial-element 1)))
    (dolist (line (file-lines #p"input/day4.input"))
      (multiple-value-bind (game-id winners) (parse-line line)
        (let ((copies (aref instances (1- game-id))))
          (when (and (> copies 0) (> winners 0))
            ;; forward propagate the copies
            (dotimes (c copies)
              (loop for i from game-id to (min (+ game-id (1- winners)) (1- *table-size*)) ; don't execeed table boundaries
                    do (incf (aref instances i))))))))
    (reduce #'+ instances)))))

(defun process-line (line)
  (multiple-value-bind (game-id winners) (parse-line line)
    (declare (ignore game-id))
    (cond
      ((zerop winners) 0)
      ((= 1 winners) 1)
      (t (expt 2 (1- winners))))))

(defun parse-line (line)
  (destructuring-bind (game-segment data-segment) (cl-ppcre:split ":\\s+" line)
    (destructuring-bind (winning-segment my-segment) (str:split "|" data-segment)
      (let ((winning-cards  (coerce (mapcar #'parse-integer (cl-ppcre:split "\\s+" (str:trim winning-segment))) 'vector))
            (my-cards       (mapcar #'parse-integer (cl-ppcre:split "\\s+" (str:trim my-segment)))))
        (let ((game-id (parse-integer (cadr (cl-ppcre:split "\\s+" game-segment))))
              (winners (loop for card in my-cards counting (s:true (find card winning-cards)))))
          (values game-id  winners))))))

(define-test example-cases-work ()
  (assert-equal 8 (process-line  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
  (assert-equal 2  (process-line "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"))
  (assert-equal 2  (process-line "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"))
  (assert-equal 1  (process-line "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"))
  (assert-equal 0  (process-line "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"))
  (assert-equal 0  (process-line "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")))

(define-test day4-part-one-works ()
  (assert-equal 25183 (day4-part-one)))

(define-test day4-part-two-works ()
  (assert-equal 5667240 (day4-part-two)))

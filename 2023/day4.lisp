(in-package :aoc.2023)

(defparameter *table-size* 213)

(defun day4-part-one ()
  (loop for line in (file-lines #p"input/day4.input")
        for winners = (parse-line line)
        sum (if (>= winners 1) (expt 2 (1- winners)) winners)))

(defun day4-part-two ()
  (let ((instances (make-array *table-size* :initial-element 1 :element-type 'fixnum)))
    (dolist (line (file-lines #p"input/day4.input"))
      (multiple-value-bind (winners game-id) (parse-line line)
        (let ((copies (aref instances (1- (the fixnum game-id)))))
          (when (and (> copies 0) (> (the fixnum winners) 0))
            ;; forward propagate the copies
            (dotimes (c copies)
              (loop for i from game-id to (min (+ game-id (1- winners)) (1- *table-size*)) ; don't execeed table boundaries
                    do (incf (aref instances i))))))))
    (reduce #'+ instances)))))

(-> parse-line (string) (values integer integer))
(defun parse-line (line)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (destructuring-bind (game-segment data-segment) (cl-ppcre:split ":\\s+" line)
    (destructuring-bind (winning-segment my-segment) (str:split "|" data-segment)
      (let ((winning-cards  (mapcar #'parse-integer (cl-ppcre:split "\\s+" (str:trim winning-segment))))
            (my-cards       (mapcar #'parse-integer (cl-ppcre:split "\\s+" (str:trim my-segment)))))
        (let ((game-id (parse-integer (cadr (cl-ppcre:split "\\s+" game-segment))))
              (winners (length (intersection winning-cards my-cards :test #'=))))
          (values (the fixnum winners) (the fixnum game-id)))))))

(define-test day4-part-one-works ()
  (assert-equal 25183 (day4-part-one)))

(define-test day4-part-two-works ()
  (assert-equal 5667240 (day4-part-two)))

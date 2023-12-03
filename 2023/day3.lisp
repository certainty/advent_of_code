(in-package :aoc.2023)

(defparameter *matrix-size* 140)

(defun day3-part-one ()
  (loop with result = 0
        with number-part-p = nil
        with current-number = nil ; we have to put it here because digits can span multiple lines
        with matrix = (input-matrix)
        for row from 0 below *matrix-size*
        do (loop
             for column from 0 below *matrix-size*
             for cur = (aref matrix row column)
             do (cond
                  ((digit-char-p cur)
                   (push cur current-number)
                   (when (adjacent-symbol-p matrix row column)
                     (setf number-part-p t)))
                  ((not (null current-number))
                   (when number-part-p
                     (incf result (parse-integer (coerce (reverse current-number) 'string))))
                   (setf current-number nil)
                   (setf number-part-p nil))))
        finally (return result)))

(defun input-matrix ()
  (with-open-file (stream (truename "input/day3.input"))
    (let ((chars (loop for line = (read-line stream nil)
                       for i from 0
                       while line
                       collect (loop for c across line collect c))))
      (make-array (list *matrix-size* *matrix-size*)
                  :initial-contents chars
                  :element-type 'character))))

(defun symbol-p (c)
  (and (not (digit-char-p c)) (not (char= c #\.))))

(defun adjacent-symbol-p (matrix row column)
  (let ((above (1- row))
        (below (1+ row))
        (left (1- column))
        (right (1+ column)))
    (or
     (and (>= above 0) (symbol-p (aref matrix above column)))
     (and (>= above 0) (>= left 0) (symbol-p (aref matrix above left)))
     (and (>= above 0) (<= right (1- *matrix-size*)) (symbol-p (aref matrix above right)))
     (and (>= left 0) (symbol-p (aref matrix row left)))
     (and (<= right (1- *matrix-size*)) (symbol-p (aref matrix row right)))
     (and (<= below (1- *matrix-size*)) (symbol-p (aref matrix below column)))
     (and (<= below (1- *matrix-size*)) (>= left 0) (symbol-p (aref matrix below left)))
     (and (<= below (1- *matrix-size*)) (<= right (1- *matrix-size*)) (symbol-p (aref matrix below right))))))

(define-test day3-part-one-test
    (assert-equal 557705 (day3-part-one)))

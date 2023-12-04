(in-package :aoc.2023)

(defparameter *matrix-size* 140)

(defun day3-part-one ()
  (loop with result = 0
        with number-part-p = nil
        with current-number = nil
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

(defun day3-part-two ()
  (loop with result = 0
        with matrix = (input-matrix)
        for row from 0 below *matrix-size*
        do (loop for column from 0 below *matrix-size*
                 for cur = (aref matrix row column)
                 when (char= #\* cur)
                   do
                      (a:when-let ((adjacent (adjacent-numbers matrix row column)))
                        (when (= (length adjacent) 2)
                          (incf result (apply #'* adjacent)))))
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
  (loop for (r  c) in (adjacent-coordinates row column)
        thereis (symbol-p (aref matrix r c))))

(defun adjacent-coordinates (row column)
  "Returns a list of valid coordinates adjacent to the given one."
  (loop for (drow dcol) in '((0 1) (0 -1) (1 0) (-1 0)
                             (-1 -1) (-1 1) (1 -1) (1 1))
        for new-row = (+ row drow)
        for new-col = (+ column dcol)
        if (and (>= new-row 0) (< new-row *matrix-size*)
                (>= new-col 0) (< new-col *matrix-size*))
          collect (list new-row new-col)))

(defun adjacent-numbers (matrix row column)
  (let ((numbers nil))
    (dolist (coord (adjacent-coordinates row column))
      (let ((r (first coord))
            (c (second coord)))
        (when (digit-char-p (aref matrix r c))
          (push (scan-number matrix r c) numbers))))
    (remove-duplicates numbers :test #'=)))

(defun scan-number (matrix row column)
  "given the start coordinates scan the whole number. Numbers can span multiple lines."
  (let ((left nil)
        (right nil))
    ;; left
    (let ((r row)
          (c (max 0 (1- column))))
      (loop
        (unless (digit-char-p (aref matrix r c))
          (return))
        (push (aref matrix r c) left)
        (when (zerop c)
          (return))
        (decf c)))

    ;; right
    (let ((r row)
          (c column))
      (loop
        (unless (digit-char-p (aref matrix r c))
          (return))
        (push (aref matrix r c) right)
        (when (= c (1- *matrix-size*))
          (return))
        (incf c)))
    (parse-integer (coerce (append left (reverse right)) 'string))))

(define-test day3-part-one-test ()
  (assert-equal 557705 (day3-part-one)))

(define-test day3-part-two-test ()
  (assert-equal 84266818 (day3-part-two)))

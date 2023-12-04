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
  (let ((above (1- row))
        (below (1+ row))
        (left (1- column))
        (right (1+ column)))
    `(,@(and (>= above 0) (list (list above column)))
      ,@(and (>= above 0) (>= left 0) (list (list above left)))
      ,@(and (>= above 0) (<= right (1- *matrix-size*)) (list (list above right)))
      ,@(and (>= left 0) (list (list row left)))
      ,@(and (<= right (1- *matrix-size*)) (list (list row right)))
      ,@(and (<= below (1- *matrix-size*)) (list (list below column)))
      ,@(and (<= below (1- *matrix-size*)) (>= left 0) (list (list below left)))
      ,@(and (<= below (1- *matrix-size*)) (<= right (1- *matrix-size*)) (list (list below right))))))

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
        (cond
          ((and (zerop c) (zerop r))
           (return))
          ((zerop c)
           (decf r)
           (setf c (1- *matrix-size*)))
          (t (decf c)))))

    ;; right
    (let ((r row)
          (c column))
      (loop
        (unless (digit-char-p (aref matrix r c))
          (return))
        (push (aref matrix r c) right)
        (cond
          ((and (= (1- *matrix-size*) c) (>= (1+ r) *matrix-size*))
           (return))
          ((= (1- *matrix-size*) c)
           (incf r)
           (setf c 0))
          (t (incf c)))))
    (parse-integer (coerce (append left (reverse right)) 'string))))

(define-test day3-part-one-test ()
  (assert-equal 557705 (day3-part-one)))

(define-test day3-part-two-test ()
  (assert-equal 84266818 (day3-part-two)))

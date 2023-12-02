(in-package :aoc.2023)

(defun day2-part-one ()
  (loop with result = 0
        for line in (file-lines #p"input/day2.input")
        for game = (parse-game-record line)
        when (game-possible-p game)
          do (incf result (game-id game))
        finally (return result)))

(s:defconstructor cube-set
  (red integer)
  (blue integer)
  (green integer))

(s:defconstructor game
  (id integer)
  (sets list))

(defun game-possible-p (game)
  (every (lambda (set)
           (and (<= (cube-set-red set) 12)
                (<= (cube-set-green set) 13)
                (<= (cube-set-blue set) 14)))
         (game-sets game)))

(defun parse-game-record (record)
  (destructuring-bind (game-info set-info) (str:split ":" record)
    (let* ((game-id (parse-integer (cadr (str:split " " game-info))))
           (sets (str:split ";" set-info)))
      (game game-id (mapcar #'parse-cube-set sets)))))

(defun parse-cube-set (input)
  (loop with red = 0
        with green = 0
        with blue = 0
        for config in (str:split "," input)
        for (num color) = (str:split " " (str:trim config))
        when (equal color "red")
          do (setf red (parse-integer num))
        when (equal color "green")
          do (setf green (parse-integer num))
        when (equal color "blue")
          do (setf blue (parse-integer num))
        finally (return (cube-set red blue green))))

(define-test day2-part-one-works ()
  (assert-equal 2685 (day2-part-one)))

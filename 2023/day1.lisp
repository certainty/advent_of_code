(in-package :aoc.2023)

(defun part-one ()
  (loop for line in (file-lines #p"input/day1.input")
        summing (callibration-values line)))

(defun part-two ()
  (loop for line in (file-lines #p"input/day1.input")
        summing (callibration-values (convert-digit-names line))))

(defun callibration-values (line)
  (let ((first-digit (find-if #'digit-char-p line))
        (last-digit (find-if #'digit-char-p line :from-end t)))
    (parse-integer (format nil "~A~A" first-digit last-digit))))

;; does more work than necessary since we process the whole string
(defun convert-digit-names (line)
  (loop with result = nil
        for idx from 0 below (length line)
        for digit = (digit-name->digit line idx)
        if digit
          do (push digit result)
        else
          do (push (aref line idx) result)
        finally (return (format nil "~{~a~}" (reverse result)))))

(defun digit-name->digit (input start-pos)
  (loop for digit from 0 to 9
        when (str:starts-with-p (format nil "~r" digit) (slice input start-pos))
          do (return-from digit-name->digit (format nil "~a" digit))))

(define-test part-one-works ()
  (assert-equal 54697 (part-one)))

(define-test part-two-works ()
  (assert-equal 54885 (part-two)))

(define-test substitution-works ()
  (assert-equal 29 (callibration-values (convert-digit-names "two1nine")))
  (assert-equal 83 (callibration-values (convert-digit-names "eightwothree")))
  (assert-equal 13 (callibration-values (convert-digit-names "abcone2threexyz")))
  (assert-equal 24 (callibration-values (convert-digit-names "xtwone3four")))
  (assert-equal 42 (callibration-values (convert-digit-names "4nineeightseven2")))
  (assert-equal 14 (callibration-values (convert-digit-names "zoneight234")))
  (assert-equal 76 (callibration-values (convert-digit-names "7pqrstsixteen"))))

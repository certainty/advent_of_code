(in-package :aoc.2023)

(defparameter *input-path* (truename #p"input/day5.example.input"))

(defun day5-part-one ()
  (with-open-file (stream *input-path*)
    (parse-input stream)))

(defun parse-input (stream)
  "Parses the input and returns the values `seeds' and `mappings'"
  (let ((seeds (read-line stream)))
    (read-line stream)
    (let ((mappings (loop for line = (read-line stream nil nil)
                          for i in '(s2s s2f f2w w2l l2t t2h h2l)
                          while line
                          collect (cons i (parse-block stream)))))
      (values
       (mapcar #'parse-integer (cdr (cl-ppcre:split "\\s+" seeds)))
       mappings))))

(defun parse-block (stream)
  "Parses a block an returns the list of mappins sourted by source range start"
  (loop for line = (read-line stream nil nil)
        while (and line (not (string= line "")))
        collect (parse-block-line line) into mappings
        finally (return (sort mappings #'< :key (lambda (mapping) (car (mapping-source-range mapping)))))))

(s:defconstructor mapping
  (source-range list)
  (target-range list))

(defun parse-block-line (line)
  (destructuring-bind (target-start source-start range-size) (mapcar #'parse-integer (cl-ppcre:split "\\s+" line))
    (mapping (cons source-start (+ source-start range-size))
             (cons target-start (+ target-start range-size)))))

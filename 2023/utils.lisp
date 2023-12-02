(in-package :aoc.2023)

(defun file-lines (filename)
  (with-open-file (stream (truename filename))
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

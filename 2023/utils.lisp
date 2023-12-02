(in-package :aoc.2023)

(defun file-lines (filename)
  (with-open-file (stream (truename filename))
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun slice (seq start &optional (end nil))
  ;; create displaced array for slice
  (let ((len (if end (- end start) (- (length seq) start))))
    (make-array len :displaced-to seq
                    :displaced-index-offset start
                    :element-type (array-element-type seq))))

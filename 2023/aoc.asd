(in-package :asdf-user)

(defsystem "aoc"
  :description "Advent of code 2023:"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/advent_of_code.git")
  :serial t
  :depends-on (:serapeum :alexandria :str :lisp-unit2)
  :components
  ((:file "packages")
   (:file "utils")
   (:file "day1")
   (:file "day2")
   (:file "day3")
   (:file "day4")))

(load "util.lisp")

(defpackage :day-01
  (:use :cl :util))
(in-package :day-01)

(defun elf-calories (stream &optional (acc 0))
  (let ((line (read-line stream nil nil)))
    (if line
        (cond
          ((zerop (length line)) acc)
          (t (elf-calories stream (+ acc (parse-integer line))))))))

(defun calories-list ()
  (with-input-file (in)
    (loop for elf-calories = (elf-calories in)
          while elf-calories
          collect elf-calories into list
          finally (return (sort list #'>)))))

(defun part-1 ()
  (first (calories-list)))

(defun part-2 ()
  (reduce #'+ (subseq (calories-list) 0 3)))

(defun day-01 ()
  (format t "answer 1: ~A~%answer 2: ~A~%" (part-1) (part-2)))

(load "util.lisp")

(defpackage :day-03
  (:use :cl :util))
(in-package :day-03)

(defun item-priority (item)
  (let ((item-code (char-code item)))
    (cond ((<= (char-code #\a) item-code (char-code #\z))
           (1+ (- item-code (char-code #\a))))
          ((<= (char-code #\A) item-code (char-code #\Z))
           (+ 27 (- item-code (char-code #\A)))))))

(defun misplaced-item-priority (&rest sets)
  (let ((misplaced-items
          (reduce #'(lambda (set1 set2)
                      (remove-duplicates (intersection set1 set2 :test #'char=)))
                  sets)))
    (assert (= (length misplaced-items) 1))
    (item-priority (first misplaced-items))))

(defun line-sets (line)
  (let ((len (length line)))
    (list
     (coerce (subseq line 0 (/ len 2)) 'list)
     (coerce (subseq line (/ len 2)) 'list))))

(defun read-group-sets (stream)
  (loop repeat 3
        for line = (read-line stream nil nil)
        while line
        collect (coerce line 'list)))

(defun part-1 ()
  (with-input-file (in)
    (loop for line = (read-line in nil nil)
          while line
          sum (apply #'misplaced-item-priority (line-sets line)))))

(defun part-2 ()
  (with-input-file (in)
    (loop for group-sets = (read-group-sets in)
          while group-sets
          sum (apply #'misplaced-item-priority group-sets))))

(defun day-03 ()
  (format t "answer 1: ~A~%answer 2: ~A~%" (part-1) (part-2)))

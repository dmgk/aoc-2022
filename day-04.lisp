(load "util.lisp")

(defpackage :day-04
  (:use :cl :util))
(in-package :day-04)

(defun read-range-pair (line)
  (let ((start 0))
    (flet ((read-n (line)
             (multiple-value-bind (n pos) (parse-integer line :start start :junk-allowed t)
               (setf start (1+ pos))
               n)))
      (list (cons (read-n line) (read-n line))
            (cons (read-n line) (read-n line))))))

(defun l-contains-r-p (l r)
  (and (<= (car l) (car r))
       (>= (cdr l) (cdr r))))

(defun containing-pair-p (range-pair)
  (or (l-contains-r-p (first range-pair) (second range-pair))
      (l-contains-r-p (second range-pair) (first range-pair))))

(defun overlapping-pair-p (range-pair)
  (let ((l (first range-pair))
        (r (second range-pair)))
    (or (<= (car l) (car r) (cdr l))
        (<= (car r) (car l) (cdr r))
        (containing-pair-p range-pair))))

(defun part-1 ()
  (with-input-file (in)
    (loop for line = (read-line in nil nil)
          while line
          if (containing-pair-p (read-range-pair line))
            count it)))

(defun part-2 ()
  (with-input-file (in)
    (loop for line = (read-line in nil nil)
          while line
          if (overlapping-pair-p (read-range-pair line))
            count it)))

(defun day-04 ()
  (format t "answer 1: ~A~%answer 2: ~A~%" (part-1) (part-2)))

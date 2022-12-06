(load "util.lisp")

(defpackage :day-06
  (:use :cl :util))
(in-package :day-06)

(defun find-marker (list subseq-length)
  (loop for i from (1- subseq-length) below (length list)
        if (apply #'char/= (subseq list (- i (1- subseq-length)) (1+ i)))
          return (1+ i)))

(defun part-1 ()
  (with-input-file (in)
    (let ((input (coerce (read-line in nil) 'list)))
      (find-marker input 4))))

(defun part-2 ()
  (with-input-file (in)
    (let ((input (coerce (read-line in nil) 'list)))
      (find-marker input 14))))

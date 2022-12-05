(load "util.lisp")

(defpackage :day-05
  (:use :cl :util))
(in-package :day-05)

(defun load-stacks (stream)
  (loop for line = (read-line stream nil nil)
        with stacks = (make-array 9 :initial-element ())
        while (position #\[ line)
        do
           (loop for i below (array-dimension stacks 0)
                 for c = (char line (1+ (* i 4)))
                 if (alpha-char-p c)
                   do (push c (aref stacks i)))
        finally
           (loop for i below (array-dimension stacks 0)
                 do (setf (aref stacks i) (nreverse (aref stacks i))))
           (return stacks)))

(defun move-op (stacks count from to &key preserve-order)
  (let ((sf (aref stacks from))
        (st (aref stacks to)))
    (if preserve-order
        (setf st (concatenate 'list (subseq sf 0 count) st)
              sf (nthcdr count sf))
        (loop repeat count
              do (push (pop sf) st)))
    (setf (aref stacks from) sf
          (aref stacks to) st)))

(defun move-containers (stream stacks &key (preserve-order nil))
  (loop for line = (read-line stream nil nil)
        while line
        do
           (let ((op (symbols-from-string line)))
             (case (car op)
               (move
                (move-op stacks (second op) (1- (fourth op)) (1- (sixth op)) :preserve-order preserve-order))))
        finally (return stacks)))

(defun part-1 ()
  (with-input-file (in)
    (loop for s across (move-containers in (load-stacks in))
          collect (first s))))

(defun part-2 ()
  (with-input-file (in)
    (loop for s across (move-containers in (load-stacks in) :preserve-order t)
          collect (first s))))

(defun day-05 ()
  (format t "answer 1: ~A~%answer 2: ~A~%" (part-1) (part-2)))

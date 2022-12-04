(load "util.lisp")

(defpackage :day-02
  (:use :cl :util))
(in-package :day-02)

;;; map to plain responses
(defun plain-strategy (round)
  (let ((map '((x . :rock)
               (y . :paper)
               (z . :scissors))))
    (cdr (assoc (cdr round) map))))

;;; map to desired game outcomes
(defun outcome-strategy (round)
  (let* ((map '((x . ((:rock . :scissors) (:paper . :rock) (:scissors . :paper))) #| lose |#
                (y . ((:rock . :rock) (:paper . :paper) (:scissors . :scissors))) #| draw |#
                (z . ((:rock . :paper) (:paper . :scissors) (:scissors . :rock))) #| win  |#))
         (outcome (cdr (assoc (cdr round) map))))
    (cdr (assoc (car round) outcome))))
  
(defun score-round (round strategy)
  (let* ((scores '((:rock . 1)
                   (:paper . 2)
                   (:scissors . 3)))
         (their (cdr (assoc (car round) scores)))
         (my (cdr (assoc (funcall strategy round) scores))))
    (case their
      (1 (case my (1 4) (2 8) (3 3)))
      (2 (case my (1 1) (2 5) (3 9)))
      (3 (case my (1 7) (2 2) (3 6))))))

(defun read-round (line)
  (let ((start 0))
    (labels ((map-sym (sym)
               (case sym (a :rock) (b :paper) (c :scissors) (t sym)))
             (read-sym (line)
               (multiple-value-bind (sym pos) (read-from-string line nil nil :start start)
                 (setf start pos)
                 (map-sym sym))))
      (cons (read-sym line) (read-sym line)))))

(defun part-1 ()
  (with-input-file (in)
    (loop for line = (read-line in nil nil)
          while line
          sum (score-round (read-round line) #'plain-strategy))))

(defun part-2 ()
  (with-input-file (in)
    (loop for line = (read-line in nil nil)
          while line
          sum (score-round (read-round line) #'outcome-strategy))))

(defun day-02 ()
  (format t "answer 1: ~A~%answer 2: ~A~%" (part-1) (part-2)))

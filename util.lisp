(defpackage :util
  (:use :cl)
  (:export #:with-input-file
           #:symbols-from-string))
(in-package :util)

(defmacro with-input-file ((var) &body body)
  `(with-open-file (,var (concatenate 'string (string-downcase (package-name *package*)) ".input"))
     ,@body))

(defun symbols-from-string (str)
  (loop with sym and start = 0
        do (setf (values sym start) (read-from-string str nil nil :start start))
        while sym
        collect sym))

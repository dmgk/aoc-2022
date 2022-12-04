(defpackage :util
  (:use :cl)
  (:export :with-input-file))
(in-package :util)

(defmacro with-input-file ((var) &body body)
  `(with-open-file (,var (concatenate 'string (string-downcase (package-name *package*)) ".input"))
     ,@body))

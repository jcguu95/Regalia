;;;; Accessor AREF
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_aref.htm

(cl:in-package #:regalia)

(defgeneric aref (array &rest subscripts))

(defmethod aref ((array array) &rest subscripts)
  (row-major-aref array (apply #'array-row-major-index array subscripts)))

(defmethod aref (array &rest subscripts)
  (error 'type-error :datum array :expected-type 'array))

(defun (setf aref) (new-element array &rest subscripts)
  (setf (row-major-aref array (array-row-major-index array subscripts)) new-element))

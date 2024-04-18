;;;; Accessor BIT
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_bt_sb.htm

(cl:in-package #:regalia)

(defgeneric bit (bit-array &rest subscripts))

(defmethod bit ((bit-array bit-array) &rest subscripts)
  (apply #'aref bit-array subscripts))

(defmethod bit (bit-array &rest subscripts)
  (error 'type-error :datum bit-array :expected-type 'bit-array))

(defun (setf bit) (new-element bit-array &rest subscripts)
  (setf (row-major-aref bit-array (array-row-major-index bit-array subscripts)) new-element))

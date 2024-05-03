;;;; Accessor SBIT
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_bt_sb.htm

(cl:in-package #:regalia)

(defgeneric sbit (simple-bit-array &rest subscripts))

(defmethod sbit ((simple-bit-array simple-bit-array) &rest subscripts)
  (apply #'aref simple-bit-array subscripts))

(defmethod sbit (simple-bit-array &rest subscripts)
  (error 'type-error :datum simple-bit-array :expected-type 'simple-bit-array))

(defun (setf sbit) (new-element simple-bit-array &rest subscripts)
  (setf (row-major-aref simple-bit-array (array-row-major-index simple-bit-array subscripts)) new-element))

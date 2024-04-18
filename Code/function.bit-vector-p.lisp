;;;; Function BIT-VECTOR-P
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_bt_vec.htm

(cl:in-package #:regalia)

(defgeneric bit-vector-p (object))

(defmethod bit-vector-p (object)
  (typep object 'bit-vector))

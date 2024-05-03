;;;; Function SIMPLE-BIT-VECTOR-P
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_smp_bt.htm

(cl:in-package #:regalia)

(defgeneric simple-bit-vector-p (object))

(defmethod simple-bit-vector-p (object)
  (typep object 'simple-bit-vector))

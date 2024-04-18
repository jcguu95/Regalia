;;;; Function SIMPLE-VECTOR-P
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_smp_ve.htm

(cl:in-package #:regalia)

(defgeneric simple-vector-p (object))

(defmethod simple-vector-p (object)
  (typep object 'simple-vector))

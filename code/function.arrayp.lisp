;;;; Function ARRAYP
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_arrayp.htm

(cl:in-package #:regalia)

(defgeneric arrayp (object))

(defmethod arrayp (object)
  (typep object 'array))

;;;; Function VECTORP
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_vecp.htm

(cl:in-package #:regalia)

(defgeneric vectorp (object))

(defmethod vectorp (object)
  (typep object 'vector))

;;;; System Class VECTOR
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_vector.htm

(cl:in-package #:regalia)

(defgeneric fill-pointer (vector))

(defclass vector (array sequence)
  ((%fill-pointer
    :initform nil
    :initarg :fill-pointer
    :accessor fill-pointer)))

(defun vectorp (object)
  (typep object 'vector))
